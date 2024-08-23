{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Main where
import Cardano.Api
import Test.Common
import System.Environment
import System.Environment.Blank qualified as Blank
import Cardano.Kuber.Data.Parsers
import Data.Text qualified as T
import Cardano.Kuber.Util
import Cardano.Kuber.Api
import V3.Spend.VerifyBLS12G1 qualified as VerifyBLS12G1
import V3.Spend.VerifyBLS12G2 qualified as VerifyBLS12G2
import V3.Spend.VerifyEcdsa qualified as VerifyECDSA
import V3.Spend.VerifyEcdsaTrace qualified as VerifyEcdsaTrace
import V3.Spend.VerifyEd25519 qualified as VerifyEd25519
import V3.Spend.VerifyEd25519Trace qualified as VerifyEd25519Trace
import V3.Spend.VerifyKeccak qualified as VerifyKeccak
import V3.Spend.VerifySchnorr qualified as VerifySchnorr
import V3.Spend.AlwaysPass qualified as AlwaysPass
import V3.Spend.VerifyBlake2b224 as VerifyBlake2b224
import Test.Core
import Test.DummyDataTypes
import Data.Time.Clock (getCurrentTime)
import Data.Time (formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing)
import GHC.IO.Handle
import GHC.IO.StdHandles
import GHC.IO.IOMode
import GHC.Real

main :: IO ()
main= do 
    currentTime <- getCurrentTime
    
    let dateStr = formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" currentTime
        reportDir = "./test-reports"
        logFileName =  dateStr ++ "-transaction-bench" ++ ".log"
    createDirectoryIfMissing True reportDir
    logFile <- openFile (reportDir ++ "/" ++ logFileName) WriteMode
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    hDuplicateTo logFile stdout
    hDuplicateTo logFile stderr
    hSetBuffering logFile LineBuffering

    chainInfo <- chainInfoFromEnv
    networkId <- evaluateKontract chainInfo  kGetNetworkId >>= throwFrameworkError
    sKey <-  getEnv "SIGNKEY_FILE" >>= getSignKey
    walletAddr <- Blank.getEnv "WALLET_ADDRESS" >>= (\case
        Just addrStr -> parseAddress @ConwayEra $ T.pack addrStr
        Nothing -> pure ( skeyToAddrInEra sKey networkId)
        )

    -- record for C1
    verifyEcdsaUntraced <- evaluateKontract chainInfo 
        (runWithRefScript "VerifyECDSA" VerifyECDSA.validator walletAddr sKey networkId v3VerifyEcdsaDatum v3VerifyEcdsaRedeemer) 
        >>= throwFrameworkError
    
    verifyEcdsaTraced <- evaluateKontract chainInfo 
        (runWithRefScript "VerifyECDSATrace" VerifyEcdsaTrace.validator walletAddr sKey networkId v3VerifyEcdsaDatum v3VerifyEcdsaRedeemer) 
        >>= throwFrameworkError
    
    let deltaFeeC1 = fee verifyEcdsaTraced - fee verifyEcdsaUntraced
        deltaBytesC1 = scriptBytes verifyEcdsaTraced - scriptBytes verifyEcdsaUntraced
        expectedC1 = deltaBytesC1 * 15 
        observedC1 = deltaFeeC1
        traceFeeC1 = observedC1 - expectedC1

    -- record for C2 
    verifyEd25519Untraced <- evaluateKontract chainInfo 
        (runWithRefScript "VerifyEd25519" VerifyEd25519.validator walletAddr sKey networkId v3VerifyEd25519Datum v3VerifyEd25519Redeemer) 
        >>= throwFrameworkError
    
    verifyEd25519Traced <- evaluateKontract chainInfo 
        (runWithRefScript "VerifyEd25519Trace" VerifyEd25519Trace.validator walletAddr sKey networkId v3VerifyEd25519Datum v3VerifyEd25519Redeemer) 
        >>= throwFrameworkError
    
    let deltaFeeC2 = fee verifyEd25519Traced - fee verifyEd25519Untraced
        deltaBytesC2 = scriptBytes verifyEd25519Traced - scriptBytes verifyEd25519Untraced
        expectedC2 = deltaBytesC2 * 15
        observedC2 = deltaFeeC2
        traceFeeC2 = observedC2 - expectedC2

    liftIO $ putStrLn "\n------------------------------------------------------------------------------------------------------------------------------"
    liftIO $ putStrLn ("VerifyECDSA: " ++ show verifyEcdsaUntraced)
    liftIO $ putStrLn ("VerifyECDSATrace: " ++ show verifyEcdsaTraced)
    liftIO $ putStrLn ("Expected cost increase due to ref script: " ++ (show expectedC1) ++ " Lovelace")
    liftIO $ putStrLn ("Observed cost increase: " ++ (show observedC1))
    liftIO $ putStrLn ("Trace implementation cost in VerifyECDSA (observed - expected) : " ++ (show traceFeeC1))
    liftIO $ putStrLn "\n------------------------------------------------------------------------------------------------------------------------------"
    liftIO $ putStrLn ("VerifyEd25519: " ++ show verifyEd25519Untraced)
    liftIO $ putStrLn ("VerifyEd25519Trace: " ++ show verifyEd25519Traced)
    liftIO $ putStrLn ("Expected cost increase due to ref script: " ++ (show expectedC2) ++ " Lovelace")
    liftIO $ putStrLn ("Observed cost increase: " ++ (show observedC2))
    liftIO $ putStrLn ("Trace implementation cost in VerifyEd25519 (observed - expected) : " ++ (show traceFeeC2))
    liftIO $ putStrLn "\n------------------------------------------------------------------------------------------------------------------------------"
    if traceFeeC1 == traceFeeC2 
        then liftIO $ putStrLn "minFeeRefScriptCostPerByte Verified"
        else liftIO $ putStrLn "minFeeRefScriptCostPerByte Failed" 

tierRefScriptFee :: Integer -> Integer
tierRefScriptFee = go 0 minFeeRefScriptCostPerByte
  where
    go acc curTierPrice n
      | n < sizeIncrement =
          floor (acc + (n % 1) * curTierPrice)
      | otherwise =
          let acc' = acc + curTierPrice * (sizeIncrement % 1)
           in go acc' (multiplier * curTierPrice) (n - sizeIncrement)
    sizeIncrement = 25600
    multiplier = 1.2
    minFeeRefScriptCostPerByte = 15