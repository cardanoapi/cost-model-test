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
import V3.Spend.VerifyEd25519 qualified as VerifyEd25519
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

    runKontract chainInfo $ runWithRefScript "AlwaysPass" AlwaysPass.validator walletAddr sKey networkId () ()
    runKontract chainInfo $ runWithRefScript "VerifyBLS12G1" VerifyBLS12G1.validator walletAddr sKey networkId blsG1Datum blsG1Redeemer
    runKontract chainInfo $ runWithRefScript "VerifyBLS12G2" VerifyBLS12G2.validator walletAddr sKey networkId blsG2Datum blsG2Redeemer
    runKontract chainInfo $ runWithRefScript "VerifyECDSA" VerifyECDSA.validator walletAddr sKey networkId v3VerifyEcdsaDatum v3VerifyEcdsaRedeemer
    runKontract chainInfo $ runWithRefScript "VerifyEd25519" VerifyEd25519.validator walletAddr sKey networkId v3VerifyEd25519Datum v3VerifyEd25519Redeemer
    runKontract chainInfo $ runWithRefScript "VerifyKeccak" (VerifyKeccak.validator v3VerifyKeccakParameter) walletAddr sKey networkId v3VerifyKeccakDatum v3VerifyKeccakRedeemer
    runKontract chainInfo $ runWithRefScript "VerifySchnorr" VerifySchnorr.validator walletAddr sKey networkId v3VerifySchnorrDatum v3VerifySchnorrRedeemer    
    runKontract chainInfo $ runWithRefScript "VerifyBlake2b224" VerifyBlake2b224.validator walletAddr sKey networkId v3VerifyBlake2b224Datum v3VerifyBlake2b224Redeemer    


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