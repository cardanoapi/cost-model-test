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
import V3.Spend.VerifyEcdsa qualified as VerifyECDSA
import V3.Spend.AlwaysPass qualified as AlwaysPass
import Test.Core
import Test.DummyDataTypes


main :: IO ()
main= do 
    chainInfo <- chainInfoFromEnv
    networkId <- evaluateKontract chainInfo  kGetNetworkId >>= throwFrameworkError
    sKey <-  getEnv "SIGNKEY_FILE" >>= getSignKey
    walletAddr <- Blank.getEnv "WALLET_ADDRESS" >>= (\case
        Just addrStr -> parseAddress @ConwayEra $ T.pack addrStr
        Nothing -> pure ( skeyToAddrInEra sKey networkId)
        )

    runKontract chainInfo $ testInfo "AlwaysPass" AlwaysPass.validator walletAddr sKey networkId () ()    
    -- runKontract chainInfo $ testInfo "VerifyBLS12G1" VerifyBLS12G1.validator walletAddr sKey networkId blsG1Datum blsG1Redeemer
    runKontract chainInfo $ testInfo "VerifyECDSA" VerifyECDSA.validator walletAddr sKey networkId v3VerifyEcdsaDatum v3VerifyEcdsaRedeemer
    
    