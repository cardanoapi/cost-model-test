{-# LANGUAGE NumericUnderscores #-}
module Test.Core where
import Test.Common
import Cardano.Kuber.Api
import Cardano.Api 
import PlutusLedgerApi.Common
import PlutusTx
import Cardano.Kuber.Data.Parsers (parseAddressBech32)


testInfo contractName validator walletAddr skey netId datum redeemer = do 
    trace ("Testing " ++ contractName)
    let scriptInfo = v3ScriptInfo netId validator
    trace("\taddress: " ++ (show $ serialiseAddress $ v3address scriptInfo))
    trace("Locking ...")
    txLock <- testLock walletAddr skey netId validator datum
    waitTxConfirmation txLock 180
    let input = TxIn (getTxId $ getTxBody $ txLock) (TxIx 0)
    trace("Redeeming ...")
    txReedeem <- testRedeem walletAddr skey netId validator input redeemer
    liftIO $ reportExUnitsandFee txReedeem
    waitTxConfirmation txReedeem 180
    trace("Passed")

testLock walletAddr sKey netId validator datum = do
    let scriptInfo = v3ScriptInfo netId validator
        scriptAddress = plutusScriptAddr (toTxPlutusScript $ v3script scriptInfo) netId
        txb = lock scriptAddress (lovelaceToValue 3_000_000) datum
            <> txWalletSignKey sKey
            <> txWalletAddress walletAddr
    runBuildAndSubmit txb

testRedeem walletAddr sKey netId validator input redeemer = do 
    let scriptInfo = v3ScriptInfo netId validator
        txb = redeem input (v3script scriptInfo) redeemer
            <> txPayTo walletAddr (lovelaceToValue 3_000_000)
            <> txWalletSignKey sKey
            <> txWalletAddress walletAddr
    runBuildAndSubmit txb