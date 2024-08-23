{-# LANGUAGE NumericUnderscores #-}
module Test.Core where
import Test.Common
import Cardano.Kuber.Api
import Cardano.Api 
import PlutusLedgerApi.Common
import PlutusTx
import Cardano.Kuber.Data.Parsers (parseAddressBech32)

data BytesAndFeeRecord = BytesAndFeeRecord {
    scriptBytes :: Int,
    fee :: Int
} deriving Show

runWithRefScript contractName validator walletAddr skey netId datum redeemer = do 
    trace ("\nTesting " ++ contractName)
    let scriptInfo = v3ScriptInfo netId validator
    trace("\taddress: " ++ (show $ serialiseAddress $ v3address scriptInfo))
    let txScript = TxScriptPlutus $ toTxPlutusScript $ v3script scriptInfo
        scriptBytes = txScriptByteSize $ txScript
    trace("\tscript bytes: " ++ (show scriptBytes ))
    trace("\nCreating Reference Script...")
    txCreateRefScriptUtxo <- testCreateRefScriptUtxo walletAddr skey txScript
    let refScriptUtxoTxId = TxIn (getTxId $ getTxBody txCreateRefScriptUtxo) (TxIx 0)
    waitTxConfirmation txCreateRefScriptUtxo 180
    trace("\nLocking...")
    txLock <- testLock walletAddr skey (v3script scriptInfo) datum
    waitTxConfirmation txLock 180
    let input = TxIn (getTxId $ getTxBody $ txLock) (TxIx 0)
    trace("\nRedeeming with reference script...")
    txReedeem <- testRedeem walletAddr skey (v3script scriptInfo) input redeemer (Just refScriptUtxoTxId)
    let fee = reportExUnitsandFee txReedeem
    waitTxConfirmation txReedeem 180
    trace("\nPassed")
    trace("\n===========================================================================================")
    return $ BytesAndFeeRecord scriptBytes fee

testInfo contractName validator walletAddr skey netId datum redeemer = do 
    trace ("Testing " ++ contractName)
    let scriptInfo = v3ScriptInfo netId validator
    trace("\taddress: " ++ (show $ serialiseAddress $ v3address scriptInfo))
    trace("Locking ...")
    txLock <- testLock walletAddr skey (v3script scriptInfo) datum
    waitTxConfirmation txLock 180
    let input = TxIn (getTxId $ getTxBody $ txLock) (TxIx 0)
    trace("Redeeming ...")
    txReedeem <- testRedeem walletAddr skey (v3script scriptInfo) input redeemer Nothing
    waitTxConfirmation txReedeem 180
    trace("Passed")

testLock walletAddr sKey script datum = do
    let 
        scriptAddress = plutusScriptAddr (toTxPlutusScript $ script) (Testnet (NetworkMagic 4))
        txb = lock scriptAddress (lovelaceToValue 20_000_000) datum
            <> txWalletSignKey sKey
            <> txWalletAddress walletAddr
    runBuildAndSubmit txb

testRedeem walletAddr sKey script input redeemer refTxIn = do 
    (txin, txout) <- resolveTxIn input
    let maybeRefTxInTxb = case refTxIn of 
            Nothing -> redeem txin script redeemer 
            Just a -> redeemWithRefScript txin txout redeemer a
        
        txb =  txPayTo walletAddr (lovelaceToValue 20_000_000)
            <> txWalletSignKey sKey
            <> txWalletAddress walletAddr
            <> maybeRefTxInTxb

    runBuildAndSubmit txb

testCreateRefScriptUtxo walletAddr sKey script = do 
    let txb = txPayToWithReferenceScript walletAddr (lovelaceToValue 20_000_000) script
            <> txWalletSignKey sKey
            <> txWalletAddress walletAddr
    runBuildAndSubmit txb