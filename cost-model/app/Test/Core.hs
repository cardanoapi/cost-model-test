{-# LANGUAGE NumericUnderscores #-}
module Test.Core where
import Test.Common
import Cardano.Kuber.Api
import Cardano.Api 
import PlutusLedgerApi.Common
import PlutusTx
import Cardano.Kuber.Data.Parsers (parseAddressBech32)


runWithRefScript contractName validator walletAddr skey netId datum redeemer = do 
    trace ("\nTesting " ++ contractName)
    let scriptInfo = v3ScriptInfo netId validator
    trace("\taddress: " ++ (show $ serialiseAddress $ v3address scriptInfo))
    let txScript = TxScriptPlutus $ toTxPlutusScript $ v3script scriptInfo
    trace("\tscript bytes: " ++ (show $ txScriptByteSize $ txScript))
    -- trace("\nCreating Reference Script UTxO ...")
    txCreateRefScriptUtxo <- testCreateRefScriptUtxo walletAddr skey txScript
    liftIO $ reportExUnitsandFee txCreateRefScriptUtxo
    let refScriptUtxoTxId = TxIn (getTxId $ getTxBody txCreateRefScriptUtxo) (TxIx 0)
    waitTxConfirmation txCreateRefScriptUtxo 180
    -- trace("\nLocking in script address ...")
    txLock <- testLock walletAddr skey (v3script scriptInfo) datum
    waitTxConfirmation txLock 180
    let input = TxIn (getTxId $ getTxBody $ txLock) (TxIx 0)
    -- trace("\nRedeeming using reference script ...")
    txReedeem <- testRedeem walletAddr skey (v3script scriptInfo) input redeemer (Just refScriptUtxoTxId)
    liftIO $ reportExUnitsandFee txReedeem
    liftIO $ reportStrippedTxBytes txReedeem datum redeemer
    waitTxConfirmation txReedeem 180
    trace("\nPassed")
    trace("\n===========================================================================================")

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
    liftIO $ reportExUnitsandFee txReedeem
    waitTxConfirmation txReedeem 180
    trace("Passed")

testLock walletAddr sKey script datum = do
    let 
        scriptAddress = plutusScriptAddr (toTxPlutusScript $ script) (Testnet (NetworkMagic 4))
        txb = lock scriptAddress (lovelaceToValue 3_000_000) datum
            <> txWalletSignKey sKey
            <> txWalletAddress walletAddr
    runBuildAndSubmit txb

testRedeem walletAddr sKey script input redeemer refTxIn = do 
    (txin, txout) <- resolveTxIn input
    let maybeRefTxInTxb = case refTxIn of 
            Nothing -> redeem txin script redeemer 
            Just a -> redeemWithRefScript txin txout redeemer a
        
        txb =  txPayTo walletAddr (lovelaceToValue 3_000_000)
            <> txWalletSignKey sKey
            <> txWalletAddress walletAddr
            <> maybeRefTxInTxb

    runBuildAndSubmit txb

testCreateRefScriptUtxo walletAddr sKey script = do 
    let txb = txPayToWithReferenceScript walletAddr (lovelaceToValue 20_000_000) script
            <> txWalletSignKey sKey
            <> txWalletAddress walletAddr
    runBuildAndSubmit txb