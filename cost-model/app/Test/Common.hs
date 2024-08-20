{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}
module Test.Common where
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import Cardano.Api
import Cardano.Kuber.Api
import Data.ByteString.Short (ShortByteString)
import qualified PlutusTx
import PlutusLedgerApi.V3 (serialiseCompiledCode)
import Cardano.Api.Shelley
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Control.Concurrent as Control
import qualified Data.Map as Map
import Cardano.Kuber.Util (dataToScriptData)
import qualified Data.Text.IO as T
import Cardano.Kuber.Data.Parsers
import System.Environment
import qualified Debug.Trace as Debug
import qualified Cardano.Ledger.Shelley.Core as L
import Control.Lens ((^.))
import qualified Cardano.Ledger.Alonzo.TxWits as L
import qualified Cardano.Ledger.Alonzo.Scripts as L
import qualified Cardano.Api.Ledger as L
import qualified Data.ByteString as BS

data V3ScriptInfo = V3ScriptInfo
    { v3address :: Address ShelleyAddr
    , v3script :: Script PlutusScriptV3
    , v3hash :: ScriptHash
    , v3sbs :: ShortByteString
    }

v3ScriptInfo :: 
    NetworkId 
    -> PlutusTx.CompiledCode a 
    -> V3ScriptInfo
v3ScriptInfo netId compiledCode = do
    let sbs = serialiseCompiledCode compiledCode
        script = PlutusScript PlutusScriptV3 $ PlutusScriptSerialised $ sbs
        hash = hashScript script
        address = makeAddressWithStake (Right hash) Nothing netId
        info = V3ScriptInfo address script hash sbs
    info


runBuildAndSubmit :: (HasKuberAPI api, HasSubmitApi api) => TxBuilder -> Kontract api w FrameworkError (Tx ConwayEra)
runBuildAndSubmit txBuilder =  do 
        tx<- kBuildTx txBuilder     
        kSubmitTx (InAnyCardanoEra ConwayEra tx) 
        liftIO $ putStrLn $ "Tx Submitted :" ++  (getTxIdFromTx tx)
        pure tx
    where 
        getTxIdFromTx :: Tx ConwayEra -> String
        getTxIdFromTx tx = T.unpack $ serialiseToRawBytesHexText $ getTxId $ getTxBody tx

makeAddressWithStake ::
    Either (C.VerificationKey C.PaymentKey) C.ScriptHash ->
    Maybe (C.VerificationKey C.StakeKey) ->
    C.NetworkId ->
    C.Address C.ShelleyAddr
makeAddressWithStake (Left paymentKey) mStakeVKey nId =
    C.makeShelleyAddress
        nId
        (C.PaymentCredentialByKey $ C.verificationKeyHash paymentKey)
        ( case mStakeVKey of
            Nothing -> C.NoStakeAddress
            Just stakeVKey -> C.StakeAddressByValue $ C.StakeCredentialByKey $ C.verificationKeyHash stakeVKey
        )
makeAddressWithStake (Right scriptHash) _mStakeVKey nId =
    C.makeShelleyAddress nId (C.PaymentCredentialByScript scriptHash) C.NoStakeAddress

lock :: PlutusTx.ToData a1 => AddressInEra ConwayEra -> Value -> a1 -> TxBuilder
lock addr value datum = txPayToScriptWithData addr value (hashData datum)

redeem :: (IsPlutusScript sc, PlutusTx.ToData a1) => TxIn -> sc -> a1 -> TxBuilder
redeem txIn script redeemer = txRedeemTxin txIn script (hashData redeemer) Nothing

waitTxConfirmation :: HasChainQueryAPI a => Tx ConwayEra -> Integer
      -> Kontract a w FrameworkError ()
waitTxConfirmation tx totalWaitSecs =
    let txId = getTxId$ getTxBody tx
    in waitTxId txId  totalWaitSecs
  where
    waitTxId txId remainingSecs =
      if remainingSecs < 0
        then kError TxSubmissionError $ "Transaction not confirmed after  " ++ show totalWaitSecs ++ " secs"
        else do
          (UTxO uMap):: UTxO ConwayEra <- kQueryUtxoByTxin $  Set.singleton (TxIn txId (TxIx 0))
          liftIO $ Control.threadDelay 2_000_000
          case Map.toList uMap of
            [] -> waitTxId txId (remainingSecs - 2)
            _ -> pure ()

hashData a = unsafeHashableScriptData $ dataToScriptData a

runKontract :: api -> Kontract api w FrameworkError v-> IO ()
runKontract api  c = do 
    evaluateKontract api  c
     >>= \case
        Left e -> putStrLn $ show e
        _ -> pure ()

getSignKey :: [Char] -> IO (SigningKey PaymentKey)
getSignKey skeyfile =
  getPath >>=  T.readFile  >>= parseSignKey
  where
  getPath = if not (null skeyfile) && head skeyfile == '~'
                          then (do
                            home <- getEnv "HOME"
                            pure  $ home ++  drop 1 skeyfile
                            )
                          else pure skeyfile

trace str = liftIO $ putStrLn str  

reportExUnitsandFee:: Tx ConwayEra -> IO ()
reportExUnitsandFee tx = case tx of
  ShelleyTx era ledgerTx -> let
    txWitnesses = ledgerTx ^. L.witsTxL
    sizeLedger = ledgerTx ^. L.sizeTxF
    sizeCapi = fromIntegral $  BS.length  $ serialiseToCBOR tx
    -- this should be exUnits of single script involved in the transaction
    exUnits = map snd $ map snd $  Map.toList $ L.unRedeemers $  txWitnesses ^. L.rdmrsTxWitsL
    txFee=L.unCoin $ ledgerTx ^. L.bodyTxL ^. L.feeTxBodyL
    in do
      (euMem,euCpu) <-case exUnits of
            [eunit]-> let eu = L.unWrapExUnits eunit
                          (mem,cpu) =   (L.exUnitsMem' eu,L.exUnitsSteps' eu)
                      in do
                        putStrLn $  "ExUnits     :  memory = " ++ show mem ++ " cpu = " ++ show cpu
                        pure (toInteger mem, toInteger cpu)
            _       -> pure  (0,0)
      putStrLn $  "Fee      :   " ++ show txFee
      if sizeLedger /= sizeCapi
        then do
          putStrLn $  "Tx Bytes (ledger):   " ++ show sizeLedger
          putStrLn $  "Tx Bytes (api)   :   " ++ show sizeCapi
        else
          putStrLn $  "Tx Bytes  :   " ++ show sizeCapi