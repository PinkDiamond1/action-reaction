{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module ExampleContract
  ( exampleContractScript
  , exampleContractScriptShortBs
  , CustomDatumType
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                ( serialise )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V1.Ledger.Address       as Addr
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           CheckFuncs
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}
voteValidatorHash :: PlutusV2.ValidatorHash
voteValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString [198, 130, 92, 225, 91, 182, 128, 151, 220, 205, 233, 185, 185, 188, 113, 178, 100, 203, 171, 241, 147, 82, 249, 217, 149, 139, 36, 137]

lockPid :: PlutusV2.CurrencySymbol
lockPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = createBuiltinByteString [86, 19, 166, 8, 166, 59, 78, 115, 117, 70, 118, 139, 197, 129, 162, 143, 216, 217, 215, 8, 146, 203, 180, 185, 231, 117, 154, 80] }

lockTkn :: PlutusV2.TokenName
lockTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString [97, 99, 116, 105, 111, 110, 95, 116, 111, 107, 101, 110] }

lockValue :: PlutusV2.Value
lockValue = Value.singleton lockPid lockTkn (1 :: Integer)

-------------------------------------------------------------------------------
-- | Create the delegation parameters data object.
-------------------------------------------------------------------------------
data DelegationType = DelegationType
    { dtPkh    :: PlutusV2.PubKeyHash
    -- ^ The did public key hash.
    , dtIouPid :: PlutusV2.CurrencySymbol
    -- ^ The dids iou policy id.
    }
PlutusTx.unstableMakeIsData ''DelegationType
-------------------------------------------------------------------------------
-- | Create the voting datum parameters data object.
-------------------------------------------------------------------------------
data VoteDatumType = VoteDatumType
    { vdtPid :: PlutusV2.CurrencySymbol
    -- ^ The voting token's policy id
    , vdtTkn :: PlutusV2.TokenName
    -- ^ The voting token's token name.
    , vdtAmt :: Integer
    -- ^ The voting token's threshold amount.
    }
PlutusTx.unstableMakeIsData ''VoteDatumType
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType {}
PlutusTx.unstableMakeIsData ''CustomDatumType
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = Unlock |
                          Debug -- remove in production
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Unlock, 0 )
                                                , ( 'Debug,  1 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator _ redeemer context =
  case redeemer of
    Unlock -> do 
      { let a = traceIfFalse "Vote Has Failed"       $ checkVoteFromDatum txRefInputs
      ; let b = traceIfFalse "Single Script Error"   $ isSingleScript txInputs
      ;         traceIfFalse "Unlock Endpoint Error" $ all (==True) [a,b]
      }
    Debug ->  True
   where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    -- inputs / outputs
    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs  info

    txRefInputs :: [PlutusV2.TxInInfo]
    txRefInputs = PlutusV2.txInfoReferenceInputs info

    -- get the vote datum from reference
    getReferenceDatum :: PlutusV2.TxOut -> Maybe VoteDatumType
    getReferenceDatum x =
      case PlutusV2.txOutDatum x of
        PlutusV2.NoOutputDatum       -> Nothing -- datumless
        (PlutusV2.OutputDatumHash _) -> Nothing -- embedded datum
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) ->
          case PlutusTx.fromBuiltinData d of
            Nothing     -> Nothing
            Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @VoteDatumType inline

    checkVoteFromDatum :: [PlutusV2.TxInInfo] -> Bool
    checkVoteFromDatum []     = traceIfFalse "No Datum Found on Reference Input" False
    checkVoteFromDatum (x:xs) =
      if traceIfFalse "Incorrect Validator Address" $ (PlutusV2.txOutAddress $ PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress voteValidatorHash
        then
          if traceIfFalse "Incorrect Starter Value" $ Value.geq (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) lockValue
            then
              case getReferenceDatum $ PlutusV2.txInInfoResolved x of
                Nothing -> checkVoteFromDatum xs
                Just voteDatum' -> checkReferenceSigners voteDatum' txRefInputs (Value.singleton Value.adaSymbol Value.adaToken (0 :: Integer))
            else checkVoteFromDatum xs
        else checkVoteFromDatum xs
    
    -- check if the outgoing datum has the correct form.
    checkReferenceSigners :: VoteDatumType -> [PlutusV2.TxInInfo] -> PlutusV2.Value -> Bool
    checkReferenceSigners voteDatum []     val = isVoteComplete (vdtPid voteDatum) (vdtTkn voteDatum) (vdtAmt voteDatum) info val -- input the voting token info
    checkReferenceSigners voteDatum (x:xs) val = 
      case PlutusV2.txOutDatum $ PlutusV2.txInInfoResolved x of
        PlutusV2.NoOutputDatum       -> checkReferenceSigners voteDatum xs val -- datumless
        (PlutusV2.OutputDatumHash _) -> checkReferenceSigners voteDatum xs val -- embedded datum
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData @DelegationType d of
            Nothing     -> checkReferenceSigners voteDatum xs val
            Just inline -> 
              if ContextsV2.txSignedBy info (dtPkh inline)
              then checkReferenceSigners voteDatum xs (val + (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x))
              else checkReferenceSigners voteDatum xs val

-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
validator' :: PlutusV2.Validator
validator' = PlutusV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = Utils.mkUntypedValidator mkValidator
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Scripts.Script
script = Scripts.unValidatorScript validator'

exampleContractScriptShortBs :: SBS.ShortByteString
exampleContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

exampleContractScript :: PlutusScript PlutusScriptV2
exampleContractScript = PlutusScriptSerialised exampleContractScriptShortBs
