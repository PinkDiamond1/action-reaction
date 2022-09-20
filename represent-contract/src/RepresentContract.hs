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
module RepresentContract
  ( representContractScript
  , representContractScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                ( serialise )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified Plutus.V1.Ledger.Address       as Addr
import qualified Plutus.V1.Ledger.Value         as Value
import           Plutus.Script.Utils.V2.Scripts as Utils
import           CheckFuncs
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 2
-}
iouTkn :: PlutusV2.TokenName
iouTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString [105, 111, 117]}

-- voting validator hash
voteValidatorHash :: PlutusV2.ValidatorHash
voteValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString [198, 130, 92, 225, 91, 182, 128, 151, 220, 205, 233, 185, 185, 188, 113, 178, 100, 203, 171, 241, 147, 82, 249, 217, 149, 139, 36, 137]

lockPid :: PlutusV2.CurrencySymbol
lockPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = createBuiltinByteString [86, 19, 166, 8, 166, 59, 78, 115, 117, 70, 118, 139, 197, 129, 162, 143, 216, 217, 215, 8, 146, 203, 180, 185, 231, 117, 154, 80] }

lockTkn :: PlutusV2.TokenName
lockTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString [97, 99, 116, 105, 111, 110, 95, 116, 111, 107, 101, 110] }

lockValue :: PlutusV2.Value
lockValue = Value.singleton lockPid lockTkn (1 :: Integer)

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
data CustomDatumType = CustomDatumType
    { cdtPkh    :: PlutusV2.PubKeyHash
    -- ^ The did public key hash.
    , cdtIouPid :: PlutusV2.CurrencySymbol
    -- ^ The dids iou policy id.
    }
PlutusTx.unstableMakeIsData ''CustomDatumType
-- old == new
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtPkh    a == cdtPkh    b ) &&
           ( cdtIouPid a == cdtIouPid b )
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data UpdateType = UpdateType
  { updateAmt :: Integer
  -- ^ The updater's amount to increase or decrease.
  , updaterPkh  :: PlutusV2.PubKeyHash
  -- ^ The updater's public key hash.
  , updaterSc  :: PlutusV2.PubKeyHash
  -- ^ The updater's staking credential.
  }
PlutusTx.unstableMakeIsData ''UpdateType
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = Increase UpdateType |
                          Decrease UpdateType |
                          Remove
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Increase, 0 )
                                                , ( 'Decrease, 1 )
                                                , ( 'Remove,   2 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case redeemer of
    Remove -> do 
      { let a = traceIfFalse "Signing Tx Error"     $ ContextsV2.txSignedBy info (cdtPkh datum)
      ; let b = traceIfFalse "Voters Are Delegated" $ hasVotngTokens
      ; let c = traceIfFalse "Single Script Error"  $ isSingleScript txInputs && isSingleScript txRefInputs
      ;         traceIfFalse "Remove Error"         $ all (==True) [a,b,c]
      }
    (Increase ut)-> do 
      { let increase     = updateAmt ut
      ; let outboundAddr = createAddress (updaterPkh ut) (updaterSc ut)
      ; let payout       = Value.singleton (cdtIouPid datum) iouTkn increase
      ; let a = traceIfFalse "Single Script Error"  $ isSingleScript txInputs && isSingleScript txRefInputs
      ; let b = traceIfFalse "Cont Payin Error"     $ isValueIncreasing increase
      ; let c = traceIfFalse "Wrong Datum Error"    $ isDatumConstant contOutputs
      ; let d = traceIfFalse "Minting Error"        $ checkMintingProcess increase
      ; let e = traceIfFalse "Minting Payout Error" $ isAddrGettingPaid txOutputs outboundAddr payout -- can allow ada too
      ; let f = traceIfFalse "Signing Tx Error"     $ ContextsV2.txSignedBy info (updaterPkh ut)
      ;         traceIfFalse "Increase Error"       $ all (==True) [a,b,c,d,e,f]
      }
    (Decrease ut)-> do 
      { let decrease     = updateAmt ut
      ; let outboundAddr = createAddress (updaterPkh ut) (updaterSc ut)
      ; let a = traceIfFalse "Single Script Error" $ isSingleScript txInputs && isSingleScript txRefInputs
      ; let b = traceIfFalse "Cont Payin Error"    $ isValueDecreasing decrease
      ; let c = traceIfFalse "Wrong Datum Error"   $ isDatumConstant contOutputs
      ; let d = traceIfFalse "Burning Error"       $ checkMintingProcess ((-1 :: Integer) * decrease)
      ; let e = traceIfFalse "FT Payout Error"     $ isVotingTokenReturning outboundAddr decrease -- can allow ada too
      ; let f = traceIfFalse "Signing Tx Error"    $ ContextsV2.txSignedBy info (updaterPkh ut)
      ;         traceIfFalse "Decrease Error"      $ all (==True) [a,b,c,d,e,f]
      }
   where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    contOutputs :: [PlutusV2.TxOut]
    contOutputs = ContextsV2.getContinuingOutputs context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs info

    txOutputs :: [PlutusV2.TxOut]
    txOutputs = PlutusV2.txInfoOutputs info

    txRefInputs :: [PlutusV2.TxInInfo]
    txRefInputs = PlutusV2.txInfoReferenceInputs info

    -- handles mint and burn
    checkMintingProcess :: Integer -> Bool
    checkMintingProcess amt =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tkn, amt')] -> (cs == cdtIouPid datum) && (tkn == iouTkn) && (amt' == amt)
        _                 -> traceIfFalse "Nothing is Minting" False
    
    isDatumConstant :: [PlutusV2.TxOut] -> Bool
    isDatumConstant []     = False
    isDatumConstant (x:xs) =
      case PlutusV2.txOutDatum x of
        PlutusV2.NoOutputDatum       -> isDatumConstant xs -- datumless
        (PlutusV2.OutputDatumHash _) -> isDatumConstant xs -- embedded datum
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> isDatumConstant xs
            Just inline -> datum == inline
        
    getVoteValue :: (PlutusV2.CurrencySymbol, PlutusV2.TokenName, Integer) -> Integer
    getVoteValue (_, _, amt) = amt

    createNewValue :: (PlutusV2.CurrencySymbol, PlutusV2.TokenName, Integer) -> Integer -> PlutusV2.Value
    createNewValue (cs, tkn, _) amt = Value.singleton cs tkn amt

    isVotingTokenReturning :: PlutusV2.Address -> Integer -> Bool
    isVotingTokenReturning addr amt = 
      case checkForVoteTokens txRefInputs of
        Nothing        -> False
        Just tokenInfo -> isAddrGettingPaid txOutputs addr (createNewValue tokenInfo amt)
    
    isValueIncreasing :: Integer -> Bool
    isValueIncreasing amt =
      case checkForVoteTokens txRefInputs of
        Nothing        -> False
        Just tokenInfo -> isValueContinuing contOutputs (validatingValue + createNewValue tokenInfo amt)
    
    isValueDecreasing :: Integer -> Bool
    isValueDecreasing amt =
      case checkForVoteTokens txRefInputs of
        Nothing        -> False
        Just tokenInfo -> isValueContinuing contOutputs (validatingValue - createNewValue tokenInfo amt)
    
    hasVotngTokens :: Bool
    hasVotngTokens =
      case checkForVoteTokens txRefInputs of
        Nothing        -> traceIfFalse "Does not have voting tokens" False
        Just tokenInfo -> getVoteValue tokenInfo == (0 :: Integer)
    
    checkForVoteTokens :: [PlutusV2.TxInInfo] -> Maybe (PlutusV2.CurrencySymbol, PlutusV2.TokenName, Integer)
    checkForVoteTokens []     = Nothing
    checkForVoteTokens (x:xs) =
      if traceIfFalse "Incorrect Address" $ (PlutusV2.txOutAddress $ PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress voteValidatorHash
        then
          if traceIfFalse "Incorrect Value" $ Value.geq (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) lockValue
            then
              case getReferenceDatum $ PlutusV2.txInInfoResolved x of
                Nothing        -> checkForVoteTokens xs
                Just voteDatum -> Just $ ((vdtPid voteDatum), (vdtTkn voteDatum), (voterTokenValue totalValue (vdtPid voteDatum) (vdtTkn voteDatum)))
            else checkForVoteTokens xs
        else checkForVoteTokens xs

    getReferenceDatum :: PlutusV2.TxOut -> Maybe VoteDatumType
    getReferenceDatum x =
      case PlutusV2.txOutDatum x of
        PlutusV2.NoOutputDatum       -> Nothing -- datumless
        (PlutusV2.OutputDatumHash _) -> Nothing -- embedded datum
        -- inline datum
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> --Just $ PlutusTx.unsafeFromBuiltinData @VoteDatumType d
          case PlutusTx.fromBuiltinData d of
            Nothing     -> Nothing
            Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @VoteDatumType inline
        

    -- token info
    voterTokenValue :: PlutusV2.Value -> PlutusV2.CurrencySymbol -> PlutusV2.TokenName -> Integer
    voterTokenValue totalValue' cur tkn = Value.valueOf totalValue' cur tkn

    totalValue :: PlutusV2.Value
    totalValue = ContextsV2.valueSpent info

    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate." -- This error should never be hit.
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input
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

representContractScriptShortBs :: SBS.ShortByteString
representContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

representContractScript :: PlutusScript PlutusScriptV2
representContractScript = PlutusScriptSerialised representContractScriptShortBs
