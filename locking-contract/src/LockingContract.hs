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
module LockingContract
  ( lockingContractScript
  , lockingContractScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                ( serialise )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           CheckFuncs
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}

lockPid :: PlutusV2.CurrencySymbol
lockPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = createBuiltinByteString [86, 19, 166, 8, 166, 59, 78, 115, 117, 70, 118, 139, 197, 129, 162, 143, 216, 217, 215, 8, 146, 203, 180, 185, 231, 117, 154, 80] }

lockTkn :: PlutusV2.TokenName
lockTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString [97, 99, 116, 105, 111, 110, 95, 116, 111, 107, 101, 110] }

lockValue :: PlutusV2.Value
lockValue = Value.singleton lockPid lockTkn (1 :: Integer)

-- token name for currency
tokenName :: PlutusV2.TokenName
tokenName = lockTkn

-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
  { cdtPolicyId :: PlutusV2.CurrencySymbol
  -- ^ The policy id from the minting script.
  , cdtNumber   :: Integer
  -- ^ The starting number for the catalog.
  }
PlutusTx.unstableMakeIsData ''CustomDatumType

checkDatumIncrease :: CustomDatumType -> CustomDatumType -> Bool
checkDatumIncrease a b =  ( cdtPolicyId    a == cdtPolicyId b ) &&
                          ( cdtNumber  a + 1 == cdtNumber   b )

-- old === new | burning
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b =  ( cdtPolicyId a == cdtPolicyId b ) &&
            ( cdtNumber   a == cdtNumber   b )
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = Mint  |
                          Debug
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Mint,  0 )
                                                , ( 'Debug, 1 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case redeemer of
    Mint -> do
      { let a = traceIfFalse "Single In/Out Error" $ isNInputs txInputs 1 && isNOutputs contOutputs 1 -- 1 script input 1 script output
      ; let b = traceIfFalse "NFT Minting Error"   $ checkMintedAmount                                -- mint an nft only
      ; let c = traceIfFalse "Invalid Datum Error" $ isEmbeddedDatumIncreasing contOutputs            -- value is cont and the datum is correct.
      ; let d = traceIfFalse "Invalid Start Token" $ Value.geq validatingValue lockValue              -- must contain the start token
      ;         traceIfFalse "Locking:Mint Error"  $ all (==True) [a,b,c,d]
      }
    Debug -> True
   where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo  context

    contOutputs :: [PlutusV2.TxOut]
    contOutputs = ContextsV2.getContinuingOutputs context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs  info

    -- token info
    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate." -- This error should never be hit.
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input

    -- minting stuff
    checkMintedAmount :: Bool
    checkMintedAmount =
      let reward = rewardAmount $ cdtNumber datum 
      in case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tkn, amt)] -> (cs == cdtPolicyId datum) && (tkn == tokenName) && (amt == reward)
        _                -> traceIfFalse "Incorrect Minting Info" False
    
    -- datum stuff
    isEmbeddedDatumIncreasing :: [PlutusV2.TxOut] -> Bool
    isEmbeddedDatumIncreasing []     = traceIfFalse "No Increasing Datum Found" False
    isEmbeddedDatumIncreasing (x:xs) =
      if PlutusV2.txOutValue x == validatingValue -- strict value continue
        then
          case PlutusV2.txOutDatum x of
            PlutusV2.NoOutputDatum       -> isEmbeddedDatumIncreasing xs -- datumless
            (PlutusV2.OutputDatumHash _) -> isEmbeddedDatumIncreasing xs -- embedded datum
            -- inline datum only
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> isEmbeddedDatumIncreasing xs -- bad data
                Just inline -> checkDatumIncrease datum inline
        else isEmbeddedDatumIncreasing xs

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

lockingContractScriptShortBs :: SBS.ShortByteString
lockingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

lockingContractScript :: PlutusScript PlutusScriptV2
lockingContractScript = PlutusScriptSerialised lockingContractScriptShortBs