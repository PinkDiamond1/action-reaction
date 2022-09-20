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
{-# LANGUAGE NumericUnderscores    #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module MintingContract
  ( mintingPlutusScript
  , mintingScriptShortBs
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
-- import           TokenHelper
{-
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}
lockPid :: PlutusV2.CurrencySymbol
lockPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = createBuiltinByteString [172, 219, 54, 158, 35, 235, 107, 196, 81, 223, 113, 35, 87, 20, 88, 25, 116, 123, 102, 201, 18, 5, 212, 88, 86, 134, 47, 130] }

lockTkn :: PlutusV2.TokenName
lockTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString [97, 99, 116, 105, 111, 110, 95, 116, 111, 107, 101, 110] }

tokenValue :: PlutusV2.Value
tokenValue = Value.singleton lockPid lockTkn (1 :: Integer)

getValidatorHash :: PlutusV2.ValidatorHash
getValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString [96, 148, 195, 168, 20, 155, 33, 15, 27, 151, 3, 198, 56, 63, 178, 94, 68, 66, 2, 161, 224, 122, 100, 225, 241, 173, 237, 47]

-------------------------------------------------------------------------------
-- | Create a proper bytestring
-------------------------------------------------------------------------------
createBuiltinByteString :: [Integer] -> PlutusV2.BuiltinByteString
createBuiltinByteString intList = flattenBuiltinByteString [ consByteString x emptyByteString | x <- intList]
  where
    flattenBuiltinByteString :: [PlutusV2.BuiltinByteString] -> PlutusV2.BuiltinByteString
    flattenBuiltinByteString []     = emptyByteString 
    flattenBuiltinByteString (x:xs) = appendByteString x (flattenBuiltinByteString xs)
-------------------------------------------------------------------------------
-- | Create the redeemer data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = CustomRedeemerType
  { crtPolicyId :: PlutusV2.CurrencySymbol
  -- ^ The policy id from the minting script.
  , crtNumber  :: Integer
  -- ^ The reward counter, starts a zero.
  }
PlutusTx.unstableMakeIsData ''CustomRedeemerType

-- old == new
instance Eq CustomRedeemerType where
  {-# INLINABLE (==) #-}
  a == b = ( crtPolicyId a == crtPolicyId b ) &&
           ( crtNumber   a == crtNumber   b )
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy _ context = do
      { let a = traceIfFalse "Mint / Burn Error" $ (checkTokenMint && checkMintDatum) -- can only mint
      ;         traceIfFalse "Minting Error"     $ all (==True) [a]
      }
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = ContextsV2.txInfoInputs info
    
    -- check the minting stuff here
    checkTokenMint :: Bool
    checkTokenMint =
      case Value.flattenValue (PlutusV2.txInfoMint info) of
        [(cs, tkn, _)] -> cs == ContextsV2.ownCurrencySymbol context && tkn == lockTkn
        _              -> traceIfFalse "Mint Error" False
    
    -- check that the locking script has the correct input and output datum hash
    checkMintDatum :: Bool
    checkMintDatum =
      case checkInputs txInputs of
        Nothing -> traceIfFalse "No Input Datum" False
        Just inputDatum  ->
          let nextDatum = CustomRedeemerType  { crtPolicyId = crtPolicyId inputDatum
                                              , crtNumber   = (crtNumber  inputDatum) + 1
                                              }
          in case datumAtValidator of
            Nothing          -> traceIfFalse "No Output Datum" False
            Just outputDatum -> nextDatum == outputDatum
    
    -- return the first datum hash from a txout going to the locking script
    checkInputs :: [PlutusV2.TxInInfo] -> Maybe CustomRedeemerType
    checkInputs [] = Nothing
    checkInputs (x:xs) =
      if PlutusV2.txOutAddress (PlutusV2.txInInfoResolved x) == Addr.scriptHashAddress getValidatorHash
        then getDatumFromTxOut $ PlutusV2.txInInfoResolved x
        else checkInputs xs
      where
        -- check if the incoming datum is the correct form.
        getDatumFromTxOut :: PlutusV2.TxOut -> Maybe CustomRedeemerType
        getDatumFromTxOut x' = 
          case PlutusV2.txOutDatum x' of
            PlutusV2.NoOutputDatum       -> Nothing -- datumless
            (PlutusV2.OutputDatumHash _) -> Nothing -- embedded datum
            -- inline datum
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> Nothing
                Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType inline

    -- get the datum at the lock validator
    datumAtValidator :: Maybe CustomRedeemerType
    datumAtValidator =
      if length scriptOutputs == 0 
        then Nothing
        else 
          let datumAtValidator' = fst $ head scriptOutputs
          in case datumAtValidator' of
            PlutusV2.NoOutputDatum       -> Nothing -- datumless
            (PlutusV2.OutputDatumHash _) -> Nothing -- embedded datum
            -- inline datum
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> Nothing
                Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomRedeemerType inline
      where 
        scriptOutputs :: [(PlutusV2.OutputDatum, PlutusV2.Value)]
        scriptOutputs = ContextsV2.scriptOutputsAt getValidatorHash info

-------------------------------------------------------------------------------
policy :: PlutusV2.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Utils.mkUntypedMintingPolicy mkPolicy

plutusScript :: Scripts.Script
plutusScript = PlutusV2.unMintingPolicyScript policy

validator :: PlutusV2.Validator
validator = PlutusV2.Validator plutusScript

scriptAsCbor :: LBS.ByteString
scriptAsCbor = serialise validator

mintingPlutusScript :: PlutusScript PlutusScriptV2
mintingPlutusScript = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LBS.toStrict $ scriptAsCbor