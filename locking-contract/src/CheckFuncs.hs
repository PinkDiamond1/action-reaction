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
module CheckFuncs
  ( isNInputs
  , isNOutputs
  , createBuiltinByteString
  , rewardAmount
  , rewardCalculation
  , totalBlocks
  , totalRewards
  ) where
import qualified Plutus.V2.Ledger.Api       as PlutusV2
import qualified PlutusTx.Builtins.Internal as Internal
import           PlutusTx.Prelude
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 1
-}
-------------------------------------------------------------------------------
-- | Calculate total rewards and total blocks(mints).
-------------------------------------------------------------------------------
-- count how many rewards per utxo
totalRewards :: Integer -> Integer
totalRewards mul = totalRewards' 1 0
  where
    totalRewards' :: Integer -> Integer -> Integer
    totalRewards' num total =
      if reward < 1000000
        then Internal.divideInteger (mul*total) 1000000
        else totalRewards' (num + 1) (total + reward)
      where
        reward :: Integer
        reward = rewardCalculation num

-- count how many blocks per utxo
totalBlocks :: Integer -> Integer
totalBlocks mul = totalBlocks' 1 0
  where
    totalBlocks' :: Integer -> Integer -> Integer
    totalBlocks' num total =
      if reward < 1000000
        then mul*total
        else totalBlocks' (num + 1) (total + 1)
      where
        reward :: Integer
        reward = rewardCalculation num

-- this just calculates a reward
rewardCalculation :: Integer -> Integer
rewardCalculation num = 18000000 - logOfXInBaseB num 2 * 1000000
  where
    logOfXInBaseB :: Integer -> Integer -> Integer
    logOfXInBaseB x b =
      if x < b
        then 0
        else 1 + logOfXInBaseB (Internal.divideInteger x b) b

-- this is what is used to the correct reward amount
rewardAmount :: Integer -> Integer
rewardAmount block =
  if reward < 1000000
    then 0
    else reward
  where
    reward :: Integer
    reward = rewardCalculation block
-------------------------------------------------------------------------
-- | Creates a proper BuiltinByteString.
-------------------------------------------------------------------------
createBuiltinByteString :: [Integer] -> PlutusV2.BuiltinByteString
createBuiltinByteString intList = flattenBuiltinByteString [ consByteString x emptyByteString | x <- intList]
  where
    -- | Appends two bytestrings together from a list, element by element
    flattenBuiltinByteString :: [PlutusV2.BuiltinByteString] -> PlutusV2.BuiltinByteString
    flattenBuiltinByteString []     = emptyByteString 
    flattenBuiltinByteString (x:xs) = appendByteString x (flattenBuiltinByteString xs)
-------------------------------------------------------------------------------
-- | Force a number of inputs to have datums
-------------------------------------------------------------------------------
isNInputs :: [PlutusV2.TxInInfo] -> Integer -> Bool
isNInputs utxos number = loopInputs utxos 0
  where
    loopInputs :: [PlutusV2.TxInInfo] -> Integer  -> Bool
    loopInputs []     counter = counter == number
    loopInputs (x:xs) counter = 
      case PlutusV2.txOutDatum $ PlutusV2.txInInfoResolved x of
        PlutusV2.NoOutputDatum       -> loopInputs xs counter
        (PlutusV2.OutputDatumHash _) -> loopInputs xs (counter + 1)
        (PlutusV2.OutputDatum     _) -> loopInputs xs (counter + 1)

-------------------------------------------------------------------------------
-- | Force a number of outputs to have datums
-------------------------------------------------------------------------------
isNOutputs :: [PlutusV2.TxOut] -> Integer -> Bool
isNOutputs utxos number = loopInputs utxos 0
  where
    loopInputs :: [PlutusV2.TxOut] -> Integer  -> Bool
    loopInputs []     counter = counter == number
    loopInputs (x:xs) counter = 
      case PlutusV2.txOutDatum x of
        PlutusV2.NoOutputDatum       -> loopInputs xs counter
        (PlutusV2.OutputDatumHash _) -> loopInputs xs (counter + 1)
        (PlutusV2.OutputDatum     _) -> loopInputs xs (counter + 1)
