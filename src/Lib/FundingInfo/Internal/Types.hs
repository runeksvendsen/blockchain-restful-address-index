{-# LANGUAGE OverloadedStrings #-}

module Lib.FundingInfo.Internal.Types where

import           Lib.FundingInfo.Types

import qualified Data.Bitcoin.Types  as BT
import qualified Data.Base58String.Bitcoin as B58S
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, Value(Object), (.:),
                             object, (.=), encode, decode)
import           Data.Word (Word32)
import           Data.Fixed (Fixed(MkFixed))
import qualified Data.Maybe as Maybe
import           Control.Monad (mzero)


toHaskoin :: AddressFundingInfoRes -> AddressFundingInfo
toHaskoin (AddressFundingInfoRes addr txid vout numConfs (MkFixed valInt)) =
    AddressFundingInfo
        (Maybe.fromJust $ decode $ encode addr)
        (Maybe.fromJust $ decode $ encode txid)
        vout
        numConfs
        valInt

data AddressFundingInfoRes = AddressFundingInfoRes {
    asiDestAddress'  :: B58S.Base58String
   ,asiFundingTxId'  :: BT.TransactionId
   ,asiFundingVout'  :: Word32
   ,asiConfs'        :: Integer
   ,asiValue'        :: BT.Btc
} deriving (Eq, Show)
