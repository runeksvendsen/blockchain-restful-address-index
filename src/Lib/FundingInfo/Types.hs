{-# LANGUAGE OverloadedStrings #-}

module Lib.FundingInfo.Types where

import qualified Data.Bitcoin.Types  as BT
import qualified Data.Base58String.Bitcoin as B58S
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC

import           Data.Aeson (ToJSON, toJSON,
                             object, (.=), encode, decode)
import           Data.Word (Word32)
import           Data.Fixed (Fixed(MkFixed))
import qualified Data.Maybe as Maybe

toHaskoin :: AddressFundingInfoRes -> AddressFundingInfo
toHaskoin (AddressFundingInfoRes addr txid vout numConfs (MkFixed valInt)) =
    AddressFundingInfo
        (Maybe.fromJust $ decode $ encode addr)
        (Maybe.fromJust $ decode $ encode txid)
        vout
        numConfs
        valInt

-- | Holds information about an output paying to an address
data AddressFundingInfo = AddressFundingInfo {
    _asiDestAddress  ::  HC.Address
   ,_asiFundingTxId  ::  HT.TxHash
   ,_asiFundingVout  ::  Word32
   ,_asiConfs        ::  Integer
   ,_asiValue        ::  Integer
} deriving (Eq, Show)

data AddressFundingInfoRes = AddressFundingInfoRes {
    asiDestAddress  ::  B58S.Base58String
   ,asiFundingTxId  ::  BT.TransactionId
   ,asiFundingVout  ::  Word32
   ,asiConfs        ::  Integer
   ,asiValue        ::  BT.Btc
} deriving (Eq, Show)

instance ToJSON AddressFundingInfo where
    toJSON (AddressFundingInfo addr txid vout confs val) =
        object [
            "address" .= addr,
            "funding_txid" .= txid,
            "funding_vout" .= vout,
            "confirmations" .= confs,
            "value" .= val
        ]

