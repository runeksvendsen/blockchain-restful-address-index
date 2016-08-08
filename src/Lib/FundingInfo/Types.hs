{-# LANGUAGE OverloadedStrings #-}

module Lib.FundingInfo.Types where

import qualified Data.Bitcoin.Types  as BT
import qualified Data.Base58String.Bitcoin as B58S
import           Data.Aeson (Value(Object, String), FromJSON, ToJSON, parseJSON, toJSON,
                            fromJSON, (.=), (.:), object)
import           Data.Word (Word32)
import           Data.String.Conversions (cs)
import           Data.Fixed (showFixed)


-- | Holds information about an output paying to an address
data AddressFundingInfo = AddressFundingInfo {
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
            "value" .= (String . cs $ showFixed False val)
        ]

