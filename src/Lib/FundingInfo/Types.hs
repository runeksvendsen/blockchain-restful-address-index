{-# LANGUAGE OverloadedStrings #-}

module Lib.FundingInfo.Types where

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, Value(Object), (.:),
                             object, (.=), encode, decode)
import           Data.Word (Word32)
import           Data.Fixed (Fixed(MkFixed))
import qualified Data.Maybe as Maybe
import           Control.Monad (mzero)

-- | Holds information about an output paying to an address
data AddressFundingInfo = AddressFundingInfo {
    asiDestAddress  ::  HC.Address
   ,asiFundingTxId  ::  HT.TxHash
   ,asiFundingVout  ::  Word32
   ,asiConfs        ::  Integer
   ,asiValue        ::  Integer
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

instance FromJSON AddressFundingInfo where
    parseJSON (Object o) =
        AddressFundingInfo <$>
            o .: "address" <*>
            o .: "funding_txid" <*>
            o .: "funding_vout" <*>
            o .: "confirmations"  <*>
            o .: "value"
    parseJSON _ = mzero