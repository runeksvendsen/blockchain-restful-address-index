{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings #-}

module APISpec.Types
(
    AddressFundingInfo(..)
  , Addr(..)
  , PushTxReq(..)
  , PushTxResp(..)
  , FundingProof(..)
  , HB.MerkleBlock
  , module Orphans
)
where

import           Lib.FundingInfo.Types      (AddressFundingInfo(..))
import           Lib.TxOutProof.Types
import           Orphans
import qualified Network.Haskoin.Transaction            as HT
import qualified Network.Haskoin.Block                  as HB
import qualified Network.Haskoin.Crypto                 as HC

import qualified Web.HttpApiData                        as Web
import           Data.Aeson

import           GHC.Generics

newtype Addr = Addr { getAddress :: HC.Address }
    deriving (Generic, Web.FromHttpApiData, Web.ToHttpApiData)

data PushTxReq = PushTxReq { tx_data :: HT.Tx } deriving Generic
data PushTxResp = PushTxResp { tx_id :: HT.TxHash } deriving Generic

instance FromJSON PushTxReq
instance ToJSON PushTxReq
instance FromJSON PushTxResp
instance ToJSON PushTxResp
