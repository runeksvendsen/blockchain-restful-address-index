{-# LANGUAGE OverloadedStrings #-}
module Lib.TxOutProof.Types where

import qualified Network.Haskoin.Block                  as HB
import           Data.Aeson (ToJSON, FromJSON, Value(String), toJSON, parseJSON, withText)

import qualified Data.ByteString.Base16     as B16
import qualified Data.Serialize             as Bin
import           Data.String.Conversions      (cs)


instance ToJSON HB.MerkleBlock where
    toJSON = String . cs . B16.encode . Bin.encode

instance FromJSON HB.MerkleBlock where
    parseJSON = withText "MerkleBlock" $
        either fail return . Bin.decode . fst . B16.decode . cs


