{-# LANGUAGE  OverloadedStrings #-}

module Lib.PublishTx.Types where

import           Network.Haskoin.Transaction as HT

import           Data.Aeson (Value(Object, String), FromJSON, ToJSON, parseJSON, toJSON,
                            fromJSON, (.=), (.:), object)
import           Data.String.Conversions (cs)
import           Control.Monad (mzero)


data PushTxResponse

data BitcoinTx = BitcoinTx { tx_hex_string :: HT.Tx }
data BitcoinTxId = BitcoinTxId { settlement_txid :: HT.TxHash }

--
-- instance ToJSON PublishTxResponse where
--     toJSON (MkPublishTxResponse txid) =
--         object [
--             "settlement_txid" .= txid
--         ]
--
-- instance FromJSON PublishTxResponse where
--     parseJSON (Object v) =
--         MkPublishTxResponse <$>
--             v .: "settlement_txid"
--     parseJSON _ = mzero

