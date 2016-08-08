{-# LANGUAGE  OverloadedStrings #-}

module Lib.PublishTx.Types where

import           Network.Haskoin.Transaction as HT


data PushTxResponse

data BitcoinTx = BitcoinTx { tx_hex :: HT.Tx }
data BitcoinTxId = BitcoinTxId { settlement_txid :: HT.TxHash }


