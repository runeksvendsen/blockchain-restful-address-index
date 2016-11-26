{-# LANGUAGE OverloadedStrings #-}
module Lib.TxOutProof.Proof where

import           Config (BTCRPCConf(..))
import           Network.Bitcoin.Api.Client       (withClient)
import qualified Network.Bitcoin.Api.UTXO       as UTXO
import qualified Data.Bitcoin.Types             as BT

import qualified Network.Haskoin.Block          as HB
import           Data.HexString                 as Hex
import qualified Data.Serialize                 as Bin
import qualified Data.Text                      as T
import           Data.String.Conversions          (cs)


parseTxIds :: [T.Text] -> [BT.TransactionId]
parseTxIds = map $ hexString . cs

getProof :: [BT.TransactionId] -> Maybe BT.BlockHash -> BTCRPCConf -> IO (Maybe HB.MerkleBlock)
getProof txIdL bhM (BTCRPCConf host port user pass _) =
    withClient host port user pass $ \client -> do
        hexM <- UTXO.txOutProof client txIdL bhM
        return $ decodeHex <$> hexM
  where decodeHex hex =
          case Bin.decode $ Hex.toBytes hex of
              Right mb -> mb
              Left   e -> error $
                  "Couldn't decode MerkleBlock from hex data: " ++ show e ++
                  "\n" ++ show hex
