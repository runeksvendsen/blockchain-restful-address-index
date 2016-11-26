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
import           Control.Exception                (try)
import           Network.HTTP.Client              (HttpException)
import           Servant



getProof :: [BT.TransactionId] -> Maybe BT.BlockHash -> BTCRPCConf -> IO (Either ServantErr HB.MerkleBlock)
getProof txIdL mB conf =
    try (getProofUnsafe txIdL mB conf) >>=
        \resE -> case resE of
            Left e -> return $ Left $ err500 { errBody = cs $ show (e :: HttpException) }
            Right bM -> return $ maybe (Left $ err400 { errBody = "Transaction(s) not found" }) Right bM


parseTxIds :: [T.Text] -> [BT.TransactionId]
parseTxIds = map $ hexString . cs

getProofUnsafe :: [BT.TransactionId] -> Maybe BT.BlockHash -> BTCRPCConf -> IO (Maybe HB.MerkleBlock)
getProofUnsafe txIdL bhM (BTCRPCConf host port user pass _) =
    withClient host port user pass $ \client -> do
        hexM <- UTXO.txOutProof client txIdL bhM
        return $ decodeHex <$> hexM
  where decodeHex hex =
          case Bin.decode $ Hex.toBytes hex of
              Right mb -> mb
              Left   e -> error $
                  "Couldn't decode MerkleBlock from hex data: " ++ show e ++
                  "\n" ++ show hex
