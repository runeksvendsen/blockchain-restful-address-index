{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lib.TxOutProof.Proof where

import           Util
import           Types
import           Lib.TxOutProof.Types
import           Config (BTCRPCConf(..))
import qualified Network.Bitcoin.Api.Types.HeaderInfo   as HDI
import           Network.Bitcoin.Api.Client               (Client, withClient)
import           Network.Bitcoin.Api.Blockchain         as Chain
import qualified Data.Aeson                             as JSON
import qualified Network.Bitcoin.Api.UTXO       as UTXO
import qualified Data.Bitcoin.Types             as BT

import qualified Network.Haskoin.Block          as HB
import qualified Network.Haskoin.Transaction    as HT
import           Data.HexString                 as Hex
import qualified Data.Serialize                 as Bin
import qualified Data.Text                      as T
import           Network.HTTP.Client              (HttpException)
import           Servant



-- | Get funding proof for given txid.
getFundingProof :: HT.TxHash -> AppM (Either ServantErr FundingProof)
getFundingProof txid' = do
    txE    <- errConv . Bin.decode . Hex.toBytes <$> withClient' (`Chain.getRawTransaction` txid)
    proofE <- getProof [txid] Nothing
    return $ FundingProof <$> txE <*> proofE
  where
    txid = convOrFail txid'
    errConv = fmapL (\str -> err500 { errBody = "JSON conversion error: " <> cs str })

convOrFail :: (Show a, JSON.ToJSON a, JSON.FromJSON b) => a -> b
convOrFail a = fromMaybe (failErr a) (JSON.decode . JSON.encode $ a)
    where failErr = error . ("JSON conversion error. Source: " ++) . show


getProof :: [BT.TransactionId] -> Maybe BT.BlockHash -> AppM (Either ServantErr HB.MerkleBlock)
getProof txIdL mB = withClient' $ \client ->
    try (getProofUnsafe client txIdL mB) >>=
        \resE -> case resE of
            Left e -> return $ Left $ err500 { errBody = cs $ show (e :: HttpException) }
            Right bM -> return $ maybe (Left $ err400 { errBody = "Transaction(s) not found" }) Right bM

parseTxIds :: [T.Text] -> [BT.TransactionId]
parseTxIds = map $ hexString . cs

getProofUnsafe :: Client -> [BT.TransactionId] -> Maybe BT.BlockHash -> IO (Maybe HB.MerkleBlock)
getProofUnsafe client txIdL bhM =
    fmap decodeHex <$> UTXO.txOutProof client txIdL bhM
  where
    decodeHex hex = case Bin.decode $ Hex.toBytes hex of
          Right mb -> mb
          Left   e -> error $
              "Couldn't decode MerkleBlock from hex data: " ++ show e ++
              "\n" ++ show hex

withClient' :: (Client -> IO a) -> AppM a
withClient' f = do
    (BTCRPCConf host port user pass _) <- ask
    liftIO $ withClient host port user pass f


-- ## TEST ##
-- mb :: HB.MerkleBlock
-- mb = either error id . Bin.decode . fst . B16.decode $ cs
--      ("0000002021a54450c9ff5ed81f012745113f0d6f8d56c0a1973fb70000000000000000007a78d8a1216379d5d30becf64b460042a869d431b31ee607323c4196a6cfd1688e603958d4e6031802591318090b00000dba3e33efc1f866dd9fe26ce91945e2d77a6d93fa19d981afb98ce2f67b7d05f2bb35ec934706cba5c4cfe50affcb02a9d8d444641fd32f468f8fef1b2b207c874352bf430e68937a8b5ab7495d18037213fe2d5e069f8803beae62c1b794d00273ece2122022bfba4e9e41939ceed2f24d10249699bbe637d72569ff82f2a63608a1b45eda15809e13a43b987a9e004ab08e550f5d95dbcaaa46299b94e5a1dad9edb06881ba37851e3b7167c284dcb2bcd2de4f38c4bb6bac500bca411d304aeece4a9c52f07e6c96cd7e9ba930133f4ac7c9ff47a3de7bc0d0591f837b2c6d117a29767a57bc03f959e3176f1df7b05797b317d2793a75e8f325b901aeb20dd0053e47aed1eeb1a1d8d70762e9a43ba418b18a1ac863c8cc644011651b7a5716710bde2bab14fe0d9f059c7228041c79ad5b6d36f97daf1e50b089b053f27db05a1fb683a910c2ebfdcff0fd74130e2314028cf062865d7f80ecf50ef2ba38e7fe61dd8e49d36313e037f4f5da30ff2bfc9523bd74f6223db2e647356a1492f1e8d1cc970c20ff559f586976aa0a30ccc1ff38054ae25631260b215fd771bb047f750100" :: String)
--      ("0000002021a54450c9ff5ed81f012745113f0d6f8d56c0a1973fb70000000000000000007a78d8a1216379d5d30becf64b460042a869d431b31ee607323c4196a6cfd1688e603958d4e6031802591318090b00000dba3e33efc1f866dd9fe26ce91945e2d77a6d93fa19d981afb98ce2f67b7d05f2bb35ec934706cba5c4cfe50affcb02a9d8d444641fd32f468f8fef1b2b207c874352bf430e68937a8b5ab7495d18037213fe2d5e069f8803beae62c1b794d00273ece2122022bfba4e9e41939ceed2f24d10249699bbe637d72569ff82f2a63608a1b45eda15809e13a43b987a9e004ab08e550f5d95dbcaaa46299b94e5a1dad9edb06881ba37851e3b7167c284dcb2bcd2de4f38c4bb6bac500bca411d304aeece4a9c52f07e6c96cd7e9ba930133f4ac8c9ff47a3de7bc0d0591f837b2c6d117a29767a57bc03f959e3176f1df7b05797b317d2793a75e8f325b901aeb20dd0053e47aed1eeb1a1d8d70762e9a43ba418b18a1ac863c8cc644011651b7a5716710bde2bab14fe0d9f059c7228041c79ad5b6d36f97daf1e50b089b053f27db05a1fb683a910c2ebfdcff0fd74130e2314028cf062865d7f80ecf50ef2ba38e7fe61dd8e49d36313e037f4f5da30ff2bfc9523bd74f6223db2e647356a1492f1e8d1cc970c20ff559f586976aa0a30ccc1ff38054ae25631260b215fd771bb047f750100" :: String)

