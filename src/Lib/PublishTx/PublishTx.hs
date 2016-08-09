module Lib.PublishTx.PublishTx
(
    bitcoindNetworkSumbitTx
--    ,PublishTxResponse(..)

) where

import           Config

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Exception (try)
import           Network.HTTP.Client (HttpException)
import           Network.Bitcoin.Api.Client (Client, withClient)
import           Network.Bitcoin.Api.Transaction (send)

import           Network.Haskoin.Transaction as HT
import           Network.Haskoin.Crypto as HC
import qualified Data.Bitcoin.Transaction as Btc

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.HexString as Hex
import           Data.Text as T
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy as BL





bitcoindNetworkSumbitTx :: BTCRPCConf -> HT.Tx -> IO (Either String HT.TxHash)
bitcoindNetworkSumbitTx (BTCRPCConf ip port user pass _) tx =
    withClient ip port user pass (tryBitcoindSubmitToNetwork tx)

tryBitcoindSubmitToNetwork :: MonadIO m =>
    HT.Tx
    -> Client
    -> m (Either String HT.TxHash)
tryBitcoindSubmitToNetwork tx conn = do
    res <- liftIO . try $ send conn $ Btc.decode . fromBytes $ serialize tx
    case res of
        Left e      -> return . Left $  "Bitcoind error: " ++ show (e :: HttpException)
        Right txid  -> return $
            maybe
                (Left $ "BUG: Failed to parse transaction ID returned by bitcoind: " ++ show txid)
                Right
                (HT.hexToTxHash (B16.encode $ toBytes txid))

serialize :: Bin.Binary a => a -> BS.ByteString
serialize = BL.toStrict . Bin.encode

