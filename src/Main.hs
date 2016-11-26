{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, LambdaCase, TypeOperators #-}

module Main where

import           Orphans ()

import qualified APISpec.Blockchain as Spec
import           APISpec.Types
import qualified Lib.FundingInfo.FundingInfo as Funding
import qualified Lib.PublishTx.PublishTx as PubTx
import qualified Lib.TxOutProof.Proof as Proof
import qualified Config as Conf

import           Control.Monad.IO.Class     (liftIO)
import           Data.String.Conversions    (cs)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import qualified Control.Monad.Error.Class as Except
import qualified Control.Monad.Reader as Reader
import qualified Data.Maybe as Maybe
import qualified Data.ByteString as BS
import qualified Data.Aeson as JSON


api :: Proxy Spec.BlockchainApi
api = Proxy

-- Enables our handlers to pull a 'BTCRPCConf' out of nowhere
type AppM = Reader.ReaderT Conf.BTCRPCConf Handler

confServer :: Conf.BTCRPCConf -> BS.ByteString -> Server Spec.BlockchainApi
confServer cfg path = enter (readerToEither cfg) (server path)

-- Magic stuff: http://stackoverflow.com/a/31098944/700597
readerToEither :: Conf.BTCRPCConf -> AppM :~> Handler
readerToEither cfg = Nat $ \x -> Reader.runReaderT x cfg

server :: BS.ByteString -> ServerT Spec.BlockchainApi AppM
server rawPath = allOutputs :<|> unspentOuts :<|> txOutProof :<|> publishTx :<|> rawCmd
    where
        allOutputs addr  = Reader.ask >>=
            liftIO . flip Funding.getAllOutputs (getAddress addr)
        unspentOuts addr = Reader.ask >>=
            liftIO . flip Funding.getUnredeemedOutputs (getAddress addr)
        txOutProof addrL = Reader.ask >>=
            liftIO . Proof.getProof (Proof.parseTxIds addrL) Nothing >>=
            \mbM -> maybe throw404 return mbM
        throw404 = Except.throwError $ err400 { errBody = "Transaction(s) not found" }
        publishTx (PushTxReq tx) = Reader.ask >>=
            liftIO . flip PubTx.bitcoindNetworkSumbitTx tx >>= onLeftThrow500
        rawCmd method args = do
            liftIO . putStrLn . unlines $
                [ "Method: " ++ show method ] ++
                [ "Args:   " ++ show args ] ++
                [ "RAW path: " ++ show rawPath ]
            return (JSON.toJSON ["hey" :: String])
        onLeftThrow500   = either (\e -> Except.throwError $ err500 { errBody = cs e })
            (return . PushTxResp)

app :: Conf.BTCRPCConf -> Wai.Application
app rpccfg request respond = do
    let rawPath = Wai.rawPathInfo request
    serve api (confServer rpccfg rawPath) request respond

main :: IO ()
main = Conf.wrapArg $ \cfg _ -> do
    port <- Maybe.fromMaybe 8000 <$> Conf.getEnvPORT
    putStrLn $ "Listening on port " ++ show port
    app <$> appInit cfg >>= Warp.run port

appInit :: Conf.Config -> IO Conf.BTCRPCConf
appInit cfg = do
    bitcoindConf <- Conf.getRPCConf cfg
    Conf.setBitcoinNetwork bitcoindConf
    putStrLn $ "Using Bitcoin Core endpoint: " ++
            Conf.rpcHost bitcoindConf ++ ":" ++ show (Conf.rpcPort bitcoindConf)
--     let testAddr = Conf.getTestAddress bitcoindConf
--     putStr "Executing test command... " >>
--             Funding.getUnredeemedOutputs bitcoindConf testAddr >>=
--             putStrLn . ("Success! " ++) . show
    return bitcoindConf



