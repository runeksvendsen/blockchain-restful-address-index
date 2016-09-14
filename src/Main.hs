{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, LambdaCase, TypeOperators #-}

module Main where

import           Orphans ()

import qualified APISpec.Blockchain as Spec
import qualified Lib.FundingInfo.FundingInfo as Funding
import qualified Lib.PublishTx.PublishTx as PubTx
import qualified Config as Conf

import           Control.Monad.IO.Class     (liftIO)
import           Data.String.Conversions    (cs)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import qualified Control.Monad.Error.Class as Except
import qualified Control.Monad.Reader as Reader
import qualified Data.Maybe as Maybe


api :: Proxy Spec.BlockchainApi
api = Proxy

-- Enables our handlers to pull a 'BTCRPCConf' out of nowhere
type AppM = Reader.ReaderT Conf.BTCRPCConf Handler

confServer :: Conf.BTCRPCConf -> Server Spec.BlockchainApi
confServer cfg = enter (readerToEither cfg) server

-- Magic stuff: http://stackoverflow.com/a/31098944/700597
readerToEither :: Conf.BTCRPCConf -> AppM :~> Handler
readerToEither cfg = Nat $ \x -> Reader.runReaderT x cfg

server :: ServerT Spec.BlockchainApi AppM
server = allOutputs :<|> unspentOuts :<|> publishTx
    where
        allOutputs addr  = Reader.ask >>= liftIO . flip Funding.getAllOutputs addr
        unspentOuts addr = Reader.ask >>= liftIO . flip Funding.getUnredeemedOutputs addr
        publishTx tx     = Reader.ask >>= liftIO . flip PubTx.bitcoindNetworkSumbitTx tx >>= onLeftThrow500
        onLeftThrow500   = either (\e -> Except.throwError $ err500 { errBody = cs e }) return

app :: Conf.BTCRPCConf -> Wai.Application
app rpccfg = serve api $ confServer rpccfg


main :: IO ()
main = Conf.wrapArg $ \cfg _ -> do
    maybePort <- Conf.getEnvPORT
    app <$> appInit cfg >>= Warp.run (Maybe.fromMaybe 8000 maybePort)

appInit :: Conf.Config -> IO Conf.BTCRPCConf
appInit cfg = do
    bitcoindConf <- Conf.getRPCConf cfg
    Conf.setBitcoinNetwork bitcoindConf
    putStrLn $ "Using Bitcoin Core endpoint: " ++
            Conf.rpcHost bitcoindConf ++ ":" ++ show (Conf.rpcPort bitcoindConf)
    let testAddr = Conf.getTestAddress bitcoindConf
    putStr "Executing test command... " >>
            Funding.getUnredeemedOutputs bitcoindConf testAddr >>=
            putStrLn . ("Success! " ++) . show
    return bitcoindConf



