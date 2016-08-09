{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified APISpec.Blockchain as Spec
import qualified Lib.FundingInfo.FundingInfo as Funding
import qualified Lib.PublishTx.PublishTx as PubTx
import qualified Config as Conf

import qualified Network.Haskoin.Crypto as HC

import           Control.Monad.IO.Class     (liftIO)
import           Data.String.Conversions    (cs)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import qualified Control.Monad.Error.Class as Except
import qualified Web.HttpApiData as Web


api :: Proxy Spec.BlockchainApi
api = Proxy

server :: Conf.BTCRPCConf -> Server Spec.BlockchainApi
server cfg = unspentOutputs :<|> publishTx
    where
        unspentOutputs addr = liftIO $ Funding.getUnredeemedOutputs cfg addr
        publishTx tx = tryIOReq $ PubTx.bitcoindNetworkSumbitTx cfg tx

app :: Conf.BTCRPCConf -> Wai.Application
app rpccfg = serve api $ server rpccfg

instance Web.FromHttpApiData HC.Address where
    parseUrlPiece txt = maybe
        (Left "failed to parse Bitcoin address") Right $
            HC.base58ToAddr (cs txt)



main :: IO ()
main = Conf.wrapArg $ \cfg _ ->
    app <$> appInit cfg >>= Warp.run 8000

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



-- Util
tryIOReq :: IO (Either String a) -> Handler a
tryIOReq req = liftIO req >>= either
    (\e -> Except.throwError $ err500 { errBody = cs e })
     return

