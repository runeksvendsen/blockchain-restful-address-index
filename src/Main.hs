{-# LANGUAGE  OverloadedStrings #-}

module Main where

import           Prelude hiding (userError)

import           Lib       (getUnredeemedOutputs)
import qualified Config as Conf

import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Constants as HCC

import           Snap (Config, Snap, serveSnaplet)
import           Snap.Snaplet (runSnaplet)
import           Snap.Http.Server (defaultConfig, httpServe)

import           Snap
import           Snap.Http.Server.Config
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (toJSON)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           System.Environment         (getArgs)
import           Data.String.Conversions    (cs)
import           Data.Base58String.Bitcoin  (b58String)


main :: IO ()
main = Conf.wrapArg $ \cfg _ ->
    serveSnaplet defaultConfig (appInit cfg)

appInit :: Conf.Config -> SnapletInit () ()
appInit cfg = makeSnaplet "BlockchainAddressIndex" "Blockchain RESTful address index" Nothing $ do
    bitcoindConf <- liftIO $ Conf.getRPCConf cfg
    liftIO $ Conf.setBitcoinNetwork bitcoindConf
    liftIO $ putStrLn $ "Using Bitcoin Core endpoint: " ++
            Conf.rpcHost bitcoindConf ++ ":" ++ show (Conf.rpcPort bitcoindConf)
    let testAddr = Conf.getTestAddress bitcoindConf
    liftIO $ putStr "Performing test request... " >>
            getUnredeemedOutputs bitcoindConf testAddr >>=
            putStrLn . ("Success! " ++) . show
    addRoutes [
        ("unspentOutputs/:address", unspentOutputHandler bitcoindConf)
       ]


unspentOutputHandler :: MonadSnap m => Conf.BTCRPCConf -> m ()
unspentOutputHandler conf = do
    writeLBS . encodePretty . toJSON =<<
        liftIO . getUnredeemedOutputs conf .
        b58String . HC.addrToBase58 =<< getBitcoinAddressArg
    writeBS "\n"

getBitcoinAddressArg :: MonadSnap m => m HC.Address
getBitcoinAddressArg = do
    paramBS <- maybe
            (userError "Empty 'address' parameter") return =<<
            getParam "address"
    maybe
        (userError $ "Bitcoin address parse failure: " ++ cs paramBS) return
        (HC.base58ToAddr paramBS)

userError :: MonadSnap m => String -> m a
userError = errorWithDescription 400

errorWithDescription :: MonadSnap m => Int -> String -> m a
errorWithDescription code errStr = do
    modifyResponse $ setResponseStatus code (cs errStr)
    finishWith =<< getResponse

