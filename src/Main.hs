{-# LANGUAGE  OverloadedStrings #-}

module Main where

import           Prelude hiding (userError)

import            Lib       (getUnredeemedOutputs)
import            Config    (BTCRPCConf(..), getRPCConf)

import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Constants as HCC

import           Snap (Config, Snap, serveSnaplet)
import           Snap.Snaplet (runSnaplet)
import           Snap.Http.Server (defaultConfig, httpServe)

import           Snap
-- import           Snap.Http.Server
import           Snap.Http.Server.Config
import           Data.Base58String.Bitcoin  (b58String)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (toJSON)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           System.Environment         (getArgs)
import           Data.String.Conversions    (cs)


main :: IO ()
main = do
    HCC.switchToTestnet3
    serveSnaplet defaultConfig appInit


appInit :: SnapletInit () ()
appInit = makeSnaplet "BlockchainAddressIndex" "Blockchain RESTful address index" Nothing $ do
    rpcConf <- liftIO . getRPCConf =<< getSnapletUserConfig
    liftIO $ putStrLn $ "Using Bitcoin Core endpoint: " ++
            rpcHost rpcConf ++ ":" ++ show (rpcPort rpcConf)
    liftIO $ putStr "Performing test request... " >>
            getUnredeemedOutputs rpcConf testAddr >>=
            putStrLn . ("Success! " ++) . show
    addRoutes [ ("unspentOutputs/:address", unspentOutputHandler rpcConf) ]


unspentOutputHandler :: MonadSnap m => BTCRPCConf -> m ()
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

testAddr = b58String "2N414xMNQaiaHCT5D7JamPz7hJEc9RG7469"






-- mainCmd :: IO ()
-- mainCmd = do
--     addr <- maybe (fail "Usage: <program> <bitcoin_address>")
--             (return . b58String . cs) . listToMaybe =<< getArgs
--     putStrLn $ "Unredeemed outputs:"
--     unredeemedOutputs <- getUnredeemedOutputs addr
--     print unredeemedOutputs
