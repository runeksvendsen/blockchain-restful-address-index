{-# LANGUAGE  OverloadedStrings #-}

import            Lib (getUnredeemedOutputs)

import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Constants as HCC

import           Prelude hiding (userError)
import           Snap
import           Snap.Http.Server
import           Data.Base58String.Bitcoin (b58String)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (toJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           System.Environment (getArgs)
import           Data.String.Conversions (cs)


main :: IO ()
main = HCC.switchToTestnet3 >> quickHttpServe site

site :: Snap ()
site =
    route [ ("unspentOutputs/:address", unspentOutputHandler) ]

unspentOutputHandler :: Snap ()
unspentOutputHandler = do
    writeLBS . encodePretty . toJSON =<<
        liftIO . getUnredeemedOutputs "192.168.1.102" 8334 "john_oliver" "KGbv6HvJ5z" .
        b58String . HC.addrToBase58 =<< getBitcoinAddressArg
    writeBS "\n"

getBitcoinAddressArg :: Snap HC.Address
getBitcoinAddressArg = do
    paramBS <- maybe
            (userError "Empty 'address' parameter") return =<<
            getParam "address"
    maybe
        (userError $ "Bitcoin address parse failure: " ++ cs paramBS) return
        (HC.base58ToAddr paramBS)

userError :: String -> Snap a
userError = errorWithDescription 400

errorWithDescription :: Int -> String -> Snap a
errorWithDescription code errStr = do
    modifyResponse $ setResponseStatus code (cs errStr)
    finishWith =<< getResponse









-- mainCmd :: IO ()
-- mainCmd = do
--     addr <- maybe (fail "Usage: <program> <bitcoin_address>")
--             (return . b58String . cs) . listToMaybe =<< getArgs
--     putStrLn $ "Unredeemed outputs:"
--     unredeemedOutputs <- getUnredeemedOutputs addr
--     print unredeemedOutputs
