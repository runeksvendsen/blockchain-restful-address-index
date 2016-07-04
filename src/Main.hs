{-# LANGUAGE  OverloadedStrings #-}

import            Lib (getUnredeemedOutputs)

import qualified  Network.Haskoin.Crypto as HC
import qualified  Network.Haskoin.Constants as HCC

import           Prelude hiding (userError)
import           Snap -- (Snap, quickHttpServe)
-- import           Snap.Snaplet (runSnaplet)
import           Snap.Http.Server -- (defaultConfig, httpServe)
import           System.Environment (lookupEnv)
import           Text.Read (readMaybe)
import           Snap.Http.Server.Config (setPort)
import           Network.HTTP.Client (HttpException (..))
import           Network.Bitcoin.Api.Client (Client, withClient)
import           Network.Bitcoin.Api.Blockchain (searchRawTransactions)
import           Network.Bitcoin.Api.UTXO (getTxOut)
import           Network.Bitcoin.Api.Types.TxInfo (TxInfo(..), Vout(..), Vin(..))
import qualified Network.Bitcoin.Api.Types.UnspentTxOut as UTXO (UnspentTxOut(..))
import qualified Data.Bitcoin.Types  as BT
import qualified Data.Base58String.Bitcoin as B58S
import           Data.Base58String.Bitcoin (b58String)
import qualified Data.Bitcoin.Types  as BT
import           Control.Monad (forM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Value(Object, String), FromJSON, ToJSON, parseJSON, toJSON,
                            fromJSON, (.=), (.:), object)
import           Data.Word (Word32)
import           Data.List (sortOn, concat)
import           Data.Maybe (isJust, fromJust, listToMaybe)
import           System.Environment (getArgs)
import           Data.String.Conversions (cs)

import           Data.Aeson.Encode.Pretty (encodePretty)


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
