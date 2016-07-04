{-# LANGUAGE  OverloadedStrings #-}

import qualified  Network.Haskoin.Transaction as HT

import           Server.App (appInit)
import           Snap (Config, Snap, serveSnaplet)
import           Snap.Snaplet (runSnaplet)
import           Snap.Http.Server (defaultConfig, httpServe)
import           System.Environment (lookupEnv)
import           Text.Read (readMaybe)
import           Snap.Http.Server.Config (setPort)
import           Network.HTTP.Client (HttpException (..))
import           Network.Bitcoin.Api.Client (Client, withClient)
import           Network.Bitcoin.Api.Blockchain (searchRawTransactions)
import           Network.Bitcoin.Api.Types.TxInfo

main :: IO ()
main = do
    withClient "http://192.168.1.102" 8334 "john_oliver" "KGbv6HvJ5z" $
        \client -> search
