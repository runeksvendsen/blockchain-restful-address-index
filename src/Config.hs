{-# LANGUAGE  OverloadedStrings #-}

module Config where

import qualified Data.Text as T

import qualified Data.ByteString as BS
import           Data.Configurator.Types
import qualified Data.Configurator as Conf (lookup)
import           Data.String.Conversions (cs)


data BTCRPCConf = BTCRPCConf {
     rpcHost :: String
    ,rpcPort :: Int
    ,rpcUser :: T.Text
    ,rpcPass :: T.Text
}

getRPCConf :: Config -> IO BTCRPCConf
getRPCConf cfg =
    BTCRPCConf <$>
        configLookupOrFail cfg "bitcoindRPC.ip"   <*>
        configLookupOrFail cfg "bitcoindRPC.port" <*>
        configLookupOrFail cfg "bitcoindRPC.user" <*>
        configLookupOrFail cfg "bitcoindRPC.pass"

configLookupOrFail :: Configured a => Config -> Name -> IO a
configLookupOrFail conf name =
    Conf.lookup conf name >>= maybe
        (fail $ "ERROR: Failed to read key \"" ++ cs name ++
            "\" in config (key not present or invalid)")
        return
