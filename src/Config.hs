{-# LANGUAGE  OverloadedStrings #-}

module Config
(
    BTCRPCConf(..)
    ,getRPCConf
    ,configLookupOrFail
    ,loadConfig
    ,wrapArg

    ,Config
)
where

import qualified Data.Text as T

import qualified Data.ByteString as BS
-- import           Data.Configurator.Types
import qualified Data.Configurator as Conf
import           Data.Configurator.Types
import           Data.String.Conversions (cs)
import           System.Environment         (getArgs, getProgName)



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

loadConfig :: String -> IO Config
loadConfig confFile = Conf.load [Conf.Required confFile]


wrapArg :: (Config -> String -> IO ()) -> IO ()
wrapArg main' = do
    args <- getArgs
    prog <- getProgName
    if  length args < 1 then
            putStrLn $ "Usage: " ++ prog ++ " /path/to/config.cfg"
        else do
            let cfgFile = head args
            putStrLn $ "Using config file " ++ show cfgFile
            cfg <- loadConfig cfgFile
            main' cfg cfgFile