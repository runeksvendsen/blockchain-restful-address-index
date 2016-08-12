{-# LANGUAGE  OverloadedStrings #-}

module Config
(
    BTCRPCConf(..)
    ,getRPCConf
    ,configLookupOrFail
    ,loadConfig
    ,wrapArg,getEnvPORT
    ,getTestAddress
    ,setBitcoinNetwork

    ,Config
)
where

import qualified Data.Text as T

import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Constants as HCC
import qualified Data.Configurator as Conf
import           Data.Configurator.Types
import           Data.String.Conversions (cs)
import qualified System.Environment as Sys
import qualified Text.Read as Read


data BTCRPCConf = BTCRPCConf {
     rpcHost :: String
    ,rpcPort :: Int
    ,rpcUser :: T.Text
    ,rpcPass :: T.Text
    ,rpcNet  :: BitcoinNet
}

data BitcoinNet = Mainnet | Testnet3

instance Configured BitcoinNet where
    convert (String "live") = return Mainnet
    convert (String "test") = return Testnet3
    convert _ = Nothing

testAddrTestnet = "2N414xMNQaiaHCT5D7JamPz7hJEc9RG7469"
testAddrLivenet = "14wjVnwHwMAXDr6h5Fw38shCWUB6RSEa63"

getTestAddress :: BTCRPCConf -> HC.Address
getTestAddress (BTCRPCConf _ _ _ _ Mainnet) = testAddrLivenet
getTestAddress (BTCRPCConf _ _ _ _ Testnet3) = testAddrTestnet

setBitcoinNetwork :: BTCRPCConf -> IO ()
setBitcoinNetwork (BTCRPCConf _ _ _ _ Mainnet) = return ()
setBitcoinNetwork (BTCRPCConf _ _ _ _ Testnet3) = HCC.switchToTestnet3

getRPCConf :: Config -> IO BTCRPCConf
getRPCConf cfg =
    BTCRPCConf <$>
        configLookupOrFail cfg "bitcoindRPC.ip"   <*>
        configLookupOrFail cfg "bitcoindRPC.port" <*>
        configLookupOrFail cfg "bitcoindRPC.user" <*>
        configLookupOrFail cfg "bitcoindRPC.pass" <*>
        configLookupOrFail cfg "bitcoindRPC.network"

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
    args <- Sys.getArgs
    prog <- Sys.getProgName
    if  length args < 1 then
            putStrLn $ "Usage: " ++ prog ++ " /path/to/config.cfg"
        else do
            let cfgFile = head args
            putStrLn $ "Using config file " ++ show cfgFile
            cfg <- loadConfig cfgFile
            main' cfg cfgFile


getEnvPORT :: IO (Maybe Int)
getEnvPORT =
    --  Get port from PORT environment variable, if it contains a valid port number
    maybe Nothing Read.readMaybe <$> Sys.lookupEnv "PORT"
