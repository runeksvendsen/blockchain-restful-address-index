module Types where

import qualified Config as Conf
import           Servant
import qualified Control.Monad.Reader as Reader

-- Enables our handlers to pull a 'BTCRPCConf' out of thin air
type AppM = Reader.ReaderT Conf.BTCRPCConf Handler
