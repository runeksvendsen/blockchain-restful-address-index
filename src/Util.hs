module Util
(
  fromMaybe
, forM
, cs
, try
, fmapL
, (<>)
, Reader.ask, Reader.liftIO
, decodeHex
)
where


import           Data.Maybe                       (fromMaybe)
import           Control.Monad                    (forM)
import           Data.Monoid                      ((<>))
import           Data.String.Conversions          (cs)
import           Control.Exception                (try)
import           Data.EitherR                     (fmapL)
import qualified Control.Monad.Reader as Reader

import qualified Network.Haskoin.Util as HU

decodeHex bs = maybe (Left "invalid hex string") Right (HU.decodeHex bs)