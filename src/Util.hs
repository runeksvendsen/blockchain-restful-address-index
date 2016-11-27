module Util
(
  fromMaybe
, forM
, cs
, try
, fmapL
, (<>)
, Reader.ask, Reader.liftIO
)
where


import           Data.Maybe                       (fromMaybe)
import           Control.Monad                    (forM)
import           Data.Monoid                      ((<>))
import           Data.String.Conversions          (cs)
import           Control.Exception                (try)
import           Data.EitherR                     (fmapL)
import qualified Control.Monad.Reader as Reader
