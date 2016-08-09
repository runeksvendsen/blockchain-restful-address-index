{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

module Orphans where

import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Util as HU
import qualified Data.Binary as Bin

import qualified Web.HttpApiData as Web
import qualified Servant.API.ContentTypes as Content
import           Data.String.Conversions (cs)
import           Data.EitherR (fmapL)


decodeHex bs = maybe (Left "invalid hex string") Right (HU.decodeHex bs)

instance Web.FromHttpApiData HC.Address where
    parseUrlPiece txt = maybe
        (Left "failed to parse Bitcoin address") Right $
            HC.base58ToAddr (cs txt)

instance Content.MimeUnrender Content.PlainText HT.Tx where
    mimeUnrender _ bs = decodeHex (cs bs) >>=
             fmapL ("failed to decode transaction: " ++) . HU.decodeToEither

instance Content.MimeRender Content.PlainText HT.Tx where
    mimeRender _ = cs . HU.encodeHex . cs . Bin.encode

instance Content.MimeRender Content.PlainText HT.TxHash where
    mimeRender _ = cs . HT.txHashToHex

instance Content.MimeUnrender Content.PlainText HT.TxHash where
    mimeUnrender _ bs = maybe
        (Left "failed to parse Bitcoin address") Right $
            HT.hexToTxHash (cs bs)


