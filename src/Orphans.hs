{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

module Orphans where

import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT
import qualified Data.Aeson as JSON

import qualified Web.HttpApiData as Web
import qualified Servant.API.ContentTypes as Content
import           Data.String.Conversions (cs)


instance Web.FromHttpApiData HC.Address where
    parseUrlPiece txt = maybe
        (Left "failed to parse Bitcoin address") Right $
            HC.base58ToAddr (cs txt)

instance Content.MimeUnrender Content.PlainText HT.Tx where
    mimeUnrender _ = JSON.eitherDecode
