{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module APISpec.Blockchain
( -- *Server API
  BlockchainApi
  -- *Endpoints (individual)
, AllOuts
, UnspentOuts
, TxOutProof
, PublishTx
, RawCmd
  -- *Re-exports
, module APISpec.Types
)
where

import           APISpec.Types
import           Servant.API
import qualified Network.Haskoin.Transaction    as HT
import qualified Data.Aeson                     as JSON
import qualified Data.Text                      as T


-- |The API exposed by this server.
type BlockchainApi =
       AllOuts
  :<|> UnspentOuts
  :<|> TxOutProof
  :<|> PublishTx
  :<|> RawCmd


type AllOuts =
    "outputs"     :> Capture "address" Addr :> "all"        :> Get  '[JSON] [AddressFundingInfo]

type UnspentOuts =
    "outputs"     :> Capture "address" Addr :> "unspent"    :> Get  '[JSON] [AddressFundingInfo]

type TxOutProof =
    "txOutProof"  :> Capture "txid" HT.TxHash               :> Get  '[JSON] FundingProof

type PublishTx =
    "publishTx"   :> ReqBody '[JSON] PushTxReq              :> Post '[JSON] PushTxResp

type RawCmd =
    "rawCmd"      :> Capture "method" String
                  :> CaptureAll "args" T.Text               :> Get  '[JSON] JSON.Value

