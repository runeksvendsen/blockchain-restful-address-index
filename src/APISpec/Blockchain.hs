{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module APISpec.Blockchain
(
    BlockchainApi
)
where

import           APISpec.Types          (Addr, PushTxReq, PushTxResp, FundingProof)
import           Lib.FundingInfo.Types  (AddressFundingInfo)
import           Servant.API
import qualified Network.Haskoin.Transaction    as HT
import qualified Data.Aeson                     as JSON
import qualified Data.Text                      as T


-- |The API exposed by this server.
type BlockchainApi =
        "outputs"    :> Capture "address" Addr :> "all"        :> Get  '[JSON] [AddressFundingInfo]
  :<|>  "outputs"    :> Capture "address" Addr :> "unspent"    :> Get  '[JSON] [AddressFundingInfo]
  :<|>  "txOutProof" :> Capture "txid" HT.TxHash               :> Get  '[JSON] FundingProof
  :<|>  "publishTx"  :> ReqBody '[JSON] PushTxReq              :> Post '[JSON] PushTxResp
  :<|>  "rawCmd"     :> Capture "method" String  :> CaptureAll "args" T.Text :> Get  '[JSON] JSON.Value


-- Example URLs, in order:
--------------------------
-- GET  /outputs/14wjVnwHwMAXDr6h5Fw38shCWUB6RSEa63/all
-- GET  /outputs/14wjVnwHwMAXDr6h5Fw38shCWUB6RSEa63/unspent
-- POST /publishTx

