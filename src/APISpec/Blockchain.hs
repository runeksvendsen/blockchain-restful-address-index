{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings #-}

module APISpec.Blockchain
(
    BlockchainApi
)
where

import           Lib.FundingInfo.Types            (AddressFundingInfo)

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC

import           Servant.API
import           Network.HTTP.Media ((//))


-- |The API exposed by this server.
type BlockchainApi =
            "outputs" :> Capture "address" HC.Address :> "all"     :> Get  '[JSON] [AddressFundingInfo]
    :<|>    "outputs" :> Capture "address" HC.Address :> "unspent" :> Get  '[JSON] [AddressFundingInfo]
    :<|>    "publishTx" :> ReqBody '[PlainText] HT.Tx              :> Post '[PlainText] HT.TxHash


-- Example URLs, in order:
--------------------------
-- GET  /outputs/14wjVnwHwMAXDr6h5Fw38shCWUB6RSEa63/all
-- GET  /outputs/14wjVnwHwMAXDr6h5Fw38shCWUB6RSEa63/unspent
-- POST /publishTx

