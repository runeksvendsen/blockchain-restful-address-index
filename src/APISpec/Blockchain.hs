{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module APISpec.Blockchain
(
    BlockchainApi,
    api
)
where

import           Lib.FundingInfo            (AddressFundingInfo)

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC

import  Servant



api :: Proxy BlockchainApi
api = Proxy

type BlockchainApi =
    -- eg. /outputs/14wjVnwHwMAXDr6h5Fw38shCWUB6RSEa63/unspent
    "outputs" :> Capture "address" HC.Address :> "unspent" :> Get '[JSON] [AddressFundingInfo]
    :<|>
    "publishTx" :> ReqBody '[JSON] HT.Tx :> Post '[JSON] HT.TxHash
