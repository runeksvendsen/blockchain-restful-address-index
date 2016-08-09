{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module APISpec.Blockchain
(
    BlockchainApi
)
where

import           Lib.FundingInfo.Types            (AddressFundingInfo)

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC

import  Servant.API


type BlockchainApi =
    -- eg. /outputs/14wjVnwHwMAXDr6h5Fw38shCWUB6RSEa63/unspent
    "outputs" :> Capture "address" HC.Address :> "unspent" :> Get '[JSON] [AddressFundingInfo]
    :<|>
    "publishTx" :> ReqBody '[PlainText] HT.Tx :> Post '[PlainText] HT.TxHash
