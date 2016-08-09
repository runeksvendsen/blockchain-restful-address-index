{-# LANGUAGE  OverloadedStrings #-}

module Lib.FundingInfo.FundingInfo
(
    module Lib.FundingInfo.FundingInfo
   ,module Lib.FundingInfo.Types
)

where

import           Lib.FundingInfo.Types
import           Config (BTCRPCConf(..))

import           Network.Bitcoin.Api.Client (Client, withClient)
import           Network.Bitcoin.Api.Blockchain (searchRawTransactions)
import           Network.Bitcoin.Api.UTXO (getTxOut)
import           Network.Bitcoin.Api.Types.TxInfo (TxInfo(..), Vout(..), Vin(..))
import qualified Network.Bitcoin.Api.Types.UnspentTxOut as UTXO (UnspentTxOut(..))
import qualified Network.Haskoin.Crypto as HC
import qualified Data.Bitcoin.Types  as BT
import qualified Data.Base58String.Bitcoin as B58S
import           Data.Base58String.Bitcoin (b58String)
import           Control.Monad (forM)
import           Data.List (sortOn)
import qualified Data.Maybe as Maybe


match :: UTXO.UnspentTxOut -> AddressFundingInfoRes -> Bool
match
    (UTXO.UnspentTxOut utxConfs utxAmount (utxAddr:_))
    (AddressFundingInfoRes afiAddr _ _ afiConfs afiAmount) =
        afiAddr == utxAddr && afiConfs == utxConfs && afiAmount == utxAmount
match
    (UTXO.UnspentTxOut _ _ [])
    (AddressFundingInfoRes _ _ _ _ _) = False

-- | Return information about all outputs in TxInfo paying to a single address if this
--      address equals the specified address.
outputsPayingToAddress :: B58S.Base58String -> TxInfo -> [AddressFundingInfoRes]
outputsPayingToAddress addr TxInfo{ txid = txid, vouts = vouts, confs = confs } =
    map (\vout -> AddressFundingInfoRes addr txid (index vout) confs (amount vout) ) $
         filter ((== addr) . head . addresses) . filter ((== 1) . length . addresses) $ vouts

redeemedBy :: AddressFundingInfoRes -> TxInfo -> Maybe BT.TransactionId
redeemedBy (AddressFundingInfoRes _ afiTxId afiIdx _ _) TxInfo{ txid = redeemingTxId, vins = vins } =
    if any (== True) vinMatchList then Just redeemingTxId else Nothing
        where vinMatchList = map (\(Vin tid idx) -> tid == afiTxId && idx == afiIdx) vins

checkSpentAndConfirmData :: Client -> AddressFundingInfoRes -> IO (Maybe AddressFundingInfoRes)
checkSpentAndConfirmData client afi@(AddressFundingInfoRes _ txid index _ _) =
    getTxOut client txid index True >>=     -- True = include unconfirmed
        maybe (return Nothing)
            (\utxOut -> if utxOut `match` afi then return (Just afi) else return Nothing)

getUnredeemedOutputs' :: BTCRPCConf -> B58S.Base58String -> IO [AddressFundingInfoRes]
getUnredeemedOutputs' (BTCRPCConf host port user pass _) addr =
    withClient host port user pass $
        \client -> do
            txiList <- searchRawTransactions client addr
            let txiListNewestFirst = reverse $ sortOn timestamp txiList
            let outsPayingAddress = concat $ fmap (outputsPayingToAddress addr) txiListNewestFirst
            Maybe.catMaybes <$> forM outsPayingAddress (checkSpentAndConfirmData client)

getUnredeemedOutputs :: BTCRPCConf -> HC.Address -> IO [AddressFundingInfo]
getUnredeemedOutputs conf addr =
    map toHaskoin <$> getUnredeemedOutputs' conf (b58String . HC.addrToBase58 $ addr)
