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
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Value(Object, String), FromJSON, ToJSON, parseJSON, toJSON,
                            fromJSON, (.=), (.:), object)
import qualified Data.Text as T
import           Data.Word (Word32)
import           Data.List (sortOn, concat)
import           Data.Maybe (isJust, fromJust, listToMaybe, catMaybes)
import           Data.String.Conversions (cs)
import           Data.Fixed (showFixed)



match :: UTXO.UnspentTxOut -> AddressFundingInfo -> Bool
match
    (UTXO.UnspentTxOut utxConfs utxAmount (utxAddr:_))
    (AddressFundingInfo afiAddr _ _ afiConfs afiAmount) =
        afiAddr == utxAddr && afiConfs == utxConfs && afiAmount == utxAmount
match
    (UTXO.UnspentTxOut utxConfs utxAmount [])
    (AddressFundingInfo afiAddr _ _ afiConfs afiAmount) = False

-- | Return information about all outputs in TxInfo paying to a single address if this
--      address equals the specified address.
outputsPayingToAddress :: B58S.Base58String -> TxInfo -> [AddressFundingInfo]
outputsPayingToAddress addr txi@TxInfo{ txid = txid, vouts = vouts, confs = confs } =
    map (\vout -> AddressFundingInfo addr txid (index vout) confs (amount vout) ) $
         filter ((== addr) . head . addresses) . filter ((== 1) . length . addresses) $ vouts

redeemedBy :: AddressFundingInfo -> TxInfo -> Maybe BT.TransactionId
redeemedBy (AddressFundingInfo _ afiTxId afiIdx _ _) txi@TxInfo{ txid = redeemingTxId, vins = vins } =
    if any (== True) vinMatchList then Just redeemingTxId else Nothing
        where vinMatchList = map (\(Vin tid idx) -> tid == afiTxId && idx == afiIdx) vins

checkSpentAndConfirmData :: Client -> AddressFundingInfo -> IO (Maybe AddressFundingInfo)
checkSpentAndConfirmData client afi@(AddressFundingInfo addr txid index _ _) =
    getTxOut client txid index True >>=     -- True = include unconfirmed
        maybe (return Nothing)
            (\utxOut -> if utxOut `match` afi then return (Just afi) else return Nothing)

getUnredeemedOutputs' :: BTCRPCConf -> B58S.Base58String -> IO [AddressFundingInfo]
getUnredeemedOutputs' (BTCRPCConf host port user pass _) addr =
    withClient host port user pass $
        \client -> do
            txiList <- searchRawTransactions client addr
            let txiListNewestFirst = reverse $ sortOn timestamp txiList
            let outsPayingAddress = concat $ fmap (outputsPayingToAddress addr) txiListNewestFirst
            catMaybes <$> forM outsPayingAddress (checkSpentAndConfirmData client)

getUnredeemedOutputs :: BTCRPCConf -> HC.Address -> IO [AddressFundingInfo]
getUnredeemedOutputs conf addr =
    getUnredeemedOutputs' conf (b58String . HC.addrToBase58 $ addr)
