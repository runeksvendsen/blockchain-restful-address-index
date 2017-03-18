{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, RecordWildCards #-}
module Lib.TxOutProof.Types
( FundingProof(..)
, verifyFundingProof
)
where

import qualified Network.Haskoin.Block                  as HB
import qualified Network.Haskoin.Transaction            as HT
import           Data.Aeson (ToJSON, FromJSON, Value(String), toJSON, parseJSON, withText)

import qualified Data.ByteString.Base16     as B16
import qualified Data.Serialize             as Bin
import           Data.String.Conversions      (cs)
import           GHC.Generics
{-# ANN module ("HLint: ignore Use camelCase"::String) #-}


-- | Independently verifiable proof that a transaction was included in a block.
data FundingProof = FundingProof
    { -- | Transaction of interest. Has an output that sends funds to our address.
      proof_tx_data :: HT.Tx
      -- | Block which includes transaction.
    , proof_data    :: HB.MerkleBlock
    } deriving (Generic, ToJSON, FromJSON)

verifyFundingProof :: FundingProof -> Either String HT.TxHash
verifyFundingProof FundingProof{..} =
    verifyMerkleBlock proof_data >>= checkTxHash
  where
    txHash = HT.txHash proof_tx_data
    checkTxHash idL =
        if txHash `elem` idL then
            Right txHash
        else
            Left "Proof is not for transaction of interest"

-- | Verify 'HB.MerkleBlock', return the txids proven to be in the block.
verifyMerkleBlock :: HB.MerkleBlock -> Either String [HT.TxHash]
verifyMerkleBlock HB.MerkleBlock{..} =
    HB.extractMatches mFlags mHashes (fromIntegral merkleTotalTxns)
        >>= \(merk, txL) ->
            if merk == HB.merkleRoot merkleHeader then
                Right txL
            else
                Left $ "Merkle root mismatch. Block header: " ++
                    show (HB.merkleRoot merkleHeader) ++
                    ", calculated: " ++ show merk


instance ToJSON HB.MerkleBlock where
    toJSON = String . cs . B16.encode . Bin.encode

instance FromJSON HB.MerkleBlock where
    parseJSON = withText "MerkleBlock" $
        either fail return . Bin.decode . fst . B16.decode . cs

instance ToJSON HB.BlockHeader where
    toJSON = String . cs . B16.encode . Bin.encode

instance FromJSON HB.BlockHeader where
    parseJSON = withText "BlockHeader" $
        either fail return . Bin.decode . fst . B16.decode . cs

-- data ProofResp = ProofResp { proof_data :: HB.MerkleBlock } deriving (Generic, ToJSON, FromJSON)

