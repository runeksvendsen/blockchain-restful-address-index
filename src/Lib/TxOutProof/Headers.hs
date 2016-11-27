{-# LANGUAGE OverloadedStrings #-}
module Lib.TxOutProof.Headers where

import           Util
import           Lib.TxOutProof.Proof                     (convOrFail)
import qualified Network.Bitcoin.Api.Types.HeaderInfo   as HDI
import           Network.Bitcoin.Api.Client               (Client)
import           Network.Bitcoin.Api.Blockchain         as Chain


import qualified Network.Haskoin.Block          as HB
import           Data.HexString                 as Hex
import qualified Data.Serialize                 as Bin


blockHeadersAfter client hash numHdr = do
    hashL  <- blockHashesAfter client numHdr hash
    hdrLHx <- forM (map convOrFail hashL) (Chain.getBlockHeader client)
    let hdrL = map (either error id . Bin.decode . Hex.toBytes) hdrLHx
    return hdrL

-- | Get, at most, the specified number of header hashes after the given header.
blockHashesAfter :: Client -> Word -> HB.BlockHash -> IO [HB.BlockHash]
blockHashesAfter c = blockHashesAfter' c []

blockHashesAfter' :: Client -> [HB.BlockHash] -> Word -> HB.BlockHash -> IO [HB.BlockHash]
blockHashesAfter' _ hashAccum 0 _    = return $ reverse hashAccum
blockHashesAfter' c hashAccum n hash =
    nextBlockHash c hash >>= maybe (return $ reverse hashAccum) loop
  where
    loop h = blockHashesAfter' c (h : hashAccum) (n-1) h

nextBlockHash :: Client -> HB.BlockHash -> IO (Maybe HB.BlockHash)
nextBlockHash client initBH =
    convOrFail . HDI.nextblockhash <$> getBlockHeaderInfo client (convOrFail initBH)
