{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Lib.FundingInfo.Types where

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, Value(Object), (.:),
                             object, (.=), encode, decode)
import           Data.Word (Word32)
import           Control.Monad (mzero)
import           GHC.Generics


tmp =
     "000000204ebd37c31d079f9ba9c2ea159962a12009c9d6cc0dcf4a020000000000000000281c6a2f55492ff926d7dc1312775fec0e31ce7f8adb3f"
 ++  "b7d21440a02ee881588dba3858d4e603187d24fd99700800000d1947afa1e8d425f32da9d91548d805450c74a8b69468a8de1fcf7c6e111c35ffc3"
 ++  "0b7975e582ac362d8f10dc237ceffb0985ba743d02bb55824c1e2a425237e7c35a1ea8317388491417bf93a011393e601cf9a1b0b1a48e9b3de4a2"
 ++  "3f904f00b77cfd548b38e7691baece39f88fe6ad53de2e4aa4f863b886b1e9cdcf4303db8141205911006beea387f2abd9e54348af0b0ab1604295"
 ++  "d14200bdd32a59c4f2139362b0318d9e6739a87f2d19f956b301b8ae2b891983ce96ce0f5e19539cefcd7ab376aa993b8436ad339381fccd5c3d8b"
 ++  "78137c2332c637f84fcab04fc018636894a4c7ac74f199d944e67f80d3f0df1edef1c4754883df22e8f48e9638d47d6117e68eb78d3b7653917579"
 ++  "325e8714a6c857c4e44a2cdaf207eff0da7ef5aebfab1c95afe4e3b75ca6e066f3c1ee542239196ad3c0d475980fffbfae3acefb36a63f0ebcf751"
 ++  "3d4870f978a4da822c370182cebb7974c8cf61d9f527385425de0368a6de8b5cc35f4a6b9b56b50f41f4bfdb95e9212564e58eb1e63ff77909dfda"
 ++  "089e3e42ef1c5fd74e5ee7994ff27295d643f023a0d35a94d96510f37404abbd0a00"

-- | Holds information about an output paying to an address
data AddressFundingInfo = AddressFundingInfo {
    asiDestAddress  ::  HC.Address
   ,asiFundingTxId  ::  HT.TxHash
   ,asiFundingVout  ::  Word32
   ,asiConfs        ::  Integer
   ,asiValue        ::  Integer
} deriving (Eq, Show, Generic)


instance ToJSON AddressFundingInfo where
    toJSON (AddressFundingInfo addr txid vout confs val) =
        object [
            "address" .= addr,
            "funding_txid" .= txid,
            "funding_vout" .= vout,
            "confirmations" .= confs,
            "value" .= val
        ]

instance FromJSON AddressFundingInfo where
    parseJSON (Object o) =
        AddressFundingInfo <$>
            o .: "address" <*>
            o .: "funding_txid" <*>
            o .: "funding_vout" <*>
            o .: "confirmations"  <*>
            o .: "value"
    parseJSON _ = mzero

-- instance ToSchema AddressFundingInfo