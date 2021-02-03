module Service where

import Control.Exception
import Data.Word (Word32, Word8)
import Network.Xoken.Block.Merkle
import Network.Xoken.Transaction.Common

data ServiceException
    = KeyValueDBLookupException
    | GraphDBLookupException
    | InvalidOutputAddressException
    deriving (Show)

instance Exception ServiceException

buildProof :: [TxHash] -> Word32 -> PartialMerkleTree
buildProof hashes index =
    snd $
    buildPartialMerkle $
    zipWith
        (\h i ->
             if i == index
                 then (h, True)
                 else (h, False))
        hashes
        [0 ..]