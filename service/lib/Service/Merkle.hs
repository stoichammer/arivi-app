{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Service.Merkle where

import Control.Exception
import Control.Monad.Writer.Lazy
import Data.Aeson
import Data.ByteArray as BA
import Data.ByteString as B
import qualified Data.ByteString.Base16 as B16 (encode)
import Data.ByteString.Short as BSS (fromShort)
import Data.Int
import qualified Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Serialize as DS
import GHC.Generics
import Network.Xoken.Block
import Network.Xoken.Crypto

data MerkleTree =
    MerkleTree
        { leafNodes :: [Hash256]
        , transposeMT :: M.Map Hash256 (Bool, Hash256)
        }
    deriving (Show, Generic)

instance FromJSON MerkleTree

instance ToJSON MerkleTree

instance FromJSONKey Hash256

instance ToJSONKey Hash256

data MerkleNode =
    MerkleNode
        { node :: !(Maybe Hash256)
        , leftChild :: !(Maybe Hash256)
        , rightChild :: !(Maybe Hash256)
        , isLeft :: !Bool
        }
    deriving (Show, Eq, Ord)

type HashCompute = (M.Map Int8 (MerkleNode), [MerkleNode])

emptyHashCompute :: HashCompute
emptyHashCompute = (M.empty, [])

emptyMerkleNode :: MerkleNode
emptyMerkleNode = MerkleNode {node = Nothing, leftChild = Nothing, rightChild = Nothing, isLeft = False}

data MerkleException
    = MerkleTreeInvalidException
    | MerkleBranchComputeException
    deriving (Show)

instance Exception MerkleException

computeTreeHeight :: Int -> Int8
computeTreeHeight ntx
    | ntx < 2 = 0
    | even ntx = 1 + computeTreeHeight (ntx `div` 2)
    | otherwise = computeTreeHeight $ ntx + 1

hashPair :: Hash256 -> Hash256 -> Hash256
hashPair a b = doubleSHA256 $ DS.encode a `B.append` DS.encode b

pushHash :: HashCompute -> Hash256 -> Maybe Hash256 -> Maybe Hash256 -> Int8 -> Int8 -> Bool -> HashCompute
pushHash (stateMap, res) nhash left right ht ind final =
    case node prev of
        Just pv ->
            if ind < ht
                then pushHash
                         ( (M.insert ind emptyMerkleNode stateMap)
                         , (insertSpecial
                                (Just pv)
                                (left)
                                (right)
                                True
                                (insertSpecial (Just nhash) (leftChild prev) (rightChild prev) False res)))
                         (hashPair pv nhash)
                         (Just pv)
                         (Just nhash)
                         ht
                         (ind + 1)
                         final
                else throw MerkleTreeInvalidException -- Fatal error, can only happen in case of invalid leaf nodes
        Nothing ->
            if ht == ind
                then (stateMap, (insertSpecial (Just nhash) left right True res))
                else if final
                         then pushHash
                                  (updateState, (insertSpecial (Just nhash) left right True res))
                                  (hashPair nhash nhash)
                                  (Just nhash)
                                  (Just nhash)
                                  ht
                                  (ind + 1)
                                  final
                         else (updateState, res)
  where
    insertSpecial sib lft rht flg lst = L.insert (MerkleNode sib lft rht flg) lst
    updateState = M.insert ind (MerkleNode (Just nhash) left right True) stateMap
    prev =
        case M.lookupIndex (fromIntegral ind) stateMap of
            Just i -> snd $ M.elemAt i stateMap
            Nothing -> emptyMerkleNode

buildTMT :: [Hash256] -> M.Map Hash256 (Bool, Hash256)
buildTMT hashes =
    let mnodes = snd . runWriter $ runPushHash emptyHashCompute hashes (computeTreeHeight $ L.length hashes)
     in M.fromList . L.concat $
        (\(MerkleNode node lc rc isLeft) ->
             if isNothing lc
                 then []
                 else [(fromJust lc, (True, fromMaybe (throw MerkleTreeInvalidException) node))] <>
                      if isNothing rc
                          then []
                          else [(fromJust rc, (False, fromMaybe (throw MerkleTreeInvalidException) node))]) <$>
        mnodes
  where
    runPushHash _ [] _ = return () :: Writer [MerkleNode] ()
    runPushHash hc (h:hs) ht = do
        let (mmap, mnodes) =
                pushHash
                    hc
                    h
                    Nothing
                    Nothing
                    ht
                    0
                    (if L.null hs
                         then True
                         else False)
        tell mnodes
        runPushHash (mmap, []) hs ht

getMerkleBranch :: Hash256 -> M.Map Hash256 (Bool, Hash256) -> [(Hash256, Bool)]
getMerkleBranch leaf tmt = L.reverse . L.init $ getNextNode [] leaf tmt
  where
    getNextNode branch node tree =
        case M.lookup node tree of
            Nothing -> branch
            (Just (isLeft, par)) -> getNextNode ((node, isLeft) : branch) par tree

getMerkleRoot :: MerkleTree -> Hash256
getMerkleRoot mt =
    let start = L.head $ leafNodes mt
     in crawlUp start $ transposeMT mt
  where
    crawlUp node tree =
        case M.lookup node tree of
            Nothing -> node
            (Just (_, par)) -> crawlUp par tree

getSibling :: Int -> [Hash256] -> (Hash256, Bool)
getSibling index leaves
    | L.length leaves < (index + 1) = throw MerkleBranchComputeException
    | L.length leaves == (index + 1) =
        if odd index
            then (leaves !! (index - 1), True)
            else (leaves !! index, False)
    | otherwise =
        if odd index
            then (leaves !! (index - 1), True)
            else (leaves !! (index + 1), False)

hash256ToHex :: Hash256 -> ByteString
hash256ToHex = B16.encode . fromShort . getHash256

buildMerkleTree :: [Hash256] -> MerkleTree
buildMerkleTree hashes = MerkleTree hashes (buildTMT hashes)

getProof :: Int -> MerkleTree -> [(ByteString, Bool)]
getProof leafIndex MerkleTree {..}
    | leafIndex < L.length leafNodes =
        let sibling = getSibling leafIndex leafNodes
            path = getMerkleBranch (leafNodes !! leafIndex) transposeMT
         in (\(h, b) -> (hash256ToHex h, b)) <$> (sibling : path)
    | otherwise = throw MerkleBranchComputeException
