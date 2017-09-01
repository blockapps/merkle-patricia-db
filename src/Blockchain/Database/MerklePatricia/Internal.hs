{-# LANGUAGE OverloadedStrings #-}

module Blockchain.Database.MerklePatricia.Internal (
  Key,
  Val,
  MPDB(..),
  StateRoot(..),
  NodeData(..),
  openMPDB,
  emptyTriePtr,
  sha2StateRoot,
  unboxStateRoot,
  unsafePutKeyVal,
  unsafeGetKeyVals,
  unsafeGetAllKeyVals,
  unsafeDeleteKey,
  getNodeData,
  putNodeData,
  keyToSafeKey,
  getCommonPrefix,
  replace,
  prependToKey
  ) where

import           Control.Monad.Trans.Resource
import           Data.ByteArray                         (convert)
import           Crypto.Hash                            as Crypto
import qualified Data.ByteString                              as B
import           Data.Default
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified Data.NibbleString                            as N
import qualified Database.LevelDB                             as DB

import           Blockchain.Data.RLP
import           Blockchain.Database.MerklePatricia.MPDB
import           Blockchain.Database.MerklePatricia.NodeData
import           Blockchain.Database.MerklePatricia.StateRoot

unsafePutKeyVal::MonadResource m=>MPDB->Key->Val->m MPDB
unsafePutKeyVal db key val = do
  dbNodeData <- getNodeData db (PtrRef $ stateRoot db)
  dbPutNodeData <- putKV_NodeData db key val dbNodeData
  p <- putNodeData db dbPutNodeData
  return db{stateRoot=p}

unsafeGetKeyVals::MonadResource m=>MPDB->Key->m [(Key, Val)]
unsafeGetKeyVals db =
  let dbNodeRef = PtrRef $ stateRoot db
  in getKeyVals_NodeRef db dbNodeRef

unsafeGetAllKeyVals::MonadResource m=>MPDB->m [(Key, Val)]
unsafeGetAllKeyVals db = unsafeGetKeyVals db N.empty

unsafeDeleteKey::MonadResource m=>MPDB->Key->m MPDB
unsafeDeleteKey db key = do
  dbNodeData <- getNodeData db (PtrRef $ stateRoot db)
  dbDeleteNodeData <- deleteKey_NodeData db key dbNodeData
  p <- putNodeData db dbDeleteNodeData
  return db{stateRoot=p}

keyToSafeKey::N.NibbleString->N.NibbleString
keyToSafeKey key =
  N.EvenNibbleString $ convert $ (Crypto.hash keyByteString :: Crypto.Digest Crypto.Keccak_256)
  where
    N.EvenNibbleString keyByteString = key


putKV_NodeData::MonadResource m=>MPDB->Key->Val->NodeData->m NodeData

putKV_NodeData _ key val EmptyNodeData =
  return $ ShortcutNodeData key (Right val)

putKV_NodeData db key val (FullNodeData options nodeValue)
  | options `slotIsEmpty` N.head key =
    do
      tailNode <- newShortcut db (N.tail key) $ Right val
      return $ FullNodeData (replace options (N.head key) tailNode) nodeValue

  | otherwise =
      do
        let conflictingNodeRef = options!!fromIntegral (N.head key)
        newNode <- putKV_NodeRef db (N.tail key) val conflictingNodeRef
        return $ FullNodeData (replace options (N.head key) newNode) nodeValue

putKV_NodeData db key1 val1 (ShortcutNodeData key2 val2)
  | key1 == key2 =
    case val2 of
      Right _  -> return $ ShortcutNodeData key1 $ Right val1
      Left ref -> do
        newNodeRef <- putKV_NodeRef db key1 val1 ref
        return $ ShortcutNodeData key2 (Left newNodeRef)

  | N.null key1 = do
      newNodeRef <- newShortcut db (N.tail key2) val2
      return $ FullNodeData (list2Options 0 [(N.head key2, newNodeRef)]) $ Just val1

  | key1 `N.isPrefixOf` key2 = do
      tailNode <- newShortcut db (N.drop (N.length key1) key2) val2
      modifiedTailNode <- putKV_NodeRef db "" val1 tailNode
      return $ ShortcutNodeData key1 $ Left modifiedTailNode

  | key2 `N.isPrefixOf` key1 =
    case val2 of
      Right val -> putKV_NodeData db key2 val (ShortcutNodeData key1 $ Right val1)
      Left ref  -> do
        newNode <- putKV_NodeRef db (N.drop (N.length key2) key1) val1 ref
        return $ ShortcutNodeData key2 $ Left newNode

  | N.head key1 == N.head key2 =
    let (commonPrefix, suffix1, suffix2) =
          getCommonPrefix (N.unpack key1) (N.unpack key2)
    in do
      nodeAfterCommonBeforePut <- newShortcut db (N.pack suffix2) val2
      nodeAfterCommon <- putKV_NodeRef db (N.pack suffix1) val1 nodeAfterCommonBeforePut
      return $ ShortcutNodeData (N.pack commonPrefix) $ Left nodeAfterCommon

  | otherwise = do
      tailNode1 <- newShortcut db (N.tail key1) $ Right val1
      tailNode2 <- newShortcut db (N.tail key2) val2
      return $ FullNodeData
        (list2Options 0 $ sortBy (compare `on` fst)
         [(N.head key1, tailNode1), (N.head key2, tailNode2)])
        Nothing

-----

getKeyVals_NodeData::MonadResource m=>MPDB->NodeData->Key->m [(Key, Val)]

getKeyVals_NodeData _ EmptyNodeData _ = return []

getKeyVals_NodeData db (FullNodeData {choices=cs}) "" = do
  partialKVs <- sequence $ (\ref -> getKeyVals_NodeRef db ref "") <$> cs
  return $ concatMap
    (uncurry $ map . (prependToKey . N.singleton)) (zip [0..] partialKVs)

getKeyVals_NodeData db (FullNodeData {choices=cs}) key
  | ref == emptyRef = return []
  | otherwise = fmap (prependToKey $ N.singleton $ N.head key) <$>
                getKeyVals_NodeRef db ref (N.tail key)
  where ref = cs !! fromIntegral (N.head key)

getKeyVals_NodeData db ShortcutNodeData{nextNibbleString=s, nextVal=Left ref} key
  | key `N.isPrefixOf` s = prependNext ""
  | s `N.isPrefixOf` key = prependNext $ N.drop (N.length s) key
  | otherwise = return []
  where prependNext key' = fmap (prependToKey s) <$> getKeyVals_NodeRef db ref key'

getKeyVals_NodeData _ ShortcutNodeData{nextNibbleString=s, nextVal=Right val} key =
  return $
    if key `N.isPrefixOf` s
    then [(s,val)]
    else []

-----

deleteKey_NodeData::MonadResource m=>MPDB->Key->NodeData->m NodeData

deleteKey_NodeData _ _ EmptyNodeData = return EmptyNodeData

deleteKey_NodeData db key nd@(FullNodeData options val)
  | N.null key = return $ FullNodeData options Nothing

  | options `slotIsEmpty` N.head key = return nd

  | otherwise = do
    let nodeRef = options!!fromIntegral (N.head key)
    newNodeRef <- deleteKey_NodeRef db (N.tail key) nodeRef
    let newOptions = replace options (N.head key) newNodeRef
    simplify_NodeData db $ FullNodeData newOptions val

deleteKey_NodeData _ key1 nd@(ShortcutNodeData key2 (Right _)) =
  return $
    if key1 == key2
    then EmptyNodeData
    else nd

deleteKey_NodeData db key1 nd@(ShortcutNodeData key2 (Left ref))
  | key2 `N.isPrefixOf` key1 = do
    newNodeRef <- deleteKey_NodeRef db (N.drop (N.length key2) key1) ref
    simplify_NodeData db $ ShortcutNodeData key2 $ Left newNodeRef

  | otherwise = return nd

-----

putKV_NodeRef::MonadResource m=>MPDB->Key->Val->NodeRef->m NodeRef
putKV_NodeRef db key val nodeRef = do
  nodeData <- getNodeData db nodeRef
  newNodeData <- putKV_NodeData db key val nodeData
  nodeData2NodeRef db newNodeData


getKeyVals_NodeRef::MonadResource m=>MPDB->NodeRef->Key->m [(Key, Val)]
getKeyVals_NodeRef db ref key = do
  nodeData <- getNodeData db ref
  getKeyVals_NodeData db nodeData key

--TODO- This is looking like a lift, I probably should make NodeRef some sort of Monad....

deleteKey_NodeRef::MonadResource m=>MPDB->Key->NodeRef->m NodeRef
deleteKey_NodeRef db key nodeRef =
  nodeData2NodeRef db =<< deleteKey_NodeData db key =<< getNodeData db nodeRef

-----

getNodeData::MonadResource m=>MPDB->NodeRef->m NodeData
getNodeData _ (SmallRef x) = return $ rlpDecode $ rlpDeserialize x
getNodeData db (PtrRef ptr@(StateRoot p)) = do
  bytes <-
    fromMaybe
    (error $ "Missing StateRoot in call to getNodeData: " ++ formatStateRoot ptr) <$>
    DB.get (ldb db) def p
  return $ bytes2NodeData bytes
    where
      bytes2NodeData::B.ByteString->NodeData
      bytes2NodeData bytes | B.null bytes = EmptyNodeData
      bytes2NodeData bytes = rlpDecode $ rlpDeserialize $ B.pack $ B.unpack bytes

putNodeData::MonadResource m=>MPDB->NodeData->m StateRoot
putNodeData db nd = do
  let bytes = rlpSerialize $ rlpEncode nd
      ptr = convert $ (Crypto.hash bytes :: Crypto.Digest Crypto.Keccak_256)

  DB.put (ldb db) def ptr bytes
  return $ StateRoot ptr

-----

-- Only used to canonicalize the DB after a
-- delete.  We need to concatinate ShortcutNodeData links, convert
-- FullNodeData to ShortcutNodeData when possible, etc.

-- Important note- this function should only apply to immediate items,
-- and not recurse deep into the database (ie- by simplifying all options
-- in a FullNodeData, etc).  Failure to adhere will result in a
-- performance nightmare!  Any delete could result in a full read through
-- the whole database.  The delete function only will "break" the
-- canonical structure locally, so deep recursion isn't required.

simplify_NodeData::MonadResource m=>MPDB->NodeData->m NodeData
simplify_NodeData _ EmptyNodeData = return EmptyNodeData
simplify_NodeData db nd@(ShortcutNodeData key (Left ref)) = do
  refNodeData <- getNodeData db ref
  case refNodeData of
    (ShortcutNodeData key2 v2) -> return $ ShortcutNodeData (key `N.append` key2) v2
    _                          -> return nd
simplify_NodeData db (FullNodeData options Nothing) = do
    case options2List options of
      [(n, nodeRef)] ->
          simplify_NodeData db $ ShortcutNodeData (N.singleton n) $ Left nodeRef
      _ -> return $ FullNodeData options Nothing
simplify_NodeData _ x = return x

-----

newShortcut::MonadResource m=>MPDB->Key->Either NodeRef Val->m NodeRef
newShortcut _ "" (Left ref) = return ref
newShortcut db key val      = nodeData2NodeRef db $ ShortcutNodeData key val

nodeData2NodeRef::MonadResource m=>MPDB->NodeData->m NodeRef
nodeData2NodeRef db nodeData =
  case rlpSerialize $ rlpEncode nodeData of
    bytes | B.length bytes < 32 -> return $ SmallRef bytes
    _     -> PtrRef <$> putNodeData db nodeData

list2Options::N.Nibble->[(N.Nibble, NodeRef)]->[NodeRef]
list2Options start [] = replicate (fromIntegral $ 0x10 - start) emptyRef
list2Options start x | start > 15 =
  error $
  "value of 'start' in list2Option is greater than 15, it is: " ++ show start
  ++ ", second param is " ++ show x
list2Options start ((firstNibble, firstPtr):rest) =
    replicate (fromIntegral $ firstNibble - start) emptyRef ++ [firstPtr] ++ list2Options (firstNibble+1) rest

options2List::[NodeRef]->[(N.Nibble, NodeRef)]
options2List theList = filter ((/= emptyRef) . snd) $ zip [0..] theList

prependToKey::Key->(Key, Val)->(Key, Val)
prependToKey prefix (key, val) = (prefix `N.append` key, val)

replace::Integral i=>[a]->i->a->[a]
replace lst i newVal = left ++ [newVal] ++ right
            where
              (left, _:right) = splitAt (fromIntegral i) lst

slotIsEmpty::[NodeRef]->N.Nibble->Bool
slotIsEmpty [] _ =
  error "slotIsEmpty was called for value greater than the size of the list"
slotIsEmpty (x:_) 0 | x == emptyRef = True
slotIsEmpty _ 0 = False
slotIsEmpty (_:rest) n = slotIsEmpty rest (n-1)


getCommonPrefix::Eq a=>[a]->[a]->([a], [a], [a])
getCommonPrefix (c1:rest1) (c2:rest2)
  | c1 == c2 = prefixTheCommonPrefix c1 (getCommonPrefix rest1 rest2)
  where
    prefixTheCommonPrefix c (p, x, y) = (c:p, x, y)
getCommonPrefix x y = ([], x, y)
