-- Suffix tree for s is a compressed trie which contains all the suffixes of s.
-- Suffix tree has at most 2|s|+1 vertices and can be built in O(|s|).
-- This implementation builds a tree using Ukkonen's algorithm in O(|s| log |s|)
-- (extra log |s| comes from the need for random access of nodes which is done
-- using IntMaps). Operations to extend a tree with are implemented with State
-- monad which stores current tree and extra information to extend it.
-- To build a suffix tree of a string us `construct`.
-- The implementation is close to C++ implementation which can be found here:
-- https://cp-algorithms.com/string/suffix-tree-ukkonen.html
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap as M
import Data.Maybe

data BNode = BNode {
  left :: Int, right :: Int,
  par :: Maybe Int, link :: Maybe Int,
  next :: M.IntMap Int
} deriving Show

emptyNode = BNode 0 0 Nothing Nothing M.empty
getNext c (BNode _ _ _ _ nxt) = M.lookup (fromEnum c) nxt
setNext c i nd@(BNode _ _ _ _ nxt) = nd { next = M.insert (fromEnum c) i nxt }

data TreeState = TreeState { node_id :: Int, pos :: Int } deriving Show

data TreeBuilderState = TreeBuilderState {
  str :: BS.ByteString,
  ptr :: TreeState,
  nodes :: M.IntMap BNode,
  n_nodes :: Int
} deriving Show

type TreeBuilder a = State TreeBuilderState a

addNode :: BNode -> TreeBuilder Int
addNode node = do
  tst@(TreeBuilderState _ _ ns nr) <- get
  put $ tst { nodes = M.insert nr node ns, n_nodes = nr + 1 }
  return nr

updNode :: (BNode -> BNode) -> Int -> TreeBuilder ()
updNode fn v = do
  tst@(TreeBuilderState _ _ ns _) <- get
  put $ tst { nodes = M.adjust fn v ns }

getNode :: Int -> TreeBuilder BNode
getNode v = ((M.! v) . nodes) <$> get

go :: TreeState -> Int -> Int -> TreeBuilder (Maybe TreeState)
go st@(TreeState node_id pos) l r = do
  if l >= r then return $ Just st
  else do
    node@(BNode ln rn _ _ _) <- getNode node_id
    s <- str <$> get
    if pos == rn - ln then
      case getNext (s `BS.index` l) node of
        Nothing -> return Nothing
        Just next -> go (TreeState next 0) l r
    else if (s `BS.index` (ln + pos)) /= (s `BS.index` l) then
      return Nothing
    else if r - l < rn - ln - pos then
      return $ Just $ st { pos = pos + r - l }
    else
      go (TreeState node_id (rn - ln)) (l + rn - ln - pos) r

splitNode :: TreeState -> TreeBuilder Int
splitNode (TreeState node_id pos) = do
  TreeBuilderState s _ nodes _ <- get
  let BNode ln rn par _ _ = nodes M.! node_id
  if pos == rn - ln then return node_id
  else if pos == 0 then return $ fromJust par
  else do
    let cp = fromEnum $ s `BS.index` (ln + pos)
    new_id <- addNode (BNode ln (ln + pos) par Nothing $ M.singleton cp node_id)
    updNode (\node -> node { left = ln + pos, par = Just new_id }) node_id
    when (isJust par) $ do
      updNode (setNext (s `BS.index` ln) new_id) $ fromJust par
    return new_id

getLink :: Int -> TreeBuilder Int
getLink v = do
  BNode vl vr par lnk _ <- ((M.! v) . nodes) <$> get
  if isNothing lnk then do
    case par of
      Nothing -> return 0
      Just p -> do
        to <- getLink p
        BNode to_l to_r _ _ _ <- ((M.! to) . nodes) <$> get
        let s_to_l = vl + if p == 0 then 1 else 0
        go_st <- go (TreeState to $ to_r - to_l) s_to_l vr
        new_lnk <- splitNode (fromJust go_st)
        updNode (\old_node -> old_node {link = Just new_lnk}) v
        return new_lnk
  else return $ fromJust lnk

treeExtend :: Int -> TreeBuilder ()
treeExtend pos = do
  TreeBuilderState s cptr _ _ <- get
  m_nptr <- go cptr pos (pos + 1)
  case m_nptr of
    Just nptr -> do
      tst <- get
      put tst { ptr = nptr }
    Nothing -> do
      mid <- splitNode cptr
      leaf_id <- addNode (BNode pos (BS.length s) (Just mid) Nothing M.empty)
      updNode (setNext (s `BS.index` pos) leaf_id) mid
      when (mid /= 0) $ do
        mid_lnk <- getLink mid
        BNode lnk_l lnk_r _ _ _ <- getNode mid_lnk
        tst <- get
        put $ tst { ptr = TreeState mid_lnk $ lnk_r - lnk_l }
        treeExtend pos

buildTree :: BS.ByteString -> TreeBuilderState
buildTree s = snd $ (flip runState initial) $ do
    forM_ [0 .. BS.length s - 1] $ \i -> treeExtend i
  where initial = TreeBuilderState s (TreeState 0 0) (M.singleton 0 emptyNode) 1

data STree = Leaf | Node (M.IntMap (BS.ByteString, STree)) deriving Show

treeBuilderStateToSTree :: TreeBuilderState -> STree
treeBuilderStateToSTree (TreeBuilderState s _ nodes _) = snd $ toSTree_ 0
  where toSTree_ v
          | null children = (node_str, Leaf)
          | otherwise = (node_str, Node children)
          where node_str = BS.take (rt - lt) $ BS.drop lt s
                BNode lt rt _ _ next = nodes M.! v
                children = fmap toSTree_ next

construct = treeBuilderStateToSTree . buildTree

-- End cut here.
-- Latin America Regional Contest 2019 problem G - Gluing Pictures

readInt s = let Just (a, _) = BS.readInt s in a
alpha c = ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9')

main = BS.interact $
    BS.unlines . map (BS.pack . show) . solve . map (BS.filter alpha) . BS.lines

solve (s : n : qs) = map (solveQuery t) $ take (readInt n) qs
  where t = construct s

solveQuery :: STree -> BS.ByteString -> Int
solveQuery t s
  | BS.null s = 0
  | l == 0 = -1
  | rest == -1 = -1
  | otherwise = 1 + rest
  where l = commonPrefTree t s
        rest = solveQuery t (BS.drop l s)

commonPrefLen a b
  | BS.null a || BS.null b = 0
  | BS.head a == BS.head b = 1 + commonPrefLen (BS.tail a) (BS.tail b)
  | otherwise = 0

commonPrefTree Leaf _ = 0
commonPrefTree (Node ls) s
  | BS.null s = 0
  | BS.length p > l = l
  | otherwise = l + commonPrefTree t (BS.drop l s)
  where cmp (_, _, a) (_, _, b) = compare a b
        (p, t) = fromMaybe (BS.empty, Leaf) $ M.lookup (fromEnum $ BS.head s) ls
        l = commonPrefLen p s
