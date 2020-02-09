-- See suftree.hs.
-- This is implementation of simplified Weiner algorithm to build a suffix tree.
-- The implementation is slightly shorter than the implementation of Ukkonen's
-- algorithm, but it is slower.
-- To build a suffix tree of a string us `construct`.
-- The implementation is close to C++ implementation which can be found here:
-- https://pastebin.com/qE1iQA1g
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap as M
import Data.Maybe

data BNode = BNode {
  pos :: Int, len :: Int, par :: Int,
  to :: M.IntMap Int, link :: M.IntMap Int
} deriving Show

getTo c (BNode _ _ _ to _) = M.lookup (fromEnum c) to
getLink c (BNode _ _ _ _ lnk) = M.lookup (fromEnum c) lnk
setTo c id nd@(BNode _ _ _ to _) = nd { to = M.insert (fromEnum c) id to }
setLink c id nd@(BNode _ _ _ _ lnk) = nd { link = M.insert (fromEnum c) id lnk }

data TreeBuilderState = TreeBuilderState {
  str :: BS.ByteString,
  nodes :: M.IntMap BNode,
  n_nodes :: Int
} deriving Show

type TreeBuilder a = State TreeBuilderState a

addNode :: BNode -> TreeBuilder Int
addNode node = do
  tst@(TreeBuilderState _ ns nr) <- get
  put $ tst { nodes = M.insert nr node ns, n_nodes = nr + 1 }
  return nr

updNode :: (BNode -> BNode) -> Int -> TreeBuilder ()
updNode fn v = do
  tst@(TreeBuilderState _ ns _) <- get
  put $ tst { nodes = M.adjust fn v ns }

attach child_id parent_id c child_len = do
  updNode (setTo c child_id) parent_id
  updNode (\child -> child { len = child_len, par = parent_id }) child_id

treeExtend :: Int -> TreeBuilder ()
treeExtend i = do
  TreeBuilderState s nodes sz_old <- get
  let hasLink v = (fromEnum $ s `BS.index` i) `M.member` (link $ nodes M.! v)
      (r_path, v : _) =
        span (not . hasLink) $ iterate (par . (nodes M.!)) (sz_old - 1)
      path = reverse r_path
      lens = map (len . (nodes M.!)) path
      vlen = (BS.length s - i) - (sum lens)
      w = fromJust $ getLink  (s `BS.index` i) $ nodes M.! v
      m_u = getTo (s `BS.index` (i + vlen)) $ nodes M.! w
  (wn, vlenn) <- case m_u of
    Nothing -> return (w, vlen)  -- No split.
    Just u -> do                 -- Split edge.
      let BNode upos ulen _ _ _ = nodes M.! u
          beforeSplit (_, l) = s `BS.index` (upos - ulen + l) ==
                               s `BS.index` (i + vlen + l)
          (ip, _):_ = dropWhile beforeSplit ([0..] `zip` (scanl (+) 0 lens))
          len = sum $ take ip lens
          (vn, pn, vlenn) = ((v:path) !! ip, upos - ulen + len, vlen + len)
      split_node_id <- addNode $ BNode pn 0 0 M.empty M.empty
      attach split_node_id w (s `BS.index` (upos - ulen)) (ulen - (upos - pn))
      attach u split_node_id (s `BS.index` pn) (upos - pn)
      updNode (setLink (s `BS.index` i) split_node_id) vn
      return (split_node_id, vlenn)

  new_node_id <- addNode $ BNode (BS.length s) 0 0 M.empty M.empty
  updNode (setLink (s `BS.index` i) new_node_id) (sz_old - 1)
  attach new_node_id wn (s `BS.index` (i + vlenn)) (BS.length s - (i + vlenn))

buildTree :: BS.ByteString -> TreeBuilderState
buildTree s = snd $ (flip runState initial) $ do
    forM_ [BS.length s - 1, BS.length s - 2 .. 0] $ \i -> treeExtend i
  where initial = TreeBuilderState s (M.fromList [(0, fake_root), (1, root)]) 2
        fake_root = BNode 0 0 0 M.empty $
            M.fromAscList [(fromEnum i, 1) | i <- [minBound::Char .. maxBound]]
        root = BNode 0 1 0 M.empty M.empty

data STree = Leaf | Node (M.IntMap (BS.ByteString, STree)) deriving Show

treeBuilderStateToSTree :: TreeBuilderState -> STree
treeBuilderStateToSTree (TreeBuilderState s nodes _) = snd $ toSTree_ 1
  where toSTree_ v
          | null children = (node_str, Leaf)
          | otherwise = (node_str, Node children)
          where node_str = BS.take len $ BS.drop (pos - len) s
                BNode pos len _ next _ = nodes M.! v
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
