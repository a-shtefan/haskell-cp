-- Treap is a structure which is a binary search tree with respect to one key
-- (usually referred to as x) and a heap with respect to another key
-- (usually referred to as y). When y is chosen to be random, expected height
-- of treap is O(log n) where n is the number of elements. Treap supports
-- split and merge operations. Split takes a treap and a key k and splits it
-- into a treap which has keys < k and a treap which has keys >= k. Merge takes
-- two treaps l and r such that keys in l are strictly less than all the keys in
-- r, and merges them into one treap. All regular operations performed on BST
-- can be expressed in terms of split and merge (e.g. adding key x can be done
-- by splitting treap by x to l and r, and then merging l `merge` x `merge` r).
-- This treap implementation uses implicit keys, key of a node in the structure
-- is the number of elements located to the left of the node in in-order tree
-- traversal. This effectively allows to have arrays that can be easily split
-- into parts and concatenated in O(log n) expected time.
import qualified Data.ByteString.Char8 as BS
import qualified System.Random as RND

data Treap a = Leaf | Node { cnt :: Int, prior :: Int
                           , val :: a
                           , lt :: Treap a, rt :: Treap a }

ccnt Leaf = 0
ccnt v = cnt v

singleton pr a = Node 1 pr a Leaf Leaf

combineNodes pr a Leaf Leaf = singleton pr a
combineNodes pr a l Leaf = Node (cnt l + 1) pr a l Leaf
combineNodes pr a Leaf r = Node (cnt r + 1) pr a Leaf r
combineNodes pr a l@(Node cl _ _ _ _) r@(Node cr _ _ _ _) =
  Node (cl + cr + 1) pr a l r

split x tree = split_ tree 0
  where split_ Leaf _ = (Leaf, Leaf)
        split_ tree@(Node _ p a l r) add
          | x <= cKey = (fst splitl, combineNodes p a (snd splitl) r)
          | otherwise = (combineNodes p a l (fst splitr), snd splitr)
          where cKey = add + ccnt l
                splitl = split_ l add
                splitr = split_ r (cKey + 1)

merge Leaf r = r
merge l Leaf = l
merge l@(Node _ p a tl tr) r@(Node _ p' a' tl' tr')
  | p > p'    = combineNodes p a tl (merge tr r)
  | otherwise = combineNodes p' a' (merge l tl') tr'

nextRand :: Int -> Int
nextRand = (+) 1442695040888963407 . (*) 6364136223846793005

build vals = foldl merge Leaf $ zipWith singleton rands vals
  where gen = RND.mkStdGen 123456
        rands = map fst $ iterate (RND.next . snd) (0, gen)
-- Alternatively rands can be
-- iterate ((+) 1442695040888963407 . (*) 6364136223846793005) 123456

toList tree = toList_ tree []
  where toList_ Leaf ans = ans
        toList_ (Node _ _ a lt rt) ans = toList_ lt (a : toList_ rt ans)

-- End cut here.
-- Hackerrank problem Array and simple queries

readInt s = let Just (a, _) = BS.readInt s in a

solve ([n,q]:lst:qs) = [[abs $ vf - vl], toList t]
  where t = foldl query (build lst) qs
        vf = val $ fst $ split 1 t
        vl = val $ snd $ split (n - 1) t

query t [qt, i, j] = [mid `merge` rest, rest `merge` mid] !! (qt - 1)
  where (lt, mid_rt) = split (i - 1) t
        (mid, rt) = split (j - i + 1) mid_rt
        rest = lt `merge` rt

main = BS.interact $ BS.unlines . map (BS.unwords . map (BS.pack . show)) . solve .
                     map (map readInt . BS.words) . BS.lines
