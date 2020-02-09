-- For information about treap see treap.hs.
-- In addition to implementation in treap.hs, this one also supports
-- calculation of some associative operation on all the elements in a subtree
-- (e.g. sum, minimum). This can be used to calculate the operation on some
-- subrange by splitting the structure and taking the result from the root of
-- the tree corresponding to the subrange.
-- To use the structure with an element of type t, t has to be an instance of
-- Combinable, which means that there has to be `combine :: t -> t -> t` an
-- associative operation.
import qualified Data.ByteString.Char8 as BS
import qualified System.Random as RND

data Treap a = Leaf | Node { cnt :: Int, prior :: Int
                           , val :: a, subtree_val :: a
                           , lt :: Treap a, rt :: Treap a }

ccnt Leaf = 0
ccnt v = cnt v

singleton pr a = Node 1 pr a a Leaf Leaf

class Combinable a where
  combine :: a -> a -> a

combineNodes pr a Leaf Leaf = singleton pr a
combineNodes pr a l Leaf =
  Node (cnt l + 1) pr a ((subtree_val l) `combine` a) l Leaf
combineNodes pr a Leaf r =
  Node (cnt r + 1) pr a (a `combine` (subtree_val r)) Leaf r
combineNodes pr a l@(Node cl _ _ sl _ _) r@(Node cr _ _ sr _ _) =
  Node (cl + cr + 1) pr a (sl `combine` a `combine` sr) l r

split x tree = split_ tree 0
  where split_ Leaf _ = (Leaf, Leaf)
        split_ tree@(Node _ p a _ l r) add
          | x <= cKey = (fst splitl, combineNodes p a (snd splitl) r)
          | otherwise = (combineNodes p a l (fst splitr), snd splitr)
          where cKey = add + ccnt l
                splitl = split_ l add
                splitr = split_ r (cKey + 1)

merge Leaf r = r
merge l Leaf = l
merge l@(Node _ p a _ tl tr) r@(Node _ p' a' _ tl' tr')
  | p > p'    = combineNodes p a tl (merge tr r)
  | otherwise = combineNodes p' a' (merge l tl') tr'

build vals = foldl merge Leaf $ zipWith singleton rands vals
  where gen = RND.mkStdGen 123456
        rands = map fst $ iterate (RND.next . snd) (0, gen)
-- Alternatively rands can be
-- iterate ((+) 1442695040888963407 . (*) 6364136223846793005) 123456

toList tree = toList_ tree []
  where toList_ Leaf ans = ans
        toList_ (Node _ _ a _ lt rt) ans = toList_ lt (a : toList_ rt ans)

-- End cut here.
-- Hackerrank problem Order exercises

readInt s = let Just (a, _) = BS.readInt s in a

data Seg = Seg { pref  :: Int, lpref :: Int
               , suf   :: Int, lsuf  :: Int
               , segv  :: Int, lseg  :: Int, rseg :: Int
               , total :: Int, len   :: Int
               } deriving Show

combineSeg l r = Seg cpref clpref csuf clsuf csegv clseg crseg ctotal clen
  where maxBy fn a b = if fn a >= fn b then a else b
        (cpref, clpref) = maxBy fst (pref l, lpref l) $
                                    (pref r + total l, lpref r + len l)
        (csuf, clsuf) = maxBy fst (suf l + total r, lsuf l + len r) $
                                  (suf r, lsuf r)
        bestSeg = maxBy $ \(v, l, r) -> (v, -l, -(r - l))
        (csegv,clseg,crseg) =
            (segv l, lseg l, rseg l) `bestSeg`
            (suf l + pref r, len l - lsuf l, len l + lpref r - 1) `bestSeg`
            (segv r, len l + lseg r, len l + rseg r)
        ctotal = total l + total r
        clen = len l + len r

instance Combinable Seg where
  combine = combineSeg

singleSeg x | x <= 0 =
  Seg{pref=0, lpref=0, suf=0, lsuf=0, segv=0, lseg=0, rseg=(-1), total=x, len=1}
singleSeg x =
  Seg{pref=x, lpref=1, suf=x, lsuf=1, segv=x, lseg=0, rseg=0, total=x, len=1}

smallSeg = singleSeg (-(10^10))
eraseMax tree = lpart `merge` (singleton p smallSeg) `merge` rrpart
  where cseg = subtree_val tree
        (l, r) = (lseg cseg, rseg cseg)
        (lpart, rpart) = split l tree
        (rlpart, rrpart) = split (r - l + 1) rpart
        p = case rlpart of
              Leaf -> 0
              _ -> prior rlpart

getMax = segv . subtree_val

solve ([n,k]:lst:_) = getSegs initial k
  where initial = build $ map singleSeg lst
        getSegs :: Treap Seg -> Int -> [Int]
        getSegs _ 0 = []
        getSegs t k
          | best <= 0 = []
          | otherwise = best : getSegs (eraseMax t) (k - 1)
          where best = getMax t

main = BS.interact $ BS.unlines . map (BS.pack . show) . solve .
                     map (map readInt . BS.words) . BS.lines
