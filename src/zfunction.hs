-- Z function for a string s is an array such that z[0] = 0, for i > 0, z[i] is
-- the maximal number of characters such that s[0 .. z[i]-1] = s[i .. i+z[i]-1].
-- Z function is calculated in O(|s|).
-- Z function can be used to find a substring t in string s by calculating Zf
-- for t ++ s and then finding i such that z[i] >= |t|, it can also be used to
-- find period of a string.
import qualified Data.Array as A
import Data.Array ((!))
import qualified Data.ByteString.Char8 as BS
import Data.Char

zf s = zf_arr
  where n = BS.length s
        zf_arr = A.listArray (0, n - 1) zf_list
        zf_list = map (\(_, a, _, _) -> a) $ iterate zfNext (1, 0, 0, 0)
        isMatch i z = i + z < n && s `BS.index` z == s `BS.index` (i + z)
        naive i z_init = head $ dropWhile (isMatch i) [z_init ..]
        zfNext (i, _, lt, rt) = (i + 1, z, ln, rn)
          where z = naive i (if i >= rt then 0
                             else min (rt - i) (zf_arr ! (i - lt)))
                (ln, rn) = if i + z > rt then (i, i + z) else (lt, rt)

-- End cut here.
-- SWERC 2018 problem K - Dishonest driver

solve s = ans ! (n, 0)
  where n = BS.length s
        ans = A.array ((1, 0), (n, n - 1)) $
            [((len, i), getAns i len) | len <- [1 .. n], i <- [0 .. n - len]]
        zfs = A.listArray (0, n - 1) $ map zf $ BS.tails s
        getAns _ 1 = 1
        getAns i l = minimum $ flip map [1 .. l - 1] $ \l_ ->
            if l `rem` l_ == 0 && (zfs ! i ! l_) >= l - l_ then ans ! (l_, i)
            else (ans ! (l_, i)) + (ans ! (l - l_, i + l_))

clean = BS.filter isAlphaNum

main = BS.interact $ BS.pack . show . solve . clean . (!! 1) . BS.words
