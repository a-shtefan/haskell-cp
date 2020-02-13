-- Prefix function for a string s is an array such that pi[0] = 0, for i > 0,
-- pi[i] is the maximal number less than i such that
-- s[i - pi[i] .. i] = s[0 .. pi[i] - 1].
-- Prefix function is calculated in O(|s|).
import qualified Data.Array as A
import Data.Array ((!))
import qualified Data.ByteString.Char8 as BS

pref s = pref_arr
  where n = BS.length s
        pref_arr = A.listArray (0, n - 1) pref_list
        pref_list = 0 : [descend i (pref_arr ! (i - 1)) | i <- [1 ..]]
        descend i j
          | s `BS.index` i == s `BS.index` j = j + 1
          | j > 0 = descend i $ pref_arr ! (j - 1)
          | otherwise = 0

-- End cut here.
-- Hackerrank problem Substring Searching

main = BS.interact $ BS.unlines . solve . tail . BS.words

solve [] = []
solve (s:t:rest) = BS.pack (if ans then "YES" else "NO") : solve rest
  where pf = pref $ t `BS.append` BS.pack "$" `BS.append` s
        ans = any (== BS.length t) $ A.elems pf
