module Main where

import Lib
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
-- Some discussions on base data type and I/O:
-- https://stackoverflow.com/questions/41656678/haskell-read-last-line-with-a-lazy-mmap
-- https://github.com/Gabriel439/Haskell-Succinct-Vector-Library/issues/3
-- https://wiki.haskell.org/Wc

-- considerations:
-- * idiomatic haskell (we want the compiler to do as much work as possible, not write C or deal with incidental complexity (as Rich Hickey would put it)
-- * fusion
-- * multi-threading

k = 32
-- https://twitter.com/Helkafen/status/701473861351526400
windows n xs = filter ((>= k) . L.length) $ map (L.take n) (L.tails xs)


               
main :: IO ()
main =  do contents <- L.getContents
           let mylines = L.lines contents
           let groups1 = groupBy (\a b -> ((L.head a) == '>') == ((L.head b) == '>')) mylines
           let groups2 = filter (('>' /=) . L.head . head) groups1
           let groups3 = map L.concat groups2
           let kmers = concat $ map (windows k) groups3
           mapM (putStrLn . show) kmers
--           putStrLn (show kmers)
           return ()
