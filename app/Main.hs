module Main where

import Lib
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import Data.List


-- Some discussions on base data type and I/O:
-- https://stackoverflow.com/questions/41656678/haskell-read-last-line-with-a-lazy-mmap
-- https://github.com/Gabriel439/Haskell-Succinct-Vector-Library/issues/3
-- https://wiki.haskell.org/Wc

-- considerations:
-- * idiomatic haskell (we want the compiler to do as much work as possible, not write C or deal with incidental complexity (as Rich Hickey would put it)
-- * fusion
-- * multi-threading

k = 31
-- https://twitter.com/Helkafen/status/701473861351526400
windows n xs = filter ((>= k) . L.length) $ map (L.take n) (L.tails xs)

buildMap :: [L.ByteString] -> M.Map L.ByteString Int
buildMap l = foldl f M.empty l
             where f amap bs = M.insertWith (+) bs 1 amap

-- TODO filter k-mers with 'N's
-- TODO count canonical k-mers

complement :: Char -> Char
complement 'C' = 'G'
complement 'G' = 'C'
complement 'A' = 'T'
complement _ = 'A'

-- TODO we should be able to decide if the revcomp is canonical without computing the entire revcomp, do that               
canonical x = let rc = revcomp x in
              if rc < x
              then rc
              else x
                  
               
revcomp :: L.ByteString -> L.ByteString
revcomp bs = L.reverse $ L.map complement bs

noNs :: L.ByteString -> Bool
noNs bs = case L.find ('N' == ) bs of
            Just _ -> False
            Nothing -> True
           
main :: IO ()
main =  do contents <- L.getContents
           let mylines = L.lines contents
           let groups1 = groupBy (\a b -> ((L.head a) == '>') == ((L.head b) == '>')) mylines
           let groups2 = filter (('>' /=) . L.head . head) groups1
           let groups3 = map L.concat groups2
           let kmers = concat $ map (windows k) groups3
           let filteredKmers = filter noNs kmers
           let canonicalKmers = fmap canonical filteredKmers
           let mymap = buildMap canonicalKmers
--           let counted = map (\x -> (length x, head x)) $ group . sort $ kmers
           putStrLn (show $ M.size mymap)
--           mapM (putStrLn . show) counted
--           mapM (putStrLn . show) kmers
--           putStrLn (show kmers)
           return ()
