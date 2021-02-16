module Main where

import Lib
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8 as BSLU
--import qualified Data.Map as M
--import qualified Data.IntMap.Strict as IM
-- import Data.Map.Unboxed.Unboxed
--import qualified Data.Map.Unboxed.Unboxed as IM
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M
import Data.List
import qualified Math.Combinatorics.Multiset as MS
import Debug.Trace
import Data.Bits
import Data.Word
import qualified FastaParse as FP
-- https://stackoverflow.com/questions/3710976/counting-unique-elements-in-a-list

--countElems4 = MS.toCounts . MS.fromList
              
-- countElems3 = map (\xs -> (head xs, length xs)) . group . sort
               
-- Some discussions on base data type and I/O:
-- https://stackoverflow.com/questions/41656678/haskell-read-last-line-with-a-lazy-mmap
-- https://github.com/Gabriel439/Haskell-Succinct-Vector-Library/issues/3
-- https://wiki.haskell.org/Wc

-- some other map implementations
-- https://github.com/gereeter/bounded-intmap
-- https://www.stackage.org/package/bytestring-trie https://hackage.haskell.org/package/bytestring-trie-0.2.5.0/docs/Data-Trie.html
-- http://ekmett.github.io/transients/Data-Transient-WordMap.html
-- https://www.reddit.com/r/haskell/comments/3htyg2/which_map_implementation_is_better_optimised_for/
-- https://stackoverflow.com/questions/7894867/performant-haskell-hashed-structure

-- considerations:
-- * idiomatic haskell (we want the compiler to do as much work as possible, not write C or deal with incidental complexity (as Rich Hickey would put it)
-- * fusion
-- * multi-threading

k = 31
-- https://twitter.com/Helkafen/status/701473861351526400
windows n xs = filter ((>= k) . L.length) $ map (L.take n) (L.tails xs)

--windows n xs = take ((length xs) - n) $ map (L.take n) (L.tails xs)               

--windows2 n xs =  fmap (\a -> (drop a (take n))   [0..(length xs) - n] 
type MapType = IM.IntMap Word8

--buildMap = countElems
buildMap ::  [Int] -> MapType
buildMap il = IM.fromListWith (\a b -> if a + b > 255 then 255 else a + b)  $ zip il $ repeat 1
-- buildMap bsl = foldl' f IM.empty bsl
--              where f amap bs = let res = IM.insertWith (+) bs 1 amap in
--                                if IM.size amap `mod` 100000 == 0
--                                then trace ("Map entries: " ++ ( show $ IM.size amap)) res
--                                else res
-- intOfBase :: Word8 -> Int
-- intOfBase 65 = 0
-- intOfBase 67 = 1
-- intOfBase 71 = 2
-- intOfBase 84 = 3

instance Semigroup Word8 where
    (<>) a b = let bigA = fromIntegral a :: Word16
                   bigB = fromIntegral b :: Word16
               in
                 if (bigA + bigB) > 255 then 255 else a + b

intOfBase :: Char -> Int
intOfBase 'A' = 0
intOfBase 'C' = 1
intOfBase 'G' = 2
intOfBase 'T' = 3
intOfBase _ = error "Non DNA letter found"                



encodeBases :: L.ByteString -> Int                
encodeBases bases = L.foldl  (\a b -> (a `shiftL` 2) .|. intOfBase b) 0  bases                                    
                                    

complementOfBase :: Char -> Char
complementOfBase 'C' = 'G'
complementOfBase 'G' = 'C'
complementOfBase 'A' = 'T'
complementOfBase 'T' = 'A'
complementOfBase _ = error "Non DNA letter found"

-- TODO explore my two instruction revcomp idea. Is it compatible with trie oriented data structures?                                                            
-- TODO convert a sequence to binary, generate the revcomp of said binary (at the same time?) and then slide two sets of windows over them.
-- TODO we should be able to decide if the revcomp is canonical without computing the entire revcomp, do that               
canonicalize x = let rc = revcomp x in
              if rc < x
              then rc
              else x
                  
               
revcomp :: L.ByteString -> L.ByteString
revcomp bs = L.reverse $ L.map complementOfBase bs

noNs :: L.ByteString -> Bool
noNs bs = case L.find ('N' == ) bs of
            Just _ -> False
            Nothing -> True

-- gcCount :: String -> Int -> Int
-- gcCount (c:cs) gc = if c == 'G' || c == 'C'
--                       then gcCount cs (gc + 1)
--                       else gcCount cs gc

-- gcCount :: L.ByteString -> Int
-- gcCount bs = L.fold (\elem accum -> 

-- try https://stackoverflow.com/questions/13758704/haskell-is-there-a-standard-function-to-provide-a-count-of-each-item-in-a-list
-- countElems :: (Ord a) => [a] -> M.Map Int Int
countElems :: [Word8] -> M.Map Word8 Int
countElems = M.fromListWith (+) . flip zip (repeat 1)


hist :: MapType -> M.Map Word8 Int
hist amap = countElems $ map snd $ IM.toList amap

showHist :: M.Map Word8 Int -> [IO ()]
showHist amap = map f [1..255]
                where f i = case amap M.!? i of
                              Just c -> putStrLn (show i ++ "\t" ++ show c)
                              Nothing -> putStrLn (show i ++ "\t0")

main :: IO ()
main =  do contents <- L.getContents
           let mylines = L.lines contents
           let numberedLines = zip mylines [1..]
           let taggedLines = FP.tag FP.Quality numberedLines
           let groupedByTag = groupBy equalTags taggedLines
                   where
                     equalTags (tag1, _) (tag2, _) = tag1 == tag2
           let sequenceGroups = filter ((FP.Sequence ==)  . fst . head) groupedByTag
           let sequences = map (L.concat . fmap snd)  sequenceGroups
           let kmers = concatMap  (windows k) sequences
           let filteredKmers = filter noNs kmers
           let canonicalKmers = fmap canonicalize filteredKmers

           let encodedKmers = fmap encodeBases  canonicalKmers
           let mymap = buildMap encodedKmers

           print (IM.size mymap)

           --putStrLn (show $ hist mymap)
           sequence_ $ showHist $ hist mymap


