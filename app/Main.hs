module Main where

import Lib
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import Data.List
import qualified Math.Combinatorics.Multiset as MS
import Debug.Trace
    
-- https://stackoverflow.com/questions/3710976/counting-unique-elements-in-a-list

--countElems4 = MS.toCounts . MS.fromList
              
-- countElems3 = map (\xs -> (head xs, length xs)) . group . sort
               
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

-- try https://stackoverflow.com/questions/13758704/haskell-is-there-a-standard-function-to-provide-a-count-of-each-item-in-a-list               
countElems :: (Ord a) => [a] -> M.Map a Int
countElems = M.fromListWith (+) . flip zip (repeat 1)

--buildMap = countElems
buildMap :: [L.ByteString] -> M.Map L.ByteString Int
buildMap bsl = foldl' f M.empty bsl
             where f amap bs = let res = M.insertWith (+) bs 1 amap in
                               if M.size amap `mod` 1000000 == 0
                               then trace (show $ M.size amap) res
                               else res

-- TODO filter k-mers with 'N's
-- TODO count canonical k-mers

complement :: Char -> Char
complement 'C' = 'G'
complement 'G' = 'C'
complement 'A' = 'T'
complement otherwise = 'A'

data FASTALine  = Header  | Sequence  | QualDelim | Quality
                  deriving (Eq, Show)
                           
-- Ghetto parsing, refactor to be less repetitive
tag :: FASTALine -> [L.ByteString] -> [(FASTALine, L.ByteString)]
tag _ [] = []       
tag lastlinetype (line:lines) = case (L.head line) of
                                  '>' -> (Header, line) : tag Header lines
                                  '@' -> (Header, line) : tag Header lines
                                  '+' -> (QualDelim, line) : tag QualDelim lines
                                  otherwise -> case lastlinetype of
                                                 Header -> (Sequence, line) : ( tag Sequence lines)
                                                 Sequence -> (Sequence, line) : ( tag Sequence lines )
                                                 QualDelim -> (Quality, line) : ( tag Quality lines )
                                                 Quality -> (Quality, line) : ( tag Quality lines )
    
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

           let taggedLines = tag Quality mylines
           let groups1 = groupBy equalTags taggedLines
                   where
                     equalTags (tag1, _) (tag2, _) = tag1 == tag2
           -- let groups1 = groupBy (\a b -> ((L.head a) == '>') == ((L.head b) == '>')) mylines
           --         where
           let groups2 = filter ((Sequence ==)  . fst . head) groups1
           let sequences = map (L.concat . (fmap snd))  groups2
           --mapM (putStrLn . show) sequences                           
           let kmers = concat $ map (windows k) sequences
           let filteredKmers = filter noNs kmers
           let canonicalKmers = fmap canonical filteredKmers
           let mymap = buildMap canonicalKmers
--           let counted = map (\x -> (length x, head x)) $ group . sort $ kmers
           putStrLn (show $ M.size mymap) -- 
--           mapM (putStrLn . show) counted
--           mapM (putStrLn . show) kmers
--           putStrLn (show kmers)
           return ()
