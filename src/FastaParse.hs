module FastaParse (FASTALine(Quality, Sequence), tag) where

import qualified Data.ByteString.Lazy.Char8 as L
import Debug.Trace
    
data FASTALine  = Header  | Sequence  | QualDelim | Quality
                  deriving (Eq, Show)
                           
-- Ghetto parsing, refactor to be less repetitive
tag :: FASTALine -> [(L.ByteString, Int)] -> [(FASTALine, L.ByteString)]
tag _ [] = []       
tag lastlinetype ((line, lineNo):lines) = if (lineNo `mod` 10000) == 0
                                          then trace ("Line: " ++ show lineNo) $ tagLine ((line, lineNo):lines)
                                          else tagLine ((line, lineNo):lines)
    where tagLine ((line, lineNo):lines) = case L.head line of
                                          '>' -> (Header, line) : tag Header lines
                                          '@' -> (Header, line) : tag Header lines
                                          '+' -> (QualDelim, line) : tag QualDelim lines
                                          otherwise -> case lastlinetype of
                                                 Header -> (Sequence, line) :  tag Sequence lines
                                                 Sequence -> (Sequence, line) :  tag Sequence lines 
                                                 QualDelim -> (Quality, line) :  tag Quality lines 
                                                 Quality -> (Quality, line) :  tag Quality lines 
