module Codons(codonSeqToAminos, isStop) where

import Data.List (find)

type Codon = String
type Amino = Char

codonSeqToAminos :: String -> [Amino]
codonSeqToAminos []     = []
codonSeqToAminos codons =
  case find ((==this) . fst) table of
    Nothing     -> []
    Just (_, c) -> c : codonSeqToAminos rest

  where this = take 3 codons
        rest = drop 3 codons

isStop :: Codon -> Bool
isStop codon = codon `elem` stops

stops :: [Codon]
stops = [ "UAA", "UAG", "UGA" ]

table :: [(Codon, Amino)]
table = [("AAA", 'K'),
         ("AAC", 'N'),
         ("AAG", 'K'),
         ("AAU", 'N'),
         ("ACA", 'T'),
         ("ACC", 'T'),
         ("ACG", 'T'),
         ("ACU", 'T'),
         ("AGA", 'R'),
         ("AGC", 'S'),
         ("AGG", 'R'),
         ("AGU", 'S'),
         ("AUA", 'I'),
         ("AUC", 'I'),
         ("AUG", 'M'),
         ("AUU", 'I'),
         ("CAA", 'Q'),
         ("CAC", 'H'),
         ("CAG", 'Q'),
         ("CAU", 'H'),
         ("CCA", 'P'),
         ("CCC", 'P'),
         ("CCG", 'P'),
         ("CCU", 'P'),
         ("CGA", 'R'),
         ("CGC", 'R'),
         ("CGG", 'R'),
         ("CGU", 'R'),
         ("CUA", 'L'),
         ("CUC", 'L'),
         ("CUG", 'L'),
         ("CUU", 'L'),
         ("GAA", 'E'),
         ("GAC", 'D'),
         ("GAG", 'E'),
         ("GAU", 'D'),
         ("GCA", 'A'),
         ("GCC", 'A'),
         ("GCG", 'A'),
         ("GCU", 'A'),
         ("GGA", 'G'),
         ("GGC", 'G'),
         ("GGG", 'G'),
         ("GGU", 'G'),
         ("GUA", 'V'),
         ("GUC", 'V'),
         ("GUG", 'V'),
         ("GUU", 'V'),
         ("UAC", 'Y'),
         ("UAU", 'Y'),
         ("UCA", 'S'),
         ("UCC", 'S'),
         ("UCG", 'S'),
         ("UCU", 'S'),
         ("UGC", 'C'),
         ("UGG", 'W'),
         ("UGU", 'C'),
         ("UUA", 'L'),
         ("UUC", 'F'),
         ("UUG", 'L'),
         ("UUU", 'F') ]
