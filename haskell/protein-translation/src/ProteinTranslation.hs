{-# language LambdaCase #-}
module ProteinTranslation(proteins) where

proteins :: String -> Maybe [String]
proteins = fmap (takeWhile (/= "STOP")) . traverse (`lookup` m) . chunksOf 3

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = \case
  [] -> []
  xs -> a : chunksOf n b
    where
    (a, b) = splitAt n xs

m :: [(String, String)]
m
  =  ["AUG"]                      |> "Methionine"
  <> ["UUU", "UUC"]               |> "Phenylalanine"
  <> ["UUA", "UUG"]               |> "Leucine"
  <> ["UCU", "UCC", "UCA", "UCG"] |> "Serine"
  <> ["UAU", "UAC"]               |> "Tyrosine"
  <> ["UGU", "UGC"]               |> "Cysteine"
  <> ["UGG"]                      |> "Tryptophan"
  <> ["UAA", "UAG", "UGA"]        |> "STOP"
  where
  (|>) :: [a] -> a -> [(a, a)]
  xs |> y = (\x -> (x, y)) <$> xs
