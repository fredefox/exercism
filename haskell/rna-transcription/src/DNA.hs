{-# Language LambdaCase #-}
module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = fmap reverse . go mempty
-- toRNA = goK id

go :: String -> String -> Either Char String
go acc = \case
  [] -> pure acc
  (x:xs) -> case lookup x m of
    Nothing -> Left x
    Just c -> go (c:acc) xs

-- Continuation-passing style.
-- goK :: (String -> a) -> String -> Either Char a
-- goK k = \case
--   [] -> pure $ k mempty
--   (x:xs) -> case lookup x m of
--     Nothing -> Left x
--     Just c -> goK (k . (:) c) xs

m :: [] (Char, Char)
m = [('G', 'C'), ('C', 'G'), ('T', 'A'), ('A', 'U')]
