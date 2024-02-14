{-# language LambdaCase #-}
import System.Environment
import Data.Foldable
import qualified Change
import Data.Array ((!))

main :: IO ()
main = do
  cs <- fmap (read @Int) <$> getArgs
  ns <- fmap (read @Int) . words <$> getContents
  let a = Change.solve (maximum ns) cs
  let p = \case
        Nothing -> "-"
        Just xs -> unwords $ fmap show xs
  traverse_ (\n -> putStrLn $ show n <> ": " <> p (a ! n)) ns
