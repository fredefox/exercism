import Test.Hspec        (Spec, it, shouldReturn, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Deque (mkDeque, pop, push, shift, unshift, toList)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

{-# ANN module "HLint: ignore Reduce duplication" #-}
specs :: Spec
specs = do

    it "push pop" $ do
      deque <- mkDeque
      push deque 'a'
      push deque 'b'
      toList deque >>= print
      False `shouldBe` True
      pop deque `shouldReturn` Just 'b'
      toList deque >>= print
      pop deque `shouldReturn` Just 'a'
      toList deque >>= print

    it "push shift" $ do
      deque <- mkDeque
      push deque 'a'
      push deque 'b'
      shift deque `shouldReturn` Just 'a'
      shift deque `shouldReturn` Just 'b'

    it "unshift shift" $ do
      deque <- mkDeque
      unshift deque 'a'
      unshift deque 'b'
      shift deque `shouldReturn` Just 'b'
      shift deque `shouldReturn` Just 'a'

    it "unshift pop" $ do
      deque <- mkDeque
      unshift deque 'a'
      unshift deque 'b'
      pop deque `shouldReturn` Just 'a'
      pop deque `shouldReturn` Just 'b'

    it "example" $ do
      deque <- mkDeque
      push deque 'a'
      push deque 'b'
      pop deque `shouldReturn` Just 'b'
      push deque 'c'
      shift deque `shouldReturn` Just 'a'
      unshift deque 'd'
      push deque 'e'
      shift deque `shouldReturn` Just 'd'
      pop deque `shouldReturn` Just 'e'
      pop deque `shouldReturn` Just 'c'

-- 5170717695db4aad125049f79fc994ce964eeda4
