{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
-- {-# options_ghc -Wno-partial-type-signatures #-}
import Data.Foldable     (for_)
import Data.Function     (on)
import Data.Tree         (Tree(Node), Forest, rootLabel)
import Data.List         (sort)
import Test.Hspec        (Spec, describe, it, shouldBe)
import qualified Test.Hspec as Hspec
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)
import GHC.Stack (HasCallStack)
import Test.QuickCheck ((==>))
import qualified Test.QuickCheck as QuickCheck
import Data.Word (Word8)
import Control.Applicative
import Data.Tree
import Control.Monad
import System.Exit

import qualified POV

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

edgesShouldMatch :: HasCallStack => Functor f => Ord a => Eq (f [(a, a)]) => Show (f [(a, a)]) => f (Tree a) -> f (Tree a) -> Hspec.Expectation
edgesShouldMatch = shouldBe `on` fmap (sort . toEdges)

specs :: Spec
specs = do

    describe "fromPOV" $ do
      let cases =
            [ ("reparenting singleton"        , singleton , Just singleton')
            , ("reparenting with sibling"     , simple    , Just simple'   )
            , ("reparenting flat"             , flat      , Just flat'     )
            , ("reparenting nested"           , nested    , Just nested'   )
            , ("reparenting kids"             , kids      , Just kids'     )
            , ("reparenting cousins"          , cousins   , Just cousins'  )
            , ("from POV of non-existent node", leaf "foo", Nothing        ) ]

          rootShouldMatch  = shouldBe `on` fmap rootLabel

          test :: HasCallStack => (String, Tree String, Maybe (Tree String)) -> Hspec.SpecWith ()
          test (name, input, output) = describe name $ do
            it "correct root"  $ POV.fromPOV "x" input `rootShouldMatch`  output
            it "correct edges" $ POV.fromPOV "x" input `edgesShouldMatch` output

          in for_ cases test

      describe "Should not be able to find a missing node" $

        let cases = [ ("singleton", singleton)
                    , ("flat"     , flat     )
                    , ("kids"     , kids     )
                    , ("nested"   , nested   )
                    , ("cousins"  , cousins  ) ]

            test (name, g) = it name $ POV.fromPOV "NOT THERE" g `shouldBe` Nothing

            in for_ cases test

      properties

    describe "tracePathBetween" $ do

      it "Can find path from x -> parent" $
        POV.tracePathBetween "x" "parent" simple
        `shouldBe` Just [ "x"
                        , "parent" ]

      it "Can find path from x -> sibling" $
        POV.tracePathBetween "x" "b" flat
        `shouldBe` Just [ "x"
                        , "root"
                        , "b"    ]

      it "Can trace a path from x -> cousin" $
        POV.tracePathBetween "x" "cousin-1" cousins
        `shouldBe` Just [ "x"
                        , "parent"
                        , "grandparent"
                        , "uncle"
                        , "cousin-1"    ]

      it "Can find path from nodes other than x" $
        POV.tracePathBetween "a" "c" flat
        `shouldBe` Just [ "a"
                        , "root"
                        , "c"    ]

      it "Can find path not involving root" $
        POV.tracePathBetween "x" "sibling-1" rootNotNeeded
        `shouldBe` Just [ "x"
                        , "parent"
                        , "sibling-1" ]

      it "Cannot trace if destination does not exist" $
        POV.tracePathBetween "x" "NOT THERE" cousins
        `shouldBe` Nothing

      it "Cannot trace if source does not exist" $
        POV.tracePathBetween "NOT THERE" "x" cousins
        `shouldBe` Nothing


properties :: Hspec.SpecWith ()
properties = describe "properties" $ do
  it "prop_id" $ QuickCheck.property prop_id
  it "prop_preserve_size" $ QuickCheck.property prop_empty
  it "prop_empty" $ QuickCheck.property prop_empty
  it "prop_empty" prop_preserves_nodes
  it "prop_missing" prop_missing

-- x \in f (x)
prop_id :: Tree Word8 -> Hspec.Expectation
prop_id t = POV.invert (const True) t `Hspec.shouldContain` [t]

prop_preserve_size :: Tree () -> Hspec.Expectation
prop_preserve_size t = guard $ all @[] ((== n) . length) $ POV.invert (const True) t
  where
  n = length t

prop_preserves_nodes :: QuickCheck.Property
prop_preserves_nodes = QuickCheck.forAllShow QuickCheck.arbitrary (const "_ :: Word8 -> Bool") prop
  where
  prop :: (Word8 -> Bool) -> Tree Word8 -> Hspec.Expectation
  prop p t = POV.invert @[] (const False) t `for_` (\t' -> t' `nodesShouldMatch'` t)
  nodesShouldMatch' = shouldBe `on` (sort . flatten)
    -- `Hspec.shouldContain` [_]

prop_missing :: QuickCheck.Property
prop_missing = QuickCheck.forAllShow QuickCheck.arbitrary (const "_ :: Word8 -> Bool") prop
  where
  prop :: Tree Word8 -> QuickCheck.Property
  prop t = all (/= n) t ==> (POV.invert (== n) t `shouldBe` Nothing)
    where
    m = maximum t
    n = if m == maxBound then minBound else succ m

-- invert (\_ -> False) x = []
prop_empty :: Tree Word8 -> Hspec.Expectation
prop_empty t = POV.invert (const False) t `Hspec.shouldBe` []

tree :: Alternative f => [a] -> Int -> f (Tree a)
tree _ 0 = empty
tree xs n = case splitAt n' xs of
  (_, []) -> empty
  (l, r@(x:_)) -> pure $ Node x (tree l n' <> tree r n')
  where
  n' = n `div` 2

-- Functions used in the tests.

leaf :: a -> Tree a
leaf v = Node v []

-- In the trees we're making, we don't care about the ordering of children.
-- This is significant when rerooting on nodes that have a parent and children.
-- The former parent can go either before or after the former children.
-- Either choice would be correct in the context of this problem.
-- So all we need to check is:
-- 1) The graph is actually rooted on the requested node.
-- 2) The sorted edge list is correct.
-- This function helps check the second condition.

toEdges :: Ord a => Tree a -> [(a, a)]
toEdges (Node r ts) = map ((r,) . rootLabel) ts ++ concatMap toEdges ts

-- Trees used in the tests.

singleton , simple , flat , kids , nested , cousins  :: Tree String
singleton', simple', flat', kids', nested', cousins' :: Tree String

singleton = leaf "x"

singleton' = leaf "x"

simple = Node "parent"
             [ leaf "x"
             , leaf "sibling"
             ]

simple' = Node "x"
              [ Node "parent"
                    [ leaf "sibling"
                    ]
              ]

flat = Node "root"
           [ leaf "a"
           , leaf "b"
           , leaf "x"
           , leaf "c"
           ]

flat' = Node "x"
            [ Node "root"
                  [ leaf "a"
                  , leaf "b"
                  , leaf "c"
                  ]
            ]

kids = Node "root"
           [ Node "x"
                 [ leaf "kid-0"
                 , leaf "kid-1"
                 ]
           ]

kids' = Node "x"
            [ leaf "kid-0"
            , leaf "kid-1"
            , leaf "root"
            ]

nested = Node "level-0"
             [ Node "level-1"
                   [ Node "level-2"
                         [ Node "level-3"
                               [ leaf "x"
                               ]
                         ]
                   ]
             ]

nested' = Node "x"
              [ Node "level-3"
                    [ Node "level-2"
                          [ Node "level-1"
                                [ leaf "level-0"
                                ]
                          ]
                    ]
              ]

cousins = Node "grandparent"
              [ Node "parent"
                    [ Node "x"
                          [ leaf "kid-a"
                          , leaf "kid-b"
                          ]
                    , leaf "sibling-0"
                    , leaf "sibling-1"
                    ]
              , Node "uncle"
                    [ leaf "cousin-0"
                    , leaf "cousin-1"
                    ]
              ]

cousins' = Node "x"
               [ leaf "kid-a"
               , leaf "kid-b"
               , Node "parent"
                     [ leaf "sibling-0"
                     , leaf "sibling-1"
                     , Node "grandparent"
                           [ Node "uncle"
                                 [ leaf "cousin-0"
                                 , leaf "cousin-1"
                                 ]
                           ]
                     ]
               ]

rootNotNeeded :: Tree String
rootNotNeeded = Node "grandparent"
                    [ Node "parent"
                          [ leaf "x"
                          , leaf "sibling-0"
                          , leaf "sibling-1"
                          ]
                    ]
