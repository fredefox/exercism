{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Satellite (treeFromTraversals)
import BinaryTree (BinaryTree(..))

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
          describe "treeFromTraversals" $ for_ cases $ test treeFromTraversals
  where
    test f Case{..} = it description $ f preorder inorder `shouldBe` expected

data Case = Case { description :: String
                 , preorder    :: String
                 , inorder     :: String
                 , expected    :: Maybe (BinaryTree Char)
                 }

cases :: [Case]
cases =
    [ Case { description = "Empty tree"
           , preorder    = ""
           , inorder     = ""
           , expected    = Nothing
           }
    , Case { description = "Tree with one item"
           , preorder    = "a"
           , inorder     = "a"
           , expected    = Just (pure 'a')
           }
    , Case { description = "Tree with two items (left)"
           , preorder    = "ab"
           , inorder     = "ba"
           , expected    = Just (Branch (pure 'b') 'a' Leaf)
           }
    , Case { description = "Tree with two items (right)"
           , preorder    = "ab"
           , inorder     = "ab"
           , expected    = Just (Branch Leaf 'a' (pure 'b'))
           }
    , Case { description = "Tree with three items (left-left)"
           , preorder    = "abc"
           , inorder     = "cba"
           , expected    = Just (Branch (Branch (pure 'c') 'b' Leaf) 'a' Leaf )
           }
    , Case { description = "Tree with three items (left-right)"
           , preorder    = "abc"
           , inorder     = "bca"
           , expected    = Just (Branch (Branch Leaf 'b' (pure 'c')) 'a' Leaf )
           }
    , Case { description = "Tree with three items (center)"
           , preorder    = "abc"
           , inorder     = "bac"
           , expected    = Just (Branch (pure 'b') 'a' (pure 'c') )
           }
    , Case { description = "Tree with three items (right-left)"
           , preorder    = "abc"
           , inorder     = "acb"
           , expected    = Just (Branch Leaf 'a' (Branch (pure 'c') 'b' Leaf) )
           }
    , Case { description = "Tree with three items (right-right)"
           , preorder    = "abc"
           , inorder     = "abc"
           , expected    = Just (Branch Leaf 'a' (Branch Leaf 'b' (pure 'c')))
           }
    , Case { description = "Tree with many items"
           , preorder    = "aixfr"
           , inorder     = "iafxr"
           , expected    = Just (Branch (Branch Leaf 'i' Leaf) 'a' (Branch (Branch Leaf 'f' Leaf) 'x' (Branch Leaf 'r' Leaf)))
           }
    , Case { description = "Reject traversals of different length"
           , preorder    = "ab"
           , inorder     = "bar"
           , expected    = Nothing
           }
    , Case { description = "Reject inconsistent traversals of same length"
           , preorder    = "xyz"
           , inorder     = "abc"
           , expected    = Nothing
           }
    , Case { description = "Reject traversals with repeated items"
           , preorder    = "aba"
           , inorder     = "baa"
           , expected    = Nothing
           }
    ]
