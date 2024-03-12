{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Data.String       (fromString)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)
import Test.QuickCheck (Property, (===), Gen)
import qualified Test.QuickCheck as QuickCheck
import Control.Applicative

import Acronym (abbreviate)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = do
  describe "abbreviate" $ for_ cases test
  describe "abbreviate property tests" propCamelCase
  where
    test Case {..} = it description $
      abbreviate (fromString input) `shouldBe` fromString expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: String
                 }

cases :: [Case]
cases = [ Case { description = "basic"
               , input       = "Portable Network Graphics"
               , expected    = "PNG"
               }
        , Case { description = "lowercase words"
               , input       = "Ruby on Rails"
               , expected    = "ROR"
               }
        -- Although this case was removed in specification 1.1.0,
        -- the Haskell track has chosen to keep it,
        -- since it makes the problem more interesting.
        , Case { description = "camelcase"
               , input       = "HyperText Markup Language"
               , expected    = "HTML"
               }
        , Case { description = "punctuation"
               , input       = "First In, First Out"
               , expected    = "FIFO"
               }
        , Case { description = "all caps word"
               , input       = "GNU Image Manipulation Program"
               , expected    = "GIMP"
               }
        , Case { description = "punctuation without whitespace"
               , input       = "Complementary metal-oxide semiconductor"
               , expected    = "CMOS"
               }
        , Case { description = "very long abbreviation"
               , input       = "Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me"
               , expected    = "ROTFLSHTMDCOALM"
               }
        , Case { description = "consecutive delimiters"
               , input       = "Something - I made up from thin air"
               , expected    = "SIMUFTA"
               }
        , Case { description = "apostrophes"
               , input       = "Halley's Comet"
               , expected    = "HC"
               }
        , Case { description = "underscore emphasis"
               , input       = "The Road _Not_ Taken"
               , expected    = "TRNT"
               }
        ]

propCamelCase :: Spec
propCamelCase
  = it "handles camel-case"
  $ QuickCheck.property
  $ QuickCheck.forAllShrink g s
  $ \xs -> fmap head xs === abbreviate (concat xs)
  where
  g :: Gen [String]
  g = QuickCheck.listOf words
  words = (:) <$> upperCase <*> QuickCheck.listOf1 lowerCase
  lowerCase = QuickCheck.elements ['a'..'z']
  upperCase = QuickCheck.elements ['A'..'Z']
  s :: [String] -> [[String]]
  s [] = []
  s (x:xs) = pure xs <|> QuickCheck.shrinkList s1 xs
  s1 :: String -> [String]
  s1 (c:_:xs@(_:_)) = pure (c:xs)
  s1 _ = empty
