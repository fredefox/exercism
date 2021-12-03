module Bob (responseFor) where

import Data.Char
import Data.Monoid

-- Bob answers 'Sure.' if you ask him a question, such as "How are you?".
-- He answers 'Whoa, chill out!' if you YELL AT HIM (in all capitals).
-- He answers 'Calm down, I know what I'm doing!' if you yell a question at him.
-- He says 'Fine. Be that way!' if you address him without actually saying anything.
-- He answers 'Whatever.' to anything else.

question, yellQ, yell, sayNothing :: String -> Bool
question s = case reverse $ filter (not . isSpace) s of
  [] -> False
  (x:_) -> x == '?'

yellQ = getAll . k
  where
  k = (All . question) <> (All . yell)

yell s = all isUpper s' && s' /= ""
  where
  s' = filter isAlpha s

sayNothing = (== "") . filter (not . isSpace)

responseFor :: String -> String
responseFor s
  | yellQ s = "Calm down, I know what I'm doing!"
  | question s = "Sure."
  | yell s = "Whoa, chill out!"
  | sayNothing s = "Fine. Be that way!"
  | otherwise = "Whatever."
