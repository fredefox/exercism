module ComplexNumbers
  ( Complex
  , conjugate
  , abs
  , exp
  , real
  , imaginary
  , mul
  , add
  , sub
  , div
  , complex
  ) where

import Prelude hiding (div, abs, exp)
import qualified Prelude

-- Data definition -------------------------------------------------------------
data Complex a = Complex a a deriving (Eq, Show)

instance Floating a => Num (Complex a) where
  (+) = add
  (-) = sub
  (*) = mul
  abs = fromReal . abs
  signum = fromReal . signum . real
  fromInteger = fromReal . fromInteger

fromReal :: Num a => a -> Complex a
fromReal n = Complex n 0

instance Floating a => Fractional (Complex a) where
  (/) = div
  fromRational = fromReal . fromRational

complex :: (a, a) -> Complex a
complex = uncurry Complex

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex a b) = Complex a (negate b)

abs :: Floating a => Complex a -> a
abs (Complex a b) = sqrt $ p2 a + p2 b

p2 :: Num n => n -> n
p2 = (^ (2 :: Int))

real :: Complex a -> a
real (Complex a _) = a

imaginary :: Complex a -> a
imaginary (Complex _ b) = b

exp :: Floating a => Complex a -> Complex a
exp (Complex a0 b0) = Complex a b
  where
  ea = Prelude.exp a0
  a = ea * cos b0
  b = ea * sin b0

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a0 b0) (Complex a1 b1) = Complex a b
  where
  a = a0 * a1 - b0 * b1
  b = a0 * b1 + a1 * b0

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex a0 b0) (Complex a1 b1) = Complex (a0 + a1) (b0 + b1)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex a0 b0) (Complex a1 b1) = Complex (a0 - a1) (b0 - b1)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a0 a1) (Complex b0 b1) = Complex a b
  where
  a = (a0 * b0 + a1 * b1) / (p2 b0 + p2 b1)
  b = (a1 * b0 - a0 * b1) / (p2 b0 + p2 b1)
