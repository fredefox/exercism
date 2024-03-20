module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors = go primes
  where
  go _ 1 = []
  go ps@(p:pss) n = case n `divMod` p of
    (n', 0) -> p : go ps n'
    _       -> go pss n
  go _ _ = error "No prime factors"

primes :: Integral n => [n]
primes = 2 : sieve primes [3..]
  where
    sieve (p:ps) xs =
      let (h,t) = span (< p*p) xs
      in  h ++ sieve ps (filter ((/=0).(`mod`p)) t)
    sieve _ _ = error "IMPOSSIBLE"
