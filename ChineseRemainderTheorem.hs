{- Made By: Jaydon Lynch -}
gcdWithCoefficients :: (Integral a) => a -> a -> (a,a,a)
gcdWithCoefficients 0 m = (m, 0, 1)
gcdWithCoefficients n m = let (g, s, t) = gcdWithCoefficients (m `mod` n) n
    in (g, t - (m `div` n) * s, s)
{- Modular Inverse -}
inverseMod :: Integral a => a -> a -> a
inverseMod p m = let (_, i, _) = gcdWithCoefficients p m in i `mod` m
{- Modular Inverstion For Members of List -}
inverseList :: Integral a => [a] -> [a]
inverseList [] = []
inverseList xs = let prod = product xs
                    in map ( \x -> inverseMod (prod `div` x) x ) xs
{- Chinese Remainer Theorem -}
secretNumber :: (Integral a, Foldable t) => t (a, a) -> (a, a)
secretNumber = foldr go (0, 1)
    where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
        where
        r = r2 + m2 * (r1 - r2) * (inverseMod m2 m1)
        m = m2 * m1
-- arePairwiseRelPrime returns true iff all the elements of a list of integers are relatively prime
arePairwiseRelPrime::(Integral a) => [a] -> Bool
arePairwiseRelPrime [] = True
arePairwiseRelPrime [n] = True
arePairwiseRelPrime (n:ns) = and [g == 1| g <- map (gcd n) ns]
                             && arePairwiseRelPrime ns
{- shareSecretNumber takes a "secret" number and a list of pairwise relatively prime numbers and
   returns a list of pairs with the secret number mod each number in the list and the number from the list
   It returns the empty list if the secret number is greater than the product of the numbers in the list
-}
shareSecretNumberHelper :: (Integral a) => a -> [a] -> [(a,a)] 
shareSecretNumberHelper secretNumber [] = []
shareSecretNumberHelper secretNumber (m:ms) = 
   if (not (arePairwiseRelPrime (m:ms)))
     then []
     else (secretNumber `mod` m, m) : (shareSecretNumberHelper secretNumber ms)
-- shareSecretNumber' verifies that secretNumber < product (m:ms) and then calls shareSecretNumberHelper
--  Otherwise, it returns the empty list
shareSecretNumber :: (Integral a) => a -> [a] -> [(a,a)]
shareSecretNumber secretNumber [] = []
shareSecretNumber secretNumber (m:ms) =
  if (secretNumber >= product (m:ms))
    then []
    else shareSecretNumberHelper secretNumber (m:ms)
