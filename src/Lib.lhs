> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveAnyClass, StandaloneDeriving #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE RankNTypes #-}
> module Lib where

> import GHC.Generics
> import Data.Heap
> import Data.Ord
> import Data.Bifunctor
> import Data.Bifoldable
> import Data.Bitraversable
> import qualified Data.Set as Set
> import Data.Set (Set)
> import qualified Data.Map.Strict as Map
> import Data.Map.Strict (Map)

Open, close.

A is open, V is close.  (They look like < and > vertically.)

> data OC a v = A a | V v
>   deriving (Eq, Ord, Show, Generic)

> instance Bifunctor OC where
>   bimap = bimapDefault
> instance Bifoldable OC where
>   bifoldMap = bifoldMapDefault
> instance Bitraversable OC where
>   bitraverse f _ (A a) = A <$> f a
>   bitraverse _ g (V v) = V <$> g v

Can we generate all possible balanced subsequences?
Can we generate all possible balanced maximal subsequences?

And that is, better than via enumeration!
Also, restrict to number of items chosen per packet.

equivalent, select maximal (?) subsequences that looks like
 ))))))  (((((
Call these cross sequences.

Not all complements of balanced subsequences look like above
--- the empty subsequences.

Conversely, not all cross-sequence complements are balanced
--- the empty cross sequence.

But maximal cross sub-sequences are complements of maximal balanced
subsequences? [This would be QuickCheck time.]  Assume, yes.

That's a nice dual property.

So, looking for a heavy maximal balanced subsequence is the same as looking for
a light maximal cross subsequence.

After 'parts':

          v  v      vv       vvvvvvvvvv  vv  vv    v vv  v
)))))     [mix, too many ) ] [balanced] [mix, too many ( ] ((((
^^^^^      ^^ ^^^^^^  ^^^^^^            ^  ^^  ^^^^ ^  ^^  ^^^^

The mixed parts are the only interesting ones.

Ah, in order for the whole algorithm to be linear, the number of maximal (light
cross / heavy balanced) subsequences has to be at most 2^O(n) in their
length, ie per element we can only spend at most a constant number of
comparisons / entropy.

(Observe: permutations have n! <= 2^O(n log n) possibilities.)

That's because all double-maximal subsequence cuts are reachable with the right
choice of order?  (Assume, yes.)  Argument: generate an arbitrary maximal
cut; if the target cut differs by at least one element, swap it.  (That's
always possible, because we are dealing with a sort-of Matroid.)

Does the median/rank reduction have the highest number of double-maximal cuts?
Seems plausible: it's the least constrained.

Silly argument: of course it's doable in 2^O(n) combinations, otherwise cuts
wouldn't be enough.

So, why are we investigating their number: because a finer approximation might
give us some insight into their structure!

Another tack: each comparison needs (one average) to cut a constant fraction of
the search space.  Problem is: we don't have a total order.

Approach:
  Without loss of generality, assume the case of too many Vs, and that all As get selected.

  In linear time: partition Vs into runs divided by As, and into a grid of
  eps-quantiles. (Chose quantile pivots randomly, to avoid some bad cases.)

  (For notational convenience, Set r * eps = 1, 0 < eps < 1.)

  Idea: dynamic instead of static grid?  Look at soft heaps!  Page 380 of
  Discrepancy Method descripes how they help with the linear selection problem.

> solve :: forall a v . (Ord a, Ord v) => [OC a v] -> [OC (Bool, a) v]
> solve x = undefined where

   (_, vs, bal, as, _) = parts x

   inside (n, a) = bimap (n,) (n,) a
   numberedVs :: [OC a (Int, v)]
   numberedVs = fmap (\(n, i) -> bimap id (,n) i) $ zip [0..] vs where
   numberedAs :: [OC (Int, a) v]
   numberedAs = fmap (\(n, i) -> bimap (,n) id i) $ zip [0..] as where

   selAs = fst $ foldr cons ([], Set.empty) numberedAs where
     cons (V (v,n)) (is, set) = 
     cons (V _) (is, Set.minView -> Just (minA, restAs)) = (V

   cons (A (n, a)) rest as = undefined
   cons (V (n, v)) rest as = undefined
     let rest as

> oc :: (a -> b) -> (v -> b) -> OC a v -> b
> oc a v (A a') = a a'
> oc a v (V v') = v v'

> isBalanced :: [OC a v] -> Bool
> isBalanced avs =
>   let (_, l, m, r, _) = parts avs
>   in null l && null r

> (.:) = (.) . (.)

> as, vs :: [OC a v] -> [Int]
> as = tail . scanl (max 0 .: (+)) 0 . map (oc (const 1)    (const (-1)))
> vs = init . scanr (max 0 .: (+)) 0 . map (oc (const (-1)) (const 1))

LE: lazy elements.

> dropWhileEndLE :: (a -> Bool) -> [a] -> [a]
> dropWhileEndLE p = foldr (\x r -> if null r && p x then [] else x:r) []

> break3 :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a], [a])
> break3 startP stopP l =
>   let (start, rest) = break startP l
>       (end, mid) = break stopP (reverse rest)
>   in (start, reverse mid, reverse end)


The 0th part has only Vs.

The first part that has too many V;
then the balanced part;
then the last part that has too many A.

The part after the last has only A.

> parts :: [OC a v] -> ([OC a v], [OC a v], [OC a v], [OC a v], [OC a v])
> parts items = clean
>       . break3 ((<=0) . fst . snd)
>                ((<=0) . snd . snd)
>       . (zip <*> (zip <$> vs <*> as))
>       $ reverse items' where
>   clean (l, m, r) = (vs', map fst l, map fst m, map fst r, reverse as')
>   (vs', rest) = break (oc (const False) (const True)) items
>   (as', items') = break (oc (const True) (const False)) rest


insert

 chooseV' :: forall a v . Ord v => [OC a v] -> [Bool]
 choosev'

This is at least as hard as median! (Since we can reduce median in linear time,
with a very simple algorithm.)  So a deterministic linear time algorithm should
be at least as complicated as median of medians.

> chooseV :: forall a v . Ord v => [OC a v] -> [Bool]
> chooseV l = undefined where
>   t = tournamentBy (comparing snd) decorated
>   decorated :: [(Int, v)]
>   decorated = foldr cons nil l 0 where
>     cons (A _) rest as = rest (as + 1)
>     cons (V v) rest as = (as, v) : rest 0
>     nil _ = []

Use soft heaps.

too many A portion plus and scan from left to right equivalent to:
- on A: insert element
- on V: remove and commit approximate maximal element.

That's exactly what a soft-heap support.

Now, we just need to see that we don't need to fix up our results too often.

> someFunc :: IO ()
> someFunc = putStrLn "someFunc"


Move below to Red Black trees:

 data Colour = Red | Black
 data Nat = Zero | Succ Nat

 data RB :: Colour -> Nat -> * -> * where
   E :: RB Black Zero a
   R :: a -> RB Black n a -> RB Black n a -> RB Red         n  a
   B :: a -> RB c     n a -> RB c'    n a -> RB Black (Succ n) a

 x :: RB _ _ Int
 x = B 0 (R 0 E E) (R 0 E E)

 lookup' :: Ord a => a -> RB c n a -> Bool
 lookup' a E = False
 lookup' a (R b l r) = case compare a b of
   LT -> lookup' a l
   EQ -> True
   GT -> lookup' a r

 data RB' a = forall n . T (RB Black n a)

 insert' :: Ord a => a -> RB' a -> RB' a
 insert' a (T E) = T (B a E E)
 insert' a (T (B b l r)) = case compare a b of
   LT -> T $ case insert' a (T l) of
     _ -> undefined
   EQ -> T (B a l r)
   GT -> T $ case insert' a (T r) of
     _ -> undefined
 insert' a (T (R b l r)) = case compare a b of
   LT -> T undefined
   EQ -> T (B a l r)
   GT -> T undefined
