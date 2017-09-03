> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveAnyClass, StandaloneDeriving #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE NamedFieldPuns #-}
> module Linear where
> import Data.List

Same problem as in Lib module, but let's take ideas from linear programming.

And figure out a visualization.

> type Input volume cost = [Item volume cost]
> data Item volume cost = Item {range :: Range volume, cost :: cost}

Cost per unit

> data Range a = Range { lo, hi :: a }

Check primal and dual validity of solution:
-> that should be optimal then.

Floats would be most natural here.  But can't add them up properly and still
compare.  So use either rational or Integer?

> type PrimalSolution volume = [volume]
> type DualSolution price = [price]
> type Solution volume cost = [(volume, cost)]

> checkLength :: Input v c -> Solution v c -> Bool
> checkLength i s = length i == length s

> checkPrimal :: (Ord v, Num v) => Input v c -> Solution v c -> Bool
> checkPrimal i s = checkBounds i s && checkForwardFlow s

> checkBounds :: Ord v => Input v c -> Solution v c -> Bool
> checkBounds i s = and $ zipWith checkBound i s where
>   checkBound (Item (Range {lo, hi}) _) (v, _) = lo <= v &&  v <= hi

> checkForwardFlow :: (Ord v, Num v) => Solution v c -> Bool
> checkForwardFlow s = head flows == 0 && all (0 <=) flows && last flows == 0 where
>   flows = scanl (+) 0 . map fst $ s

maximize: v0 * price0 + v1 * price1 + ...
-> cutoff ?

> checkDual :: (Ord v, Ord c) => Input v c -> Solution v c -> Bool
> checkDual i s = checkAscending s && checkCutOff i s

> checkAscending ::  Ord c => Solution v c -> Bool
> checkAscending s = ascending . map snd $ s where
>   ascending l = l == sort l

> checkCutOff :: Input v c -> Solution v c -> Bool
> checkCutOff i s = undefined

---

How about a full DAG? (Assume processing in topological order?)

Each DAG note has: to, from links.  range of capacity to on/off load.  Flow
condition hold: ie no negative / backward flows.  No further flow restrictions
on the links. flow(link) >= 0 is all.

What are necessary and sufficient conditions to check admissibility and optimality?
- ascending cutoffs on all paths. Checkable in linear time, O(links).  Even without transitive triangles, max number of links = nodes^2 / 4 (https://math.stackexchange.com/questions/659942/maximum-number-of-edges-in-a-dag-without-transitivity-condition)
- flow condition: flow(link) >= 0.  Checkable in linear time from the nodes alone?
    * Can we get from information about nodes to information about links?
    * Or do we just have to ask that the solution contains information about links?
    * We can go from links to node information by just adding.
    * For now, assume information about link flow is provided.
    * (Does transitive closure change anything?  Don't think so.)


We'd still have ascending cut-offs on all paths.  But that wouldn't be enough?

First: can we calculate the maximum flow

---

What about full directed graph, including cycles? (I assume, we just collapse
strongly connected components in linear time, and then work on the DAG.)

TODO: Formalise that this works.  Mostly: is being able to calculate medians in
linear time good enough to solve the collapsed sub-graphs?  Perhaps the
structure of supply-demand would become a convex piece-wise linear function?
(Can we get those structures via a piece of DAG, or ever linear DAG, always,
too?  Ie collapse to DAG instead of single node?)

---

Sanity check: can we do simple unweighted paren matching in general graphs?


---

Other generalization is in introducing max link-flows.  But that would more
likely become non-linear in runtime?  We definitely need to stay out of
anything that maximum flow could be reduced to.



Restriction on flow to buy/sell at node can be modelled by price, obviously:

           _____ neg inf
       ----      finite
inf ___

(That's the marginal price of epsilon extra volume.  It's two sided, but
pertubation can get rid of that complication?.  For verifying a static
solution, that's good.  But we need more information for the dynamic case of
finding a solution?)


