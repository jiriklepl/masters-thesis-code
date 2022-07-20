{-# LANGUAGE Safe #-}

module CMM.Inference.FunDeps where

import safe qualified Data.Bool as B
import safe Data.Bool (bool)
import safe Data.List (sortOn)
import safe Data.Tuple ()
import safe Data.Tuple.Extra (first3, thd3)

import safe CMM.Data.List (count)
import safe CMM.Data.Trilean (Trilean, trilean)
import safe qualified CMM.Data.Trilean as T

-- | returns `True` iff the left-hand set is a subset of the right-hand set (both represented by a bitmap)
subsetOf :: [Bool] -> [Bool] -> Bool
subsetOf (B.True:_) (B.False:_) = B.False
subsetOf (_:rest) (_:others) = subsetOf rest others
subsetOf [] _ = B.True
subsetOf xs [] = B.not $ or xs

-- | combines two functional dependency rules using the || operator
combineRules :: [Trilean] -> [Trilean] -> [Trilean]
combineRules = zipWith (T.||)

-- | returns the bitmap representing the assumptions ("from") of the given functional dependency rule
ruleFrom :: Functor f => f Trilean -> f Bool
ruleFrom = fmap $ trilean B.True B.False B.False

-- | returns the bitmap representing the consequences ("to") of the given functional dependency rule
ruleTo :: Functor f => f Trilean -> f Bool
ruleTo = fmap $ trilean B.False B.False B.True

-- | takes a list of functional dependency set and a bitmap of known elements,
--   performs a closure of elements being fixed under the given functional dependency set
closure :: [[Trilean]] -> [Bool] -> [Bool]
closure = (thd3 .) . go
  where
    go [] set = (B.False, [], set) -- no rules to apply -> we return the original set unchanged
    go (rule:rules) set
      | to `subsetOf` set = go' -- the rule is irrelevant
      | from `subsetOf` set =
        first3 (const B.True) . go rules $ zipWith (B.||) set to -- the rule is used
      | change = first3 (const B.True) $ go aside' set' -- we try again on the changed set' with put aside rules (including rule)
      | otherwise = (B.False, aside', set) -- no change, we return the original set and all put aside rules (including rule)
      where
        aside' = rule : aside
        go'@(change, aside, set') = go rules set
        from = ruleFrom rule
        to = ruleTo rule

-- | turns off (to `T.Unknown`) all the consequences ("to") of the given functional dependency rule, except for the n-th
preserveNth :: (Num n, Eq n) => n -> [Trilean] -> [Trilean]
preserveNth _ [] = []
preserveNth n (x:others) =
  case x of
    T.True
      | n == 0 -> T.Unknown : preserveNth'
      | n == 1 -> T.True : preserveNm1th'
      | otherwise -> T.Unknown : preserveNm1th' -- n > 1
    _ -> x : preserveNth'
  where
    preserveNth' = preserveNth n others
    preserveNm1th' = preserveNth (n - 1) others

-- | decomposes each functional dependency rule into atomic rules (see `preserveNth`)
decompose :: [[Trilean]] -> [[Trilean]]
decompose [] = []
decompose (rule:rules) = go $ decompose rules
  where
    go =
      case count (== T.True) rule :: Int of
        0 -> id
        1 -> (rule :)
        x -> (fmap (`preserveNth` rule) [1 .. x] ++)

-- | forgets the n-th assumption ("from"), turning it to `T.Unknown`, of the given functional dependency rule
forgetNth :: (Num n, Eq n) => n -> [Trilean] -> [Trilean]
forgetNth _ [] = []
forgetNth n (x:others) =
  case x of
    T.False
      | n == 0 -> T.False : forgetNth'
      | n == 1 -> T.Unknown : forgetNm1th'
      | otherwise -> T.False : forgetNm1th' -- n > 1
    _ -> x : forgetNth'
  where
    forgetNth' = forgetNth n others
    forgetNm1th' = forgetNth (n - 1) others

-- | For each functional dependency rule in the given functional dependency set,
--   if an assumption can be forgotten without changing the meaning of the functional dependency set,
--   forgets it (recursively)
strengthen :: [[Trilean]] -> [[Trilean]] -> [[Trilean]]
strengthen [] bonus = bonus
strengthen allRules@(rule:rules) bonus =
  case count (== T.False) rule :: Int of
    0 -> strengthen rules (rule : bonus)
    x -> go newRules
      where newRules = fmap (`forgetNth` rule) [1 .. x]
            go [] = strengthen rules (rule : bonus)
            go (newRule:others)
              | ruleTo newRule `subsetOf` closure' =
                strengthen (newRule : rules) bonus
              | otherwise = go others
              where
                closure' = closure (allRules ++ bonus) (ruleFrom newRule)

-- | Removes redundant rules from the given functional dependency set
weaken :: [[Trilean]] -> [[Trilean]] -> [[Trilean]]
weaken [] bonus = bonus
weaken (rule:rules) bonus
  | ruleTo rule `subsetOf` closure' = weaken rules bonus
  | otherwise = weaken rules (rule : bonus)
  where
    closure' = closure (rules ++ bonus) (ruleFrom rule)

-- | composes the consecutive functional dependency rules
--   with the same assumptions ("from") together,
--   combining their consequences ("to")
compose :: [[Trilean]] -> [[Trilean]]
compose (rule:rules@(otherRule:otherRules))
  | ruleFrom rule == ruleFrom otherRule =
    compose $ combineRules rule otherRule : otherRules
  | otherwise = rule : compose rules
compose rules = rules

-- | replaces each rule in the given functional dependency set
--   with its closure under the given functional dependency set
generalize :: [[Trilean]] -> [[Trilean]] -> [[Trilean]]
generalize [] bonus = bonus
generalize allRules@(rule:rules) bonus
  | newTo `subsetOf` ruleTo rule = go rule
  | otherwise = go $ fmap (bool T.False T.True) newTo `combineRules` rule
  where
    go = generalize rules . (: bonus)
    newTo = zipWith (B.&&) closure' (B.not <$> ruleFrom rule)
    closure' = closure (allRules ++ bonus) (ruleFrom rule)

-- | simplifies the given functional dependency set into an equivalent canonical form
funDepsSimplify :: [[Trilean]] -> [[Trilean]]
funDepsSimplify original = generalize composed []
  where
    composed = compose sorted
    sorted = sortOn onFrom weakened
    onFrom = (trilean T.False T.Unknown T.Unknown <$>)
    weakened = weaken strengthened []
    strengthened = strengthen decomposed []
    decomposed = decompose original

-- | returns `True` iff the given functional dependency rule is trivial
--   (assumes all type variables are set, has no consequences)
isTrivial :: [Trilean] -> Bool
isTrivial = all (== T.False)

-- | adds a trivial functional dependency rule to the given functional dependency set
addTrivialDep :: [[Trilean]] -> [[Trilean]]
addTrivialDep rules =
  case rules of
    [] -> []
    rule:_ -> (T.False <$ rule) : rules
