{-# LANGUAGE Safe #-}

module CMM.Inference.FunDeps where
import safe Data.Bool (Bool, bool, otherwise)
import safe qualified Data.Bool as B
import safe CMM.Data.Trilean (Trilean, trilean, toBool, fromBool)
import safe qualified CMM.Data.Trilean as T
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List
import Data.Tuple
import Data.Tuple.Extra
import CMM.Data.List
import Data.Eq
import CMM.Data.Num
import Data.Int

subsetOf :: [Bool] -> [Bool] -> Bool
subsetOf (B.True:_) (B.False:_) = B.False
subsetOf (_:rest) (_:others) = subsetOf rest others
subsetOf [] _ = B.True
subsetOf xs [] = B.not $ or xs

ruleFrom :: Functor f => f Trilean -> f Bool
ruleFrom =
  fmap $ trilean B.True B.False B.False

ruleTo :: Functor f => f Trilean -> f Bool
ruleTo =
  fmap $ trilean B.False B.False B.True

closure :: [[Trilean]] -> [Bool] -> [Bool]
closure = (thd3 .) . go
  where
    go [] set = (B.False, [], set) -- no rules to apply -> we return the original set unchanged
    go (rule:rules) set
      | to `subsetOf` set = go' -- the rule is irrelevant
      | from `subsetOf` set = first3 (const B.True) . go rules $ zipWith (B.||) set to -- the rule is used
      | change = first3 (const B.True) $ go aside' set' -- we try again on the changed set' with put aside rules (including rule)
      | otherwise = (B.False, aside', set) -- no change, we return the original set and all put aside rules (including rule)
      where
        aside' = rule:aside
        go'@(change, aside, set') = go rules set
        from = ruleFrom rule
        to = ruleTo rule

preserveNth :: (Num n, Eq n) => n -> [Trilean] -> [Trilean]
preserveNth _ [] = []
preserveNth n (x:others) = case x of
  T.True
    | n == 0 -> T.Unknown : preserveNth'
    | n == 1 -> T.True : preserveNm1th'
    | otherwise -> T.Unknown : preserveNm1th' -- n > 1
  _ -> x : preserveNth'
  where
    preserveNth' = preserveNth n others
    preserveNm1th' = preserveNth (n - 1) others

decompose :: [[Trilean]] -> [[Trilean]]
decompose [] = []
decompose (rule:rules) = go $ decompose rules
  where
    go = case count (== T.True) rule :: Int of
      0 -> id
      1 -> (rule :)
      x -> (fmap (`preserveNth` rule) [1..x] ++)

forgetNth :: (Num n, Eq n) => n -> [Trilean] -> [Trilean]
forgetNth _ [] = []
forgetNth n (x:others) = case x of
  T.False
    | n == 0 -> T.False : forgetNth'
    | n == 1 -> T.Unknown : forgetNm1th'
    | otherwise -> T.False : forgetNm1th' -- n > 1
  _ -> x : forgetNth'
  where
    forgetNth' = forgetNth n others
    forgetNm1th' = forgetNth (n - 1) others

strengthen :: [[Trilean]] -> [[Trilean]] -> [[Trilean]]
strengthen [] bonus = bonus
strengthen allRules@(rule:rules) bonus = case count (== T.False) rule :: Int of
  0 -> strengthen rules (rule:bonus)
  x -> go newRules
    where
      newRules = fmap (`forgetNth` rule) [1..x]
      go [] = strengthen rules (rule:bonus)
      go (newRule:others)
        | ruleTo newRule `subsetOf` closure' = strengthen (newRule:rules) bonus
        | otherwise = go others
        where closure' = closure (allRules ++ bonus) (ruleFrom newRule)

weaken :: [[Trilean]] -> [[Trilean]] -> [[Trilean]]
weaken [] bonus = bonus
weaken (rule:rules) bonus
  | ruleTo rule `subsetOf` closure' = weaken rules bonus
  | otherwise = weaken rules (rule:bonus)
  where
    closure' = closure rules (ruleFrom rule)

compose :: [[Trilean]] -> [[Trilean]]
compose (rule:rules@(otherRule:otherRules))
  | ruleFrom rule == ruleFrom otherRule =
    compose $ zipWith (T.||) rule otherRule : otherRules
  | otherwise = rule : compose rules
compose rules = rules

funDepsSimplify :: [[Trilean]] -> [[Trilean]]
funDepsSimplify original = compose sorted
  where
    sorted = sortOn onFrom weakened
    onFrom = (trilean T.False T.Unknown T.Unknown <$>)
    weakened = weaken strengthened []
    strengthened = strengthen decomposed []
    decomposed = decompose original
