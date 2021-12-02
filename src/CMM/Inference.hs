{-# LANGUAGE Safe #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module CMM.Inference where

import safe Control.Monad.Writer.Lazy
import safe Control.Lens.Getter
import safe Control.Lens.Setter
import safe Control.Lens.Tuple
import safe Data.Data
import safe Data.Functor
import safe Data.Generics.Aliases
import safe Data.Text (Text)
import safe qualified Data.Text as Text
import safe Data.Map (Map)
import safe qualified Data.Map as Map
import safe Data.Maybe
import safe qualified Data.Set as Set
import Prelude hiding (const)

import safe CMM.Inference.Type
import safe CMM.Inference.State


class Data a =>
      Apply a
  where
  apply :: Subst -> a -> a
  apply subst = go
    where
      go :: Data d => d -> d
      go = gmapT go `extT` typeCase
      typeCase =
        \case
          t@(SimpleType (VarType tVar)) -> fromMaybe t $ tVar `Map.lookup` subst
          t -> gmapT go t

instance Apply Type

instance Apply SimpleType

instance Apply Fact

instance Apply Subst where
  subst' `apply` subst = (apply subst' <$> subst) <> subst'

data UnificationError
  = Occurs TypeVar Type
  | Mismatch Type Type
  | NoSubType Type Type -- supertype; subtype
  | NoConstness Constness Type
  | NoKind Text Type
  | NoRegister Text Type
  | TupleMismatch [Type] [Type]
  | GotErrorType Text
  | IllegalPolytype Type
  | BadKind Type Type

class Unify a where
  unify ::
       MonadWriter [UnificationError] m
    => a
    -> a
    -> m (Subst, a)

equivalent :: Facts -> Facts -> Bool
equivalent = undefined

freshInst :: (MonadInferencer m, MonadIO m, Data a) => Scheme a -> m (a, Facts)
freshInst (given :. facts :=> t) = do
  tVars <- traverse freshTypeHandle given
  let varMap = Map.fromList $ zip [0..] tVars -- TODO: int map?
      leaf (SimpleType (LamType (TypeLam i _))) = varMap Map.! i
      leaf t' = gmapT transform t'
      transform :: Data d => d -> d
      transform = gmapT transform `extT` leaf
  return (transform t, transform facts)

-- superAnnots; subAnnots
subAnnots :: TypeAnnotations -> TypeAnnotations -> Bool
(mKind, const, mReg) `subAnnots` (mKind', const', mReg')
  = (null mKind || mKind == mKind')
  && const `subConst` const'
  && (null mReg || mReg == mReg')

--superConst; subConst
subConst :: Constness -> Constness -> Bool
Unknown `subConst` _ = True
LinkExpr `subConst` ConstExpr = True
const `subConst` const'
  | const == const' = True
_ `subConst` _ = False

matchKind :: (HasKind a, HasKind b) => a -> b -> Bool
matchKind a b = getKind a == getKind b

bind :: (MonadWriter [UnificationError] m) => TypeVar -> (a -> Type) -> a -> m (Subst, a)
bind tVar f t'
    | not (tVar `Set.member` freeTypeVars (f t')) =
      if matchKind tVar (f t')
        then return (Map.singleton tVar (f t'), t')
        else tell [BadKind (SimpleType (VarType tVar)) (f t')] $> (mempty, t')
    | otherwise =
      tell [Occurs tVar (f t')] $> (mempty, t')

instance Unify Type where
  unify NoType NoType = return (mempty, NoType)
  unify t@(ErrorType text) _ = tell [GotErrorType text] $> (mempty, t)
  unify _ t@(ErrorType text) = tell [GotErrorType text] $> (mempty, t)
  unify (SimpleType (VarType tVar)) t' = bind tVar id t'
  unify t' (SimpleType (VarType tVar)) = bind tVar id t'
  unify (SimpleType t) (SimpleType t') = (_2 %~ SimpleType) <$> unify t t'
  unify annotT@(AnnotType annots t) annotT'@(AnnotType annots' t')
    | annots == annots' = (_2 %~ AnnotType annots) <$> unify t t'
    | otherwise = tell [Mismatch annotT annotT'] $> (mempty, annotT)
  unify scheme@Forall{} scheme'@Forall{}
    = tell [IllegalPolytype scheme, IllegalPolytype scheme'] $> (mempty, scheme)
  unify t t' = tell [Mismatch t t'] $> (mempty, t)

instance Unify SimpleType where
  unify (VarType tVar) t' = bind tVar SimpleType t' -- this case is only for completeness
  unify t' t@VarType {} = unify t t' -- this case is only for completeness
  unify (TupleType ts) (TupleType ts') = go ts ts' id mempty
    where
      go (h:t) (h':t') acc subst = do
        (subst', h'') <- (subst `apply` h) `unify` (subst `apply` h')
        go t t' (acc . (h'' :)) (subst' `apply` subst)
      go [] [] acc subst = return (subst, TupleType $ acc [])
      go _ _ acc subst =
        tell [TupleMismatch ts ts'] $> (subst, TupleType $ acc [])
  unify (FunctionType args ret) (FunctionType args' ret') = do
    (subst, args'') <- unify args args'
    (subst', ret'') <- unify (subst `apply` ret) (subst `apply` ret')
    return (subst' `apply` subst, FunctionType (subst' `apply` args'') ret'')
  unify (AddrType t) (AddrType t') = fmap AddrType <$> unify t t'
  unify t t'
    | t == t' = return (mempty, t)
    | otherwise =
       tell [Mismatch (SimpleType t) (SimpleType t')] $> (mempty, t)

-- TODO: add cases with `ErrorType`
-- supertype; subtype; returns Nothing if cannot be determined
subUnify :: MonadWriter [UnificationError] m => Type -> Type -> m (Maybe (Subst, Type))
subUnify simpT@(SimpleType t@VarType{}) (SimpleType t'@VarType{})
  | t == t' = return $ Just (mempty, simpT)
subUnify (SimpleType VarType{}) _ =
  return Nothing
subUnify _ (SimpleType VarType{}) =
  return Nothing

-- the following cases' arguments should be always generated by substitution:
subUnify t@(AnnotType (Nothing, Unknown, Nothing) _) t'@SimpleType{} =
  Just <$> unify t t'
subUnify t@AnnotType{} t'@SimpleType{} =
  Just <$> unify t t' <* tell [NoSubType t t']
subUnify annotT@(AnnotType annots t) annotT'@(AnnotType annots' t')
  | annots `subAnnots` annots' = go
  | otherwise = go <* tell [NoSubType annotT annotT']
  where go = Just . (_2 %~ AnnotType annots') <$> unify t t'
subUnify t t' = Just <$> unify t t' -- TODO: check correctness

-- returns `Just []` on success; `Nothing` if yet to be determined
constLimit :: Constness -> Type -> Maybe [UnificationError]
constLimit _ (SimpleType VarType{}) = Nothing
constLimit _ (ErrorType text) = Just [GotErrorType text]
constLimit Unknown _ = Just []
constLimit const (AnnotType (_, Unknown, _) _)
  | const /= Unknown = Nothing
constLimit const t@(AnnotType (_, const', _) _)
  | const `subConst` const' = Just []
  | otherwise = Just [NoConstness const t]
constLimit _ SimpleType{} = Just []
constLimit _ NoType{} = Just []
constLimit _ t@Forall{} = Just [IllegalPolytype t]

hasKind :: Text -> Type -> Maybe [UnificationError]
hasKind _ (SimpleType VarType{}) = Nothing
hasKind _ (ErrorType text) = Just [GotErrorType text]
hasKind _ NoType = Just []
hasKind kind t@Forall{} = Just [NoKind kind t]
hasKind kind (AnnotType (Just kind', _, _) _)
  | kind == kind' = Just []
hasKind kind t = Just [NoKind kind t]

onRegister :: Text -> Type -> Maybe [UnificationError]
onRegister _ (SimpleType VarType{}) = Nothing
onRegister _ (ErrorType text) = Just [GotErrorType text]
onRegister _ NoType = Just []
onRegister reg t@Forall{} = Just [NoRegister reg t]
onRegister reg (AnnotType (_, _, Just reg') _)
  | reg == reg' = Just []
onRegister reg t = Just [NoRegister reg t]

infer :: MonadInferencer m => Facts -> m Facts
infer [] = return []
infer (first:others) = do
  currSubst <- use currentSubst
  let fact = currSubst `apply` first
  ((<>) <$> infer others) <*> case fact of
   Union t t' -> do
     let ((subst, _), errs) = runWriter $ unify t t' -- TODO: do something about `errs`
     currentSubst %= apply subst
     return []
   InstType (Forall scheme) t -> do
     (t', facts') <- freshInst scheme
     let ((subst, _), errs) = runWriter $ unify t (SimpleType t') -- TODO: do something about `errs`
     currentSubst %= apply subst
     return facts'
   InstType _ _ ->
     undefined -- TODO: this should be illegal
   SubType t t' -> do
     let (ret, errs) = runWriter $ subUnify t t' -- TODO: do something about `errs`
     case ret of
       Nothing -> return [fact]
       Just (subst, _) -> currentSubst %= apply subst >> return []
   ConstnessLimit const t ->
     case constLimit const t of
       Nothing -> return [fact]
       Just errs -> return [] -- TODO: do something about `errs`
   HasKind kind t ->
     case hasKind kind t of
       Nothing -> return [fact]
       Just errs -> return [] -- TODO: do something about `errs`
   OnRegister reg t ->
     case onRegister reg t of
       Nothing -> return [fact]
       Just errs -> return [] -- TODO: do something about `errs`
   Constraint classHandle ts ->
     return [fact] -- TODO: continue from here
