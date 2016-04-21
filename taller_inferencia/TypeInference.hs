module TypeInference (TypingJudgment, Result(..), inferType)

where

import Data.List(intersect)
import Exp
import Type
import Unification

------------
-- Errors --
------------
data Result a = OK a | Error String


--------------------
-- Type Inference --
--------------------
type TypingJudgment = (Env, AnnotExp, Type)


inferType :: PlainExp -> Result TypingJudgment
inferType e = case infer' e 0 of
    OK (_, tj) -> OK tj
    Error s -> Error s


infer' :: PlainExp -> Int -> Result (Int, TypingJudgment)

-- COMPLETAR DESDE AQUI

infer' (VarExp x) n = OK (n + 1, (extendE emptyEnv x (TVar n), VarExp x, TVar n))
infer' (ZeroExp) n = OK (n, (emptyEnv, ZeroExp, TNat))
infer' (TrueExp) n = OK (n, (emptyEnv, TrueExp, TBool))
infer' (FalseExp) n = OK (n, (emptyEnv, FalseExp, TBool))
infer' (SuccExp e) n = case infer' e n of
        Error s -> Error s
        OK (n', (env', e', t')) -> case mgu [(t', TNat )] of 
            UError u1 u2 -> uError u1 u2 
            UOK subst -> OK (n', (subst <.> env', subst <.> SuccExp e', TNat))

infer' (PredExp e) n = case infer' e n of
        Error s -> Error s
        OK (n', (env', e', t')) -> case mgu [(t', TNat )] of 
            UError u1 u2 -> uError u1 u2 
            UOK subst -> OK (n', (subst <.> env', subst <.> PredExp e', TNat))

infer' (IfExp condExp ifTrueExp ifFalseExp) n = case infer' condExp n of
        Error s -> Error s
        OK (n', (envCond, eCond, tCond)) -> case infer' ifTrueExp n' of
            Error s -> Error s
            OK (n'', (envIfTrue, eIfTrue, tIfTrue)) -> case infer' ifFalseExp n'' of
                Error s -> Error s
                OK (n''', (envIfFalse, eIfFalse, tIfFalse)) -> case mgu ([(tCond, TBool), (tIfFalse, tIfTrue)] ++ frulaLista [envCond, envIfTrue, envIfFalse]) of 
                    UError u1 u2 -> uError u1 u2 
                    UOK subst -> OK (n''', (joinE [subst <.> envCond, subst <.> envIfTrue, subst <.> envIfFalse], subst <.> IfExp eCond eIfTrue eIfFalse, subst <.> tIfTrue))

infer' (LamExp x () e) n = case infer' e n of
        Error s -> Error s
        OK (n', (env', e', t')) -> let 
            existeEnEnv = x `elem` (domainE env')
            t'' = if existeEnEnv then evalE env' x else TVar (n')
            n'' = if existeEnEnv then n' else n' + 1
            in OK (n'', (removeE env' x, LamExp x t'' e',  TFun t'' t'))

infer' (AppExp e1 e2) n = case infer' e1 n of
        Error s -> Error s
        OK (n', (env1', e1', te1')) -> case infer' e2 n' of
            Error s -> Error s
            OK (n'', (env2', e2', te2')) -> case mgu ([(te1', TFun te2' (TVar n''))] ++ frulaLista [env1', env2']) of 
                    UError u1 u2 -> uError u1 u2 
                    UOK subst -> OK (n'' + 1, (joinE [subst <.> env1', subst <.> env2'], subst <.> (AppExp e1' e2'), subst <.> TVar n''))

frulaLista envs = concat [ frula env1 env2 | env1 <- envs, env2 <- envs ]
frula env1 env2 = [(evalE env1 symEnv1, evalE env2 symEnv2) | symEnv1 <- domainE env1, symEnv2 <- domainE env2, symEnv1 == symEnv2] 

--------------------------------
-- YAPA: Error de unificacion --
--------------------------------
uError :: Type -> Type -> Result (Int, a)
uError t1 t2 = Error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
