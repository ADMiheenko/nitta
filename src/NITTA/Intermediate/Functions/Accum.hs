{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.Intermediate.Functions
Description : Accum function
Copyright   : (c) Daniil Prohorov, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.Intermediate.Functions.Accum
    ( Acc(..), Action(..), Sign(..)
      -- * Acc constructors
    , acc, accFromStr
      -- * Utils functions
    , pullActionGroups, pushActionGroups
    ) where

import           Data.List                     (partition)
import           Data.List.Split               (splitWhen)
import           Data.Set                      (elems, fromList)
import           Data.Typeable
import           NITTA.Intermediate.Types
import           NITTA.Utils                   (unionsMap)
import           Text.InterpolatedString.Perl6 (qc)
import           Text.Regex

data Sign = Plus | Minus deriving (Typeable, Eq)

instance Show Sign where
    show Plus  = "+"
    show Minus = "-"

data Action v = Push Sign (I v) | Pull (O v) deriving (Typeable, Eq)

instance (Show v) => Show ( Action v ) where
    show (Push s v)   = [qc| { show s }{ show v }|]
    show (Pull (O v)) = concatMap (\res -> [qc| => {show res}|]) (elems v) ++ ";"

newtype Acc v x = Acc {actions :: [Action v]} deriving (Typeable, Eq)

instance ( Show v ) => Show (Acc v x) where
    show (Acc lst) =  concatMap show lst

instance Label (Acc v x) where label Acc{} = "+"

acc lst = F $ Acc lst

isPull Pull{} = True
isPull _      = False

isPush Push{} = True
isPush _      = False

fromPush (Push _ (I v)) = v
fromPush _              = error "Error in fromPush function in acc"

fromPull (Pull (O vs)) = vs
fromPull _             = error "Error in fromPull function in acc"

instance (Ord v) => Function (Acc v x) v where
    inputs (Acc lst) = fromList $ map fromPush $ filter isPush lst
    outputs (Acc lst) = unionsMap fromPull $ filter isPull lst

instance ( Ord v ) => Patch (Acc v x) (v, v) where
    patch diff (Acc lst) = Acc $ map
        (\case
            Push s v -> Push s (patch diff v)
            Pull vs  -> Pull (patch diff vs)
        ) lst


exprPattern = mkRegex "[+,=,-]*[a-zA-Z0-9]+|;"
toBlocksSplit exprInput = let
        splitBySemicolon = filter (not . null) . splitWhen ( == ";")
        matchAll p inpS res =
            case matchRegexAll p inpS of
                Just (_, x, xs, _) -> x : matchAll p xs res
                Nothing            -> []
        filtered = subRegex (mkRegex "[ ]+") exprInput ""
    in
        splitBySemicolon $ matchAll exprPattern filtered []

accGen blocks = let
        partedExpr = map (partition (\(x:_) -> x /= '='))
        signPush ('+':name) = Push Plus (I name)
        signPush ('-':name) = Push Minus (I name)
        signPush _          = error "Error in matching + and -"
        pushCreate lst = map signPush lst
        pullCreate lst = Pull $ O $ fromList $ foldl (\buff (_:name) -> name : buff ) [] lst
    in
        Acc $ concatMap (\(push, pull) -> pushCreate push ++ [pullCreate pull]) $ partedExpr blocks

accFromStr = accGen . toBlocksSplit

pushActionGroups lst = let
        signCheck (Push Plus (I v))  = (False, v)
        signCheck (Push Minus (I v)) = (True, v)
        signCheck _                  = error "Error . pattern matching in signCheck func in Functions.hs"
    in
        map (map signCheck) $ filter (not . null) $ splitWhen isPull lst

pullActionGroups lst = concatMap (map (elems . fromPull)) $ filter (not . null) $ splitWhen isPush lst

instance ( Var v ) => Locks (Acc v x) v where
    locks accList = let
            pushGroups (Acc lst) = map (map fromPush) $ filter (not . null) $ splitWhen isPull lst

            pullGroups (Acc lst) = map (concatMap (elems . fromPull)) $ filter (not . null) $ splitWhen isPush lst

            locksPush []     buff                     = filter (not . null . fst) buff
            locksPush (x:xs) []                       = locksPush xs [([], x)]
            locksPush (x:xs) buff@((lastL, lastLB):_) = locksPush xs ((x, lastL ++ lastLB):buff)

            locksPull []              buff                     = buff
            locksPull (x:xs)          []                       = locksPull xs [x]
            locksPull ((inp, out):xs) buff@((lastL, lastLB):_) = locksPull xs ((inp, out ++ lastL ++ lastLB):buff)

            pushList = pushGroups accList
            pullList = pullGroups accList
            exprTuple = zip pullList pushList
            locksListPush = locksPush pushList []
            locksListPull = locksPull exprTuple []
            allLocks = locksListPush ++ locksListPull

        in concatMap (\eachLock ->
            [ Lock { locked = y, lockBy = x }
            | x <- snd eachLock
            , y <- fst eachLock
            ]) allLocks

instance ( Var v, Num x ) => FunctionSimulation (Acc v x) v x where
    simulate cntx (Acc lst) = let
            operation v s accum context
                | Right x <- getX context v = case s of
                    Plus  -> (accum + x, Right context)
                    Minus -> (accum - x, Right context)
                | otherwise = (accum, Left "Error in accum Push value to context")

            select (accum, Right context) (Push s (I v)) = operation v s accum context
            select (accum, Right context) (Pull (O vs)) = (accum, setZipX context vs accum)
            select (accum, Left err) _ = (accum, Left err)

            (_, eitherContext) = foldl select (0, Right cntx) lst
        in eitherContext
