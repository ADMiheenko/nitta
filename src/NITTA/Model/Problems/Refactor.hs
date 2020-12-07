{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : NITTA.Model.Problems.Refactor
Description : Automatic manipulation over an intermediate representation
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

Some times, CAD can not synthesis a target system because of a features of an
algorithm and microarchitecture (too less process units, too many functions, too
complicated algorithm).

In this case user can manually add some tweaks to the algorithm, but for that he
should be an expert with deep understanding of NITTA project. Of course, it is
not acceptable. This module defines type of that tweaks.
-}
module NITTA.Model.Problems.Refactor
    ( Refactor(..), RefactorProblem(..)
    , recLoop, recLoopOut, recLoopIn
    , prepareBuffer
    , maxBufferStack
    ) where

import           Data.Default
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           NITTA.Intermediate.DataFlow
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Refactor.Accum
import           NITTA.Model.Problems.Refactor.Types
import           NITTA.Utils.Base


instance ( Var v, Val x
        ) => RefactorProblem (DataFlowGraph v x) v x where
    refactorOptions dfg = optimizeAccumOptions dfg

    refactorDecision dfg r@ResolveDeadlock{} = let
            ( buffer, diff ) = prepareBuffer r
            fs' = buffer : map (patch diff) (functions dfg)
        in fsToDataFlowGraph fs'

    refactorDecision (DFCluster leafs) bl@BreakLoop{} = let
            origin = recLoop bl
        in DFCluster
            $ DFLeaf (recLoopIn bl){ funHistory=[origin] }
            : DFLeaf (recLoopOut bl){ funHistory=[origin] }
            : ( leafs L.\\ [ DFLeaf origin ] )

    refactorDecision dfg ref@OptimizeAccum{} = optimizeAccumDecision dfg ref

    refactorDecision _ _ = error "DataFlowGraph "



recLoop BreakLoop{ loopX, loopO, loopI }
    = packF $ Loop (X loopX) (O loopO) (I loopI)
recLoop _ = error "applicable only for BreakLoop"

recLoopIn BreakLoop{ loopX, loopO, loopI }
    = packF $ LoopIn (Loop (X loopX) (O loopO) (I loopI)) (I loopI)
recLoopIn _ = error "applicable only for BreakLoop"

recLoopOut BreakLoop{ loopX, loopO, loopI }
    = packF $ LoopOut (Loop (X loopX) (O loopO) (I loopI)) (O loopO)
recLoopOut _ = error "applicable only for BreakLoop"



prepareBuffer :: ( Var v, Val x ) => Refactor v x -> ( F v x, Changeset v )
prepareBuffer (ResolveDeadlock vs) = let
        bufferI = bufferSuffix $ oneOf vs
        bufferO = S.elems vs
        diff = def{ changeO=M.fromList $ map (\o -> (o, S.singleton bufferI)) bufferO }
    in ( reg bufferI bufferO, diff )

prepareBuffer _ = undefined



-- |The constant, which restrict maximum length of a buffer sequence.
maxBufferStack = 2 :: Int
