{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.Problems.Bind
Description : Function distribution between processor units
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.BindPU (
    BindPU (..),
    BindPUProblem (..),
) where

import Data.Data
import Data.String.ToString
import NITTA.Model.Networks.Types
import NITTA.Model.ProcessorUnits

data BindPU tag v x t where
    BindPU ::
        (UnitTag tag, PUClasses pu v x t) =>
        { bpTag :: tag
        , bpPU :: pu
        , bpIoPorts :: IOPorts pu
        } ->
        BindPU tag v x t

instance Show (BindPU tag v x t) where
    show BindPU{bpTag, bpPU} = "BindPU " <> toString bpTag <> show (typeOf bpPU) -- TODO: BasicEC Add PU desc

class BindPUProblem u tag v x t | u -> tag v x t where
    bindPUOptions :: u -> [BindPU tag v x t]
    bindPUDecision :: u -> BindPU tag v x t -> u
