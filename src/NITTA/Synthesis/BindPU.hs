{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

module NITTA.Synthesis.BindPU (
    ) where

import Data.Aeson
import GHC.Generics
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.BindPU
import NITTA.Model.TargetSystem
import NITTA.Synthesis

data BindPUMetrics = BindPUMetrics
    deriving (Generic)

instance ToJSON BindPUMetrics

instance
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) tag v x t) tag v x t) -- ctx
        (TargetSystem (BusNetwork tag v x t) tag v x t) -- m
        (BindPU tag v x t) -- o
        (BindPU tag v x t) -- d
        BindPUMetrics -- p
    where
    decisions SynthesisState{sTarget} o = [(o, bindPUDecision sTarget o)]

    -- parameters :: ctx -> o -> d -> p
    parameters SynthesisState{} BindPU{} BindPU{} = BindPUMetrics

    -- estimate :: ctx -> o -> d -> p -> Float
    estimate SynthesisState{} BindPU{} BindPU{} BindPUMetrics = -1