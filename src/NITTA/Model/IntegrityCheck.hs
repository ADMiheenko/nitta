{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}

{- |
Module      : NITTA.Model.IntegrityCheck
Description : Module for checking model description consistency
Copyright   : (c) Artyom Kostyuchik, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.IntegrityCheck (
    checkIntegrity,
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits
import NITTA.Utils
import Safe

checkIntegrity pu =
    let getInterMap =
            M.fromList
                [ (pID, f)
                | step@Step{pID, pDesc} <- steps $ process pu
                , isFB step
                , f <- case pDesc of
                    (FStep f) -> [f]
                    _ -> []
                ]
        getEpMap =
            M.fromListWith (++) $
                concat
                    [ concatMap (\v -> [(v, [(pID, ep)])]) $ variables ep
                    | step@Step{pID, pDesc} <- steps $ process pu
                    , isEndpoint step
                    , ep <- case pDesc of
                        (EndpointRoleStep e) -> [e]
                        _ -> []
                    ]
        getInstrMap =
            M.fromList
                [ (pID, pDesc)
                | Step{pID, pDesc} <- steps $ process pu
                , isInstruction pDesc
                ]
        -- (pid, f)
        getCadFunctions =
            let filterCad (_, f)
                    | Just Loop{} <- castF f = True
                    | Just (LoopBegin Loop{} _) <- castF f = True
                    | Just (LoopEnd Loop{} _) <- castF f = True
                    | otherwise = False
             in M.fromList $ filter filterCad $ M.toList getInterMap

        -- (Loop (pid, f)) , where Loop is show instance
        -- TODO: add nothing
        getCadSteps =
            M.fromList $
                concat
                    [ concatMap (\l -> [(l, (pID, step))]) pDesc'
                    | step@Step{pID} <- steps $ process pu
                    , pDesc' <- case getCAD step of
                        Just msg -> [msg]
                        _ -> []
                    ]
     in and
            [ checkEndpointToIntermidiateRelation getEpMap getInterMap $ process pu
            , checkInstructionToEndpointRelation getInstrMap getEpMap $ process pu
            , checkCadToFunctionRelation getCadFunctions getCadSteps $ process pu
            ]

-- every function should be binded to CAD step
-- at the moment check LoopBegin/End
checkCadToFunctionRelation cadFs cadSt pr = S.isSubsetOf makeCadVertical rels
    where
        rels = S.fromList $ filter isVertical $ relations pr
        showLoop f = "bind " <> show f
        makeCadVertical =
            S.fromList $
                concatMap
                    ( \(h, f) ->
                        concatMap
                            ( \v -> [uncurry Vertical (h, fst $ cadSt M.! v)]
                            )
                            $ showLoop f
                    )
                    $ M.toList cadFs
            if length l > 1
                then Vertical h $ fst $ findJust (\(k, _) -> Vertical h k `elem` rels) l
                else Vertical h $ fst $ head l
        makeRelationList =
            S.fromList $
                concatMap
                    ( \(h, f) ->
                        concatMap
                            ( \v -> [findRel (h, eps M.! v)]
                            )
                            $ variables f
                    )
                    $ M.toList ifs

checkInstructionToEndpointRelation ins eps pr = and makeRelationList
    where
        rels = S.fromList $ map (\(Vertical r1 r2) -> (r1, r2)) $ filter isVertical $ relations pr
        eps' = M.fromList $ concat $ M.elems eps
        makeRelationList =
            concatMap
                ( \(r1, r2) -> case eps' M.!? r1 of
                    Just _ | (InstructionStep _) <- ins M.! r2 -> [True]
                    _ -> []
                )
                rels
