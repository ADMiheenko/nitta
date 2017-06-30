{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module NITTA where

import           Base
import           Control.Monad.State
import           Data.Default
import           Data.List           (find, intersect, partition)
import qualified Data.List           as L
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe, isJust)
import           FB
import qualified FB


data NITTA title (variant :: * -> * -> *) (action :: * -> * -> *) v t k where
  NITTA ::
    { niRemains :: [FB v]
    , delegated :: [FB v]
    , dpus      :: M.Map title (PU (Action TimeConstrain) (Action Moment) v t k)
    } -> NITTA title variant action v t k

instance Default (NITTA title variant action var time key) where
  def = NITTA def def def

instance ( Ord title, Enum key, Var v, Time t
         ) => PUClass
         (NITTA title) (Transport TimeConstrain title) (Transport Moment title) v t key where
  evaluate ni@NITTA{..} fb = Just ni{ niRemains=fb : niRemains }
  -- variants = nittaVariants
  -- step = nittaStep
  -- process = undefined



nittaVariants nitta@NITTA{..} = justTransport
  where
    avs = availableVars nitta

    dpuVariants = concat $ map (\(title, d) -> zip (repeat title) $ map act $ variants d)
                  $ M.assocs dpus
    -- если pull есть в DPU, то он точно есть в alg.
    (pulls, pushs) = partition (\case { (_, (Pull _, _)) -> True; _ -> False }) dpuVariants
    justTransport =
      [ Transport
        { pullFrom=title
        , pullAt=time
        , push=M.fromList [ (v, if v `elem` avs
                              then fmap (\(title, (_, time)) -> (title, time)) $ findPush v pushs
                              else Nothing
                            )
                          | v <- vs
                          ]
        }
      | (title, (Pull vs, time)) <- pulls
      ]
    findPush v = find (\case
                          (title, (Push v', time)) | v == v' -> True
                          _ -> False
                      )




delegatable fb dpu = isJust $ evaluate dpu fb

delegate fb dpuTitle nitta@NITTA{..} = nitta
  { dpus=M.adjust (\dpu -> fromMaybe undefined $ evaluate dpu fb) dpuTitle dpus
  , niRemains=filter (/= fb) niRemains
  , delegated=fb:delegated
  }

delegationVariants NITTA{..} = concatMap delegationVariants' niRemains
  where
    delegationVariants' fb =
      map (uncurry $ variantsAfterDelegation fb) $ M.assocs dpus
    variantsAfterDelegation fb dpuTitle dpu =
      let (act, _) `variantOf` fb = not $ null (variables act `intersect` variables fb)
          vs = case evaluate dpu fb of
            -- try to reuse dpu'
            Just dpu' -> filter (`variantOf` fb) $ variants dpu'
            Nothing   -> []
      in (fb, dpuTitle, vs)


-- only for delegated FB
availableVars NITTA{..} =
  let fbs = niRemains ++ delegated
      alg = foldl
        (\m (a, b) -> M.adjust ((:) b) a m)
        (M.fromList [(v, []) | v <- concat $ map variables fbs])
        $ concat $ map dependency fbs
  in concat $ map snd $ filter (not . null . snd) $ M.assocs alg

-- step :: dpu -> Interaction var -> time -> time -> dpu
-- nittaStep :: NITTA title key var time -> Transport title var time -> NITTA title key var time
nittaStep ni@NITTA{..} Transport{..} =
  ni{ dpus=foldl (\s n -> n s) dpus steps }
  where
    pullStep = M.adjust (\dpu -> step dpu (pullVars, pullAt)) pullFrom
    pushStep (var, (dpuTitle, pushAt)) =
      M.adjust (\dpu -> step dpu ((Push var), pushAt)) dpuTitle
    pushSteps = map pushStep $ M.assocs push'
    steps = pullStep : pushSteps

    pullVars = Pull $ M.keys push'
    push' = M.map (fromMaybe undefined) $ M.filter isJust push
