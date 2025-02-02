{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

{- |
Module      : NITTA.UIBackend
Description : HTTP backend for the NITTA web UI
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.UIBackend (
    backendServer,
    prepareJSAPI,
    restDocs,
    apiPath,
) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Either
import Data.String.Interpolate
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import NITTA.Synthesis
import NITTA.UIBackend.REST
import Network.Simple.TCP (connect)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Servant.Docs hiding (path)
import qualified Servant.JS as SJS
import System.FilePath.Posix (joinPath)

apiPath = joinPath [".", "web", "src", "services", "gen"]

restDocs port =
    markdown $
        docsWithIntros
            [ DocIntro
                "NITTA UI REST API description"
                [ "Autogenerated by `nitta-api-gen`"
                , "Expected port: " <> show port
                , "Request helper with axios can be finded here: " <> "`rest_api.js`"
                , "Typescript interfaces can be finded here: " <> "`types.ts`"
                ]
            ]
            $ pretty (Proxy :: Proxy (SynthesisAPI String String Int Int))

prepareJSAPI port path = do
    let prefix =
            [__i|
                import axios from 'axios';
                var jsAPI = {};
                export default jsAPI;
                /* eslint no-useless-concat: "off" */
            |]
    let axios' =
            SJS.axiosWith
                SJS.defAxiosOptions
                SJS.defCommonGeneratorOptions
                    { SJS.urlPrefix = [i|http://localhost:#{ port }|]
                    , SJS.moduleName = "jsAPI"
                    }
    SJS.writeJSForAPI (Proxy :: Proxy (SynthesisAPI _ String Int Int)) ((prefix <>) . axios') $ joinPath [path, "rest_api.js"]

application receivedValues model outputPath = do
    root <- synthesisTreeRootIO model
    return $
        serve
            ( Proxy ::
                Proxy
                    ( SynthesisAPI _ _ _ _
                        :<|> Get '[JSON] () -- root
                        :<|> Raw
                    )
            )
            ( synthesisServer BackendCtx{root, receivedValues, outputPath}
                :<|> throwError err301{errHeaders = [("Location", "index.html")]}
                :<|> serveDirectoryWebApp (joinPath ["web", "build"])
            )

isLocalPortFree port =
    isLeft <$> (try $ connect "localhost" (show port) (\_ -> return ()) :: IO (Either SomeException ()))

-- |Run backend server.
backendServer port receivedValues outputPath modelState = do
    putStrLn $ "Running NITTA server at http://localhost:" <> show port <> " ..."
    -- on OS X, if we run system with busy port - application ignore that.
    -- see: https://nitta.io/nitta-corp/nitta/issues/9
    isFree <- isLocalPortFree port
    unless isFree $ error "resource busy (Port already in use)"
    app <- application receivedValues modelState outputPath
    setLocaleEncoding utf8
    run port $ simpleCors app
