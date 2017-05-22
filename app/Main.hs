{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Text as T

import Web.Spock
import Web.Spock.Config
import System.Environment (lookupEnv)
import Network.Wai.Internal (Request, requestHeaderHost)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Database.Persist.TH
import Database.Persist hiding (get)
import Database.Persist.MySQL hiding (get)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson
import Network.URI

type DB = ReaderT SqlBackend (LoggingT IO)

data AppSettings = AppSettings { appDatabaseConf :: MySQLConf
                               , appPort :: Int
                               , appBase :: Text
                               }


instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        appPort <- o .: "port"
        appBase <- o .: "base-url"
        appDatabaseConf <- o .: "database"
        return AppSettings {..}


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Site json
  url Text
  provider Text
  deriving Show
Redirect json
  originalPath Text
  destinationPath Text
|]

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn


getDomain :: Request -> Maybe Text
getDomain req = decodeUtf8 <$> requestHeaderHost req


getProvider :: Request -> DB Text
getProvider req = do
    res <- runMaybeT $ do
        domain <- MaybeT . return . getDomain $ req
        Entity _ site <- MaybeT $ selectFirst [SiteUrl ==. domain] []
        return $ siteProvider site
    return $ fromMaybe "" res


main :: IO ()
main = do
    settings <- liftIO $ loadYamlSettings ["settings.yaml"] [] useEnv
    pool <- runStdoutLoggingT $ createMySQLPool
        (myConnInfo $ appDatabaseConf settings)
        (myPoolSize $ appDatabaseConf settings)
    spockCfg <- defaultSpockCfg () (PCPool pool) settings
    runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
    runSpock (appPort settings) (spock spockCfg app)


app :: SpockM SqlBackend () AppSettings ()
app = do
    get root $ do
        settings <- getState
        req <- request
        provider <- runSQL $ getProvider req
        redirect $ appBase settings <> provider
    get wildcard $ \path -> do
        settings <- getState
        mredirection <- runSQL $ selectFirst [RedirectOriginalPath ==. path] []
        case mredirection of
             Nothing -> redirect $ appBase settings
             Just (Entity _ redirection) -> redirect $ appBase settings <> redirectDestinationPath redirection
