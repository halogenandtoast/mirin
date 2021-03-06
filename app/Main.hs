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

import Web.Spock
import Web.Spock.Config
import Network.Wai.Internal (Request, requestHeaderHost)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Database.Persist.TH
import Database.Persist hiding (get)
import Database.Persist.MySQL hiding (get)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)
import Data.Aeson
import Network.HTTP.Types.Status (status301)

type DB = ReaderT SqlBackend (LoggingT IO)
type MirinAction ctx a = SpockActionCtx ctx SqlBackend () AppSettings a

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
Site
  url Text
  provider Text
Redirect
  originalPath Text
  destinationPath Text
  provider Text
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


redirect301 :: MonadIO m => Text -> ActionT m ()
redirect301 url = do
    setStatus status301
    setHeader "Location" url
    html $ "<html><body>You are being <a src=\"" <> url <> "\">redirected</a>.</body></html>"

settingsWithProvider :: MirinAction ctx (AppSettings, Text)
settingsWithProvider = do
  settings <- getState
  req <- request
  provider <- runSQL $ getProvider req
  return (settings, provider)

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
        (settings, provider) <- settingsWithProvider
        redirect301 $ appBase settings <> provider
    get wildcard $ \path -> do
        (settings, provider) <- settingsWithProvider
        mredirection <- runSQL $ selectFirst [RedirectOriginalPath ==. path, RedirectProvider ==. provider] []
        case mredirection of
             Nothing -> redirect301 $ appBase settings
             Just (Entity _ redirection) -> redirect301 $ appBase settings <> redirectDestinationPath redirection
