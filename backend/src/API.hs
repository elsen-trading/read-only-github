{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module API where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import Data.ByteString (ByteString, append)
import qualified Data.Map as Map
import Data.Map (Map, fromList)
import GHC.Generics
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wreq hiding (Proxy)
import Servant
import Text.InterpolatedString.Perl6

data Config = Config
            { _cfgheader :: String
            , _cfgrepo :: String
            , _cfgproject :: String
            , _cfgtoken :: ByteString
            , _cfgapi :: ByteString
            , _cfglogins :: [(ByteString,ByteString)]
            } deriving (Show)

data Project = Project
             { _pid :: Int
             , _pname :: String
             , _pbody :: String
             , _pupdated_at :: String
             , _pnumber :: Int
             } deriving (Generic)

instance ToJSON Project where
  toJSON = genericToJSON defaultOptions
            { fieldLabelModifier = drop 2 }
instance FromJSON Project where
  parseJSON = genericParseJSON defaultOptions
            { fieldLabelModifier = drop 2 }

type ColumnId = Int

data Column = Column
            { _cid :: ColumnId
            , _cname :: String
            , _cupdated_at :: String
             } deriving (Show,Eq,Ord,Generic)

instance ToJSON Column where
  toJSON = genericToJSON defaultOptions
            { fieldLabelModifier = drop 2 }
instance FromJSON Column where
  parseJSON = genericParseJSON defaultOptions
            { fieldLabelModifier = drop 2 }

data Card = Card
          { _cardcontent_url :: String
          } deriving (Show,Generic)

instance ToJSON Card where
  toJSON = genericToJSON defaultOptions
            { fieldLabelModifier = drop 5 }
instance FromJSON Card where
  parseJSON = genericParseJSON defaultOptions
            { fieldLabelModifier = drop 5 }

data Issue = Issue
           { _iurl :: String
           , _ihtml_url :: String
           , _inumber :: Int
           , _ititle :: String
           , _istate :: String
           , _icomments :: Int
           , _iupdated_at :: String
           , _ibody :: String
           } deriving (Show,Generic)

instance ToJSON Issue where
  toJSON = genericToJSON defaultOptions
            { fieldLabelModifier = drop 2 }
instance FromJSON Issue where
  parseJSON = genericParseJSON defaultOptions
            { fieldLabelModifier = drop 2 }

getProjects :: Config -> IO [Project]
getProjects cfg = do
  res <- getWith (opts cfg) $ projectsURL $ _cfgrepo cfg
  return $ either error id . eitherDecode $ res ^. responseBody

getTargetProject :: Config -> IO (Maybe Project)
getTargetProject cfg = do
  projects <- getProjects cfg
  let go []     = Nothing
      go (x:xs) = case (_pname x) == (_cfgproject cfg) of
        True  -> Just x
        False -> go xs
  return $ go projects

getColumns :: Config -> Project -> IO [Column]
getColumns cfg p = do
  res <- getWith (opts cfg) $ columnsURL p
  return $ either error id . eitherDecode $ res ^. responseBody

getTargetProjectLink :: Config -> IO String
getTargetProjectLink cfg = do
  project <- getTargetProject cfg 
  return $ case project of
    Nothing -> ""
    Just p  -> projectURL p $ _cfgrepo cfg

getTargetColumns :: Config -> IO [Column]
getTargetColumns cfg = do
  project <- getTargetProject cfg
  case project of
    Nothing -> return []
    Just p  -> getColumns cfg p

getCards :: Config -> Column -> IO (String,[Card])
getCards cfg c = do
  res <- getWith (opts cfg) $ cardsURL $ _cid c
  return $ (,) (_cname c) $ either error id . eitherDecode $ res ^. responseBody

getColumnCards :: Config -> ColumnId -> IO [Card]
getColumnCards cfg cid = do
  res <- getWith (opts cfg) $ cardsURL cid
  return $ either error id . eitherDecode $ res ^. responseBody

getColumnIssues :: Config -> ColumnId -> IO [Issue]
getColumnIssues cfg cid = do
  cards <- getColumnCards cfg cid
  mapConcurrently (getIssue cfg) cards

getTargetCards :: Config -> IO (Map String [Card])
getTargetCards cfg = do
  columns <- getTargetColumns cfg
  fmap fromList $ mapM (getCards cfg) columns

getIssue :: Config -> Card -> IO Issue
getIssue cfg c = do
  res <- getWith (opts cfg) $ _cardcontent_url c
  return $ either error id . eitherDecode $ res ^. responseBody

base :: String
base = "https://api.github.com"

mirrorName :: Config -> String
mirrorName cfg = [qc|{_cfgheader cfg} {_cfgproject cfg}|]

projectURL :: Project -> String -> String
projectURL p repo = [qc|https://github.com/{repo}/projects/{_pnumber p}|]

projectsURL :: String -> String
projectsURL repo = [qc|{base}/repos/{repo}/projects|]

columnsURL :: Project -> String
columnsURL p = [qc|{base}/projects/{_pid p}/columns|]

cardsURL :: ColumnId -> String
cardsURL cid = [qc|{base}/projects/columns/{cid}/cards|]

opts :: Config -> Network.Wreq.Options
opts cfg = defaults
  & header "Accept" .~ [ _cfgapi cfg ]
  & header "Authorization" .~ [ append "token " (_cfgtoken cfg) ]

app :: Config -> Application
app cfg = simpleCors $ serveWithContext api ctx srv
  where
    ctx = authServerContext cfg
    srv = server cfg

type Api =
       "api"
          :>"columns"
          :> BasicAuth "foobar" User
          :> Get '[JSON] [Column]
  :<|> "api"
          :> "columns"
          :> BasicAuth "foobar" User
          :> Capture "id" ColumnId
          :> Get '[JSON] [Issue]
  :<|> "api"
          :> "name"
          :> BasicAuth "foobar" User
          :> Get '[JSON] String
  :<|> "api"
          :> "link"
          :> BasicAuth "foobar" User
          :> Get '[JSON] String

server :: Config -> Server Api
server cfg = let
  h1 (user :: User) = (liftIO $ getTargetColumns cfg)
  h2 (user :: User) = (liftIO . getColumnIssues cfg)
  h3 (user :: User) = (return $ mirrorName cfg)
  h4 (user :: User) = (liftIO $ getTargetProjectLink cfg)
  in h1 :<|> h2 :<|> h3 :<|> h4

api :: Proxy Api
api = Proxy

data User = Basic

-- auth shamelessly grabbed from http://haskell-servant.readthedocs.io/en/stable/tutorial/Authentication.html
authCheck :: Config -> BasicAuthCheck User
authCheck cfg = BasicAuthCheck check
  where
    check (BasicAuthData user pass) =
      if validLogin cfg user pass
      then return (Authorized Basic)
      else return Unauthorized

validLogin :: Config -> ByteString -> ByteString -> Bool
validLogin cfg user pass = any (go user pass) (_cfglogins cfg)
  where
    go u p (x,y) = u == x && p == y

authServerContext :: Config -> Servant.Context '[BasicAuthCheck User]
authServerContext cfg = (authCheck cfg) :. EmptyContext