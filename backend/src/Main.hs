{-# LANGUAGE OverloadedStrings #-}

module Main where

import API
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Configurator
import Data.Configurator.Types
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  cfg <- load [Required "project.cfg"]

  header  <- liftIO $ require cfg "header" :: IO String
  repo    <- liftIO $ require cfg "repo" :: IO String
  project <- liftIO $ require cfg "project" :: IO String
  token   <- liftIO $ require cfg "token" :: IO ByteString
  api     <- liftIO $ require cfg "api" :: IO ByteString
  port    <- liftIO $ require cfg "port" :: IO Int
  user    <- liftIO $ require cfg "user" :: IO ByteString
  pass    <- liftIO $ require cfg "pass" :: IO ByteString

  let logins = [(user,pass)]
  let cfg'   = Config header repo project token api logins

  run port $ app cfg'
