{-# LANGUAGE OverloadedStrings #-}

module Application.WebCanvas.Client.Config where

import Control.Applicative
import Data.Configurator as C
import Data.Configurator.Types

data WebcanvasClientConfiguration = WebcanvasClientConfiguration { 
  webcanvasServerURL :: String,
  webcanvasClientURL :: String
} deriving (Show)

getWebcanvasClientConfiguration :: Config -> IO (Maybe WebcanvasClientConfiguration)
getWebcanvasClientConfiguration config = do  
  s <- C.lookup config "server" 
  c <- C.lookup config "client" 
  return  (WebcanvasClientConfiguration  <$> s <*> c )
