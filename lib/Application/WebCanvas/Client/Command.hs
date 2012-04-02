module Application.WebCanvas.Client.Command where

import Application.WebCanvas.Client.ProgType
import Application.WebCanvas.Client.Job
import Application.WebCanvas.Client.Config
import Data.Configurator

commandLineProcess :: Webcanvas_client -> IO ()
-- commandLineProcess (Create cfg mn) = do 
--  putStrLn "create called"
--   mc <- getWebcanvasClientConfiguration =<< load [Required cfg] 
--  maybe (error "cannot parse config") (flip startCreate mn) mc
commandLineProcess (Get cfg n) = do 
  putStrLn "get called"
  mc <- getWebcanvasClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startGet n) mc
-- commandLineProcess (Put cfg n mn) = do 
--   putStrLn "put called"
--  mc <- getWebcanvasClientConfiguration =<< load [Required cfg] 
--   maybe (error "cannot parse config") (\c-> startPut c n mn) mc
commandLineProcess (Delete cfg n) = do 
  putStrLn "delete called"
  mc <- getWebcanvasClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startDelete n) mc
commandLineProcess (GetList cfg) = do 
  putStrLn "getlist called"
  mc <- getWebcanvasClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") startGetList mc
commandLineProcess _ = do 
  putStrLn "not yet implemented" 