{-# LANGUAGE OverloadedStrings #-}

module Application.WebCanvas.Client.Job where

import Debug.Trace

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC
import Data.Aeson.Types
import Data.Aeson.Encode as E
import Data.Aeson.Parser
import qualified Data.Attoparsec as A

import Network.HTTP.Types hiding (statusCode)
import Network.HTTP.Enumerator

import System.Directory 
import System.FilePath
import Unsafe.Coerce

import Application.WebCanvas.Client.Config
import Application.WebCanvas.Type
import Data.UUID
import Data.UUID.V5
import qualified Data.ByteString as B
import Data.Time.Clock

type Url = String 

nextUUID :: WebcanvasClientConfiguration -> IO UUID
nextUUID mc = do 
  let c = webcanvasClientURL mc 
  t <- getCurrentTime 
  return . generateNamed namespaceURL . B.unpack . SC.pack $ c ++ "/" ++ show t 

startCreate :: WebcanvasClientConfiguration -> UTCTime -> IO () 
startCreate mc ctime = do 
  putStrLn "job started"
  cwd <- getCurrentDirectory
  let url = webcanvasServerURL mc 
  uuid <- nextUUID mc
  let info = WebCanvasItem { webcanvas_uuid = uuid , webcanvas_creationtime = ctime } 
  response <- webcanvasToServer url ("uploadwebcanvas") methodPost info
  putStrLn $ show response 


startGet :: WebcanvasClientConfiguration -> String -> IO () 
startGet mc idee = do 
  putStrLn $"get " ++ idee
  let url = webcanvasServerURL mc 
  r <- jsonFromServer url ("webcanvas" </> idee) methodGet
  putStrLn $ show r 


startPut :: WebcanvasClientConfiguration 
         -> String  -- ^ webcanvas idee
         -> UTCTime  -- ^ webcanvas creation time 
         -> IO () 
startPut mc idee ctime = do 
  putStrLn "job started"
  cwd <- getCurrentDirectory
  let url = webcanvasServerURL mc 
      info = case fromString idee of 
               Nothing -> error "strange in startPut" 
               Just idee' -> WebCanvasItem { webcanvas_uuid = idee', webcanvas_creationtime = ctime }
  response <- webcanvasToServer url ("webcanvas" </> idee) methodPut info
  putStrLn $ show response 


startDelete :: WebcanvasClientConfiguration -> String -> IO () 
startDelete mc idee = do 
  putStrLn "job started"
  let url = webcanvasServerURL mc 
  r <- jsonFromServer url ("webcanvas" </> idee) methodDelete
  putStrLn $ show r 


startGetList :: WebcanvasClientConfiguration -> IO () 
startGetList mc = do 
  putStrLn "getlist: "
  let url = webcanvasServerURL mc 
  r <- jsonFromServer url ("listwebcanvas") methodGet
  putStrLn $ show r 


jsonFromServer :: Url -> String -> Method -> IO (Either String (Result Value))
jsonFromServer url api mthd = do 
  request <- parseUrl (url </> api)
  withManager $ \manager -> do
    let requestjson = request { 
          method = mthd,
          requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] } 
    r <- httpLbs requestjson manager 
    if statusCode r == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode r)) 

webcanvasToServer :: Url -> String -> Method -> WebCanvasItem -> IO (Either String (Result Value))
webcanvasToServer url api mthd mi = do 
  request <- parseUrl (url </> api)
  withManager $ \manager -> do
    let mijson = E.encode (toJSON mi)
        myrequestbody = RequestBodyLBS mijson 
    let requestjson = request 
          { method = mthd
          , requestHeaders = [ ("Accept", "application/json; charset=utf-8") ]
          , requestBody = myrequestbody } 
    r <- httpLbs requestjson manager 
    if statusCode r == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode r)) 

parseJson :: (FromJSON a) => SC.ByteString -> Either String (Result a)
parseJson bs =
  let resultjson = trace (SC.unpack bs) $ A.parse json bs 
  in case resultjson of 
       (A.Done rest rjson) -> return (parse parseJSON rjson)
       _                 -> Left "parseJson" 