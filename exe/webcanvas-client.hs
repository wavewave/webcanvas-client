module Main where

import System.Console.CmdArgs

import Application.WebCanvas.Client.ProgType
import Application.WebCanvas.Client.Command

main :: IO () 
main = do 
  putStrLn "webcanvas-client"
  param <- cmdArgs mode
  commandLineProcess param