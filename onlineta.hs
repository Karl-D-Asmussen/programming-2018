#!/usr/bin/env stack
{- stack
   --resolver lts-12.6
   --install-ghc
   runghc
   --package http-client-tls
   --package zip-archive
-}

-- Check your submission with OnlineTA <https://find.incorrectness.dk/grade/>.
--
-- This script will zip your src directory (must be present!) and try to submit
-- the generated src.zip file to OnlineTA.  Warning: If you already have a
-- src.zip file, this can overwrite files in it.
--
-- Usage: ./onlineta.hs <assignment number> <work directory>
--
-- Example use:
--
-- $ ./onlineta.hs 0 ~/ap/assignments/0
--
-- This will test the files ~/ap/assignments/0 for assignment 0.
--
-- This works thanks to the "shebang" at the top of this file.
--
-- Otherwise, you will need to compile this Haskell file to an executable first:
--
-- $ stack ghc -- -Wall onlineta.hs
-- $ ./onlineta 0 ~/ap/assignments/0

{-# LANGUAGE OverloadedStrings #-}

import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as BS
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Client.MultipartFormData
import qualified System.Directory as SD
import System.Environment ( getArgs )


formURL :: String
formURL = "https://find.incorrectness.dk/grade/"

showUsage :: IO ()
showUsage = do
  putStrLn "Usage: ./onlineta.hs <assignment number> <work directory>"

checkFiles :: IO ()
checkFiles = do
  srcExists <- SD.doesDirectoryExist "src"
  if srcExists
  then return ()
  else do
    showUsage
    wd <- SD.getCurrentDirectory
    fail $ "\n  The directory \n    " ++ wd ++
      "\n  lacks a src subdirectory." ++
      "\n  Cowardly refusing to conduct further tests."

archiveSrc :: IO ()
archiveSrc = do
  arch <- Zip.addFilesToArchive [Zip.OptRecursive] Zip.emptyArchive ["src"]
  BS.writeFile "src.zip" (Zip.fromArchive arch)

checkSubmission :: String -> IO ()
checkSubmission url = do
  manager <- newManager tlsManagerSettings
  request <- parseUrlThrow url
  let files = [partFileSource "src" "src.zip"]
  body <- formDataBody files request
  response <- responseBody <$> httpLbs body manager
  BS.putStr response
  return ()

main :: IO ()
main = do
  args <- getArgs
  cur <- SD.getCurrentDirectory
  case args of
    [assignment, directory] -> do
      SD.setCurrentDirectory directory
      checkFiles
      archiveSrc
      checkSubmission (formURL ++ assignment)
    _ -> showUsage
