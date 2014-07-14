module System.Process.Exts
  ( runCommand
  , runCommandCleanly
  )  where

import Prelude hiding (null)
import System.FilePath
import System.Exit
import System.Process (CreateProcess(..), createProcess, waitForProcess, CmdSpec(RawCommand), StdStream(CreatePipe))

import Control.Concurrent.Async

import Data.ByteString.Char8 (ByteString, hPut, hGetContents, null)

------------------------------------------------------------------------

runCommand :: FilePath -> [String] -> ByteString -> IO (Int, ByteString, ByteString)
runCommand cmd args input = do
  (Just hIn, Just hOut, Just hErr, pHandle) <- createProcess $
    CreateProcess (RawCommand cmd args) Nothing Nothing
                  CreatePipe CreatePipe CreatePipe True False True
  ((_, eCode), (out, err)) <- concurrently
     (concurrently (hPut hIn input)    (waitForProcess pHandle))
     (concurrently (hGetContents hOut) (hGetContents hErr))
  case eCode of
    ExitSuccess   -> return (0, out, err)
    ExitFailure c -> return (c, out, err)

------------------------------------------------------------------------

runCommandCleanly :: FilePath -> [String] -> ByteString -> IO (Maybe ByteString)
runCommandCleanly cmd args input = do
  (rc, out, err) <- runCommand cmd args input
  return $ if rc == 0 && null err
           then Just out
           else Nothing

------------------------------------------------------------------------
