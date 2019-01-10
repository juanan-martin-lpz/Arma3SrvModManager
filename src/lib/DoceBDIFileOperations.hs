
module DoceBDIFileOperations (removeIfExists,
                              createModDirectory,
                              removeModDirectory,
                              moveModDirectory,
                              copyModDirectory,
                              moveModFile,
                              copyModFile) where
  import Prelude hiding (catch)
  import System.Directory
  import qualified Shelly as S
  import Data.Text
  import Control.Exception
  import System.IO.Error hiding (catch)

  -- From https://stackoverflow.com/questions/8502201/remove-file-if-it-exists

  removeIfExists :: FilePath -> IO ()
  removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e


  createModDirectory :: FilePath -> IO ()
  createModDirectory mx = createDirectoryIfMissing True mx `catch` handleExists
    where handleExists e
            | isAlreadyExistsError e = return ()
            | otherwise = throwIO e

  removeModDirectory :: FilePath -> IO ()
  removeModDirectory mx = removeDirectoryRecursive mx

  copyModDirectory :: FilePath -> FilePath -> IO ()
  copyModDirectory from to = S.shelly $ S.cp_r (S.fromText $ pack from) (S.fromText $ pack to)

  moveModDirectory :: FilePath -> FilePath -> IO ()
  moveModDirectory from to = renamePath from to `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

  moveModFile :: FilePath -> FilePath -> IO ()
  moveModFile from to = renameFile from to `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

  copyModFile :: FilePath -> FilePath -> IO ()
  copyModFile from to = copyFile from to
