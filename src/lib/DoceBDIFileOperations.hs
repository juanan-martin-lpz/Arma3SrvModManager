
module DoceBDIFileOperations (removeIfExists) where
  import Prelude hiding (catch)
  import System.Directory
  import Control.Exception
  import System.IO.Error hiding (catch)

  -- From https://stackoverflow.com/questions/8502201/remove-file-if-it-exists

  removeIfExists :: FilePath -> IO ()
  removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e
