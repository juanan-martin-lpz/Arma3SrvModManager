
module ArmaSMMFileOperations (removeIfExists,
                              createModDirectory,
                              removeModDirectory,
                              moveModDirectory,
                              copyModDirectory,
                              moveModFile,
                              copyModFile,
                              getDirectories,
                              readDir) where
  import Prelude hiding (catch)
  import System.Directory
  --import qualified Shelly as S
  import Data.Text
  import Control.Exception
  import System.IO.Error hiding (catch)
  import System.FilePath.Posix
  import Control.Monad
  import System.Directory.Tree
  import qualified Path.IO as P


  readDir :: FilePath -> IO (DirTree FilePath)
  readDir p = do
    (base :/ dt) <- buildL p
    --return $ T.flattenDir dt
    return dt

  getDirectories :: FilePath -> IO [FilePath]
  getDirectories p = do
      let lista = listDirectory p >>= filterM (\x -> doesDirectoryExist $ p </> x)
      lista

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
  removeModDirectory mx = do

    dstE <- doesDirectoryExist mx

    if dstE then
      removeDirectoryRecursive mx
    else
      return ()

  copyModDirectory :: FilePath -> FilePath -> IO ()
  copyModDirectory from to = do
    fr <- P.resolveDir' from
    ty <- P.resolveDir' to

    P.copyDirRecur fr ty


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
  copyModFile from to = do
    let p = takeDirectory to
    e <- doesDirectoryExist p

    if not e then
      createModDirectory p
    else
      return ()

    putStrLn ("Copiando : " ++ (show from))

    copyFile from to
