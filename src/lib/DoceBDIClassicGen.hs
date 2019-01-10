{-# LANGUAGE OverloadedStrings #-}

module DoceBDIClassicGen ( processRepository,
                           processRepositories,
                           processAddon,
                           generateRepositories) where

  import Data.Digest.XXHash.FFI
  import DoceBDIFileWork
  import DoceBDIFileOperations
  import DoceBDIData
  import Data.Word
  import qualified Codec.Base64 as C
  import Data.ByteString.Lazy.Builder
  import Data.Text as T
  import qualified Data.Text.Lazy.IO as T
  import qualified Data.Text.Lazy.Encoding as T
  import Data.Aeson
  import Data.Aeson.Encode.Pretty
  import System.Posix
  import System.FilePath.Posix
  import Data.List as L
  import System.Directory.Tree as T
  import Data.Foldable as F
  import System.Directory
  import Control.Monad
  import qualified Data.ByteString.Lazy as BS
  import Data.Maybe

  getDirectories :: FilePath -> IO [FilePath]
  getDirectories p = do
      let lista = listDirectory p >>= filterM (\x -> doesDirectoryExist $ p </> x)
      lista

  readDir :: FilePath -> IO (T.DirTree FilePath)
  readDir p = do
    (base :/ dt) <- T.buildL p
    --return $ T.flattenDir dt
    return dt

  calculateXXHash :: FilePath -> IO String
  calculateXXHash path = do
    content <- readFileLazy path
    let hash = xxh64 content 0
    let hashLE = word64LE hash
    return . T.unpack $ (C.encode :: Builder -> T.Text) hashLE


  getFileLength :: FilePath -> IO Integer
  getFileLength entry = do
   stat <- getFileStatus entry
   let size = fileSize stat
   return $ fromIntegral size

  getRelativePath :: FilePath -> FilePath -> FilePath
  getRelativePath base path = do
    let baseL = splitPath base
    let pathL = splitPath path
    let diff = (pathL \\ baseL)
    joinPath diff


  getFileName :: FilePath -> FilePath
  getFileName fname = takeFileName fname

  processRepositories :: FilePath -> IO ()
  processRepositories base = do
    repos <- getDirectories base
    mapM_ (\x -> processRepository $ base </> x) repos
    generateRepositories base repos

  generateRepositories :: FilePath -> [FilePath] -> IO ()
  generateRepositories base repos = do
    r <- mapM (\x -> generateRepository base x) repos
    T.writeFile (base </> "repositories.json") (T.decodeUtf8 . encodePretty' (defConfig {confCompare=keyOrder ["Nombre","Mods","MustUpdate"]}) $ r)
    m <- mapM (\x -> return $ mods x ) r
    un <- mapM (\r -> return $ RepoMod (modName r)) $ F.concat m
    writeRepomods (base </> "mods.txt") $ L.nub un

  generateRepository :: FilePath -> FilePath -> IO Repository
  generateRepository base p = do
    addons <- getDirectories $ base </> p
    mods <- mapM generateMod addons
    return $ Repository p mods False

  generateMod :: String -> IO Mod
  generateMod m = do
    return $ Mod m ""


  processRepository :: String -> IO ()
  processRepository repo = do
    addons <- getDirectories repo
    ficheros <- mapM (\x -> processAddon' repo x) addons
    --let json = encode $ F.concat ficheros
    T.writeFile (repo </> "ficheros.json") (T.decodeUtf8 . encodePretty' (defConfig {confCompare=keyOrder ["Mod","Ruta","Nombre","Firma","Tamano"]}) $ F.concat ficheros)


  -- repo must be a global path
  -- params: repo addon pathtoNewAddon
  processAddon :: FilePath -> String -> FilePath -> IO ()
  processAddon repo addon newpath = do
    fichero <- T.readFile (repo </> "ficheros.json")
    let ficheros = decode $ T.encodeUtf8 fichero :: Maybe [Ficheros]
    filtered <- filterM (\x -> pure $ (modFolder x) /= addon) $ fromJust ficheros
    removeModDirectory $ repo </> addon
    moveModDirectory newpath (repo </> addon)
    datos <- processAddon' repo addon
    let global = [filtered, datos]
    T.writeFile (repo </> "ficheros.json") (T.decodeUtf8 . encodePretty' (defConfig {confCompare=keyOrder ["Mod","Ruta","Nombre","Firma","Tamano"]}) $ F.concat global)


  -- repo must be a global path
  -- params: repo addon
  processAddon' :: FilePath -> String -> IO [Ficheros]
  processAddon' repo addon = do
    dirs <- readDir $ repo </> addon
    let lista = toList dirs
    let x = L.map (\x -> processFile addon repo x) lista
    sequence x

  -- params: addonName addonPath
  processFile :: String -> FilePath -> FilePath -> IO Ficheros
  processFile a re p = do
    h <- calculateXXHash p
    t <- getFileLength p
    let r = if equalFilePath (re </> a) (takeDirectory p) then
              "/"
            else
              makeRelative (re </> a) (takeDirectory p)

    let n = getFileName p

    return $ Ficheros a r n h t
