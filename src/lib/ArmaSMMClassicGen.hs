{-# LANGUAGE OverloadedStrings #-}

module ArmaSMMClassicGen ( processRepository,
                           processRepositories,
                           processSingleAddon,
                           generateRepositories) where

  import Data.Digest.XXHash.FFI
  import ArmaSMMFileWork
  import ArmaSMMFileOperations
  import ArmaSMMData
  import Data.Word
  import qualified Codec.Base64 as C
  import Data.ByteString.Lazy.Builder
  import Data.Text as T
  import qualified Data.Text.Lazy.IO as T
  import qualified Data.Text.Lazy.Encoding as T
  import Data.Aeson
  import Data.Aeson.Encode.Pretty
  import System.PosixCompat.Files
  import System.FilePath
  import System.IO
  import Data.List as L
  import System.Directory.Tree as T
  import Data.Foldable as F
  import System.Directory
  import Control.Monad
  import Control.Exception
  import qualified Data.ByteString.Lazy as BS
  import Data.Maybe


  calculateXXHash :: FilePath -> IO String
  calculateXXHash path = do
    catch (do
      content <- readFileStrict path
      let hash = xxh64 content 0
      let hashLE = word64LE hash
      return . T.unpack $ (C.encode :: Builder -> T.Text) hashLE )
      (\e -> do
             let err = show (e :: IOException)
             hPutStr stdout ("Error: No se puede calcular hash -> " ++ path ++ ": " ++ err)
             return "" )


  getFileLength :: FilePath -> IO Integer
  getFileLength entry = do
   catch (do
     stat <- getFileStatus entry
     let size = fileSize stat
     return $ fromIntegral size)
     (\e -> do
            let err = show (e :: IOException)
            hPutStr stdout ("Error: No se puede obtener el tamano del fichero -> " ++ entry ++ ": " ++ err)
            return 0 )

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
    writeRepomods (base </> "modlist.txt") $ L.nub un

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
  processSingleAddon :: FilePath -> String -> FilePath -> IO ()
  processSingleAddon newpath addon repo = do
    fichero <- T.readFile (repo </> "ficheros.json")
    let ficheros = decode $ T.encodeUtf8 fichero :: Maybe [Ficheros]
    filtered <- filterM (\x -> pure $ (modFolder x) /= addon) $ fromJust ficheros

    r' <- canonicalizePath $ repo </> addon
    n' <- canonicalizePath newpath

    de <- doesDirectoryExist r'

    if de then
      removeModDirectory r'
    else
      return ()

    copyModDirectory n' r'

    exist <- doesDirectoryExist $ repo </> addon

    if exist then
      removeModDirectory $ repo </> addon
    else
      return ()

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
    p' <- canonicalizePath p
    re' <- canonicalizePath re

    h <- calculateXXHash p'
    t <- getFileLength p'

    let r = if equalFilePath (re' </> a) (takeDirectory p) then
              "/"
            else
              makeRelative (re' </> a) (takeDirectory p)

    let n = getFileName p'

    return $ Ficheros a r n h t

  -- repo must be a global path
  -- params: repo addon pathtoNewAddon
  removeAddon :: FilePath -> String -> IO ()
  removeAddon repo addon = do
    fichero <- T.readFile (repo </> "ficheros.json")
    let ficheros = decode $ T.encodeUtf8 fichero :: Maybe [Ficheros]
    filtered <- filterM (\x -> pure $ (modFolder x) /= addon) $ fromJust ficheros
    removeModDirectory $ repo </> addon
    T.writeFile (repo </> "ficheros.json") (T.decodeUtf8 . encodePretty' (defConfig {confCompare=keyOrder ["Mod","Ruta","Nombre","Firma","Tamano"]}) $ filtered)
