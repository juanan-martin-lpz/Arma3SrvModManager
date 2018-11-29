
module SteamCmd ( readSteamWorkshopLocalConfig,
                  createScript,
                  publishRepo ) where


  import Control.Monad.Reader
  import Data.Maybe
  import Data.Text
  import System.FilePath
  import System.Exit
  import System.IO
  import System.Directory

  import LauncherData
  import DoceBDIExternalPrograms
  import DoceBDIFileOperations
  import DoceBDIFileWork
  import DoceBDIData

  readSteamWorkshopLocalConfig :: String -> IO SteamWorkshop
  readSteamWorkshopLocalConfig cfg = do
    env <- (readJSON cfg >>= parseSteamCmdJson)

    if isNothing env
      then  hPutStrLn stdout "Error:\nSe necesita el fichero steamws.json en la misma carpeta que el ejecutable" >> exitFailure
    else
      sequence_ []

    return $ let s = steamcmdpath $ fromJust env
                 c = contentsjson $ fromJust env
                 m = modspath $ fromJust env
                 u = user $ fromJust env
                 p = pwd $ fromJust env
             in
                SteamWorkshop { steamcmdpath = s, contentsjson = c, modspath = m, user = u, pwd = p }

  headerScript :: String -> String -> String -> IO String
  headerScript usr pwd ipath = return $ "@NoPromptForPassword 1\n" <> "login " <> usr <> " " <> pwd <> "\n" <> "force_install_dir " <> ipath <> "\n"

  createScript :: SteamWorkshop -> IO String
  createScript l = do

    header <- let u = user l
                  p = pwd l
                  ipath = modspath l
              in
                headerScript u p ipath

    body <- bodyScript l
    footer <- footerScript

    return (header <> body <> footer)

  footerScript :: IO String
  footerScript = return $ "logout\n" <> "quit\n"


  bodyScript :: SteamWorkshop -> IO String
  bodyScript cfg = do
    let baseScript = readFile "./script.txt"
    raw <- readJSON $ contentsjson cfg
    content <- parseContentsJson raw
    case content of
      Nothing -> return "\n"      -- Si no hay mods retornamos un linea vacia
      Just (c) -> return $ Prelude.concat $ [ ("workshop_download_item 107410 " <> modId m <> "\n") | m <- c ]

  publishRepo :: SteamWorkshop -> IO ()
  publishRepo config = do
    let path = contentsjson config
    mods <- (readJSON path >>= parseContentsJson)
    let modpath = modspath config
    let base = modpath </> "steamapps/workshop/content/107410/"
    let handleMods b m (x:xs) = processMod m b x >> handleMods b m xs
        handleMods _ _ [] = return ()
      in
        handleMods base modpath (fromJust mods)

    removeOld modpath

  processMod :: FilePath -> FilePath -> ContentsJson -> IO ()
  processMod path base mx = do
    let str = "Moviendo a carpeta " <> (path </> repositorio mx </> carpeta mx)
    putStrLn str
    createModDirectory $ path </> repositorio mx

    let m = modId mx
        r = repositorio mx </> carpeta mx
      in
          moveModDirectory (base </> m) (path </> r)


  removeOld :: FilePath -> IO ()
  removeOld modpath = do
    let base = modpath </> "steamapps"
    removeModDirectory base
