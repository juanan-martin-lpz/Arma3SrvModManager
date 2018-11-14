
module SteamCmd ( readSteamWorkshopLocalConfig,
                  makeDownloadScript ) where

  import LauncherData
  import DoceBDIExternalPrograms
  import Control.Monad.Reader
  import DoceBDIFileWork
  import DoceBDIData
  import Data.Maybe
  import Data.Text


  readSteamWorkshopLocalConfig :: String -> IO Launcher
  readSteamWorkshopLocalConfig cfg = do
    env <- (readJSON cfg >>= parseSteamCmdJson)
    return $ let  s = steamcmdpath $ fromJust env
                  c = contentsjson $ fromJust env
                  m = modspath $ fromJust env in
                  SteamCmd { scmdpath = Just s, contjson = Just c, mpath = Just m } where

  makeDownloadScript :: Launcher -> IO String
  makeDownloadScript cfg = do
    let baseScript = readFile "./script.txt"
    raw <- readJSON $ fromJust $ contjson cfg
    content <- parseContentsJson raw
    return $ Prelude.concat $ [ (" workshop_download_item 107410 " <> modId m) | m <- (fromJust content) ]
