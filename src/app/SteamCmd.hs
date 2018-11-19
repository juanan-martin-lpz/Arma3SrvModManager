
module SteamCmd ( readSteamWorkshopLocalConfig,
                  createScript ) where

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
                  m = modspath $ fromJust env
                  u = user $ fromJust env
                  p = pwd $ fromJust env
             in
                  SteamCmd { scmdpath = Just s, contjson = Just c, mpath = Just m, usr = Just u, pass = Just p  }

  headerScript :: String -> String -> String -> IO String
  headerScript usr pwd ipath = return $ "@NoPromptForPassword 1\n" <> "login " <> usr <> " " <> pwd <> "\n" <> "force_install_dir " <> ipath

  createScript :: Launcher -> IO String
  createScript l = do

    header <- let u = fromJust $ usr l
                  p = fromJust $ pass l
                  ipath = fromJust $ mpath l
              in
                headerScript u p ipath

    body <- bodyScript l
    footer <- footerScript

    return (header <> body <> footer)

  footerScript :: IO String
  footerScript = return $ "logout\n" <> "quit\n"


  bodyScript :: Launcher -> IO String
  bodyScript cfg = do
    let baseScript = readFile "./script.txt"
    raw <- readJSON $ fromJust $ contjson cfg
    content <- parseContentsJson raw
    return $ Prelude.concat $ [ (" workshop_download_item 107410 " <> modId m <> "\n") | m <- (fromJust content) ]
