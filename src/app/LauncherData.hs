{-# LANGUAGE DeriveDataTypeable #-}

module LauncherData ( Launcher(..)) where

  import System.Environment
  import System.Console.CmdArgs
  import Data.Maybe

  data Launcher =   Deltas { new :: Maybe FilePath, old :: Maybe FilePath, diff :: Maybe FilePath }
                    | Hashes { src :: Maybe FilePath, dst :: Maybe FilePath, move :: Maybe Bool, defaultorder :: Maybe Bool, olddata :: Maybe Bool}
                    | SteamCmd { scmdpath :: Maybe FilePath, contjson :: Maybe FilePath, mpath :: Maybe FilePath }
                    | Complete
                    | GUI
                    deriving (Data,Typeable,Show,Eq)
