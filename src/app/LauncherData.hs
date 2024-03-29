{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}



module LauncherData ( Launcher(..)) where

  import System.Environment
  import System.Console.CmdArgs
  import Data.Maybe

  data Launcher =   Deltas { new :: Maybe FilePath, old :: Maybe FilePath, diff :: Maybe FilePath }
                    | Hashes { dst :: Maybe FilePath, ado :: Maybe String, move :: Maybe Bool, defaultorder :: Maybe Bool }
                    | InsertAddon { src :: Maybe FilePath, dst :: Maybe FilePath, ado :: Maybe String}
                    | SteamCmd { scmdpath :: Maybe FilePath, contjson :: Maybe FilePath, mpath :: Maybe FilePath, usr :: Maybe String, pass :: Maybe String }
                    | Complete
                    | GUI
                    deriving (Data,Typeable,Show,Eq)
