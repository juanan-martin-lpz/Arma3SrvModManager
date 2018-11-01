{-# LANGUAGE OverloadedStrings #-}

module DoceBDIConfig (readJSONFile,
                      parseConfig,
                      getSourcePath,
                      getDestinyPath,
                      getDiffPath ) where

  import DoceBDIData
  import System.IO as F
  import Control.Exception
  import Data.Aeson
  import Data.Maybe
  import Data.ByteString.Lazy.Char8 as BS

  readJSONFile :: String -> IO ByteString
  readJSONFile fname = do BS.readFile fname
  -- readJSONFile fname = do F.readFile fname

  parseConfig:: ByteString -> IO (Maybe Settings)
  parseConfig config = do
    let setm = decode config    -- setm = Maybe Settings || Nothing
    return setm      -- return IO (Maybe Settings)
    {-
    let content = BS.pack config
    let setm = decode content :: Maybe Settings
    return (fromJust setm)
    -}

  getSourcePath :: Settings -> String
  getSourcePath s = do
    sp <- sourcePath s
    return sp

  getDestinyPath :: Settings -> String
  getDestinyPath d = do
    dp <- destinyPath d
    return dp

  getDiffPath :: Settings -> String
  getDiffPath d = do
    dp <- diffPath d
    return dp
