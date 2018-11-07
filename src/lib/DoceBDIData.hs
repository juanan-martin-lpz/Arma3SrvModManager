{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DoceBDIData (
  Settings(..),
  Entry (..),
  Ficheros(..),
  Repositories(..),
  Servidores(..),
  Repositorios(..),
  RepoMod(..)
                    ) where

  import Data.Aeson
  import Prelude.Compat
  import Data.Maybe

  import Control.Applicative
  import Data.Semigroup ((<>))

  import qualified Data.ByteString as BS

  data Settings = DeltaSettings {  sourcePath :: String
                                  , destinyPath :: String
                                  , diffPath :: String }

  instance Show Settings where
    show (DeltaSettings s d dp) = show s ++ "---" ++ show d ++ "---" ++ show dp

  instance ToJSON Settings where
    toJSON DeltaSettings {..}     = object [ "sourcePath" .= sourcePath, "destinyPath" .= destinyPath, "diffPath" .= diffPath ]
    toEncoding DeltaSettings {..} = pairs $ "sourcePath" .= sourcePath <> "destinyPath" .= destinyPath <> "diffPath" .= diffPath

  instance FromJSON Settings where
    parseJSON (Object s) = DeltaSettings <$> s .: "sourcePath" <*> s .: "destinyPath" <*>  s .: "diffPath"
    parseJSON _          = empty

  -- Alias
  type FileRelativePath = String
  type FileName = String
  type FileLength = Integer
  type FileHash = BS.ByteString

  {-
    Un Entry define un elemento en el arbol de directorios
    Puede crearse con su constructor predeterminado
    La lista de elementos ha de ser creada antes que la creacion del directorio actual

  -}
  data Entry        = FileEntry { filename :: FileName, filelength :: FileLength, relativepath :: FileRelativePath, hash :: FileHash } |
                      DirectoryEntry { dirname ::String, entries :: Maybe [Entry] } |
                      EmptyEntry
                      deriving Show

  -- Ficheros de Intercambio entre Cliente y Servidor
  data Mod          = Mod { modName :: String, icon :: Maybe String }
  data Repositories = Repository { repoName :: String, mods :: [Mod], mustUpdate :: Bool }
  data Ficheros     = Ficheros { modFolder :: String, ruta :: String, rfilename :: String, firma :: String, tamano :: Integer}

  -- NO Aeson
  -- Lanzador
  data Servidores   = Servidor {version :: Int, nombreServer :: String, ip :: String, port :: Int, modList :: [ServerMod]}
  data ServerMod    = ServerMod { nMod :: String }
  data ModOrder     = ModOrder { nombreMod :: String }
  type ModItem      = String

  -- Nueva Version

  data Repositorios = Repositorios { repon :: String }
  data RepoMod      = RepoMod { repomodn :: String }
  -- ficheros mantiene la misma estructura


  -- Show Instancias

  instance Show Repositorios where
    show (Repositorios r) = show r

  instance Show Mod where
    show (Mod m _) = show m

  instance Show Repositories where
    show (Repository r _ _) = show r

  instance Show Ficheros where
    show (Ficheros m r f _ _) = show m ++ "/" ++ show r ++ "/" ++ show f

  instance Show Servidores where
    show (Servidor _ n _ p _) = show n ++ "( " ++ show p ++ " )"

  -- AESON Instancias

  instance ToJSON Mod where
    toJSON Mod {..}     = object [ "Nombre" .= modName, "Icon" .= icon ]
    toEncoding Mod {..} = pairs $ "Nombre" .= modName <> "Icon" .= icon

  instance FromJSON Mod where
    parseJSON (Object s) = Mod <$> s .: "Nombre" <*> s .: "Icon"
    parseJSON _          = empty


  instance ToJSON Ficheros where
    toJSON Ficheros {..}     = object [ "Mod" .= modFolder, "Ruta" .= ruta, "Nombre" .= rfilename, "Firma" .= firma, "Tamano" .= tamano ]
    toEncoding Ficheros {..} = pairs $ "Mod" .= modFolder <> "Ruta" .= ruta <> "Nombre" .= rfilename <> "Firma" .= firma <> "Tamano" .= tamano

  instance FromJSON Ficheros where
    parseJSON (Object s) = Ficheros <$> s .: "Mod" <*> s .: "Ruta" <*> s .: "Nombre" <*> s .: "Firma" <*> s .: "Tamano"
    parseJSON _          = empty

  instance ToJSON Repositories where
    toJSON Repository {..}     = object [ "Nombre" .= repoName, "Mods" .= mods, "MustUpdate" .= mustUpdate ]
    toEncoding Repository {..} = pairs $ "Nombre" .= repoName <> "Mods" .= mods <> "MustUpdate" .= mustUpdate

  instance FromJSON Repositories where
    parseJSON (Object s) = Repository <$> s .: "Nombre" <*> s .: "Mods" <*> s .: "MustUpdate"
    parseJSON _          = empty
