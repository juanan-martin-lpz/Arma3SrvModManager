{-# LANGUAGE DeriveDataTypeable #-}

module Main where

  import System.Environment
  import System.Console.CmdArgs
  import DoceBDIExternalPrograms
  import Data.Text
  import System.Console.GetOpt
  import Control.Monad.Reader
  import DoceBDIFileWork
  import DoceBDIData
  import DoceBDIFileOperations
  import Data.Maybe
  import LauncherData
  import SteamCmd
  import System.IO


  complete = Complete &= help "Ejecuta todas las tareas"

  gui = GUI &= help "Abre el programa en modo grafico"

  deltas = Deltas
      { new = def &= help "Directorio Base" &= typDir
       ,old = def &= help "Directorio Antiguo" &= typDir
       ,diff = def &= help "Directorio Resultados" &= typDir
      } &= help "Crea los ficheros Diff entre Base (mas nuevo) y Antiguo (anterior)"

  hashes = Hashes
      { src = def &= help "Directorio de los Addons" &= typDir
       ,dst = def &= help "Directorio destino para Copia final" &= typDir
       ,move = def &= help "Mover en lugar de copiar"
       ,defaultorder = def &= help "Generar modorder.txt con orden por defecto"
       ,olddata = def &= help "Generar datos adicionales para el Lanzador antiguo"
      } &= help "Calcula las firmas y genera los ficheros necesarios para ello"

  steamwork = SteamCmd
      { scmdpath = def &= help "Path de SteamCmd" &= typDir
       ,contjson = def &= help "Path del fichero de descargas" &= typDir
       ,mpath = def &= help "Path para copia final de los mods" &= typDir
       ,usr = def &= help "Usuario de Steam" &= typDir
       ,pass = def &= help "Pass de Steam" &= typDir
      } &= help "Descarga de Steam Workshop los addons especificados"



{-
  readSteamWorkshopLocalConfig :: String -> IO Launcher
  readSteamWorkshopLocalConfig cfg = do
    env <- (readJSON cfg >>= parseSteamCmdJson)
    return $ let  s = steamcmdpath $ fromJust env
                  c = contentsjson $ fromJust env
                  m = modspath $ fromJust env in
                  SteamCmd { scmdpath = Just s, contjson = Just c, mpath = Just m } where
-}

  -- Rutina principal

  director :: Launcher -> IO ()

  director (GUI) = undefined
  director (Complete) = undefined
  director (Hashes _ _ _ _ _) = undefined
  director (Deltas _ _ _) = undefined

  director (SteamCmd Nothing Nothing Nothing Nothing Nothing) = do
    cfg <- readSteamWorkshopLocalConfig "./steamws.json"
    script <- createScript cfg
    fsc <- writeToTmp script
    let steam = steamcmdpath cfg <> "/steamcmd.exe " <> "+runscript " <> fsc
    execProgram $ pack steam
    removeIfExists fsc
    return ()
  director (SteamCmd s c m u p) = do
    -- cfg
    --doJob cfg
    return ()

  main :: IO ()
  main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

    options <- cmdArgs (modes [complete, gui, deltas, hashes, steamwork] &= help "Generador de repositorios de Arma 3" &= program "12bdi-launcher" &= summary "12BDI Launcher v1.0\nCross Platform Multitool")

    director options
