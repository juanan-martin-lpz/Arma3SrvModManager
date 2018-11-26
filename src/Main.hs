{-# LANGUAGE DeriveDataTypeable #-}

module Main where

  import System.Environment
  import System.Console.CmdArgs
  import Data.Text
  import System.Console.GetOpt
  import Control.Monad.Reader
  import Data.Maybe
  import System.IO
  import Control.Concurrent
  import System.Directory
  import System.Exit
  
  import LauncherData
  import DoceBDIFileWork
  import DoceBDIData
  import DoceBDIFileOperations
  import DoceBDIExternalPrograms
  import SteamCmd


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


  processSteamCmd :: SteamWorkshop -> IO ()
  processSteamCmd cfg = do
    jsonfile <- makeAbsolute $ contentsjson cfg
    existjson <- doesFileExist jsonfile
    if existjson
      then sequence_ []
      else hPutStrLn stdout "Error:\nEl fichero de contenido no se encuentra en la ruta especificada" >> exitFailure

    script <- createScript cfg
    fsc <- writeToTmp script
    steamcommand <- makeAbsolute $ steamcmdpath cfg <> "/steamcmd.exe"
    existcmd <- doesFileExist steamcommand
    if existcmd
      then sequence_ []
      else hPutStrLn stdout "Error:\nSteamCmd.exe no se encuentra en la ruta especificada" >> exitFailure


    let steam = steamcommand <> " + api_logging 'verbose' +runscript " <> fsc
    execProgram $ pack steam
    removeIfExists fsc
    threadDelay 1500000
    publishRepo cfg


  -- Rutina principal

  director :: Launcher -> IO ()

  director (GUI) = undefined
  director (Complete) = undefined
  director (Hashes _ _ _ _ _) = undefined
  director (Deltas _ _ _) = undefined

  director (SteamCmd Nothing Nothing Nothing Nothing Nothing) = do
    cfg <- readSteamWorkshopLocalConfig "./steamws.json"
    processSteamCmd cfg

  director (SteamCmd s c m u p) = do
    cfg <- readSteamWorkshopLocalConfig "./steamws.json"

    let cmd = case s of
                Nothing   -> steamcmdpath cfg
                Just (s)  -> s

    let con = case c of
                Nothing   -> contentsjson cfg
                Just (c)  -> c


    let mpa = case m of
                Nothing   -> modspath cfg
                Just (m)  -> m

    let us = case u of
                Nothing   -> user cfg
                Just (u)  -> u

    let pa = case p of
                Nothing   -> pwd cfg
                Just (p)  -> p

    let newconfig = SteamWorkshop { steamcmdpath = cmd, contentsjson = con, modspath = mpa, user = us, pwd = pa }

    print newconfig

    processSteamCmd newconfig
  main :: IO ()
  main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

    options <- cmdArgs (modes [complete, gui, deltas, hashes, steamwork] &= help "Generador de repositorios de Arma 3" &= program "12bdi-launcher" &= summary "12BDI Launcher v1.0\nCross Platform Multitool")

    director options
