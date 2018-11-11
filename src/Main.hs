{-# LANGUAGE DeriveDataTypeable #-}

module Main where

  import System.Environment
  import System.Console.CmdArgs
  import DoceBDIExternalPrograms
  import Data.Text
  import System.Console.GetOpt
  import Data.Maybe ( fromMaybe )

  data Launcher =   Deltas { new :: Maybe FilePath, old :: Maybe FilePath, diff :: Maybe FilePath }
                    | Hashes { src :: Maybe FilePath, dst :: Maybe FilePath, move :: Maybe Bool, defaultorder :: Maybe Bool, olddata :: Maybe Bool}
                    | Complete
                    | GUI
                    deriving (Data,Typeable,Show,Eq)


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

  main :: IO ()
  main = do
    options <- cmdArgs (modes [complete, gui, deltas, hashes] &= help "Generador de repositorios de Arma 3" &= program "12bdi-launcher" &= summary "12BDI Launcher v1.0\nCross Platform")

    print options

    return ()
