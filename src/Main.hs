module Main where

import DoceBDIExternalPrograms
import Data.Text

main :: IO ()
main = do putStrLn "Executing SteamCmd!!"
          execProgram (pack "C:/Users/the_b/Downloads/steamcmd/steamcmd.exe +login 12bdi ak74-pso +force_install_dir G:/JUEGOS/Servidores/Minimo +app_update 233780")
          putStrLn "Done!!!!"
          return ()
