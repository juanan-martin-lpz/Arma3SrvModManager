module DoceBDIExternalPrograms ( execProgram ) where

  import System.Process
  import System.Exit
  import System.IO
  import Data.Text

  execProgram :: Text -> IO ExitCode
  execProgram program =
    do
      (_, _, _, pHandle) <- createProcess (shell (unpack program)) --{ std_out = CreatePipe }
      waitForProcess pHandle
