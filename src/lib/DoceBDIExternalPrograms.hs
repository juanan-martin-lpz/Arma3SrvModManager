module DoceBDIExternalPrograms ( execProgram ) where

  import System.Process
  import System.Exit
  import System.IO
  import Data.Text
  import Control.Exception
  import System.IO.Error hiding (catch)

  execProgram :: Text -> IO ExitCode
  execProgram program =
    do
      (_, _, _, pHandle) <- createProcess (shell (unpack program))  --{ std_err = CreatePipe }

      waitForProcess pHandle
