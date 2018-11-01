module DoceBDIExternalPrograms ( execProgram ) where

  import System.Process as P
  import System.IO
  import Data.Text

  execProgram :: Text -> IO Int
  execProgram program =
    do
      createProcess (P.shell (unpack program)) --{ std_out = CreatePipe }
      return 0
