module DoceBDIFileWork (readJSON,
                        parseFicherosJson,
                        parseRepositoriesJson,
                        readServidores2Txt) where

  import DoceBDIData
  import System.IO as F
  import Control.Exception

  import Data.Aeson
  import Data.Maybe
  import Data.ByteString.Lazy.Char8 as BS
  import Data.Text

  readJSON :: String -> IO ByteString
  readJSON fname = do BS.readFile fname

  parseFicherosJson:: ByteString -> IO (Maybe [Ficheros])
  parseFicherosJson content = do
    let setm = decode content    -- setm = Maybe Ficheros || Nothing
    return setm      -- return IO (Maybe Ficheros)


  parseRepositoriesJson:: ByteString -> IO (Maybe [Repositories])
  parseRepositoriesJson content = do
    let setm = decode content    -- setm = Maybe Ficheros || Nothing
    return setm      -- return IO (Maybe Ficheros)

  readServidores2Txt :: String -> IO String
  readServidores2Txt fname = do
    let content = F.readFile fname
    content

  readModOrder :: String -> [ModOrder]
  readModOrder mo = do
    let content = F.readFile mo
    let rows = lines content
    ModOrder [ rows | r <- rows]
    
  processMod :: String -> [String]
  processMod m | wordsWhen (==':') =

  processMod m | m!!0 == '%' =
    let mod = [ m | c <- m , c /= '%']
    -- Cargar modorder.txt


  processServidorMods :: String -> [ServerMod]
  processServidorMods m =
    -- Para cada elemento retornado si empieza por % es un repo entero
    -- y hay que leer su ModOrder
    -- Si contiene : hay que leer ese ModOrder
    ServerMod [x = wordsWhen (==';') m | processMod ]


  createServidor :: [String] -> Servidores
  createServidor (v:n:i:p:m:[]) =

    Servidor (read v) n i (read p) $ processServidorMods m

  parseServidor :: String -> Servidores
  parseServidor s =
    createServidor $ wordsWhen (=='|') s


  -- From StackOverflow (https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell)

  wordsWhen     :: (Char -> Bool) -> String -> [String]
  wordsWhen p s =  case Prelude.dropWhile p s of
                        "" -> []
                        s' -> w : wordsWhen p s''
                              where (w, s'') = Prelude.break p s'
  -- createWorkingTree :: String -> IO Entry
  -- createWorkingTree startDir =
    -- Leer la estructura de la carpeta
    -- Por cada DirectoryEntry
        -- Entrar e iniciar proceso
    -- Crear Entries
