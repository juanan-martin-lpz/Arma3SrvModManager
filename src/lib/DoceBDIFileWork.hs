module DoceBDIFileWork (readJSON,
                        parseFicherosJson,
                        parseRepositoriesJson,
                        readRepositorios,
                        readRepomods,
                        writeRepositorios,
                        writeRepomods,
                        parseDeltaJson,
                        parseGeneratorJson,
                        parseSteamCmdJson,
                        parseContentsJson,
                        writeToTmp) where

  import System.IO as F
  import Control.Exception
  import System.Directory
  import Data.Aeson
  import Data.Maybe
  import Data.ByteString.Lazy.Char8 as BS
  import Data.Text
  import System.IO.Error hiding (catch)

  import DoceBDIData
  

  readJSON :: String -> IO ByteString
  readJSON fname = do BS.readFile fname `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return BS.empty
            | otherwise = throwIO e

  parseFicherosJson:: ByteString -> IO (Maybe [Ficheros])
  parseFicherosJson content | content == BS.empty = return (Just [])
                            | otherwise = do
    let setm = decode content    -- setm = Maybe Ficheros || Nothing
    return setm      -- return IO (Maybe Ficheros)


  parseRepositoriesJson:: ByteString -> IO (Maybe [Repositories])
  parseRepositoriesJson content | content == BS.empty = return (Just [])
                                | otherwise = do
    let setm = decode content    -- setm = Maybe Ficheros || Nothing
    return setm      -- return IO (Maybe Ficheros)

  parseGeneratorJson:: ByteString -> IO (Maybe GeneratorSettings)
  parseGeneratorJson content | content == BS.empty = return Nothing
                             | otherwise = do
    let setm = decode content    -- setm = Maybe Ficheros || Nothing
    return setm      -- return IO (Maybe Ficheros)

  parseDeltaJson:: ByteString -> IO (Maybe Settings)
  parseDeltaJson content | content == BS.empty = return Nothing
                         | otherwise = do
    let setm = decode content    -- setm = Maybe Ficheros || Nothing
    return setm      -- return IO (Maybe Ficheros)

  parseSteamCmdJson:: ByteString -> IO (Maybe SteamWorkshop)
  parseSteamCmdJson content | content == BS.empty = return Nothing
                            | otherwise = do
    let setm = decode content    -- setm = Maybe Ficheros || Nothing
    return setm      -- return IO (Maybe Ficheros)

  parseContentsJson:: ByteString -> IO (Maybe [ContentsJson])
  parseContentsJson content | content == BS.empty = return (Just [])
                            | otherwise = do
    let setm = decode content    -- setm = Maybe Ficheros || Nothing
    return setm      -- return IO (Maybe Ficheros)


  readIdx :: String -> IO [String]
  readIdx fname = do
    let content = F.readFile fname
    l <- content
    let c = Prelude.lines l
    return c

  writeIdx :: String -> [String] -> IO ()
  writeIdx fname contents = do
    let cs = Prelude.concat $ [x ++ "\n" | x <- contents]
    F.writeFile fname cs

  readRepositorios :: String -> IO [Repositorios]
  readRepositorios fname = do
    c <- readIdx fname
    let r = [ Repositorios s | s <- c]
    return r

  writeRepositorios :: String -> IO [Repositorios] -> IO ()
  writeRepositorios fname repos = do
    r <- repos
    let cs = [repon x | x <- r]
    writeIdx fname cs

  readRepomods :: String -> IO [RepoMod]
  readRepomods fname = do
    c <- readIdx fname
    let r = [ RepoMod s | s <- c]
    return r

  writeRepomods :: String -> IO [RepoMod] -> IO ()
  writeRepomods fname repos = do
    r <- repos
    let cs = [repomodn x | x <- r]
    writeIdx fname cs

  writeToTmp :: String -> IO FilePath
  writeToTmp content = do
    dir <- getTemporaryDirectory
    file <- F.openTempFile dir "script.txt"
    let l = Prelude.length content
    F.hPutStr (snd file) content
    F.hClose $ snd file
    return $ fst file

  {-
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
<<<<<<< HEAD
  processMod m | m!!0 == '%' =
  processMod m |
=======
  processMod m | wordsWhen (==':') =

  processMod m | m!!0 == '%' =
    let mod = [ ModOrder m | c <- m , c /= '%']
    -- Cargar modorder.txt

>>>>>>> 31ce96a41c5143dd18d45ade053f200d2a16de3a

  processServidorMods :: String -> [ServerMod]
  processServidorMods m =
    -- Para cada elemento retornado si empieza por % es un repo entero
    -- y hay que leer su ModOrder
    -- Si contiene : hay que leer ese ModOrder
    let mods = wordsWhen (==';') m

    let pmods = [processMod x | x <- mods]

    ServerMod [x = wordsWhen (==';') m | processMod ]


  createServidor :: [String] -> Servidores
  createServidor (v:n:i:p:m:[]) =

    Servidor (read v) n i (read p) $ processServidorMods m

  parseServidor :: String -> Servidores
  parseServidor s =
    createServidor $ wordsWhen (=='|') s
  -}

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
