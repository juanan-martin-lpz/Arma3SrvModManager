module DoceBDIFileWork (readJSON,
                        parseFicherosJson,
                        parseRepositoriesJson,
                        readSteamWorkshopLocalConfig,
                        readRepositorios,
                        readRepomods,
                        writeRepositorios,
                        writeRepomods,
                        parseDeltaJson,
                        parseGeneratorJson,
                        parseSteamCmdJson,
                        parseContentsJson,
                        writeToTmp,
                        readFileLazy,
                        printTo,
                        printLine,
                        printText,
                        readFileStrict) where

  import System.IO as F
  import Control.Exception
  import System.Directory
  import System.Exit
  import Data.Aeson
  import Data.Maybe
  import qualified Data.ByteString.Lazy.Char8 as BS
  import qualified Data.ByteString.Char8 as BSS
  import qualified Data.Text as T
  import System.IO.Error hiding (catch)
  import DoceBDIData


  readJSON :: String -> IO BS.ByteString
  readJSON fname = do BS.readFile fname `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return BS.empty
            | otherwise = throwIO e


  readSteamWorkshopLocalConfig :: String -> IO SteamWorkshop
  readSteamWorkshopLocalConfig cfg = do
    env <- (readJSON cfg >>= parseSteamCmdJson)

    if isNothing env
      then  F.hPutStrLn stdout "Error:\nSe necesita el fichero steamws.json en la misma carpeta que el ejecutable" >> exitFailure
    else
      sequence_ []

    return $ let s = steamcmdpath $ fromJust env
                 c = contentsjson $ fromJust env
                 m = modspath $ fromJust env
                 u = user $ fromJust env
                 p = pwd $ fromJust env
             in
                SteamWorkshop { steamcmdpath = s, contentsjson = c, modspath = m, user = u, pwd = p }


  parseFicherosJson:: BS.ByteString -> IO (Maybe [Ficheros])
  parseFicherosJson content | content == BS.empty = return (Just [])
                            | otherwise = do
    let setm = decode content    -- setm = Maybe Ficheros || Nothing
    return setm      -- return IO (Maybe Ficheros)


  parseRepositoriesJson:: BS.ByteString -> IO (Maybe [Repository])
  parseRepositoriesJson content | content == BS.empty = return (Just [])
                                | otherwise = do
    let setm = decode content    -- setm = Maybe Ficheros || Nothing
    return setm      -- return IO (Maybe Ficheros)

  parseGeneratorJson:: BS.ByteString -> IO (Maybe GeneratorSettings)
  parseGeneratorJson content | content == BS.empty = return Nothing
                             | otherwise = do
    let setm = decode content    -- setm = Maybe Ficheros || Nothing
    return setm      -- return IO (Maybe Ficheros)

  parseDeltaJson:: BS.ByteString -> IO (Maybe Settings)
  parseDeltaJson content | content == BS.empty = return Nothing
                         | otherwise = do
    let setm = decode content    -- setm = Maybe Ficheros || Nothing
    return setm      -- return IO (Maybe Ficheros)

  parseSteamCmdJson:: BS.ByteString -> IO (Maybe SteamWorkshop)
  parseSteamCmdJson content | content == BS.empty = return Nothing
                            | otherwise = do
    let setm = decode content    -- setm = Maybe Ficheros || Nothing
    return setm      -- return IO (Maybe Ficheros)

  parseContentsJson:: BS.ByteString -> IO (Maybe [ContentsJson])
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

  writeRepomods :: String -> [RepoMod] -> IO ()
  writeRepomods fname repos = do
    -- r <- repos
    let cs = [repomodn x | x <- repos]
    writeIdx fname cs

  writeToTmp :: String -> IO FilePath
  writeToTmp content = do
    dir <- getTemporaryDirectory
    file <- F.openTempFile dir "script.txt"
    let l = Prelude.length content
    F.hPutStr (snd file) content
    F.hClose $ snd file
    return $ fst file

  readFileLazy :: FilePath -> IO BS.ByteString
  readFileLazy fname = do
    catch (do
      BS.readFile fname )
      (\e -> do
             let err = show (e :: IOException)
             printLine ("Error: No se puede abrir el archivo de forma perezosa -> " ++ fname ++ ": " ++ err)
             return BS.empty )

  readFileStrict :: FilePath -> IO BSS.ByteString
  readFileStrict fname = do
    bracket(openBinaryFile fname ReadMode)
            hClose
            (\h -> do
              hSetBinaryMode h True
              content <- BSS.hGetContents h
              return content
              `catch` (\e -> do
                          let err = show (e :: IOException)
                          printLine ("Error al leer el archivo :" ++ fname ++ " : " ++ err)
                          return BSS.empty)
            )
    {-
    BS.readFile fname `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return BS.empty
            | otherwise = throwIO e
    -}


  printTo :: Handle -> String -> Bool -> IO ()
  printTo handle content endl = do
    F.hPutStr handle content
    F.hFlush handle

    if endl then
      F.hPutStr handle "\n"
    else
      return ()

  printLine :: String -> IO ()
  printLine content = do
    printTo stdout content True

  printText :: String -> IO ()
  printText content = do
    printTo stdout content False

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
