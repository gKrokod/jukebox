module DataBase where
import Control.Exception (SomeException, displayException, try)
import Data.Time (addUTCTime )
import Handlers.Engine (Track(..), Library)
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath ( (</>), takeExtension )
import System.OsPath (encodeUtf)
import Monatone.Common  (parseMetadata)
import Monatone.Metadata  (Metadata(..), AudioProperties (duration))
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode, eitherDecode)
import Data.Maybe ( fromMaybe )
import qualified Data.Map.Strict as Map
import Data.Time ( getCurrentTime ) 
import Control.Monad (filterM)
import Control.Concurrent.STM
    (  newTVarIO,  TVar )
import qualified Data.Text as T

initLibrary :: FilePath -> FilePath -> IO (TVar Library)
initLibrary dir file = do
  library <- migration dir file
  cleanUpLibrary <- Map.fromList <$> filterM (doesFileExist . T.unpack . fst) (Map.toList  library)
  newTVarIO cleanUpLibrary
--
parseTrack :: FilePath -> IO (Track)
parseTrack file = do
  time <- Data.Time.getCurrentTime
  osPath <- encodeUtf file
  metadata <- parseMetadata osPath
  case metadata of
    Left _ -> error "parse error"
    Right md -> pure $ Track { 
      path = T.pack file,
      duration = fromIntegral $ fromMaybe 0 md.audioProperties.duration,
      interval = 0,
      count = 0,
      lastPlay = Nothing,
      planPlay = Just $ addUTCTime 86400 time} -- tomorrow for new track
--
migration :: FilePath -> FilePath -> IO Library
migration dir file = do
  eFileDB <- loadFromFileDB dir 
  dirDB <- loadFromDir dir
  case eFileDB of
    Left _ -> BL.writeFile file (encode dirDB) >> pure dirDB
    Right fileDB -> do
      pure $ Map.union fileDB dirDB
--
-- *****
loadFromDir :: FilePath -> IO Library
loadFromDir dir = do
  -- Получаем список всех элементов в текущей директории
  items <- listDirectory dir

  -- Превращаем их в полные пути
  let fullPaths = map (dir </>) items

  -- Разделяем пути на папки и файлы
  subDirs <- filterM doesDirectoryExist fullPaths
  let files = filter (`notElem` subDirs) fullPaths

  -- 1. Обрабатываем MP3 файлы в текущей директории
  let mp3s = filter (\f -> takeExtension f `elem` [".mp3",".flac",".wav",".ogg"]) files
  tracks <- mapM parseTrack mp3s
  let currentMap = Map.fromList (zip (map T.pack mp3s) tracks)

  -- 2. Рекурсивно заходим во все подпапки
  subMaps <- mapM loadFromDir subDirs

  -- 3. Объединяем карту текущей папки со всеми картами подпапок
  pure (Map.unions (currentMap : subMaps))

-- *****

loadFromFileDB :: FilePath -> IO (Either String Library)
loadFromFileDB path =
  either (Left . displayException) eitherDecode
    <$> try @SomeException (BL.readFile (path <> "/jukebox.json"))
