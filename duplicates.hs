import qualified System.Directory as D
import System.Posix.Files
import System.Posix.Types (FileOffset)
import System.FilePath.Posix
import System.Environment
import qualified Data.Map.Strict as Map
import GHC.Fingerprint
import Data.List


main = do
    [arg] <- getArgs
    allFiles <- getAllFiles arg
    dups <- findActualDups allFiles

    dupsWithSizes <- sequence $ map dupListWithSize dups

    putStrLn $ show $ reverse $ sortOn (\(size, _) -> size) dupsWithSizes


dupListWithSize :: [FilePath] -> IO (FileOffset, [FilePath])
dupListWithSize l = do
    status <- getFileStatus $ head l
    return (fileSize status, l)


findActualDups :: [FilePath] -> IO [[FilePath]]
findActualDups paths = do
    fileSizeList <- sequence $ map fileToSizeTuple paths
    let fileSizeMap = Map.fromListWith (\l1 l2 -> l1 ++ l2) [(k, [v]) | (k, v) <- fileSizeList]
    let possibleDups = filter ((> 1) . length) $ Map.elems fileSizeMap
    let allPossibleDups = concat possibleDups

    fileFingerprintList <- sequence $ map fileToFingerprintTuple allPossibleDups
    let fingerprintMap = Map.fromListWith (++) [(k, [v]) | (k, v) <- fileFingerprintList]
    return $ filter ((> 1) . length) $ Map.elems fingerprintMap


fileToFingerprintTuple :: FilePath -> IO (Fingerprint, FilePath)
fileToFingerprintTuple path = (,) <$> getFileHash path <*> return path


fileToSizeTuple :: FilePath -> IO (FileOffset, FilePath)
fileToSizeTuple path = (,) <$> fileSize <$> getFileStatus path <*> return path


getAllFiles :: FilePath -> IO [FilePath]
getAllFiles d = do
    status <- getSymbolicLinkStatus d
    isSymbolic <- D.pathIsSymbolicLink d
    if isSymbolicLink status || isSymbolic
       then return []
    else if isRegularFile status 
       then return [d]
    else if isDirectory status
       then do
           listed <- D.listDirectory d
           foldr lambda (return []) [d </> name | name <- listed]
    else return []
        where lambda file acc = (++) <$> getAllFiles file <*> acc

