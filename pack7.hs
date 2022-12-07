import System.IO  
import System.Directory
import Data.Text
import qualified Data.ByteString.Lazy as B
-- Pack 7. IO

-- task 1 Sanity Check
printFile :: String -> IO ()
printFile fileToPrint = do
  handle <- openFile fileToPrint ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

-- task 2 Text Check
areEqualText :: FilePath -> FilePath -> IO (Bool)
areEqualText file1 file2 = do
  f1 <- openFile file1 ReadMode
  f2 <- openFile file2 ReadMode
  c1 <- hGetContents f1
  c2 <- hGetContents f2
  hClose f1
  hClose f2
  return (c1 == c2)

-- task 3 Dos2Unix
dos2unix :: String -> IO ()
dos2unix  str = do
  fin <- openFile str ReadMode
  content <- hGetContents fin
  hClose fin
  fout <-openFile str WriteMode
  hPutStr fout (unpack (replace (pack str) (pack "\r\n") (pack "\n")))
  hClose fout


unix2dos :: String -> IO ()
unix2dos str = do
  fin <- openFile str ReadMode
  content <- hGetContents fin
  hClose fin
  fout <-openFile str WriteMode
  hPutStr fout (unpack (replace (pack str) (pack "\n") (pack "\r\n")))
  hClose fout

-- task 4 Binary check
areEqualBin :: FilePath -> FilePath -> IO (Bool)
areEqualBin file1 file2 = do
  f1 <- openBinaryFile file1 ReadMode
  f2 <- openBinaryFile file2 ReadMode
  c1 <- B.hGetContents f1
  c2 <- B.hGetContents f2
  hClose f1
  hClose f2
  return (c1 == c2)

-- task 5 No Vimmers?
fileIsBeingEdited :: FilePath -> IO (Bool)
fileIsBeingEdited file = doesFileExist (file ++ ".sw")
