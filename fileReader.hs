module FileReader where
import Connect4
import Data.Char (isDigit)

data MixedFileContent = MixedFileContent {
    loadedBoard :: Board,
    moveList :: [Int]
} deriving (Show)


charToField :: Char -> Field
charToField 'Z' = Taken Z
charToField 'C' = Taken C
charToField _ = Empty

parseMixedFile :: String -> MixedFileContent
parseMixedFile content =
    let (tableLines, numberLines) = break (all isDigit) (lines content)
        parsedBoard = Board $ map (map charToField . filter (/= '|')) tableLines
        parsedNumbers = map read numberLines
    in MixedFileContent parsedBoard parsedNumbers

readMixedFile :: FilePath -> IO MixedFileContent
readMixedFile path = do
    content <- readFile path
    return $ parseMixedFile content

getBoard :: MixedFileContent -> Board
getBoard = loadedBoard

getMoveList :: MixedFileContent -> [Int]
getMoveList = moveList