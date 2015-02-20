import Data.List.Split
import Data.Maybe

basis = [" _     _  _     _  _  _  _  _ ",
         "| |  | _| _||_||_ |_   ||_||_|",
         "|_|  ||_  _|  | _||_|  ||_| _|"]


toGlyphs :: [String] -> [(String, String, String)]
toGlyphs s = zip3 a b c
  where subglyphs = map (chunksOf 3) s
        [a, b, c] = subglyphs

-- convert a string into groups of 3 lines, dropping each fourth line
-- of input.
readEntries :: String -> [[String]]
readEntries l = map (take 3) scans
  where
    scans = chunksOf 4 $ lines l

index = zip (toGlyphs basis) [0..9]

-----------------------------------
-- This reads the entire input string and converts it into a sequence
-- of entries, where each entry is a list of 9 Maybe Ints representing
-- a line in the input.
parse :: String -> [[Maybe Int]]
parse l = map parseEntry glyph_sets
  where
    entries = readEntries l             :: [[String]]
    glyph_sets = map toGlyphs entries   :: [[(String, String, String)]]
    parseEntry = map $ (flip lookup) index

-- Test stuff

test_data =
     " _     _  _     _  _  _  _ \n"
  ++ "| |  | _| _||_||_ |_   ||_|\n"
  ++ "|_|  ||_  _|  | _||_|  ||_|\n"
  ++ "\n"
  ++ "    _  _  _  _  _  _     _ \n"
  ++ "|_||_|| || ||_   |  |  ||_ \n"
  ++ "  | _||_||_||_|  |  |  | _|\n"
  ++ ""

test = parse test_data
