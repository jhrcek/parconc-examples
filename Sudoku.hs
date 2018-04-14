--
-- Sudoku solver using constraint propagation.  The algorithm is by
-- Peter Norvig http://norvig.com/sudoku.html; the Haskell
-- implementation is by Manu and Daniel Fischer, and can be found on
-- the Haskell Wiki http://www.haskell.org/haskellwiki/Sudoku
--
-- The Haskell wiki license applies to this code:
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- this work (the "Work"), to deal in the Work without restriction,
-- including without limitation the rights to use, copy, modify, merge,
-- publish, distribute, sublicense, and/or sell copies of the Work, and
-- to permit persons to whom the Work is furnished to do so.
--
-- THE WORK IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE WORK OR THE USE OR OTHER DEALINGS IN THE WORK.

module Sudoku (solve, printGrid) where

import Control.Monad (foldM, guard, msum)
import Data.Array (Array, array, assocs, elems, (!), (//))
import Data.Char (isDigit)
import Data.List (delete, intercalate, nub)

-- Types
type Digit = Char
type Square = (Char,Char)
type Unit = [Square]

-- We represent our grid as an array
type Grid = Array Square [Digit]

-- Setting Up the Problem
rows, cols, digits :: String
rows = "ABCDEFGHI"
cols = "123456789"
digits = "123456789"

box :: (Square, Square)
box = (('A','1'),('I','9'))

cross :: String -> String -> [Square]
cross rs cs = [ (r,c) | r <- rs, c <- cs ]

squares :: [Square]
squares = cross rows cols -- [('A','1'),('A','2'),('A','3'),...]

peers :: Array Square [Square]
peers = array box [(s, set (units!s)) | s <- squares ]
  where
    set = nub . concat

unitlist :: [Unit]
unitlist =
    [ cross rows [c] | c <- cols ] ++
    [ cross [r] cols | r <- rows ] ++
    [ cross rs cs | rs <- ["ABC","DEF","GHI"],
                    cs <- ["123","456","789"]]

-- this could still be done more efficiently, but what the heck...
units :: Array Square [Unit]
units =
    array box [(s, [filter (/= s) u | u <- unitlist, s `elem` u ]) |
                s <- squares]


allPossibilities :: Grid
allPossibilities = array box [ (s,digits) | s <- squares ]

-- Parsing a grid into an Array
parsegrid :: String -> Maybe Grid
parsegrid g = do
    guard (isValidGrid g)
    foldM assign allPossibilities (zip squares g)
   where
     isValidGrid :: String -> Bool
     isValidGrid = all (`elem` ".0123456789")

-- Propagating Constraints
assign :: Grid -> (Square, Digit) -> Maybe Grid
assign g (s,d) =
    if isDigit d then do
        let ds = g ! s
            toDump = delete d ds
        foldM eliminate g (zip (repeat s) toDump)
    else -- d == '.'
        return g

eliminate :: Grid -> (Square, Digit) -> Maybe Grid
eliminate g (s,d) =
    let cell = g ! s in
    if d `notElem` cell then -- already eliminated
        return g
    else do -- else d is deleted from s' values
        let newCell = delete d cell
            newV = g // [(s,newCell)]
        newV2 <- case newCell of
        -- contradiction : Nothing terminates the computation
             []   -> Nothing
        -- if there is only one value left in s, remove it from peers
             [d'] -> do let peersOfS = peers ! s
                        foldM eliminate newV (zip peersOfS (repeat d'))
        -- else : return the new grid
             _    -> return newV
        -- Now check the places where d appears in the peers of s
        foldM (locate d) newV2 (units ! s)

locate :: Digit -> Grid -> Unit -> Maybe Grid
locate d g u =
    case filter ((d `elem`) . (g !)) u of
        []  -> Nothing
        [s] -> assign g (s,d)
        _   -> return g

-- Search
search :: Grid -> Maybe Grid
search g =
    case [(l,(s,xs)) | (s,xs) <- assocs g, let l = length xs, l /= 1] of
        [] -> return g
        ls -> do
            let (_,(s,ds)) = minimum ls
            msum [assign g (s,d) >>= search | d <- ds]

solve :: String -> Maybe Grid
solve str = do
    grd <- parsegrid str
    search grd

-- Display solved grid
printGrid :: Grid -> IO ()
printGrid = putStrLn . gridToString

gridToString :: Grid -> String
gridToString g =
  let l0 = elems g
      -- [("1537"),("4"),...]
      l1 = map (\s -> " " ++ s ++ " ") l0
      -- ["1 "," 2 ",...]
      l2 = map concat (sublist 3 l1)
      -- ["1  2  3 "," 4  5  6 ", ...]
      l3 = sublist 3 l2
      -- [["1  2  3 "," 4  5  6 "," 7  8  9 "],...]
      l4 = map (intercalate "|") l3
      -- ["1  2  3 | 4  5  6 | 7  8  9 ",...]
      l5 = intercalate [line] (sublist 3 l4)
  in unlines l5
    where
      sublist _ [] = []
      sublist n xs = ys : sublist n zs
        where (ys,zs) = splitAt n xs
      line = hyphens ++ "+" ++ hyphens ++ "+" ++ hyphens
      hyphens = replicate 9 '-'
