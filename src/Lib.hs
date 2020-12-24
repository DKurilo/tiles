module Lib
    ( solve
    ) where

import           Control.Applicative          ((<|>))
import           Data.Bifunctor               (first)
import           Data.List                    (sortOn)
import qualified Data.Set                     as S
import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read

data Tile = X | N Int deriving (Show, Eq, Ord)

instance Read Tile where
    readPrec = (N <$> readPrec) <|> (do
        lift $ P.char 'X'
        return X)

pTilesLine = do
    t <- readPrec
    ts <- (do
        lift $ P.char ','
        pTilesLine) <|> return []
    return (t:ts)

pTilesLines :: ReadPrec [[Tile]]
pTilesLines = do
    tl <- pTilesLine
    tls <- (do
        lift P.skipSpaces
        pTilesLines) <|> return []
    return (tl: tls)

data Board = Board Int Int Int Int [[Tile]] deriving (Show, Eq, Ord)

instance Read Board where
    readPrec = do
        ts <- pTilesLines
        let (i, j) = (\(x, y, _) -> (x, y)) . head
                   . filter (\(_, _, t) -> t == X) . tilesCoords $ ts
            w = length . head $ ts
            h = length ts
        return (Board w h i j ts)

data Turn = L | R | U | D deriving (Show, Eq)

tilesCoords :: [[Tile]] -> [(Int, Int, Tile)]
tilesCoords = concat . zipWith (\y -> map (\(x, t) -> (x, y, t))) [0..] . map (zip [0..])

isWin (Board w h i j ts) = i == w - 1 && j == h - 1 && (all (\(x, y, N n) -> n == y * w + x + 1)
                         . filter (\(_, _, t) -> t /= X)  . tilesCoords $ ts)

data State = St [Turn] Board deriving (Show)

score :: Board -> Int
score (Board w h i j ts) = sum [ manhattan x y (ts !! y !! x)
                               | x <- [0..w - 1], y <- [0..h - 1], x /= i && y /= j
                               ]
    where manhattan x y (N n) = abs (x - (n - 1) `mod` w) + abs (y - (n - 1) `div` w)
          manhattan x y X = abs (x - w + 1) + abs (y - h + 1)

move :: S.Set Board -> [State] -> (S.Set Board, [State])
move mem [] = (mem, [])
move mem sts = (\(mem', sts', _) -> (mem', sts')) . foldl go (mem, [], False) $ sts
  where go (mem', sts', haveWin) st@(St ts b@(Board w h i j tls))
          | length ts > 350 = (mem', sts', haveWin)
          | haveWin = (mem', sts', True)
          | isWin b = (mem', [st], True)
          | otherwise = (S.insert b mem', turnU ++ turnD ++ turnL ++ turnR ++ sts', haveWin)
          where turnU
                  | (not . null) ts && head ts == D = []
                  | j > 0 = [St (U:ts) (Board w h i (j - 1) [[ if x == i && y == j
                                                                  then tls !! (j - 1) !! i
                                                                  else if x == i && y == (j - 1)
                                                                          then X
                                                                          else tls !! y !! x | x <- [0..w - 1]] | y <- [0..h - 1]])]
                  | otherwise = []
                turnD
                  | (not . null) ts && head ts == U = []
                  | j < (h - 1) = [St (D:ts) (Board w h i (j + 1) [[ if x == i && y == j
                                                                        then tls !! (j + 1) !! i
                                                                        else if x == i && y == (j + 1)
                                                                                then X
                                                                                else tls !! y !! x | x <- [0..w - 1]] | y <- [0..h - 1]])]
                  | otherwise = []
                turnL
                  | (not . null) ts && head ts == R = []
                  | i > 0 = [St (L:ts) (Board w h (i - 1) j [[ if x == i && y == j
                                                                  then tls !! j !! (i - 1)
                                                                  else if x == (i - 1) && y == j
                                                                          then X
                                                                          else tls !! y !! x | x <- [0..w - 1]] | y <- [0..h - 1]])]
                  | otherwise = []
                turnR
                  | (not . null) ts && head ts == L = []
                  | i < (w - 1) = [St (R:ts) (Board w h (i + 1) j [[ if x == i && y == j
                                                                        then tls !! j !! (i + 1)
                                                                        else if x == (i + 1) && y == j
                                                                                then X
                                                                                else tls !! y !! x | x <- [0..w - 1]] | y <- [0..h - 1]])]
                  | otherwise = []

solve' :: S.Set Board -> [State] -> [Turn]
solve' mem sts
  | (not . null) winsts = (\(St ts _ : _) -> ts) winsts
  | otherwise = solve' mem' . take 100000 . sortOn (\(St _ b) -> score b) $ sts'
  where (mem', sts') = move mem sts
        winsts = filter (\(St _ b) -> isWin b) sts'

solve :: Board -> [Turn]
solve b = reverse . solve' S.empty $ [St [] b]
