module App where

import Args ( Args, parseArgs )
import Options.Applicative ( handleParseResult )
import System.Environment (getArgs)
import Data.List

program :: IO ()
program =
  getArgs >>= (handleParseResult . parseArgs) >>= program'

program' :: Args -> IO()
program' = const (putStrLn "hello!")

-- Assumptions:
-- - starting position: top-left
-- - grid size: 20x20

data Player = PlayerA | PlayerB
data Strategy = SequentialH | SequentialV -- | Random
data Move = LeftM | RightM | UpM | DownM
data Position = Pos Int Int
data Outcome = Draw | Winner Player

play :: Position -> Player -> Player -> Outcome
play fp p1 p2 =
  let ms1 = play' fp p1
      ms2 = play' fp p2
  in if length ms1 == length ms2 then Draw
     else if length ms1 < length ms2 then Winner p1
     else Winner p2

play' :: Position -> Player -> [Position]
play' (Pos flagX flagY) p = undefined

journeyH :: [Position]
journeyH = unfoldr nextPosH (Pos 0 0)

nextPosH :: Position -> Maybe (Position, Position)
nextPosH p = fmap (\p' -> (p, p')) $ (=<<) (pos p) (nextMoveH p)
-- nextMoveH(p).flatMap(m => pos(p, m)).map(p' => (p, p'))

pos :: Position -> Move -> Maybe Position
pos (Pos x y) LeftM = if x > 0 then Just (Pos (x - 1) y) else Nothing
pos (Pos x y) RightM = if x < 19 then Just (Pos (x + 1) y) else Nothing
pos (Pos x y) UpM = if y > 0 then Just (Pos x (y - 1)) else Nothing
pos (Pos x y) DownM = if y < 19 then Just (Pos x (y + 1)) else Nothing

nextMoveH :: Position -> Maybe Move
nextMoveH (Pos x y) =
  if y `mod` 2 == 0 then (if x < 19 then Just RightM else (if y < 19 then Just DownM else Nothing))
  else (if x > 0 then Just LeftM else (if y < 19 then Just DownM else Nothing))
