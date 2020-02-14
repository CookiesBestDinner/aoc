module Day2 where

import           Data.List                                ( scanl'
                                                          , foldl'
                                                          )
import qualified Data.Map.Strict               as Map

data Direction = U | D | R | L deriving (Show, Read)

main = do
  input <- map (map (read . (: []))) . lines <$> getContents
  let begin = 5
  putStrLn $ concatMap show $ drop 1 $ scanl' multiMove begin input
  let begin2 = '5'
  putStrLn $ drop 1 $ scanl' multiMove2 begin2 input
  putStrLn "redo with less duplication because reasons"
  let tour  = drop 1 $ scanl' (multiGo pad1) (1, 1) input
      tour2 = drop 1 $ scanl' (multiGo pad2) (0, 0) input
  putStrLn $ map (pad1 Map.!) tour
  putStrLn $ map (pad2 Map.!) tour2

multiMove :: Int -> [Direction] -> Int
multiMove = foldl' move

multiMove2 :: Char -> [Direction] -> Char
multiMove2 = foldl' move2

multiGo pad = foldl' (go pad)

go pad loc@(x, y) dir | newLoc `Map.member` pad = newLoc
                      | otherwise               = loc
 where
  newLoc = case dir of
    U -> (x, y + 1)
    D -> (x, y - 1)
    R -> (x + 1, y)
    L -> (x - 1, y)

-- 1 2 3
-- 4 5 6
-- 7 8 9
pad1 = Map.fromList
  [ ((0, 0), '7')
  , ((0, 1), '4')
  , ((0, 2), '1')
  , ((1, 0), '8')
  , ((1, 1), '5')
  , ((1, 2), '2')
  , ((2, 0), '9')
  , ((2, 1), '6')
  , ((2, 2), '3')
  ]

-- could also have generated a map from this:
-- [ "  1  "
-- , " 234 "
-- , "56789"
-- , " ABC "
-- , "  D  "
-- ]

--     1
--   2 3 4
-- 5 6 7 8 9
--   A B C
--     D
pad2 = Map.fromList
  [ ((0, 0) , '5')
  , ((1, 0) , '6')
  , ((2, 0) , '7')
  , ((3, 0) , '8')
  , ((4, 0) , '9')
  , ((1, 1) , '2')
  , ((2, 1) , '3')
  , ((3, 1) , '4')
  , ((2, 2) , '1')
  , ((1, -1), 'A')
  , ((2, -1), 'B')
  , ((3, -1), 'C')
  , ((2, -2), 'D')
  ]

-- okay, yeah, this is super silly.
-- an array + some logic to navigate it would be less error prone.

move2 '1' U = '1'
move2 '1' D = '3'
move2 '1' L = '1'
move2 '1' R = '1'

move2 '2' U = '2'
move2 '2' D = '6'
move2 '2' L = '2'
move2 '2' R = '3'

move2 '3' U = '1'
move2 '3' D = '7'
move2 '3' L = '2'
move2 '3' R = '4'

move2 '4' U = '4'
move2 '4' D = '8'
move2 '4' L = '3'
move2 '4' R = '4'

move2 '5' U = '5'
move2 '5' D = '5'
move2 '5' L = '5'
move2 '5' R = '6'

move2 '6' U = '2'
move2 '6' D = 'A'
move2 '6' L = '5'
move2 '6' R = '7'

move2 '7' U = '3'
move2 '7' D = 'B'
move2 '7' L = '6'
move2 '7' R = '8'

move2 '8' U = '4'
move2 '8' D = 'C'
move2 '8' L = '7'
move2 '8' R = '9'

move2 '9' U = '9'
move2 '9' D = '9'
move2 '9' L = '8'
move2 '9' R = '9'

move2 'A' U = '6'
move2 'A' D = 'A'
move2 'A' L = 'A'
move2 'A' R = 'B'

move2 'B' U = '7'
move2 'B' D = 'D'
move2 'B' L = 'A'
move2 'B' R = 'C'

move2 'C' U = '8'
move2 'C' D = 'C'
move2 'C' L = 'B'
move2 'C' R = 'C'

move2 'D' U = 'B'
move2 'D' D = 'D'
move2 'D' L = 'D'
move2 'D' R = 'D'



move 1 U = 1
move 1 D = 4
move 1 L = 1
move 1 R = 2

move 2 U = 2
move 2 D = 5
move 2 L = 1
move 2 R = 3

move 3 U = 3
move 3 D = 6
move 3 L = 2
move 3 R = 3

move 4 U = 1
move 4 D = 7
move 4 L = 4
move 4 R = 5

move 5 U = 2
move 5 D = 8
move 5 L = 4
move 5 R = 6

move 6 U = 3
move 6 D = 9
move 6 L = 5
move 6 R = 6

move 7 U = 4
move 7 D = 7
move 7 L = 7
move 7 R = 8

move 8 U = 5
move 8 D = 8
move 8 L = 7
move 8 R = 9

move 9 U = 6
move 9 D = 9
move 9 L = 8
move 9 R = 9
move 9 R = 9
