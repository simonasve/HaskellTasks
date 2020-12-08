module Task1Message
where

-- ┌           ┐
-- │ X X X O X │
-- │ O X O X O │
-- │ X O X X X │
-- │ O O X X O │
-- │ X O O O O │
-- └           ┘
-- seed: -398915757126590735
-- encoding: BenArr
-- from: COO
-- to: ARR

size :: Int
size = 5

message :: String
message = "ll1:x1:01:y1:01:v1:Xel1:x1:11:y1:01:v1:Xel1:x1:21:y1:01:v1:Xel1:x1:31:y1:01:v1:Oel1:x1:41:y1:01:v1:Xel1:x1:01:y1:11:v1:Oel1:x1:11:y1:11:v1:Xel1:x1:21:y1:11:v1:Oel1:x1:31:y1:11:v1:Xel1:x1:41:y1:11:v1:Oel1:x1:01:y1:21:v1:Xel1:x1:11:y1:21:v1:Oel1:x1:21:y1:21:v1:Xel1:x1:31:y1:21:v1:Xel1:x1:41:y1:21:v1:Xel1:x1:01:y1:31:v1:Oel1:x1:11:y1:31:v1:Oel1:x1:21:y1:31:v1:Xel1:x1:31:y1:31:v1:Xel1:x1:41:y1:31:v1:Oel1:x1:01:y1:41:v1:Xel1:x1:11:y1:41:v1:Oel1:x1:21:y1:41:v1:Oel1:x1:31:y1:41:v1:Oel1:x1:41:y1:41:v1:Oee"

type From = [(Int, Int, Char)]
type To = ([Int], [Int], [Char])

expectedFrom :: From
expectedFrom = [(0, 0, 'X'), (1, 0, 'X'), (2, 0, 'X'), (3, 0, 'O'), (4, 0, 'X'), (0, 1, 'O'), (1, 1, 'X'), (2, 1, 'O'), (3, 1, 'X'), (4, 1, 'O'), (0, 2, 'X'), (1, 2, 'O'), (2, 2, 'X'), (3, 2, 'X'), (4, 2, 'X'), (0, 3, 'O'), (1, 3, 'O'), (2, 3, 'X'), (3, 3, 'X'), (4, 3, 'O'), (0, 4, 'X'), (1, 4, 'O'), (2, 4, 'O'), (3, 4, 'O'), (4, 4, 'O')]

expectedTo :: To
expectedTo = ([0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4], [0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4], ['X', 'X', 'X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X', 'X', 'X', 'O', 'O', 'X', 'X', 'O', 'X', 'O', 'O', 'O', 'O'])