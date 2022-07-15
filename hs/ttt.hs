module Main where

import Text.Read (readMaybe)

data Player = X
            | O
            deriving (Eq, Show)

otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X

type Board = [Maybe Player]

newBoard :: Board
newBoard = replicate 9 Nothing

projectToBoard :: Int -> Int -> Int
projectToBoard x y = (x - 1) + (y - 1) * 3

putPiece :: Board -> Player -> Int -> Int -> Board
putPiece board piece x y =
  let proj = projectToBoard x y
  in map (\(p,idx) ->
        if (idx == proj)
        then (Just piece)
        else p
        )
     $ zip board [0..]

prettyPiece :: Maybe Player -> String
prettyPiece Nothing = " "
prettyPiece (Just X) = "X"
prettyPiece (Just O) = "O"

prettyBoard :: Board -> String
prettyBoard board = foldl (\acc (p,idx) ->
                             acc
                             <> "|"
                             <> prettyPiece p
                             <> if ((idx :: Int) `mod` 3 == 2)
                                then "|\n"
                                else "")
                    "" $ zip board [0..]

winner :: Board -> Maybe Player
winner board = foldl (\acc w -> case acc of Nothing -> w; _ -> acc)
                 Nothing
                 (reduceDiagonal win <> reduceRow win <> reduceColumn win)
  where
    reduceDiagonal, reduceRow, reduceColumn :: (Maybe Player -> Maybe Player -> Maybe Player -> d) -> [d]
    reduceDiagonal f = [f (board !! projectToBoard 1 1)
                          (board !! projectToBoard 2 2)
                          (board !! projectToBoard 3 3)
                       ,f (board !! projectToBoard 3 1)
                          (board !! projectToBoard 2 2)
                          (board !! projectToBoard 1 3)]
    reduceRow f = map (\i -> f (board !! projectToBoard 1 i)
                               (board !! projectToBoard 2 i)
                               (board !! projectToBoard 3 i)) [1..3]
    reduceColumn f = map (\i -> f (board !! projectToBoard i 1)
                                  (board !! projectToBoard i 2)
                                  (board !! projectToBoard i 3)) [1..3]
    win (Just X) (Just X) (Just X) = Just X
    win (Just O) (Just O) (Just O) = Just O
    win _ _ _ = Nothing

hasEmpty :: Board -> Bool
hasEmpty = any (Nothing ==)

isEmptyAt :: Board -> Int -> Int -> Bool
isEmptyAt b x y = Nothing == (b !! projectToBoard x y)

readPosition :: Player -> IO (Int,Int)
readPosition p = do
  putStrLn ("It is player "<> prettyPiece (Just p) <> "'s turn.")
  str <- getLine
  case (str :: String) of
       (x:',':y:_) -> case (readMaybe [x], readMaybe [y]) of
         (Just x', Just y') -> return (x',y')
         _ -> tryAgain
       _ -> tryAgain
  where
    tryAgain = do putStrLn "I did not understand"
                  readPosition p

gameLoop :: Board -> Player -> IO (Maybe Player)
gameLoop board player = do
  putStrLn $ prettyBoard board
  (x,y) <- readEmptyPosition
  let board' = putPiece board player x y
  case (winner board', hasEmpty board) of
    (Just _,_) -> return $ winner board'
    (_, False) -> return Nothing
    _ -> gameLoop board' (otherPlayer player)
  where
    readEmptyPosition :: IO (Int,Int)
    readEmptyPosition = do
      (x,y) <- readPosition player
      if not (isEmptyAt board x y)
        then do
             putStrLn "The square is occupied!"
             readEmptyPosition
        else return (x,y)


main :: IO ()
main = do
  outcome <- gameLoop newBoard X
  case outcome of
    Nothing -> putStrLn "it's a draw!"
    p -> putStrLn $ "Player " <> prettyPiece p <> " has won!"
