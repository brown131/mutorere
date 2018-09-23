{-# LANGUAGE LambdaCase  #-}

module Main where

import Data.Maybe
import FRP.Elerea.Simple
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

data LocationValue = Black
                   | White
                   | Empty
                   deriving (Show, Eq)

data PlayerRole = Human | Machine

data GameState = GameState { locations :: [LocationValue],
                             turn      :: LocationValue,
                             player1   :: PlayerRole,
                             player2   :: PlayerRole }
                 
type InputEvent = G.Event

offset :: Float -> Float
offset x = x * tan (pi / 8.0)  -- 45 degrees
             
kewai :: [Point]
kewai = [((offset 200), 200),      
          (200, (offset 200)),
          (200, (offset (-200))),
          ((offset 200), (-200)),
          ((offset (-200)), (-200)),
          ((-200), (offset (-200))),
          ((-200), (offset 200)),
          ((offset (-200)), 200)]
        
putahi :: Point
putahi = (0, 0)

pieceSize :: Float
pieceSize = 80

initialGameState :: GameState
initialGameState = GameState { locations = [Black, Black, Black, Black, White, White, White, White, Empty],
                               turn = Black,
                               player1 = Human,
                               player2 = Machine }

renderPiece :: LocationValue -> Point -> Picture
renderPiece loc (x, y) = Translate x y $ Color (if (loc == White) then white else black) $ ThickCircle 1 pieceSize

renderLocation :: Int -> LocationValue -> Picture
renderLocation _ Empty = Text ""
renderLocation 8 loc   = renderPiece loc putahi
renderLocation i loc   = renderPiece loc (kewai !! i)

clickedLocation :: Point -> Maybe Int
clickedLocation (x, y) = listToMaybe $ filter (\i -> let (x', y') = if i == 8 then putahi else kewai !! i
                                              in sqrt ((x - x')**2 + ((y - y')**2)) <= (pieceSize / 2)) [0..8] 

findLocation :: Maybe InputEvent -> Maybe Int
findLocation (Just (G.EventKey (G.MouseButton G.LeftButton) G.Down _ p)) = clickedLocation p
findLocation _ = Nothing

getVal :: GameState -> Int -> LocationValue
getVal g i = (locations g) !! i

movableLocation :: Maybe Int -> GameState -> Maybe Int
movableLocation (Just 8) g = if (getVal g 8) == (turn g) then (Just 8) else Nothing
movableLocation (Just i) g = if (getVal g i) == (turn g) &&
                                -- Must be next to an empty location.
                                ((getVal g 8) == Empty ||
                                 (getVal g (mod (i + 1) 8)) == Empty ||
                                 (getVal g (mod (i + 7) 8)) == Empty) &&
                                -- Cannot move if a player's kewai is on either side.
                                not ((getVal g (mod (i + 1) 8)) == (turn g) &&
                                     (getVal g (mod (i + 7) 8)) == (turn g))
                             then (Just i) else Nothing
movableLocation Nothing _  = Nothing 

setLocation :: Int -> LocationValue -> [LocationValue] -> [LocationValue]
setLocation _ _ [] = []
setLocation n val (x:xs)
  | n == 0 = val:xs
  | otherwise = x:setLocation (n - 1) val xs
                  
renderBoard :: GameState -> Picture 
renderBoard g = Pictures $ [(lineLoop kewai)] ++
                  (map (\i -> Line [(kewai !! i), putahi]) [0..7]) ++
                  (map (\i -> renderLocation i ((locations g) !! i)) [0..8])

movePiece :: Maybe InputEvent -> GameState -> GameState
movePiece e g = do
                  let emptyLocation = head $ filter (\i -> (getVal g i) == Empty) [0..8]
                  case (movableLocation (findLocation e) g) of
                    Just i  -> GameState { locations = (setLocation emptyLocation (getVal g i) $
                                                        setLocation i Empty (locations g)),
                                           turn = if (turn g) == Black then White else Black,
                                           player1 = (player1 g),
                                           player2 = (player2 g) }
                    Nothing -> g

network :: Signal (Maybe Float)
        -> Signal (Maybe InputEvent)
        -> SignalGen (Signal Picture)
network _ glossEvent = do
  newGame <- transfer initialGameState (\e g -> movePiece e g) glossEvent
  return $ renderBoard <$> newGame

main :: IO ()
main = do
  (tickGen, tickSink) <- external Nothing
  (inputGen, inputSink) <- external Nothing
    
  recomputePicture <- start $ network tickGen inputGen
  initialPicture <- recomputePicture
    
  G.playIO (InWindow "Mu Torere" (500, 500) (0, 0))
    (greyN 0.85)
    100
    initialPicture
    return
    (\e _ -> do
        inputSink (Just e)
        recomputePicture)
    (\t _ -> do
        tickSink (Just t)
        recomputePicture)
