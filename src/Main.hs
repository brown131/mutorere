{-# LANGUAGE LambdaCase  #-}

module Main where

import Data.Maybe
import FRP.Elerea.Simple
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

data Location = Black
              | White
              | Empty
              deriving (Show, Eq)

data PlayerRole = Human | Machine

data GameState = GameState { locations :: [Location],
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
                               player1 = Human,
                               player2 = Machine }

renderPiece :: Location -> Point -> Picture
renderPiece loc (x, y) = Translate x y $ Color (if (loc == White) then white else black) $ ThickCircle 1 pieceSize

renderLocation :: Int -> Location -> Picture
renderLocation _ Empty = Text ""
renderLocation 8 loc   = renderPiece loc putahi
renderLocation s loc   = renderPiece loc (kewai !! s)
                       
renderEvent :: Maybe InputEvent -> Picture
renderEvent (Just e) = Text $ show e
renderEvent Nothing  = Text "Nothing"

clickedLocation :: Point -> Maybe Int
clickedLocation (x, y) = listToMaybe $ filter (\s -> let (x', y') = if s == 8 then putahi else kewai !! s
                                              in sqrt ((x - x')**2 + ((y - y')**2)) <= (pieceSize / 2)) [0..8] 

findLocation :: Maybe InputEvent -> Maybe Int
findLocation (Just (G.EventKey (G.MouseButton G.LeftButton) G.Down _ p)) = clickedLocation p
findLocation _ = Nothing

setLocation :: Int -> Location -> [Location] -> [Location]
setLocation _ _ [] = []
setLocation n val (x:xs)
  | n == 0 = val:xs
  | otherwise = x:setLocation (n - 1) val xs
                  
renderBoard :: GameState -> Picture 
renderBoard g = Pictures $ [(lineLoop kewai)] ++
                  (map (\s -> Line [(kewai !! s), putahi]) [0..7]) ++
                  (map (\s -> renderLocation s ((locations g) !! s)) [0..8])

movePiece :: Maybe InputEvent -> GameState -> GameState
movePiece e g = do
                  let emptyLocation = head $ filter (\s -> ((locations g) !! s) == Empty) [0..8]
                  case (findLocation e) of
                    Just s  -> GameState { locations = (setLocation emptyLocation ((locations g) !! s) $
                                                        setLocation s Empty (locations g)),
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
