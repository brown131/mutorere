
module Main where

import Data.List
import Data.Maybe
import FRP.Elerea.Simple
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

data Stone = Black | White deriving (Show, Eq)

type LocationValue = Maybe Stone

data GameState = GameState { locations :: [LocationValue],
                             move      :: Stone }
                 
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
initialGameState = GameState { locations = [Just Black, Just Black, Just Black, Just Black,
                                            Just White, Just White, Just White, Just White, Nothing],
                               move = Black }

clickedLocation :: Point -> Maybe Int
clickedLocation (x, y) = listToMaybe $ filter (\i -> let (x', y') = if i == 8 then putahi else kewai !! i
                                              in sqrt ((x - x')**2 + ((y - y')**2)) <= (pieceSize / 2)) [0..8] 

findLocation :: Maybe InputEvent -> Maybe Int
findLocation (Just (G.EventKey (G.MouseButton G.LeftButton) G.Down _ p)) = clickedLocation p
findLocation _ = Nothing

getVal :: GameState -> Int -> LocationValue
getVal g i = (locations g) !! i

movableLocation :: Maybe Int -> GameState -> Maybe Int
movableLocation (Just 8) g | (getVal g 8) == Just (move g) = (Just 8)
movableLocation (Just i) g | (getVal g i) == Just (move g) &&
                             -- Must be next to an empty location.
                             ((getVal g 8) == Nothing ||
                               (getVal g ((i + 1) `mod` 8)) == Nothing ||
                               (getVal g ((i + 7) `mod` 8)) == Nothing) &&
                             -- Cannot move if a player's own kewai are on both sides.
                             not ((getVal g ((i + 1) `mod` 8)) == Just (move g) &&
                                  (getVal g ((i - 1) `mod` 8)) == Just (move g)) = (Just i)
movableLocation _ _ = Nothing 

setLocation :: Int -> LocationValue -> [LocationValue] -> [LocationValue]
setLocation _ _ [] = []
setLocation n val (x:xs)
  | n == 0 = val:xs
  | otherwise = x:setLocation (n - 1) val xs

nextMove :: GameState -> Stone
nextMove g | (move g) == Black = White
           | otherwise = Black

renderPiece :: LocationValue -> Point -> Picture
renderPiece loc (x, y) = Translate x y $ Color (if (loc == Just White) then white else black) $ ThickCircle 1 pieceSize

renderLocation :: Int -> LocationValue -> Picture
renderLocation _ Nothing = Text ""
renderLocation 8 loc   = renderPiece loc putahi
renderLocation i loc   = renderPiece loc (kewai !! i)

renderStatusBar :: GameState -> [Picture]
renderStatusBar g = [Translate (-250) (-290) $ Color white $ Polygon [(0, 0), (0, 30), (500, 30), (500, 0)]] ++
                    [Translate (-240) (-280) $ Scale 0.1 0.1 $ Text $ if (gameOver g)
                                                                      then (show (nextMove g)) ++ " wins!"
                                                                      else ("Move: " ++ (show $ move g))]

renderBoard :: GameState -> Picture 
renderBoard g = Pictures $ [(lineLoop kewai)] ++
                  (map (\i -> Line [(kewai !! i), putahi]) [0..7]) ++
                  (map (\i -> renderLocation i (getVal g i)) [0..8]) ++
                  (renderStatusBar g)

gameOver :: GameState -> Bool
gameOver g = (find (\i -> ((getVal g i) == Just (move g)) &&
                          (not ((movableLocation (Just i) g) == Nothing))) [0..8]) == Nothing
  
movePiece :: Maybe InputEvent -> GameState -> GameState
movePiece e g = do
                  let openLocation = head $ filter (\i -> (getVal g i) == Nothing) [0..8]
                  case (movableLocation (findLocation e) g) of
                    Just i  -> g { locations = (setLocation openLocation (getVal g i) $
                                                 setLocation i Nothing (locations g)),
                                   move = (nextMove g) }
                    Nothing -> g

network :: SignalGen (Signal (Maybe Float))
        -> SignalGen (Signal (Maybe InputEvent))
        -> SignalGen (Signal Picture)
network _ e = do
  glossEvent <- e
  newGame <- transfer initialGameState movePiece glossEvent
  return $ renderBoard <$> newGame

main :: IO ()
main = do
  (tickGen, tickSink) <- external Nothing
  (inputGen, inputSink) <- external Nothing
    
  recomputePicture <- start $ do network tickGen inputGen
    
  initialPicture <- recomputePicture
    
  G.playIO (InWindow "Mu Torere" (500, 580) (0, 0))
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
    
