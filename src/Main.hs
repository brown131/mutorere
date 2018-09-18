{-# LANGUAGE LambdaCase  #-}

module Main where

import Debug.Trace (trace)
import Data.Maybe
import FRP.Elerea.Simple
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

data Location = Black
              | White
              | Empty
              deriving (Show, Eq)
             
type InputEvent = G.Event

type GameState = [Location]

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
initialGameState = [Black, Black, Black, Black, White, White, White, White, Empty]

renderPiece :: Location -> Point -> Picture
renderPiece loc (x, y) = Translate x y $ Color (if (loc == White) then white else black) $ ThickCircle 1 pieceSize

renderLocation :: Int -> Location -> Picture
renderLocation _ Empty = Text ""
renderLocation 8 loc   = renderPiece loc putahi
renderLocation s loc   = renderPiece loc (kewai !! s)
                        
renderBoard :: GameState -> Picture 
renderBoard g = Pictures $ [(lineLoop kewai)] ++
                (map (\s -> Line [(kewai !! s), putahi]) [0..7]) ++
                (map (\s -> renderLocation s (g !! s)) [0..8])
  
renderEvent :: Maybe InputEvent -> Picture
renderEvent (Just e) = Text $ show e
renderEvent Nothing  = Text "Nothing"

-- Determine mouse slot (0..8)
findSlot :: Point -> Maybe Int
findSlot (x, y) = listToMaybe $ filter (\s -> let (x', y') = if s == 8 then putahi else kewai !! s
                                              in sqrt ((x - x')^2 + ((y - y')^2)) <= pieceSize) [0..8] 

findSelectedSlot :: Maybe InputEvent -> Maybe Int
findSelectedSlot (Just (G.EventKey (G.MouseButton G.LeftButton) G.Up _ p)) = findSlot p
findSelectedSlot _ = Nothing

replaceNth :: Int -> Location -> GameState -> GameState
replaceNth _ _ [] = []
replaceNth n val (x:xs)
  | n == 0 = val:xs
  | otherwise = x:replaceNth (n - 1) val xs
   
movePiece :: Maybe InputEvent -> GameState -> GameState
movePiece a b | trace ("movePiece " ++ show b ) False = undefined
movePiece e g = let emptySlot = head $ filter (\s -> (g !! s) == Empty) [0..8]
                    slot = findSelectedSlot e
                in case slot of
                     Just s  -> replaceNth emptySlot (g !! s) $ replaceNth s Empty g
                     Nothing -> g
  
network :: Signal (Maybe Float)
        -> Signal (Maybe InputEvent)
        -> Signal GameState
        -> SignalGen (Signal Picture)
network _ glossEvent game = do
  newGame <- transfer2 undefined (\e g _ -> movePiece e g) glossEvent game
  sig <- transfer2 undefined (\e g _ -> renderBoard g) glossEvent newGame
  return sig

main :: IO ()
main = do
  (tickGen, tickSink) <- external Nothing
  (inputGen, inputSink) <- external Nothing
  (gameGen, gameSink) <- external $ initialGameState
    
  recomputePicture <- start $ network tickGen inputGen gameGen
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
