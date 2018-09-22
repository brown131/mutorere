{-# LANGUAGE LambdaCase  #-}

module Main where

import Control.Monad.State.Lazy
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

type GameState a = StateT [Location] IO a

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

locations :: [Location]
locations = [Black, Black, Black, Black, White, White, White, White, Empty]

initialGameState :: GameState ()
initialGameState = do
  put locations

renderPiece :: Location -> Point -> Picture
renderPiece loc (x, y) = Translate x y $ Color (if (loc == White) then white else black) $ ThickCircle 1 pieceSize

renderLocation :: Int -> Location -> Picture
renderLocation _ Empty = Text ""
renderLocation 8 loc   = renderPiece loc putahi
renderLocation s loc   = renderPiece loc (kewai !! s)
                       
renderEvent :: Maybe InputEvent -> Picture
renderEvent (Just e) = Text $ show e
renderEvent Nothing  = Text "Nothing"

-- Determine mouse slot (0..8)
findSlot :: Point -> Maybe Int
findSlot (x, y) = listToMaybe $ filter (\s -> let (x', y') = if s == 8 then putahi else kewai !! s
                                              in sqrt ((x - x')^2 + ((y - y')^2)) <= pieceSize) [0..8] 

findSelectedSlot :: Maybe InputEvent -> Maybe Int
findSelectedSlot (Just (G.EventKey (G.MouseButton G.LeftButton) G.Down _ p)) = findSlot p
findSelectedSlot _ = Nothing

replaceNth :: Int -> Location -> [Location] -> [Location]
replaceNth _ _ [] = []
replaceNth n val (x:xs)
  | n == 0 = val:xs
  | otherwise = x:replaceNth (n - 1) val xs
                  
renderBoard :: [Location] -> Picture 
renderBoard g = Pictures $ [(lineLoop kewai)] ++
                  (map (\s -> Line [(kewai !! s), putahi]) [0..7]) ++
                  (map (\s -> renderLocation s (g !! s)) [0..8])

movePiece :: Maybe InputEvent -> [Location] -> [Location]
--movePiece a b | trace ("movePiece " ++ show b ) False = undefined
movePiece e g = do
                let emptySlot = head $ filter (\s -> (g !! s) == Empty) [0..8]
                let slot = findSelectedSlot e
                case slot of
                  Just s  -> replaceNth emptySlot (g !! s) $ replaceNth s Empty g
                  Nothing -> g
                  
--prerenderBoard :: 

network :: Signal (Maybe Float)
        -> Signal (Maybe InputEvent)
        -> SignalGen (Signal Picture)
network _ glossEvent = do
  newGame <- transfer locations  (\e g -> movePiece e g) glossEvent
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
