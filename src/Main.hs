{-# LANGUAGE LambdaCase  #-}

module Main where

--import Debug.Trace()
import FRP.Elerea.Simple
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

data Location = Black
              | White
              | Free
             
type InputEvent = G.Event

type GameState = [Location]
             
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

offset :: Float -> Float
offset x = x * tan (pi / 8.0)  -- 45 degrees

renderBoard :: Picture -> Picture 
renderBoard t = Pictures $ [(lineLoop kewai)] ++
                (map (\s -> Line [(kewai !! s), putahi]) [0..7]) ++
                (map (\s -> Translate (fst $ (kewai !! s)) (snd $ (kewai !! s))
                            $ Color (if (s > 3) then white else black)$ ThickCircle 20 40) [0..7]) ++
                [(Translate (-150) 0 $ Scale 0.1 0.1 $ t)]
  
renderEvent :: Maybe InputEvent -> Picture
--renderEvent a | trace ("renderEvent " ++ show a) False = undefined   
renderEvent (Just e) = Text $ show e
renderEvent Nothing  = Text "Nothing"

network :: Signal (Maybe Float)
        -> Signal (Maybe InputEvent)
        -> SignalGen (Signal Picture)
network _ glossEvent = do
  sig <- transfer undefined (\e _ -> (renderBoard $ renderEvent e)) glossEvent 
  return sig

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
