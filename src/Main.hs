
module Main where

import FRP.Elerea.Simple
import GlossInterface
import Graphics.Gloss

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

board :: Signal (Maybe InputEvent) -> SignalGen Picture 
board e = do
            return $ Pictures $ [(lineLoop kewai)] ++
              (map (\s -> Line [(kewai !! s), putahi]) [0..7]) ++
              (map (\s -> Translate (fst $ (kewai !! s)) (snd $ (kewai !! s))
                     $ Color (if (s > 3) then white else black)$ ThickCircle 20 40) [0..7]) ++
              [(Translate (-150) 0 $ Scale 0.1 0.1 $ Text $ show e)]

mainElerea :: Signal (Maybe Float)
           -> Signal (Maybe InputEvent)
           -> SignalGen (Signal Picture)
mainElerea _ glossEvent = do
  pic <- board glossEvent
  sig <- transfer pic (\t a -> a) glossEvent 
  return sig

main :: IO ()
main = do
  playElerea (InWindow "Mu Torere" (500, 500) (0, 0))
    (greyN 0.85)
    100
    mainElerea
