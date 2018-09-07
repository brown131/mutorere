
module Main where

import Graphics.Gloss
import FRP.Yampa

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

main :: IO ()
main = do
  let window = InWindow "Mu Torere" (500, 500) (0, 0)
  -- | Display the last event received as text.
  play window
    (greyN 0.85)
    100
    ""
    (\str -> (Pictures $ [(lineLoop kewai)] ++
               (map (\s -> Line [(kewai !! s), putahi]) [0..7]) ++
               (map (\s -> Translate (fst $ (kewai !! s)) (snd $ (kewai !! s))
                           $ Color (if (s > 3) then white else black)$ ThickCircle 20 40) [0..7]) ++
               [(Translate (-150) 0 $ Scale 0.1 0.1 $ Text str)]))
    (\e _ -> show e)
    (\_ world -> world)
