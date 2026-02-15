-- Mū Tōrere is a traditional Māori, two-player, abstract strategy game

-- Mu Torere Game Implementation
-- A two-player strategic board game with 8 outer positions and 1 center position
-- Players move their pieces by swapping with empty spaces

module Main where

import Data.List
import Data.Maybe
import FRP.Elerea.Simple
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

-- | Represents a stone/piece, either Black or White
data Stone = Black | White deriving (Show, Eq)

-- | A location on the board can either be empty (Nothing) or contain a Stone
type LocationValue = Maybe Stone

-- | GameState tracks all pieces and whose turn it is
-- locations: list of 9 LocationValue items (indices 0-7 are outer ring, 8 is center)
-- move: whose turn it is (Black or White)
data GameState = GameState { locations :: [LocationValue],
                             move      :: Stone }
                 
-- | Calculates offset for octagonal board layout using tangent of 45 degrees
offset :: Float -> Float
offset x = x * tan (pi / 8.0)  -- 45 degrees

-- | The kewai are the eight outer positions of the octagonal board (indices 0-7)
-- Positioned in a circular pattern around the center
kewai :: [Point]
kewai = [(offset 200, 200),      
         (200, offset 200),
         (200, offset (-200)),
         (offset 200, - 200),
         (offset (- 200), - 200),
         (- 200, offset (- 200)),
         (- 200, offset 200),
         (offset (- 200), 200)]

-- | The pūtahi is the center location of the board (index 8)
putahi :: Point
putahi = (0, 0)

-- | Visual size of game pieces in pixels
pieceSize :: Float
pieceSize = 80

-- | Initial game state: 4 black pieces on left outer positions, 
-- 4 white pieces on right outer positions, center is empty
initialGameState :: GameState
initialGameState = GameState { locations = [Just Black, Just Black, Just Black, Just Black,
                                            Just White, Just White, Just White, Just White, Nothing],
                               move = Black }

-- | Determines which location (if any) was clicked based on mouse coordinates
-- Returns the index (0-8) of the clicked location, or Nothing if no location was clicked
clickedLocation :: Point -> Maybe Int
clickedLocation (x, y) = find (\i -> let (x', y') = if i == 8 then putahi else kewai !! i
                                     in sqrt ((x - x') ** 2 + ((y - y') ** 2)) <= pieceSize / 2) [0 .. 8]

-- | Retrieves the stone value at a specific location index in the game state
getVal :: GameState -> Int -> LocationValue
getVal g i = locations g !! i

-- | Checks if a clicked location is a valid move for the current player
-- Rules:
-- - Can only move your own pieces
-- - Can move from center (index 8) anytime
-- - Can move from outer position if: center is empty OR an adjacent position is empty
-- - Cannot move if your own pieces are on both adjacent sides (blocked)
movableLocation :: Maybe Int -> GameState -> Maybe Int
movableLocation (Just 8) g | getVal g 8 == Just (move g) = Just 8
movableLocation (Just i) g | getVal g i == Just (move g) &&
                             -- Must be next to an open location.
                             (isNothing (getVal g 8) ||
                              isNothing (getVal g ((i + 1) `mod` 8)) ||
                              isNothing (getVal g ((i - 1) `mod` 8))) &&
                             -- Cannot move if a player's own kewai are on both sides.
                             not (getVal g ((i + 1) `mod` 8) == Just (move g) &&
                                  getVal g ((i - 1) `mod` 8) == Just (move g)) = Just i
movableLocation _ _ = Nothing 

-- | Updates a location in the board state with a new value
-- Uses list indexing to replace the value at position n
setLocation :: Int -> LocationValue -> [LocationValue] -> [LocationValue]
setLocation _ _ [] = []
setLocation n val (x:xs)
  | n == 0 = val:xs
  | otherwise = x:setLocation (n - 1) val xs

-- | Determines the next player's turn (alternates between Black and White)
nextMove :: GameState -> Stone
nextMove g | move g == Black = White
           | otherwise = Black

-- | Checks if the current player has lost (cannot make any valid moves)
gameOver :: GameState -> Bool
gameOver g = isNothing $ find (\i -> getVal g i == Just (move g) &&
             isJust (movableLocation (Just i) g)) [0..8]

-- | Renders a single game piece at the given coordinates
-- White pieces are rendered in white, Black pieces in black
renderPiece :: LocationValue -> Point -> Picture
renderPiece loc (x, y) = Translate x y $ Color (if loc == Just White then white else black) $
                           ThickCircle 1 pieceSize

-- | Renders a location on the board with its piece (if any)
renderLocation :: Int -> LocationValue -> Picture
renderLocation _ Nothing = Text ""
renderLocation 8 loc = renderPiece loc putahi
renderLocation i loc = renderPiece loc $ kewai !! i

-- | Renders the status bar showing whose turn it is or who won
renderStatusBar :: GameState -> [Picture]
renderStatusBar g =  Translate (- 250) (- 290)
  (Color white $ Polygon [(0, 0), (0, 30), (500, 30), (500, 0)])
  : [Translate (- 240) (- 280)
       $ Scale 0.1 0.1
           $ Text
               $ if gameOver g then
                     show (nextMove g) ++ " wins!"
                 else
                     "Move: " ++ show (move g)]

-- | Renders the entire game board including:
-- - Octagonal outline connecting the 8 outer positions
-- - Lines from each outer position to the center
-- - All game pieces at their locations
-- - Status bar with game state
renderBoard :: GameState -> Picture 
renderBoard g = Pictures $ [lineLoop kewai] ++
                  map (\i -> Line [kewai !! i, putahi]) [0..7] ++
                  map (\i -> renderLocation i (getVal g i)) [0..8] ++
                  renderStatusBar g

-- | Extracts the clicked location from a mouse click event
-- Returns the location index if left mouse button was clicked, Nothing otherwise
findLocation :: Maybe G.Event -> Maybe Int
findLocation (Just (G.EventKey (G.MouseButton G.LeftButton) G.Down _ p)) = clickedLocation p
findLocation _ = Nothing

-- | Processes a piece movement in response to a mouse click event
-- If the clicked location is a valid move, swaps the piece with the empty location
-- and updates the game state to reflect the next player's turn
movePiece :: Maybe G.Event -> GameState -> GameState
movePiece e g = case movableLocation (findLocation e) g of
                  Just i  -> g { locations = setLocation openLocation (getVal g i) $
                                             setLocation i Nothing (locations g),
                                 move = nextMove g }
                              where openLocation = head $ filter (isNothing . getVal g) [0..8]
                  Nothing -> g

-- | Reactive network that processes events and renders the game
-- Uses FRP (Functional Reactive Programming) with Elerea for event handling
-- Combines input events into game state updates and renders the result
network :: SignalGen (Signal (Maybe Float))
        -> SignalGen (Signal (Maybe G.Event))
        -> SignalGen (Signal Picture)
network _ e =  do
  inputEvent <- e
  newGame <- transfer initialGameState movePiece inputEvent
  return $ renderBoard <$> newGame

-- | Main entry point
-- Sets up IO channels for tick and input events
-- Creates a reactive network to handle game updates
-- Launches the Gloss window with game loop at 100 FPS
main :: IO ()
main = do
  -- External channels for event communication
  (tickGen, tickSink) <- external Nothing
  (inputGen, inputSink) <- external Nothing
    
  -- Start the reactive network and get the picture update function
  recomputePicture <- start $ do network tickGen inputGen
    
  -- Get the initial game picture
  initialPicture <- recomputePicture
    
  -- Launch the game window with event handlers
  G.playIO (InWindow "Mu Torere" (500, 580) (0, 0))
    (greyN 0.85)
    100
    initialPicture
    return
    -- Handle input events (mouse clicks)
    (\e _ -> do
        inputSink $ Just e
        recomputePicture)
    -- Handle time ticks
    (\t _ -> do
        tickSink $ Just t
        recomputePicture)
        
