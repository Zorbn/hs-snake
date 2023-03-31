{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Array
import qualified Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Monoid
import qualified Debug.Trace as Debug
import Foreign.C.Types
import GHC.Word
import SDL (($=))
import qualified SDL
import qualified SDL as SDl
import System.Random

boardSize = 20 :: Int

cBoardSize = fromIntegral boardSize :: CInt

tileSize = 16 :: Int

cTileSize = fromIntegral tileSize :: CInt

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (cBoardSize * cTileSize, cBoardSize * cTileSize)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window <-
    SDL.createWindow
      "Snake"
      SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight
        }

  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedRenderer,
          SDL.rendererTargetTexture = False
        }

  let loop snake lastMove apple rng = do
        events <- SDL.pollEvents
        let eventPayloads = map SDL.eventPayload events
        let quit = SDL.QuitEvent `elem` eventPayloads
        let pressedKeys =
              mapMaybe
                ( \case
                    SDL.KeyboardEvent e
                      | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                          Just (SDL.keysymScancode $ SDL.keyboardEventKeysym e)
                    _ -> Nothing
                )
                eventPayloads

        let isKeyPressed x = x `elem` pressedKeys
        let [left, right, up, down] =
              map
                isKeyPressed
                [ SDL.ScancodeLeft,
                  SDL.ScancodeRight,
                  SDL.ScancodeUp,
                  SDL.ScancodeDown
                ]

        let horizontalAxis = fromEnum right - fromEnum left
        let verticalAxis = (1 - abs horizontalAxis) * (fromEnum down - fromEnum up)
        let axes = (horizontalAxis, verticalAxis)

        let move =
              if axes == zeroPoint || axes == inversePoint lastMove
                then lastMove
                else axes

        SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 maxBound
        SDL.clear renderer

        SDL.rendererDrawColor renderer $= SDL.V4 0 50 0 maxBound
        SDL.fillRect
          renderer
          ( Just $
              SDL.Rectangle
                (SDL.P $ SDL.V2 0 0)
                ( SDL.V2
                    (cTileSize * cBoardSize)
                    (cTileSize * cBoardSize)
                )
          )

        SDL.rendererDrawColor renderer $= SDL.V4 255 0 0 maxBound
        SDL.fillRect
          renderer
          ( Just $
              SDL.Rectangle
                ( SDL.P $
                    SDL.V2
                      (fromIntegral $ fst apple * tileSize)
                      (fromIntegral $ snd apple * tileSize)
                )
                (SDL.V2 cTileSize cTileSize)
          )

        SDL.rendererDrawColor renderer $= SDL.V4 0 maxBound 0 maxBound
        drawSnake renderer snake

        SDL.present renderer

        threadDelay 100000

        let (nextRng, nextSnake, nextApple) = updateGame move (rng, snake, apple)

        unless quit (loop nextSnake move nextApple nextRng)

  rng <- newStdGen
  loop newSnake zeroPoint zeroPoint rng

  SDL.destroyWindow window
  SDL.quit

type Point = (Int, Int)

type Snake = [Point]

type Apple = Point

diagonalPoint :: Int -> Point
diagonalPoint x = (x, x)

inversePoint :: Point -> Point
inversePoint p = (-(fst p), -(snd p))

zeroPoint :: Point
zeroPoint = (0, 0)

newSnake :: Snake
newSnake = [diagonalPoint $ boardSize `div` 2]

moveSnake :: (Int, Int) -> Snake -> Snake
moveSnake (moveX, moveY) snake =
  let (headX, headY) = head snake
   in (mod (moveX + headX) boardSize, mod (moveY + headY) boardSize) : init snake

growSnake :: Snake -> Snake
growSnake snake = snake ++ [last snake]

snakeHitApple :: Snake -> Apple -> Bool
snakeHitApple (x : xs) apple = x == apple

drawSnake :: SDL.Renderer -> Snake -> IO ()
drawSnake renderer [] = do return ()
drawSnake renderer (x : xs) = do
  SDL.fillRect
    renderer
    ( Just $
        SDL.Rectangle
          ( SDL.P $
              SDL.V2
                (fromIntegral $ fst x * tileSize)
                (fromIntegral $ snd x * tileSize)
          )
          (SDL.V2 cTileSize cTileSize)
    )
  drawSnake renderer xs

isSegmentColliding :: (Foldable t, Eq a) => a -> t a -> Bool
isSegmentColliding seg = foldr (\x -> (||) (seg == x)) False

isSnakeColliding :: Eq a => [a] -> Bool
isSnakeColliding [] = False
isSnakeColliding (x : xs) = isSegmentColliding x xs || isSnakeColliding xs

handleGrowth :: RandomGen a => (a, Snake, (Int, Int)) -> (a, Snake, (Int, Int))
handleGrowth (rng, snake, apple) =
  let (nextAppleX, rng') = randomR (0, boardSize - 1 :: Int) rng
      (nextAppleY, rng'') = randomR (0, boardSize - 1 :: Int) rng'
   in if snakeHitApple snake apple
        then (rng'', growSnake snake, (nextAppleX, nextAppleY))
        else (rng'', snake, apple)

handleMove :: RandomGen a => (Int, Int) -> (a, Snake, Apple) -> (a, Snake, Apple)
handleMove move (rng, snake, apple) = (rng, moveSnake move snake, apple)

handleDeath :: RandomGen a => (a, Snake, Apple) -> (a, Snake, Apple)
handleDeath (rng, snake, apple) =
  ( rng,
    if isSnakeColliding snake
      then newSnake
      else snake,
    apple
  )

updateGame :: RandomGen a => (Int, Int) -> (a, Snake, Apple) -> (a, Snake, Apple)
updateGame move =
  let handleMove' = handleMove move
   in handleGrowth . handleDeath . handleMove'