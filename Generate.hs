module Generate where

import System.Random (StdGen, newStdGen, randomR)
import Control.Monad.State (State, get, put, evalState)
import Data.List ((\\))

import Board hiding (put)

-------------------------------------------------------------------------------------                    
-- TWORZENIE PLANSZY

-- koordynaty sąsiednich pól
sasiedzi :: (Int,Int) -> Int -> Int -> [(Int,Int)]
sasiedzi (x,y) h w | x==0 && y==0     = [ (a,b) | a <- [0,1],      b <- [0,1]      ] \\ [(x,y)]        
                   | x==0 && y==w-1   = [ (a,b) | a <- [0,1],      b <- [w-2,w-1]  ] \\ [(x,y)]    
                   | x==h-1 && y==0   = [ (a,b) | a <- [h-2,h-1],  b <- [0,1]      ] \\ [(x,y)]    
                   | x==h-1 && y==w-1 = [ (a,b) | a <- [h-2,h-1],  b <- [w-2,w-1]  ] \\ [(x,y)]
                   | x==0             = [ (a,b) | a <- [0,1],      b <- [y-1..y+1] ] \\ [(x,y)]   
                   | y==0             = [ (a,b) | a <- [x-1..x+1], b <- [0,1]      ] \\ [(x,y)]   
                   | x==h-1           = [ (a,b) | a <- [h-2,h-1],  b <- [y-1..y+1] ] \\ [(x,y)]
                   | y==w-1           = [ (a,b) | a <- [x-1..x+1], b <- [w-2,w-1]  ] \\ [(x,y)]
                   | otherwise        = [ (a,b) | a <- [x-1..x+1], b <- [y-1..y+1] ] \\ [(x,y)]

bomba :: Char -> Int
bomba 'x' = 1
bomba 'o' = 0

convert :: [String] -> Int -> Int -> (Int,Int) -> Pole
convert t h w coord | t!coord == 'x' = Pole 9 Hidden
                    | otherwise = Pole (sum $ map bomba [ t!ind | ind  <- sasiedzi coord h w ]) Hidden

-- tablica podanych rozmiarów o polach zawiąrających ich koordynaty
array :: Int -> Int -> Array (Int,Int)
array h w = [[ (x,y) | y <- [0..w-1]] | x <- [0..h-1]]

-- wybiera m r oznych pozycji z zadanej listy
takeRandomUnique :: Eq a => Int -> State (StdGen,[a],[a]) [a]
takeRandomUnique m = do
    (seed, base, result) <- get
    if length result == m
        then return result
        else do
            let (i,nseed) = randomR (0,length base - 1) seed
            put (nseed, base \\ [base!!i], (base!!i) : result)
            takeRandomUnique m

-- losujemy koordynaty, na których będą bomb
-- tworzymy tablicę złożoną z liter 'x' (bomba) oraz 'o' (bez bomby)
-- następnie konwertujemy ją do właściwej planszy sapera
generateSaper :: Int -> Int -> Int -> (Int,Int) -> IO Saper
generateSaper h w mines mv = do
    stdGen <- newStdGen
    let bombs = evalState (takeRandomUnique $ mines) (stdGen, (concat $ array h w) \\ [mv], [])
    let t = mapArray (\x -> if elem x bombs then 'x' else 'o') (array h w)    
    let mineField = mapArray (convert t h w) (array h w)   
    return $ Saper mineField (h*w - mines) mines