module Move where

import Text.Read (readMaybe)
import Control.Monad.State (State, get, runState)
import qualified Control.Monad.State as State (put)

import Board
import Generate

-------------------------------------------------------------------------------------
-- MODYFIKOWANIE PLANSZY NA PODSTAWIE PODANEGO RUCHU

properMoves = "oxOr"

getMove :: Int -> Int -> IO Move
getMove h w = do
    putStrLn "Twój ruch:"
    line <- getLine
    case words line of
        ["help"] -> zasadyPoruszania >> getMove h w
        [x0,y0,[c]] -> case sequence (map readMaybe [x0,y0]) :: Maybe [Int] of
                        Nothing -> getMove h w
                        Just [x,y] -> if 0 <= x && x <= h && 0 <= y && y <= w && elem c properMoves
                            then return (x,y,c)
                            else getMove h w
        _ -> getMove h w

getFirstMove :: Int -> Int -> IO Move
getFirstMove h w = do
    m <- getMove h w
    case m of
        (_,_,'o') -> return m
        (_,_,'O') -> return m
        _ -> getFirstMove h w

addChar :: Char -> (Int,Int) -> Move
addChar c (x,y) = (x,y,c)

move :: Move -> State Saper Result
move (x,y,c) = do
    (Saper t d n) <- get

    let p = t!(x,y) 
    let h = length t 
    let w = length $ t!!0

    case (c, bomby p, stan p) of
        -- Zaznaczenie/odznacczenie bomby
        ('x',_,Hidden) -> State.put ( Saper ( put (x,y) (Pole (bomby p) Marked) t ) d (n-1) ) >> return Continue
        ('x',_,Marked) -> State.put ( Saper ( put (x,y) (Pole (bomby p) Hidden) t ) d (n+1) ) >> return Continue
        
        -- Odkrycie pola ukrytego
        ('o',9,Hidden) -> State.put ( Saper ( put (x,y) (Pole (bomby p) Exploaded) t ) d n ) >> return Lose
        ('o',0,Hidden) -> do
                            let (res_tab,sap) = (runState $ sequence $ map (move.(addChar 'O')) (sasiedzi (x,y) h w)) ( Saper ( put (x,y) (Pole (bomby p) Shown) t ) (d-1) n )
                            State.put sap
                            if any (==Lose) res_tab
                            then return Lose
                            else return Continue
        ('o',_,Hidden) -> State.put ( Saper ( put (x,y) (Pole (bomby p) Shown) t ) (d-1) n ) >> return Continue
        
        -- Odkrycie pola ukrytego lub zaznaczonego
        ('O',9,  _   ) -> State.put ( Saper ( put (x,y) (Pole (bomby p) Exploaded) t ) d n ) >> return Continue
        ('O',0,Hidden) -> do
                            let (res_tab,sap) = (runState $ sequence $ map (move.(addChar 'O')) (sasiedzi (x,y) h w)) ( Saper ( put (x,y) (Pole (bomby p) Shown) t ) (d-1) n )
                            State.put sap
                            if any (==Lose) res_tab
                            then return Lose
                            else return Continue
        ('O',0,Marked) -> do
                            let (res_tab,sap) = (runState $ sequence $ map (move.(addChar 'O')) (sasiedzi (x,y) h w)) ( Saper ( put (x,y) (Pole (bomby p) Shown) t ) (d-1) (n+1) )
                            State.put sap
                            if any (==Lose) res_tab
                            then return Lose
                            else return Continue
        ('O',_,Hidden) -> State.put ( Saper ( put (x,y) (Pole (bomby p) Shown) t ) (d-1) n ) >> return Continue
        ('O',_,Marked) -> State.put ( Saper ( put (x,y) (Pole (bomby p) Shown) t ) (d-1) (n+1) ) >> return Continue
        
        -- Odkrycie pól dookoła oznaczonego pola
        ('r',m,Shown)  -> if m == (length.(filter ((== Marked).stan)).(map (t!)) $ sasiedzi (x,y) h w)
                            then do
                                let (res_tab,sap) = (runState $ sequence $ map (move.(addChar 'o')) (sasiedzi (x,y) h w)) ( Saper t d n )
                                State.put sap
                                if any (==Lose) res_tab
                                then return Lose
                                else return Continue 
                            else return Continue
        
        -- Jakakolwiek inna kombinacja nic nie zmienia
        _  -> return Continue

-------------------------------------------------------------------------------------

zasadyPoruszania :: IO ()
zasadyPoruszania = do
    putStrLn ""
    putStrLn "Ruch polega na podaniu koordynatów pola oraz działania, np."
    putStrLn "3 4 o"
    putStrLn "lub"
    putStrLn "7 5 x"
    putStrLn ""
    putStrLn "Dostępne działania to:"
    putStrLn "x -- zaznaczenie/odznaczenie pola,"
    putStrLn "o -- odkrycie ukrytego pola,"
    putStrLn "O -- odkrycie ukrytego lub zaznaczonego pola,"
    putStrLn "r -- odkrycie niezaznaczonych pól wokół odkrytego pola, którego wartość jest równa liczbie zaznaczonych dookoła pól."
    putStrLn ""
    putStrLn "Wiersze i kolumny indeksowane są od 0."
    putStrLn "Najpierw podajemy wiersz, potem kolumnę."
    putStrLn ""