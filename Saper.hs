import Control.Monad.State (StateT, get, modify, lift, evalStateT, execState, runState, put)
import Text.Read (readMaybe)
-- import System.Process (system)

import Board hiding (put)
import qualified Board as Array (put)
import Move
import Generate

-------------------------------------------------------------------------------------

-- pobranie liczby z ograniczeniem dolnym
getIntL :: String -> Int -> IO Int
getIntL info lowest = do
    putStrLn info
    ns <- getLine
    case readMaybe ns :: Maybe Int of
        Nothing -> getIntL info lowest
        Just n ->
            if lowest <= n
                then return n
                else getIntL info lowest

-- pobranie liczby nieujemnej z ograniczeniem górnym
getIntH :: String -> Int -> IO Int
getIntH info heighest = do
    putStrLn info
    ns <- getLine
    case readMaybe ns :: Maybe Int of
        Nothing -> getIntH info heighest
        Just n ->
            if 0 <= n && n <= heighest
                then return n
                else getIntH info heighest

-------------------------------------------------------------------------------------

-- zainicjowanie gry na podstawie pierwszego ruchu
initiate :: Int -> Int -> Int -> Move -> IO Saper
initiate h w mines (x,y,c) = do
    sap <- generateSaper h w mines (x,y)
    return $ execState (move (x,y,c)) sap

-- clear = system "clear"
clear = putStr "\ESCc"

play :: StateT Saper IO Result
play = do
    lift clear
    board <- get
    if zostalo_dobrych board == 0
    then do
        (lift.putStrLn.bigShow) board
        return Win
    else do
        lift $ print board
        (x,y,c) <- lift $ getMove (height board) (width board)
        let (res,newBoard) = runState (move (x,y,c)) board
        put newBoard
        if res == Lose
        then do
            lift clear
            (lift.putStrLn.bigShow) newBoard
            return Lose
        else     
            play

-- typ oraz funkcje potrzebne do gromadzenia statystyk
type Stats = Array Int

win :: Int -> Stats -> Stats
win t stats = Array.put (0,t) (stats!(0,t) + 1) stats

lose :: Int -> Stats -> Stats
lose t stats = Array.put (1,t) (stats!(1,t) + 1) stats

koniec :: StateT Stats IO ()
koniec = do
    lift $ putStrLn "Gramy jeszcze raz? (y/n)"
    resp <- lift $ getLine
    case resp of
        "y" -> game
        "n" -> pozeganie
        _   -> koniec

pozeganie :: StateT Stats IO ()
pozeganie = do
    lift clear
    stats <- get
    lift $ do 
        putStrLn "Dzięki za grę!"
        putStrLn ""
        putStrLn "Oto Twoje statystyki:"
        putStrLn ""
        putStrLn $ "Rozegrane gry: " ++ show (sum $ concat stats)
        putStrLn ""
        sequence $ map putStrLn ["Wygrane gry w trybie " ++ show x ++ ": " ++ show (stats!(0,x)) | x <- [0..3]]
        putStrLn ""
        sequence $ map putStrLn ["Przegrane gry w trybie " ++ show x ++ ": " ++ show (stats!(1,x)) | x <- [0..3]]
        return ()

------------------------------------------------------------------------------------

game :: StateT Stats IO ()
game = do
    t <- lift $ getIntH "\nPodaj tryb (0, 1, 2 lub 3):" 3
    g <- case t of
        0 -> do
            m <- lift $ getFirstMove 9 9
            lift $ putStrLn "Inicjuję grę..."
            lift $ initiate 9 9 10 m
        1 -> do
            m <- lift $ getFirstMove 16 16
            lift $ putStrLn "Inicjuję grę..."
            lift $ initiate 16 16 40 m
        2 -> do
            m <- lift $ getFirstMove 30 16
            lift $ putStrLn "Inicjuję grę..."
            lift $ initiate 30 16 99 m
        3 -> do
            h <- lift $ getIntL "Podaj wysokość (co najmniej 2):" 2 
            w <- lift $ getIntL "Podaj szerokość (co najmniej 2):" 2 
            mines <- lift $ getIntH ( "Podaj liczbę min (maksymalnie " ++ show (h*w-1) ++ "):" ) (h*w-1)
            m <- lift $ getFirstMove h w
            lift $ putStrLn "Inicjuję grę..."
            lift $ initiate h w mines m

    res <- lift $ evalStateT play g
    case res of
        Win  -> modify (win t)  >> lift (putStrLn "Wygrałeś :D\n")
        Lose -> modify (lose t) >> lift (putStrLn "Przegrałeś :<\n")
    koniec

------------------------------------------------------------------------------------

main = do
    putStrLn "Witaj!"
    putStrLn ""
    putStrLn "Zasady są proste:"
    zasadyPoruszania
    putStrLn "Zawsze przed podaniem ruchu możesz wpisać \"help\", żeby zobaczyć przypomnienie powyższych zasad."
    tryby
    putStrLn ""
    putStrLn "Plansza zostanie wylosowana po podaniu pierwszego ruchu (musi on być odkrywający)."
    putStrLn ""
    putStrLn "To wszystko! Miłej zabawy!"
    evalStateT game [[0,0,0,0],[0,0,0,0]]

tryby :: IO ()
tryby = do
    putStrLn ""
    putStrLn "Dostępne są 4 tryby:"
    putStrLn "Łatwy   (0): plansza 9x9, 10 bomb,"
    putStrLn "Średni  (1): plansza 16x16, 40 bomb,"
    putStrLn "Trudny  (2): plansza 30x16, 99 bomb,"
    putStrLn "Dowolny (3): Ty podajesz rozmiar planszy i liczbę bomb."