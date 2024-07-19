module Board where

import Data.List (intercalate)

-------------------------------------------------------------------------------------
-- DEFINIUJEMY POTRZEBNE TYPY

type Array a = [[a]]
data Saper = Saper {pola :: Array Pole, zostalo_dobrych :: Int, zostalo_bomb :: Int}
data Pole = Pole {bomby :: Int, stan :: Stan}
data Stan = Hidden | Marked | Shown | Exploaded deriving Eq
type Move = (Int,Int,Char)
data Result = Win | Lose | Continue deriving Eq

(!) :: Array a -> (Int,Int) -> a
(!) t (x,y) = (t!!x)!!y

mapArray :: (a -> b) -> Array a -> Array b
mapArray f = map (map f)

-- zmiana jednego pola
put :: (Int,Int) -> a -> Array a -> Array a
put (x,y) p t = r1++(c1++(p:c2)):r2 where
    r1 = take x t
    r2 = drop (x+1) t    
    c1 = take y (t!!x) 
    c2 = drop (y+1) (t!!x)

height :: Saper -> Int
height s = length $ pola s

width :: Saper -> Int
width s = length $ (pola s)!!0
        
-------------------------------------------------------------------------------------
-- DEFINIUJEMY SPOSOBY WYŚWIETLANIA PLANSZY

-- wyświetlanie wartości liczbowych pól na odpowiednie kolory
-- możemy to "dezaktywować", zakomentowując/odkomentowując odpowiednie linie
color :: Int -> String
-- color 0 = "\ESC[48;2;255;255;255m \ESC[0m" -- wyświetlenie 0 jako białego prostokąta
color 1 = "\ESC[38;2;0;0;255m1\ESC[0m"
color 2 = "\ESC[38;2;0;255;0m2\ESC[0m"
color 3 = "\ESC[38;2;255;0;0m3\ESC[0m"
color 4 = "\ESC[38;2;0;0;139m4\ESC[0m"
color 5 = "\ESC[38;2;128;0;0m5\ESC[0m"
color 6 = "\ESC[38;2;64;224;208m6\ESC[0m"
color 7 = "\ESC[38;2;0;0;0m7\ESC[0m"
color 8 = "\ESC[38;2;128;128;128m8\ESC[0m"
color n = show n 

-- pokolorowanie Stringa na czerwono
red :: String -> String
red s = "\ESC[38;2;255;0;0m" ++ s ++ "\ESC[0m"
-- red = id

-- wyświetlanie pola w trakcie gry
instance Show Pole where
    show (Pole _ Hidden) = " "
    show (Pole _ Marked) = red "⚑"
    show (Pole _ Exploaded) = error "Znaleziono błąd oprogramowania! Eksplozja nie zakończyła się porażką!" -- "*"
    show (Pole n Shown)  = color n

-- wyświetlanie pola na koniec gry
showWholePole :: Pole -> String
showWholePole (Pole _ Exploaded) = red "*"
showWholePole (Pole 9 Marked) = red "⚑"
showWholePole (Pole 9 Hidden) = red "x"
showWholePole (Pole n Shown) = color n
showWholePole (Pole n Marked) = "⚑"
showWholePole (Pole n Hidden) = show n

-- określona liczba spacji
tabulator :: Int -> String
tabulator n = take n $ repeat ' '

-- dodaje tyle spacji, aby całość miała podaną długość
fit :: Show a => Int -> a -> String
fit size x = tabulator (size - (length.show) x) ++ show x

-- "specyfikacja" fit dla typu Pole
fitPole :: Int -> Pole -> String
fitPole size x = tabulator (size - 1) ++ show x

-- "specyfikacja" fit dla typu Pole na koniec gry
fitPoleKoniec :: Int -> Pole -> String
fitPoleKoniec size x = tabulator (size - 1) ++ showWholePole x

-- rozmiar pola oznaczającego indeks wiersza
rowIndexSize :: Saper -> Int
rowIndexSize s = (length.show) $ height s - 1

-- rozmiar "komórki" planszy
cellSize :: Saper -> Int
cellSize s = (length.show) $ width s - 1

-- wyświetlanie planszy w trakcie gry; pokazujemy numry kolumn
instance Show Saper where
    show s = "Zostało " ++ (show.zostalo_bomb) s ++ " bomb\n\n" ++
             (tabulator $ rowIndexSize s) ++ " " ++
             intercalate " " (map (fit $ cellSize s) [0..width s - 1]) ++ "\n" ++
             showSaper s 0

-- wyświetlanie kolejnych wierszy planszy w trakcie gry
showSaper :: Saper -> Int -> String
showSaper s i | i < height s = (fit $ rowIndexSize s) i ++ " " ++
                               intercalate " " (map (fitPole $ cellSize s) ((pola s)!!i)) ++ " " ++ show i ++ "\n" ++
                               showSaper s (i+1)
              | otherwise = (tabulator $ rowIndexSize s) ++ " " ++
                            intercalate " " (map (fit $ cellSize s) [0..width s - 1]) ++ "\n"

-- wyświetlanie planszy na koniec gry; pokazujemy numery kolumn
bigShow :: Saper -> String
bigShow s = "Koniec!\n\n" ++
             (tabulator $ rowIndexSize s) ++ " " ++
             intercalate " " (map (fit $ cellSize s) [0..width s - 1]) ++ "\n" ++
             showWholeSaper s 0

-- wyświetlanie kolejnych wierszy planszy na koniec gry
showWholeSaper :: Saper -> Int -> String
showWholeSaper s i | i < height s = (fit $ rowIndexSize s) i ++ " " ++
                                    intercalate " " (map (fitPoleKoniec $ cellSize s) ((pola s)!!i)) ++ " " ++ show i ++ "\n" ++
                                    showWholeSaper s (i+1)
                   | otherwise = (tabulator $ rowIndexSize s) ++ " " ++
                                 intercalate " " (map (fit $ cellSize s) [0..width s - 1]) ++ "\n"