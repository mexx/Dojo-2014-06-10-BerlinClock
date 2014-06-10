module Main where

import Text.Printf

generateRow :: Int -> Int -> (Int -> Char) -> String
generateRow segments value on =
    let countOn = value `mod` (segments + 1) in
    [if x <= countOn then (on x) else 'O' | x <- [1..segments]]
    
berlinClock :: Int -> Int -> Int -> String
berlinClock hh mm ss =
    printf "%s\n%s\n%s\n%s\n%s"
        (generateRow 1 (ss + 1) (\_ -> 'Y'))
        (generateRow 4 (hh `div` 5) (\_ -> 'R'))
        (generateRow 4 hh (\_ -> 'R'))
        (generateRow 11 (mm `div` 5) (\x -> if x `mod` 3 == 0 then 'R' else 'Y'))
        (generateRow 4 mm (\_ -> 'Y'))
    
main :: IO ()
main = do
    putStrLn "00:00:00"
    putStrLn (berlinClock 00 00 00)
    putStrLn "13:17:01"
    putStrLn (berlinClock 13 17 01)
    putStrLn "23:59:59"
    putStrLn (berlinClock 23 59 59)
    putStrLn "24:00:00"
    putStrLn (berlinClock 24 00 00)
