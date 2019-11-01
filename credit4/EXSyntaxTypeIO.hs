main = do
    putStr   "Pick a natural number less than 10,"
    putStrLn "and I'll try to guess what it is!"

    let loop :: Int -> Int -> IO ()
        loop a b
         | a==b      = putStrLn ("The number is "++show a)
         | otherwise = do
                        let try = (a + b) `div` 2
                        putStrLn ("Is it more than "
                                 ++ show try
                                 ++ " ? (True/False)")
                        isGreater <- getLine
                        if (read isGreater)
                             then loop (try + 1) b
                             else loop a       try
    loop 1 10
    putStrLn "Thanks for playing"

a) do blocks
  kaksi do-blokkia
  Ensimmäinen riviltä 1 loppuun (do -> putStrLn "Thanks for playing")
  Toinen ekan sisällä, riviltä 8 riville 16 (do -> else loop a try)

b) statements in each do block

Ensimmäisen do-blokin statementit:
1. putStr "Pick a natural number less than 10,"
2. putStrLn "and I'll try to guess what it is!"
3. loop a b
4. loop 1 10
5. putStrLn "Thanks for playing"

Toisen
1. let try = (a + b) `div` 2
2. putStrLn ("Is it more than " jne
3. isGreater <- getLine
4. if (read isGreater) ...

c) statement kinds

Ensimmäisen
1. action
2. action
3. bind-statement
4. bind-statement
5. action

Toinen
1. pure binding
2. action
3. bind-statement
4. bind-statement (koska kutsuu loop-funktiota, joka on bind-statement?)

d) types for each statement

Ensimmäinen
1. String -> IO ()
2. String -> IO ()
3. Int -> Int -> IO ()
4. Int -> Int -> IO ()
5. String -> IO ()

Toinen
1. Int -> Int -> Int
2. String -> IO ()
3. IO String
4. read :: Read a => String -> a
   isGreater :: Bool 
