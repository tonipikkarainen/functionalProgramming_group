module Main where

{--main :: IO ()
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
    putStrLn "Thanks for playing"--}

main :: IO ()
main = 
    putStr "Pick a natural number less than 10," >>
    putStrLn "and I'll try to guess what it is!" >>
    let 
        loop a b 
            | a==b      = putStrLn ("The number is "++show a)
            | otherwise = 
                        let 
                            try = (a + b) `div` 2
                        in 
                            putStrLn ("Is it more than "
                                     ++ show try
                                     ++ " ? (True/False)") >>
                            getLine >>= \ isGreater -> 
                            if (read isGreater)
                                 then loop (try + 1) b
                                 else loop a       try
    in 
        loop 1 10 >>
        putStrLn "Thanks for playing"
