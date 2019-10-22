sequence :: [IO a] -> IO [a]
sequence [] = pure []
sequence (op:ops) =
    op >>= \x -> 
    (sequence ops) >>= \xs -> 
    return (x:xs)