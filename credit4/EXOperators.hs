
sequence1 :: [IO a] -> IO [a]
sequence1 [] = pure []
sequence1 (op:ops) = do
    x <- op
    xs <- sequence1 ops
    return (x:xs)

-- sequence with >> and >>=
sequence2 :: [IO a] -> IO [a]
sequence2 [] = pure []
sequence2 (op:ops) =
         op >>= \x -> sequence2 ops >>= \xs ->
         return (x:xs)

--sequence3 :: [IO a] -> IO [a]
--sequence3 [] = pure []
--sequence3 (op:ops) = do
--  op >> sequence ops 
