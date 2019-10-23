module Main where

sequ :: [IO a] -> IO [a]
sequ [] = pure []
-- sequ (op:ops) = do
    -- x <- op
    -- xs <- sequ ops
    -- return (x:xs)
    
sequ (op:ops) =
    op >>= (\op -> sequ ops >>= (\ops -> return (op:ops)))

main :: IO ()
main = do
  putStrLn "hello world"
  
