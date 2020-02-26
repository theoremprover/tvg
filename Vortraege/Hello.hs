module Main where

main = do
    putStrLn "Hello!"
    putStrLn $ "fac(7) = " ++ show (fac 7)

fac 1 = 1
fac x = x * fac (x-1)
