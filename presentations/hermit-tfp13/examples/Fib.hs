import Criterion.Main

main = defaultMain [bench "20" $ whnf fib 20]

fib :: Int -> Int
fib n = if n < 2
        then 1
        else fib (n - 1) + fib (n - 2)
