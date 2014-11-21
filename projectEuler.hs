-- LogicalLagamorph
-- Code@Rabbit12

import Data.Int
-- problem 1
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

problem1 = sum [ x | x <-[1..999], ((x `mod` 3) == 0) ||  ((x `mod` 5) == 0)] 

fibSeq =  fib 1 1
fib :: Int -> Int -> [Int]
fib a b
	| b < 4000000 = a:fib b (a+b)
	| otherwise   = [a]

problem2 = sum [ x | x<- fibSeq, (x `mod` 2) == 0]

-- Problem 3
--The prime factors of 13195 are 5, 7, 13 and 29.
--What is the largest prime factor of the number 600851475143 ?
isPrime' :: Integer -> Integer -> Bool
isPrime' a b
	| a==1            = True
	| (b `mod` a) == 0 = False
	| otherwise = isPrime' (a-1) b

isPrime :: Integer -> Bool
isPrime a = isPrime' (squareRoot a) a

(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters

problem3 = last [ x | x <-[2..(squareRoot 600851475143)] , (600851475143 `mod` x == 0 ), (isPrime x == True)]

-- Problem 4
--A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
--Find the largest palindrome made from the product of two 3-digit numbers.

-- 100 * 100 = 10,000

intToString :: Integer -> String
intToString x = show x :: String

stringToInt :: String ->Integer
stringToInt x = read x :: Integer

reverseString :: String -> String
reverseString x
	| x == [] = x
	|otherwise = last x: reverseString (init x)

palindromeHelper :: Integer -> Integer
palindromeHelper x = stringToInt (reverseString (intToString x) )

palindromeTest :: Integer -> Bool
palindromeTest input = input == palindromeHelper input

problem4 = maximum [ x*y | x <- [100..999], y<- [100..999] ,palindromeTest (x*y)]


-- Problem 5

--2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
--What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

modTest x = and [ x `mod` y == 0 | y<-[1..20] ]

problem5Solver :: Integer -> Integer
problem5Solver x
	|modTest x == True = x
	|otherwise = problem5Solver (x+1)
-- the answer is 232792560, bruteforce takes roughly 20 mins
problem5 = (problem5Solver 1)

-- problem 6
--The sum of the squares of the first ten natural numbers is,

--12 + 22 + ... + 102 = 385
--The square of the sum of the first ten natural numbers is,

--(1 + 2 + ... + 10)2 = 552 = 3025
--Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

--Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
problem6 =((sum [ x | x <-[1..100]])^2) - (sum [x^2 | x <-[1..100]])

-- problem 7 
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
--What is the 10 001st prime number?

-- Sieve of Eratosthenes method. I don't believe it actually has the log n * log n time that is advertise in the psuedo code. 
-- Probably due to my implementation of it.

primes = [x |x <-[2..],  isPrime x]
problem7 = (last ( take 10001 [x |x <-[2..] , isPrime x]))



problem9 = [ x*y*z | x<-[0..1000], y<-[(x+1)..(1000-x)], z<-[(y+1)..(1000-y)] , ((x^2+y^2) == z^2) && (x+y+z == 1000)]

problem10 = sum (filter isPrime [2..2000000])

