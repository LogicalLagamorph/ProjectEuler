-- LogicalLagamorph
-- Code@Rabbit12

import Data.Int

{- 

Problem 1

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.

-}
problem1 = sum [ x | x <-[1..999], ((x `mod` 3) == 0) ||  ((x `mod` 5) == 0)] 

fibSeq =  fib 1 1
fib :: Int -> Int -> [Int]
fib a b
	| b < 4000000 = a:fib b (a+b)
	| otherwise   = [a]

problem2 = sum [ x | x<- fibSeq, (x `mod` 2) == 0]


{-

Problem 3

The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ?

-}
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

{-

Problem 4

A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
Find the largest palindrome made from the product of two 3-digit numbers.

-}

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

{-

Problem 5

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

Note : The answer is 232792560, bruteforce takes roughly 20 mins
-}

modTest x = and [ x `mod` y == 0 | y<-[1..20] ]

problem5Solver :: Integer -> Integer
problem5Solver x
	|modTest x == True = x
	|otherwise = problem5Solver (x+1)

problem5 = (problem5Solver 1)

{-

Problem 6

The sum of the squares of the first ten natural numbers is,
12 + 22 + ... + 102 = 385
The square of the sum of the first ten natural numbers is,(1 + 2 + ... + 10)2 = 552 = 3025.
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

-}

problem6 =((sum [ x | x <-[1..100]])^2) - (sum [x^2 | x <-[1..100]])

{-

Problem 7 

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
What is the 10 001st prime number?

-}

problem7 = (last ( take 10001 [x |x <-[2..] , isPrime x]))

{-

Problem 9 

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which, a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52. 
There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

-}
problem9 = [ x*y*z | x<-[0..1000], y<-[(x+1)..(1000-x)], z<-[(y+1)..(1000-y)] , ((x^2+y^2) == z^2) && (x+y+z == 1000)]


{-
problem 10

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
Find the sum of all the primes below two million.

Note: Takes a long time, the isPrime function is slow it took roughly 30 minutes to calculate, answer was 142913828922.
-}
problem10 = sum $ filter isPrime [2..2000000]

{-
problem 11


In the 20×20 grid below, four numbers along a diagonal line have been marked in red.

08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 -89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 -94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 -97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 -87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48

The product of these numbers is 26 × 63 × 78 × 14 = 1788696.
What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?

	They have been marked with a '-'. No mathmatics were used to get the solution I just guessed(it was my first one too lol).
	I simply found a chain of large numbers and used a calculor to find the product.

Note: Added code below because I was annoyed that two comment blocks where toching
-}

problem11 = [87,97,94,89]

{-

Problem12

The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.
The first ten terms would be: 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
Let us list the factors of the first seven triangle numbers:

    1: 1
     3: 1,3
     6: 1,2,3,6
    10: 1,2,5,10
    15: 1,3,5,15
    21: 1,3,7,21
    28: 1,2,4,7,14,28

We can see that 28 is the first triangle number to have over five divisors.

What is the value of the first triangle number to have over five hundred divisors?

-}
factor' :: Integer -> Integer-> [Integer]
factor' number current
	|current > number 			= []
	| number `mod` current == 0  = current: factor' number (current+1)
	|otherwise = factor' number (current+1)

factor :: Integer -> [Integer]
factor x = factor' x 1

problem12' :: Integer -> Integer
problem12' x 
	| (length $ factor $ sum ([ i | i<-[1..(squareRoot x)] ]++[x]) ) == 500 = x
	| otherwise = problem12' (x+1)

problem12 = problem12' 0
