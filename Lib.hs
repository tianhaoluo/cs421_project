module Lib where

import Prelude hiding (foldr, sum, product, length, (.), map, iterate,
                       sqrt, zipWith, maximum, minimum, take)

import Data.List (intersperse)
import qualified Data.List as D (sort, sortBy)
import qualified Prelude as P (map, foldr, sum, length, maximum,
                               minimum, take)


-- Part 3: Gluing functions together

data List a = Cons a (List a) | Nil deriving (Eq,Show)

l1 = Cons 1 (Cons 2 (Cons 3 Nil))
l2 = Cons 4 (Cons 5 (Cons 6 Nil))

l3 = Cons True (Cons False (Cons True Nil))
l4 = Cons True (Cons True Nil)
l5 = Cons False (Cons False Nil)

sum = foldr (+) 0

foldr f x Nil = x
foldr f x (Cons a l) = f a (foldr f x l)

anytrue = foldr (||) False

alltrue = foldr (&&) True

append a b = foldr Cons b a

length = foldr count 0 where count a n = n + 1

doubleall = foldr doubleandcons Nil where doubleandcons n list = Cons (2*n) list

double n = 2*n
(f.g) h = f (g h)
 
map f = foldr (Cons . f) Nil

doubleall2 = map double

m1 = Cons l1 (Cons l2 Nil)

data Tree a = Node a (List (Tree a)) deriving (Eq,Show)

tree1 = Node 1 
       (Cons (Node 2 Nil)
                (Cons (Node 3
                    (Cons (Node 4 Nil)Nil))Nil
                       ))

foldTree f g a (Node label subtrees) =
    f label (foldr (g . foldTree f g a) a subtrees)

sumTree = foldTree (+) (+) 0

labels = foldTree Cons append Nil

mapTree f = foldTree (Node . f) Cons Nil

-- Part 4. Gluing Programs Together

--- Newton-Raphson Square Roots

next n x = (x + n/x)/2

repeat1 f a = Cons a (repeat1 f (f a))

within eps (Cons a (Cons b rest))
			| abs(a-b) <= eps = b
			| otherwise = within eps (Cons b rest)

sqrt a0 eps n = within eps (repeat1 (next n) a0)

relative eps (Cons a (Cons b rest))
	| abs (a/b-1) <= eps = b
	| otherwise = relative eps (Cons b rest)


relativeSqrt a0 eps n = relative eps (repeat1 (next n) a0)

---- Numerical differentiation

easyDiff f x h = (f (x+h) - f x)/h
differentiate h0 f x = map (easyDiff f x) (repeat1 halve h0) where halve x = x/2

elimError n (Cons a (Cons b rest)) =  Cons ((b*(2**n)-a)/(2**n-1)) (elimError n (Cons b rest))

order (Cons a (Cons b (Cons c rest))) = fromIntegral(round(logBase 2 ((a-c)/(b-c)-1)))

improve s = elimError (order s) s

super s = map second (repeat1 improve s) where second (Cons a (Cons b rest)) = b

---- Numerical integration
easyIntegrate f a b = (f a + f b)*(b-a)/2

zipWith f (Cons a s) (Cons b t) = Cons (f a b) (zipWith f s t)

integrate f a b = integ f a b (f a) (f b)
integ f a b fa fb = 
	Cons ((fa+fb)*(b-a)/2) (zipWith (+) (integ f a m fa fm) (integ f m b fm fb)) 
	where m = (a+b)/2
	      fm = f m


