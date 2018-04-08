module Stream
    ( Stream(..)
    , streamToList
    , streamRepeat
    , streamMap
    , streamIterate
    , streamInterleave
    , nats
    , ruler
    ) where 

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show stream = show $ take 20 (streamToList stream)

-- function to convert stream to infinite list
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- function to generate a stream containing infinite 
-- copies of the given element
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- function which applies a function to every element of a stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap func (Cons x xs) = Cons (func x) (streamMap func xs)

-- function which generates a stream from a seed of type a
-- which is the first element, and an "unfolding" rule of
-- type a -> a which specifies how to transform the seed
-- into a new seed
-- e.g. streamIterate ('x' :) "o" == ["o", "xo", "xxo", "xxxo", "xxxxo", …
streamIterate :: (a -> a) -> a -> Stream a
streamIterate unfolder seed = Cons seed (streamIterate unfolder (unfolder seed))

-- function which interleaves the elements from 2 streams and is lazy in 2nd param
-- e.g. streamInterleave (streamRepeat 0) (streamRepeat 1) == [0, 1, 0, 1, 0, 1, …
streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) sndStream = Cons x (streamInterleave sndStream xs)

-- stream which contains the infinite list of natural numbers 0,1,2,...
nats :: Stream Integer
nats = streamIterate (+1) 0

-- stream which corresponds to the ruler function, where the nth element
-- in the stream is the largest power of 2 that evenly divides n 
-- e.g. 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,…
ruler :: Stream Integer
ruler = streamInterleave (streamRepeat 0) (streamMap (+1) ruler)