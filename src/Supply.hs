module Supply
    ( Supply(..)
    , get
    , pureSupply
    , mapSupply
    , mapSupply2
    , bindSupply
    , runSupply
    ) where 

import Stream

-- supply is a computation that consumes a supply of s's to produce
-- a value of type a
data Supply s a = S (Stream s -> (a, Stream s))

-- returns first element of stream as a result, with the remaining stream
get :: Supply s s
get = S (\(Cons element stream) -> (element, stream))

-- produces a supply using computation that doesn't consume any of the stream
-- this is analogous to Monad "return"
pureSupply :: a -> Supply s a
pureSupply x = S (\stream -> (x, stream))

-- apply function to supply
mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply func (S streamConsumer) = S newStreamConsumer
    where newStreamConsumer stream = let (element, stream') = streamConsumer stream     -- stream' is modified stream (models side effect)
                                     in (func element, stream')

-- apply binary function to supply
-- the first supply computation reads from the supply first
mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 func (S fstStreamConsumer) (S sndStreamConsumer) = S newStreamConsumer
    where newStreamConsumer stream = let (a, stream') = fstStreamConsumer stream
                                         (b, stream'') = sndStreamConsumer stream'
                                        in (func a b, stream'')

-- bind
bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S streamConsumer) computation = S go
    where go stream = let (a, stream') = streamConsumer stream      -- get element from the stream, this modifies the stream
                          (S newStreamConsumer) = computation a     -- pass element to the computation which creates a new supply
                          (b, stream'') = newStreamConsumer stream' -- get element from the new stream, this modifies the new stream
                          in (b, stream'')                          -- return new element and the modified stream

-- run computation of type supply
runSupply :: Stream s -> Supply s a -> a
runSupply stream (S streamConsumer) = fst (streamConsumer stream)

instance Functor (Supply s) where
    fmap = mapSupply

instance Applicative (Supply s) where
    pure = pureSupply -- ignores the supply of values
    (<*>) = mapSupply2 id

instance Monad (Supply s) where
    return = pureSupply
    (>>=) = bindSupply