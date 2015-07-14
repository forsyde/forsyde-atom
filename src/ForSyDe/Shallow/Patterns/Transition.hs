{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.Communication
-- Copyright   :  ...
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ...
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------
module ForSyDe.Shallow.Patterns.Transition where

import ForSyDe.Shallow.Core
import ForSyDe.Shallow.Patterns.Vector

indexes :: Vector Int
indexes = vector [1..]


-- | 'fanoutPN' is the equivalent of repeat on lists. It is given a new name because instead of a 
-- plain copy, this transition network gains a new meaning: it distributes the same value or signal
-- to all the connected processes down the line. Depending on the target platform and the refinement 
-- decisions involved, it may be interpreted in the following implementations:
--
--  * global or shared memory in case of a massively parallel platform (e.g. GPU)
--
--  * a (static) memory or cache location in memory-driven architectures (e.g. CPU)
--
--  * a fanout in case of a HDL PNstem
--
--  * a physical copy in case of a distributed PNstem
--
--  * etc. The point is that one should not be stuck in the apparent \"copy\" behavior
fanoutPN   :: VSig vsa => vsa -> Vector vsa

-- | it is the same as 'fanoutPN' with an additional length parameter
fanoutnPN     :: (VSig vsb, Num a, Eq a, Ord a) => a -> vsb -> Vector vsb


-- |  The function 'replacePN' replaces an element in a vector.
replacePN :: VSig vsa => Vector vsa -> Int -> vsa -> Vector vsa

-- | The functions 'headPN' returns the first element of a vector.
headPN :: VSig vsa => Vector vsa -> vsa

-- | The function 'lastV' returns the last element of a vector.
lastPN :: VSig vsa => Vector vsa -> vsa

-- | The function 'groupPN' groups a vector into a vector of vectors of size n.
groupPN :: VSig vsa => Int -> Vector vsa -> Vector (Vector vsa)

-- | The operator attachPN attaches a signal at the end of a vector.
attachPN :: VSig vsa => Vector vsa -> vsa -> Vector vsa

-- | The operator 'catPN' concatinates two vectors.
catPN :: VSig vsa => Vector vsa -> Vector vsa -> Vector vsa

-- | The function 'concatV' transforms a vector of vectors to a single vector. 
concatPN :: VSig vsa => Vector (Vector vsa) -> Vector vsa

-- | The function 'reversePN' reverses the order of elements in a vector. 
reversePN  :: VSig vsa => Vector vsa -> Vector vsa

-- | The function 'shiftlV' shifts a value from the left into a vector. 
shiftlPN :: VSig vsa => Vector vsa -> vsa -> Vector vsa 

-- | The function 'shiftrV' shifts a value from the right into a vector. 
shiftrPN :: VSig vsa => Vector vsa -> vsa -> Vector vsa

-- | The function 'rotlV' rotates a vector to the left. Note that this fuctions does not change the size of a vector.
rotlPN   :: VSig vsa => Vector vsa -> Vector vsa

-- | The function 'rotrV' rotates a vector to the right. Note that this fuction does not change the size of a vector.
rotrPN :: VSig vsa => Vector vsa -> Vector vsa

stridedSelPN  :: VSig vsa => Int -> Int -> Int -> Vector vsa -> Vector vsa

-- | 'gatherIdxPN' selects an element based on a function of indexes in a vector of signals
gatherIdxPN :: VSig vsa => (Int -> Bool) -> Vector vsa -> Vector vsa

-- | 'selectIdx1PN' selects signals based on a vector of indexes in a vector of signals
gatherVecPN  :: VSig vsa => Vector Int -> Vector vsa -> Vector vsa
gatherVec2PN :: VSig vsa => Vector (Vector Int) -> Vector vsa -> Vector (Vector vsa)
gatherVec3PN :: VSig vsa => Vector (Vector (Vector Int)) -> Vector vsa -> Vector (Vector (Vector vsa))
gatherVec4PN :: VSig vsa => Vector (Vector (Vector (Vector Int))) -> Vector vsa -> Vector (Vector (Vector (Vector vsa)))
gatherVec5PN :: VSig vsa => Vector (Vector (Vector (Vector (Vector Int)))) -> Vector vsa -> Vector (Vector (Vector (Vector (Vector vsa))))

-- | 'selectIdx1AdpPN' selects signals based on signal of  vector of indexes in a vector of signals
gatherAdpPN  :: UnzippableS s => s (Vector Int) -> Vector (s a) -> Vector (s a)
gatherAdp2PN :: UnzippableS s => s (Vector (Vector Int)) -> Vector (s a) -> Vector (s (Vector a))
gatherAdp3PN :: UnzippableS s => s (Vector (Vector (Vector Int))) -> Vector (s a) -> Vector (s (Vector (Vector a)))
gatherAdp4PN :: UnzippableS s => s (Vector (Vector (Vector (Vector Int)))) -> Vector (s a) -> Vector (s (Vector (Vector (Vector a))))
gatherAdp5PN :: UnzippableS s => s (Vector (Vector (Vector (Vector (Vector Int))))) -> Vector (s a) -> Vector (s (Vector (Vector (Vector (Vector a)))))

-- | special cases of 'filteridxPN'
tailPN        :: VSig vsa => Vector vsa -> Vector vsa
initPN        :: VSig vsa => Vector vsa -> Vector vsa
takePN        :: VSig vsa => Int ->  Vector vsa -> Vector vsa
dropPN        :: VSig vsa => Int -> Vector vsa -> Vector vsa
oddsPN        :: VSig vsa => Vector vsa -> Vector vsa
evensPN       :: VSig vsa => Vector vsa -> Vector vsa

splitatPN     :: VSig vsa => Int ->  Vector vsa -> Vector vsa
bitrevPN      :: VSig vsa => Vector vsa -> Vector vsa

-- | The pattern 'zipPN' \"zips\" two incoming signals into one signal of tuples.
zipPN  :: Vector a -> Vector b -> Vector (a,b)

-- | Works as 'zipPN', but takes three input signals.
zip3PN :: Vector a -> Vector b -> Vector c -> Vector (a,b,c)

-- | Works as 'zipPN', but takes four input signals.
zip4PN :: Vector a -> Vector b -> Vector c -> Vector d -> Vector (a,b,c,d)

-- | Works as 'zipPN', but takes four input signals.
zip5PN :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector (a,b,c,d,e)

-- | Works as 'zipPN', but takes four input signals.
zip6PN :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector (a,b,c,d,e,f)

-- | The pattern 'unzipPN' \"unzips\" a signal of tuples into two signals.
unzipPN  :: Vector (a,b) -> (Vector a,Vector b)

-- | Works as 'unzipPN', but has three output signals.
unzip3PN :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)

-- | Works as 'unzipPN', but has four output signals.
unzip4PN :: Vector (a,b,c,d) -> (Vector a,Vector b,Vector c,Vector d)

-- | Works as 'unzipPN', but has four output signals.
unzip5PN :: Vector (a,b,c,d,e)  -> (Vector a,Vector b,Vector c,Vector d,Vector e)

-- | Works as 'unzipPN', but has four output signals.
unzip6PN :: Vector (a,b,c,d,e,f) -> (Vector a,Vector b,Vector c,Vector d,Vector e,Vector f)


fanoutPN a    = a :> fanoutPN a
fanoutnPN n a | n > 0     = a :> fanoutnPN (n-1) a
              | otherwise = NullV

attachPN     = (<:)
catPN        = (<+>)
concatPN     = concatV
headPN       = headV
lastPN       = lastV
groupPN      = groupV
replacePN    = replaceV
reversePN    = reverseV
shiftlPN     = shiftlV
shiftrPN     = shiftrV
rotlPN       = rotlV
rotrPN       = rotrV
stridedSelPN = selectV

gatherIdxPN f     = (§>) snd . filterV (\(idx,v) -> f idx) . zipV indexes

gatherVecPN ix v  = (§>) (atV v) ix
gatherVec2PN ix v = ((§>).(§>)) (atV v) ix
gatherVec3PN ix v = ((§>).(§>).(§>)) (atV v) ix
gatherVec4PN ix v = ((§>).(§>).(§>).(§>)) (atV v) ix
gatherVec5PN ix v = ((§>).(§>).(§>).(§>).(§>)) (atV v) ix

gatherAdpPN ixs v  = unzipxPN $ (\ixv vec -> (§>) (atV vec) ixv) §- ixs -§- zipxPN v
gatherAdp2PN ixs v = unzipxPN $ (\ixv vec -> ((§>).(§>)) (atV vec) ixv) §- ixs -§- zipxPN v
gatherAdp3PN ixs v = unzipxPN $ (\ixv vec -> ((§>).(§>).(§>)) (atV vec) ixv) §- ixs -§- zipxPN v
gatherAdp4PN ixs v = unzipxPN $ (\ixv vec -> ((§>).(§>).(§>).(§>)) (atV vec) ixv) §- ixs -§- zipxPN v
gatherAdp5PN ixs v = unzipxPN $ (\ixv vec -> ((§>).(§>).(§>).(§>).(§>)) (atV vec) ixv) §- ixs -§- zipxPN v


tailPN        = gatherIdxPN (>1)
initPN v      = gatherIdxPN (< lengthV v) v
takePN n      = gatherIdxPN (<=n)
dropPN n      = gatherIdxPN (>n)
oddsPN        = gatherIdxPN odd
evensPN       = gatherIdxPN even

splitatPN n v       = (takePN n v) <+> (dropPN n v)
bitrevPN (x:>NullV) = x:>NullV
bitrevPN xs         = bitrevPN (evensPN xs) <+> bitrevPN (oddsPN xs)


zipPN xs ys              = (,) §> xs <§> ys
zip3PN xs ys zs          = (,,) §> xs <§> ys <§> zs
zip4PN xs ys zs as       = (,,,) §> xs <§> ys <§> zs <§> as
zip5PN xs ys zs as bs    = (,,,,) §> xs <§> ys <§> zs <§> as <§> bs
zip6PN xs ys zs as bs cs = (,,,,,) §> xs <§> ys <§> zs <§> as <§> bs <§> cs


unzipPN xs  = (fst §> xs,snd §> xs)
unzip3PN xs = ((\(x,_,_) -> x) §> xs,
               (\(_,x,_) -> x) §> xs,
               (\(_,_,x) -> x) §> xs)
unzip4PN xs = ((\(x,_,_,_) -> x) §> xs,
               (\(_,x,_,_) -> x) §> xs,
               (\(_,_,x,_) -> x) §> xs,
               (\(_,_,_,x) -> x) §> xs)
unzip5PN xs = ((\(x,_,_,_,_) -> x) §> xs,
               (\(_,x,_,_,_) -> x) §> xs,
               (\(_,_,x,_,_) -> x) §> xs,
               (\(_,_,_,x,_) -> x) §> xs,
               (\(_,_,_,_,x) -> x) §> xs)
unzip6PN xs = ((\(x,_,_,_,_,_) -> x) §> xs,
               (\(_,x,_,_,_,_) -> x) §> xs,
               (\(_,_,x,_,_,_) -> x) §> xs,
               (\(_,_,_,x,_,_) -> x) §> xs,
               (\(_,_,_,_,x,_) -> x) §> xs,
               (\(_,_,_,_,_,x) -> x) §> xs)




{-



-- | 'gather' distributes a vector of signals towards different "worker" vectors based on a gather rule, with respect to a vector of idexes. TODO: make a typeclass for b ( = a | [a] | [[a]] ... )
gatherPN :: (Int -> Vector a -> b) 
         -> Vector Int 
         -> Vector a 
         -> Vector (Signal b)
gatherPN r ix v = mapV (\i -> mapS (r i) (zipxPN v)) ix
    where mapS _ NullS   = NullS
          mapS f (x:-xs) = f x :- mapS f xs

-- | 'gather1' extends 'gather' by vectorizing the gather rule. Thus now each worker has its own rule.
gather1PN :: Vector (Int -> Vector a -> b) 
          -> Vector Int 
          -> Vector a 
          -> Vector (Signal b)
gather1PN vr ix v = zipWithV (\r i -> mapS (r i) (zipxPN v)) vr ix
    where mapS _ NullS   = NullS
          mapS f (x:-xs) = f x :- mapS f xs

-- | 'gatherAdpPN' is the adaptive version of 'gather'. It inputs the indexes through a signal of vectors
gatherAdpPN :: (Int -> Vector a -> b) 
            -> Signal (Vector Int) 
            -> Vector a 
            -> Vector (Signal b)
gatherAdpPN r ixs v = unzipxPN $ zipWithS (\i tok -> mapV (\x -> (r x tok)) i) ixs (zipxPN v)
    where zipWithS f (x:-xs) (y:-ys) = f x y :- (zipWithS f xs ys)
          zipWithS _ _       _       = NullS


-- * MoC-specific patterns




-- | 'unzipPN'  unzips a vector of signals of tuples into a tuple of vectors of signals
unzipPN :: (Signals s) => Vector (s (a, b)) -> (Vector (s a), Vector (s b))
unzipPN = foldrV f (NullV, NullV) . mapV unzipPN
    where f x tp = (fst x:>fst tp, snd x:>snd tp)



dualsPN :: Vector a -> Vector (Signal (a,a))
undualsPN :: Vector (Signal (a,a)) -> Vector a
dualsPN v = zipPN (takeV k v) (dropV k v)
	where k = lengthV v `div` 2
undualsPN v = x <+> y
	where (x,y) = unzipPN v

-}
