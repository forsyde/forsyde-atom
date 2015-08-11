{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Communication
-- Copyright   :  ...
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ...
-- Stability   :  experimental
-- Portability :  portable
--
-- ...
-----------------------------------------------------------------------------
module ForSyDe.Patterns.Transition where

import ForSyDe.Core

import Prelude hiding (zip,zip3, unzip, unzip3, take, drop, head, tail, init, last, odds, evens)


-- | Returns a vector with indexes, i.e. @\<0,1,2,...\>@
indexes :: Vector Int
indexes = vector [0..]


-- | 'fanout' is the equivalent of repeat on lists. It is given a new name because instead of a 
-- plain copy, this transition network gains a new meaning: it distributes the same value or signal
-- to all the connected processes down the line. Depending on the target platform and the refinement 
-- decisions involved, it may be interpreted in the following implementations:
--
--  * global or shared memory in case of a massively parallel platform (e.g. GPU)
--
--  * a (static) memory or cache location in memory-driven architectures (e.g. CPU)
--
--  * a fanout in case of a HDL stem
--
--  * a physical copy in case of a distributed stem
--
--  * etc. The point is that one should not be stuck in the apparent \"copy\" behavior
fanout   :: VecSig vsa => vsa -> Vector vsa

-- | it is the same as 'fanout' with an additional length parameter
fanoutn     :: (VecSig vsb, Num a, Eq a, Ord a) => a -> vsb -> Vector vsb

fanout a    = a :> fanout a
fanoutn n a | n > 0     = a :> fanoutn (n-1) a
              | otherwise = NullV



-- | The pattern 'head' returns the first element of a vector.
head :: VecSig vsa => Vector vsa -> vsa

-- | The pattern 'last' returns the last element of a vector.
last :: VecSig vsa => Vector vsa -> vsa

-- | The pattern 'tail' returns a vector without the first element.
tail        :: VecSig vsa => Vector vsa -> Vector vsa

-- | The pattern 'init' returns a vector without the last element.
init        :: VecSig vsa => Vector vsa -> Vector vsa

-- | The pattern 'take' returns the first /n/ elements of a vector.
take        :: VecSig vsa => Int ->  Vector vsa -> Vector vsa

-- | The pattern 'drop' drops the first /n/ elements of a vector.
drop        :: VecSig vsa => Int -> Vector vsa -> Vector vsa

-- | The pattern 'odds' returns elements of a vector with odd indexes.
odds        :: VecSig vsa => Vector vsa -> Vector vsa

-- | The pattern 'evens' returns elements of a vector with even indexes.
evens       :: VecSig vsa => Vector vsa -> Vector vsa

-- | The pattern 'stridedSel' returns the elements in a vector based on a defined stride: the first element, the step size and the number of elements.
stridedSel  :: VecSig vsa => Int -> Int -> Int -> Vector vsa -> Vector vsa
-- |  The function 'replace' replaces an element in a vector.
replace :: VecSig vsa => Vector vsa -> Int -> vsa -> Vector vsa

-- | The operator attach attaches a signal at the end of a vector.
attach :: VecSig vsa => Vector vsa -> vsa -> Vector vsa

-- | The operator 'cat' concatinates two vectors.
cat :: VecSig vsa => Vector vsa -> Vector vsa -> Vector vsa

-- | The function 'concat' transforms a vector of vectors to a single vector. 
concat :: VecSig vsa => Vector (Vector vsa) -> Vector vsa

-- | The function 'splitat' splits a vector into two at a determined position. 
splitat :: VecSig vsa => Int ->  Vector vsa -> (Vector vsa, Vector vsa)

-- | The function 'group' groups a vector into a vector of vectors of size n.
group :: VecSig vsa => Int -> Vector vsa -> Vector (Vector vsa)

-- | The function 'shiftl' shifts a value from the left into a vector. 
shiftl :: VecSig vsa => Vector vsa -> vsa -> Vector vsa 

-- | The function 'shiftr' shifts a value from the right into a vector. 
shiftr :: VecSig vsa => Vector vsa -> vsa -> Vector vsa

-- | The function 'rotl' rotates a vector to the left. Note that this fuctions does not change the size of a vector.
rotl   :: VecSig vsa => Vector vsa -> Vector vsa

-- | The function 'rotr' rotates a vector to the right. Note that this fuction does not change the size of a vector.
rotr :: VecSig vsa => Vector vsa -> Vector vsa

-- | The function 'reverse' reverses the order of elements in a vector. 
reverse :: VecSig vsa => Vector vsa -> Vector vsa

-- | The function 'bitrev' rearranges a vector in a bit-reverse pattern. An example of a bit-reverse pattern is the butterfly interconnection in a FFT network. 
bitrev :: VecSig vsa => Vector vsa -> Vector vsa



tail        = gatherIdx (>1)
init v      = gatherIdx (< lengthV v) v
take n      = gatherIdx (<=n)
drop n      = gatherIdx (>n)
odds        = gatherIdx odd
evens       = gatherIdx even

splitat n v       = (take n v, drop n v)
bitrev (x:>NullV) = x:>NullV
bitrev xs         = bitrev (evens xs) <+> bitrev (odds xs)

attach     = (<:)
cat        = (<+>)
concat     = concatV
head       = headV
last       = lastV
group      = groupV
replace    = replaceV
reverse    = reverseV
shiftl     = shiftlV
shiftr     = shiftrV
rotl       = rotlV
rotr       = rotrV
stridedSel = selectV


-- | 'gatherIdx' returns the elements based on a boolean function on indexes in a vector of 'VecSig's
gatherIdx :: VecSig vsa => (Int -> Bool) -> Vector vsa -> Vector vsa

-- | 'gatherVec' returns the elements of a vector (of 'VecSig's) based on a vector of indexes.
gatherVec  :: VecSig vsa => Vector Int -> Vector vsa -> Vector vsa

-- | 'gatherVec2' returns the elements of a vector (of 'VecSig's) based on a nested vector of indexes with depth 2. The output vector wil have the same shape (depth) as the vector of indexes.
gatherVec2 :: VecSig vsa => Vector (Vector Int) -> Vector vsa -> Vector (Vector vsa)

-- | 'gatherVec3' is similar to 'gatherVec2', but the vector of indexes and respectively the output vector are nested with depth 3.
gatherVec3 :: VecSig vsa => Vector (Vector (Vector Int)) -> Vector vsa -> Vector (Vector (Vector vsa))

-- | 'gatherVec4' is similar to 'gatherVec2', but the vector of indexes and respectively the output vector are nested with depth 4.
gatherVec4 :: VecSig vsa => Vector (Vector (Vector (Vector Int))) -> Vector vsa -> Vector (Vector (Vector (Vector vsa)))

-- | 'gatherVec5' is similar to 'gatherVec2', but the vector of indexes and respectively the output vector are nested with depth 5.
gatherVec5 :: VecSig vsa => Vector (Vector (Vector (Vector (Vector Int)))) -> Vector vsa -> Vector (Vector (Vector (Vector (Vector vsa))))

-- | 'gatherAdp' is the adaptive version of 'gatherVec' where the vector of indexes for each event is carried by a signal.
gatherAdp  :: Signals s => Vector (s Int) -> Vector (s a) -> Vector (s a)

-- | 'gatherAdp2' is the adaptive version of 'gatherVec2' where the indexes for each event is carried by a signal.
gatherAdp2 :: Signals s => Vector (Vector (s Int)) -> Vector (s a) -> Vector (Vector (s a))

-- | 'gatherAdp3' is the adaptive version of 'gatherVec3' where the indexes for each event is carried by a signal.
gatherAdp3 :: Signals s => Vector (Vector (Vector (s Int))) -> Vector (s a) -> Vector (Vector (Vector (s a)))

-- | 'gatherAdp4' is the adaptive version of 'gatherVec4' where the indexes for each event is carried by a signal.
gatherAdp4 :: Signals s => Vector (Vector (Vector (Vector (s Int)))) -> Vector (s a) -> Vector (Vector (Vector (Vector (s a))))

-- | 'gatherAdp5' is the adaptive version of 'gatherVec5' where the indexes for each event is carried by a signal.
gatherAdp5 :: Signals s => Vector (Vector (Vector (Vector (Vector (s Int))))) -> Vector (s a) -> Vector (Vector (Vector (Vector (Vector (s a)))))


gatherIdx f       = (§>) snd . filterV (\(idx,v) -> f idx) . zipV indexes
gatherVec ix vs   = (§>)                       (atV vs) ix
gatherVec2 ix vs  = ((§>).(§>))                (atV vs) ix
gatherVec3 ix vs  = ((§>).(§>).(§>))           (atV vs) ix
gatherVec4 ix vs  = ((§>).(§>).(§>).(§>))      (atV vs) ix
gatherVec5 ix vs  = ((§>).(§>).(§>).(§>).(§>)) (atV vs) ix
gatherAdp vsix vs = unzipx $ (\v -> (§>) (flat . (<$>) (atV v))) §§- zipx vs -§§- zipx vsix
gatherAdp2 ixs vs = (§>)                  (\ix -> gatherAdp ix vs) ixs
gatherAdp3 ixs vs = ((§>).(§>))           (\ix -> gatherAdp ix vs) ixs
gatherAdp4 ixs vs = ((§>).(§>).(§>))      (\ix -> gatherAdp ix vs) ixs
gatherAdp5 ixs vs = ((§>).(§>).(§>).(§>)) (\ix -> gatherAdp ix vs) ixs


-- | The pattern 'zip' \"zips\" two incoming signals into one signal of tuples.
zip  :: Vector a -> Vector b -> Vector (a,b)

-- | Works as 'zip', but takes three input signals.
zip3 :: Vector a -> Vector b -> Vector c -> Vector (a,b,c)

-- | Works as 'zip', but takes four input signals.
zip4 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector (a,b,c,d)

-- | Works as 'zip', but takes four input signals.
zip5 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector (a,b,c,d,e)

-- | Works as 'zip', but takes four input signals.
zip6 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector (a,b,c,d,e,f)

-- | The pattern 'unzip' \"unzips\" a signal of tuples into two signals.
unzip  :: Vector (a,b) -> (Vector a,Vector b)

-- | Works as 'unzip', but has three output signals.
unzip3 :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)

-- | Works as 'unzip', but has four output signals.
unzip4 :: Vector (a,b,c,d) -> (Vector a,Vector b,Vector c,Vector d)

-- | Works as 'unzip', but has four output signals.
unzip5 :: Vector (a,b,c,d,e)  -> (Vector a,Vector b,Vector c,Vector d,Vector e)

-- | Works as 'unzip', but has four output signals.
unzip6 :: Vector (a,b,c,d,e,f) -> (Vector a,Vector b,Vector c,Vector d,Vector e,Vector f)

zip xs ys              = (,) §> xs <§> ys
zip3 xs ys zs          = (,,) §> xs <§> ys <§> zs
zip4 xs ys zs as       = (,,,) §> xs <§> ys <§> zs <§> as
zip5 xs ys zs as bs    = (,,,,) §> xs <§> ys <§> zs <§> as <§> bs
zip6 xs ys zs as bs cs = (,,,,,) §> xs <§> ys <§> zs <§> as <§> bs <§> cs


unzip xs  = (fst §> xs,snd §> xs)
unzip3 xs = ((\(x,_,_) -> x) §> xs,
               (\(_,x,_) -> x) §> xs,
               (\(_,_,x) -> x) §> xs)
unzip4 xs = ((\(x,_,_,_) -> x) §> xs,
               (\(_,x,_,_) -> x) §> xs,
               (\(_,_,x,_) -> x) §> xs,
               (\(_,_,_,x) -> x) §> xs)
unzip5 xs = ((\(x,_,_,_,_) -> x) §> xs,
               (\(_,x,_,_,_) -> x) §> xs,
               (\(_,_,x,_,_) -> x) §> xs,
               (\(_,_,_,x,_) -> x) §> xs,
               (\(_,_,_,_,x) -> x) §> xs)
unzip6 xs = ((\(x,_,_,_,_,_) -> x) §> xs,
               (\(_,x,_,_,_,_) -> x) §> xs,
               (\(_,_,x,_,_,_) -> x) §> xs,
               (\(_,_,_,x,_,_) -> x) §> xs,
               (\(_,_,_,_,x,_) -> x) §> xs,
               (\(_,_,_,_,_,x) -> x) §> xs)

duals :: VecSig vsa => Vector (vsa) -> Vector (vsa,vsa)

unduals :: VecSig vsa => Vector (vsa,vsa) -> Vector (vsa)

duals v = let k = lengthV v `div` 2
          in  zip (take k v) (drop k v)

unduals v = let (x,y) = unzip v 
            in  x <+> y

