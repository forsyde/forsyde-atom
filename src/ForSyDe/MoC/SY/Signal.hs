{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home, prune #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.MoC.SY.Signal
-- Copyright   :  (c) George Ungureanu, KTH/ICT/ESY 2015; 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements the sychronous Signal. The functions necessary for
-- the standard ForSyDe framework are re-exported by the parent module 
-- 'ForSyDe.MoC.SY.SY'. 
--
-- This module should be separately imported only for experimental purpose,
-- and should not be included in a standard ForSyDe design- 
-----------------------------------------------------------------------------

module ForSyDe.MoC.SY.Signal where

import ForSyDe.Core

-- | The 'Signal' type denotes a 'Stream' that behaves according to the Sychronous MoC. Its tokens ('TokT') are 'AbstExt' values.
newtype Signal a = Signal {
  -- | Function to unwrap a SY signal to its base 'Stream'. This way one can operate directly on the tokens.
  toS :: Stream (AbstExt a) 
}

-- | the function 'fromS' replaces the 'Signal' constructor for convenience. Wraps a 'Stream' of 'AbstExt' values into a SY 'Signal'.
fromS = Signal

-- | converts a list of 'AbstExt'-wrapped values into a SY 'Signal'. 
signal :: [AbstExt a] -> Signal a 
signal = fromS . stream 

-- | The strict version of 'signal'. It converts a list of values into a SY 'Signal' with all tokens being 'Prst'. 
ssignal :: [a] -> Signal a 
ssignal = signal . map Prst

-- | The function 'fromSignal' converts a SY 'Signal' into a list of 'AbstExt'-wrapped values.
fromSignal :: Signal a -> [AbstExt a]
fromSignal = fromStream . toS

--- THE FOLLOWING IS NOT EXPORTED TO THE STANDARD DESIGN FRAMEWORK

-- | 'liftS' applies a function of streams on a 'Signal', and returns a 'Signal'
liftS :: (Stream (AbstExt a) -> Stream (AbstExt b)) -> Signal a -> Signal b
liftS f = fromS . f . toS

-- | 'liftS2' applies a function of 2 streams on two 'Signal's, and returns a 'Signal'
liftS2 :: (Stream (AbstExt a) -> Stream (AbstExt b) -> Stream (AbstExt c)) 
       -> Signal a -> Signal b -> Signal c
liftS2 f xs ys = fromS $ f (toS xs) (toS ys)


infixl 4 §-, -§-,  §§!-, -§§!-
-- | this is the basic functor operator used in process constructors. It takes a function on a token and applies it on a 'Signal'. It returns a 'Stream' (the key for constructing functions on multiple parameters), thus it needs to be further 'tokenize'd in order to get the corresponding result 'Signal'
(§-) :: (AbstExt a -> b) -> Signal a -> Stream b
(§-) f = (<$>) f . toS

-- | this is the basic lifting operator used in process constructors. It inputs a stream of functions on a tokens and applies it on a 'Signal'. It returns a 'Stream' (the key for constructing functions on multiple parameters), thus it needs to be further 'tokenize'd in order to get the corresponding result 'Signal'
(-§-) :: Stream (AbstExt a -> b) -> Signal a -> Stream b
(-§-) fs = (<*>) fs . toS

-- | guarded functor operator for strict signals. It adds a guard to §§- (which bypasses the token wrapper, see 'Signals') to verify if the strict assumption is correct, ergo there is no 'Abst' value.
(§§!-) :: (a -> b) -> Signal a -> Signal b
f §§!- xs | anyS isAbsent $ toS xs = error "Unexpected absent value in szip"
          | otherwise         = f §§- xs

-- | guarded lift operator for strict signals. It adds a guard to -§§- (which bypasses the token wrapper, see 'Signals') to verify if the strict assumption is correct, ergo there is no 'Abst' value.
(-§§!-) :: Signal (a -> b) -> Signal a -> Signal b
fs -§§!- xs | anyS isAbsent $ toS xs = error "Unexpected absent value in szip"
            | otherwise         = fs -§§- xs

-- | turns the result of the applicative operators §- and -§- back into 'Signal's.
tokenize = fromS


-- | Only 'Show' instance. To 'Read' it, use the 'Stream' instance.
instance (Show a) => Show (Signal a) where
  showsPrec p = showsPrec p . toS

-- | instance defining the Synchronous MoC behavior
instance Signals Signal where
 -- To abide the synchronicicy law, a data-filtered signal must replace missing tokens with 
 --   absent values, thus the filtered type is 'AbstExt'
    type TokT Signal a = AbstExt a

 -- (§§-) :: (a -> b) -> Signal a -> Signal b
    (§§-) f = liftS (((<$>).(<$>)) f)

 -- (-§§-) :: Signal (a -> b) -> Signal a -> Signal b
    (-§§-) = liftS2 (\f x -> (<*>) <$> f <*> x)

 -- (->-) :: Signal a -> AbstExt a -> Signal a
    xs ->- i = liftS (i :-) xs

 -- filt :: (a -> Bool) -> Signal a -> Signal a 
    filt p = liftS ((\x -> if (p <$> x) == Prst True then x else Abst) <$>) 

 -- zipx :: Vector (Signal a) -> Signal (Vector (AbstExt a))
    zipx = zippx

 -- unzipx :: Signal (Vector (AbstExt a)) -> Vector (Signal a)
    unzipx  = (§>) fromS . unzippx . toS

zippx :: Vector (Signal a) -> Signal (Vector (AbstExt a))
zippx NullV   = (fromS . pure . pure) NullV
zippx (x:>xs) = tokenize $ ((<$>).(:>)) §- x -§- zippx xs

unzippx :: Stream (AbstExt (Vector (AbstExt a))) -> Vector (Stream (AbstExt a))
unzippx NullS            = pure NullS
unzippx (Abst :- xs)     = (:-) §> pure Abst <§> unzippx xs
unzippx ((Prst x) :- xs) = (:-) §> x <§> unzippx xs


