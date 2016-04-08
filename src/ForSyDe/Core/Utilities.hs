module ForSyDe.Core.Utilities where

import Prelude hiding (unzip3)

at21 (x,_)             = x
at22 (_,x)             = x
at31 (x,_,_)           = x
at32 (_,x,_)           = x
at33 (_,_,x)           = x
at41 (x,_,_,_)         = x
at42 (_,x,_,_)         = x
at43 (_,_,x,_)         = x
at44 (_,_,_,x)         = x
at51 (x,_,_,_,_)       = x
at52 (_,x,_,_,_)       = x
at53 (_,_,x,_,_)       = x
at54 (_,_,_,x,_)       = x
at55 (_,_,_,_,x)       = x
at61 (x,_,_,_,_,_)     = x
at62 (_,x,_,_,_,_)     = x
at63 (_,_,x,_,_,_)     = x
at64 (_,_,_,x,_,_)     = x
at65 (_,_,_,_,x,_)     = x
at66 (_,_,_,_,_,x)     = x
at71 (x,_,_,_,_,_,_)   = x
at72 (_,x,_,_,_,_,_)   = x
at73 (_,_,x,_,_,_,_)   = x
at74 (_,_,_,x,_,_,_)   = x
at75 (_,_,_,_,x,_,_)   = x
at76 (_,_,_,_,_,x,_)   = x
at77 (_,_,_,_,_,_,x)   = x
at81 (x,_,_,_,_,_,_,_) = x
at82 (_,x,_,_,_,_,_,_) = x
at83 (_,_,x,_,_,_,_,_) = x
at84 (_,_,_,x,_,_,_,_) = x
at85 (_,_,_,_,x,_,_,_) = x
at86 (_,_,_,_,_,x,_,_) = x
at87 (_,_,_,_,_,_,x,_) = x
at88 (_,_,_,_,_,_,_,x) = x

fat21 :: Functor f => f (a1, a2) -> f a1
fat21 = (<$>) at21
fat22 :: Functor f => f (a1, a2) -> f a2
fat22 = (<$>) at22
fat31 :: Functor f => f (a1, a2, a3) -> f a1
fat31 = (<$>) at31
fat32 :: Functor f => f (a1, a2, a3) -> f a2
fat32 = (<$>) at32
fat33 :: Functor f => f (a1, a2, a3) -> f a3
fat33 = (<$>) at33
fat41 :: Functor f => f (a1, a2, a3, a4) -> f a1
fat41 = (<$>) at41
fat42 :: Functor f => f (a1, a2, a3, a4) -> f a2
fat42 = (<$>) at42
fat43 :: Functor f => f (a1, a2, a3, a4) -> f a3
fat43 = (<$>) at43
fat44 :: Functor f => f (a1, a2, a3, a4) -> f a4
fat44 = (<$>) at44
fat51 :: Functor f => f (a1, a2, a3, a4, a5) -> f a1
fat51 = (<$>) at51
fat52 :: Functor f => f (a1, a2, a3, a4, a5) -> f a2
fat52 = (<$>) at52
fat53 :: Functor f => f (a1, a2, a3, a4, a5) -> f a3
fat53 = (<$>) at53
fat54 :: Functor f => f (a1, a2, a3, a4, a5) -> f a4
fat54 = (<$>) at54
fat55 :: Functor f => f (a1, a2, a3, a4, a5) -> f a5
fat55 = (<$>) at55
fat61 :: Functor f => f (a1, a2, a3, a4, a5, a6) -> f a1
fat61 = (<$>) at61
fat62 :: Functor f => f (a1, a2, a3, a4, a5, a6) -> f a2
fat62 = (<$>) at62
fat63 :: Functor f => f (a1, a2, a3, a4, a5, a6) -> f a3
fat63 = (<$>) at63
fat64 :: Functor f => f (a1, a2, a3, a4, a5, a6) -> f a4
fat64 = (<$>) at64
fat65 :: Functor f => f (a1, a2, a3, a4, a5, a6) -> f a5
fat65 = (<$>) at65
fat66 :: Functor f => f (a1, a2, a3, a4, a5, a6) -> f a6
fat66 = (<$>) at66
fat71 :: Functor f => f (a1, a2, a3, a4, a5, a6, a7) -> f a1
fat71 = (<$>) at71
fat72 :: Functor f => f (a1, a2, a3, a4, a5, a6, a7) -> f a2
fat72 = (<$>) at72
fat73 :: Functor f => f (a1, a2, a3, a4, a5, a6, a7) -> f a3
fat73 = (<$>) at73
fat74 :: Functor f => f (a1, a2, a3, a4, a5, a6, a7) -> f a4
fat74 = (<$>) at74
fat75 :: Functor f => f (a1, a2, a3, a4, a5, a6, a7) -> f a5
fat75 = (<$>) at75
fat76 :: Functor f => f (a1, a2, a3, a4, a5, a6, a7) -> f a6
fat76 = (<$>) at76
fat77 :: Functor f => f (a1, a2, a3, a4, a5, a6, a7) -> f a7
fat77 = (<$>) at77
fat81 :: Functor f => f (a1, a2, a3, a4, a5, a6, a7, a8) -> f a1
fat81 = (<$>) at81
fat82 :: Functor f => f (a1, a2, a3, a4, a5, a6, a7, a8) -> f a2
fat82 = (<$>) at82
fat83 :: Functor f => f (a1, a2, a3, a4, a5, a6, a7, a8) -> f a3
fat83 = (<$>) at83
fat84 :: Functor f => f (a1, a2, a3, a4, a5, a6, a7, a8) -> f a4
fat84 = (<$>) at84
fat85 :: Functor f => f (a1, a2, a3, a4, a5, a6, a7, a8) -> f a5
fat85 = (<$>) at85
fat86 :: Functor f => f (a1, a2, a3, a4, a5, a6, a7, a8) -> f a6
fat86 = (<$>) at86
fat87 :: Functor f => f (a1, a2, a3, a4, a5, a6, a7, a8) -> f a7
fat87 = (<$>) at87
fat88 :: Functor f => f (a1, a2, a3, a4, a5, a6, a7, a8) -> f a8
fat88 = (<$>) at88


funzip2 x = (at21 <$> x, at22 <$> x)
funzip3 x = (at31 <$> x, at32 <$> x, at33 <$> x)
funzip4 x = (at41 <$> x, at42 <$> x, at43 <$> x, at44 <$> x)
funzip5 x = (at51 <$> x, at52 <$> x, at53 <$> x, at54 <$> x, at55 <$> x)
funzip6 x = (at61 <$> x, at62 <$> x, at63 <$> x, at64 <$> x, at65 <$> x, at66 <$> x)
funzip7 x = (at71 <$> x, at72 <$> x, at73 <$> x, at74 <$> x, at75 <$> x, at76 <$> x, at77 <$> x)
funzip8 x = (at81 <$> x, at82 <$> x, at83 <$> x, at84 <$> x, at85 <$> x, at86 <$> x, at87 <$> x, at88 <$> x)


ffunzip2 f x = (f at21 <$> x, f at22 <$> x)
ffunzip3 f x = (f at31 <$> x, f at32 <$> x, f at33 <$> x)
ffunzip4 f x = (f at41 <$> x, f at42 <$> x, f at43 <$> x, f at44 <$> x)
ffunzip5 f x = (f at51 <$> x, f at52 <$> x, f at53 <$> x, f at54 <$> x, f at55 <$> x)
ffunzip6 f x = (f at61 <$> x, f at62 <$> x, f at63 <$> x, f at64 <$> x, f at65 <$> x, f at66 <$> x)
ffunzip7 f x = (f at71 <$> x, f at72 <$> x, f at73 <$> x, f at74 <$> x, f at75 <$> x, f at76 <$> x, f at77 <$> x)
ffunzip8 f x = (f at81 <$> x, f at82 <$> x, f at83 <$> x, f at84 <$> x, f at85 <$> x, f at86 <$> x, f at87 <$> x, f at88 <$> x)


psi11 f a1                      =         (f <$> a1)
psi12 f a1                      = funzip2 (f <$> a1)
psi13 f a1                      = funzip3 (f <$> a1)
psi14 f a1                      = funzip4 (f <$> a1)
psi15 f a1                      = funzip5 (f <$> a1)
psi16 f a1                      = funzip6 (f <$> a1)
psi17 f a1                      = funzip7 (f <$> a1)
psi18 f a1                      = funzip8 (f <$> a1)
psi21 f a1 a2                   =         (f <$> a1 <*> a2)
psi22 f a1 a2                   = funzip2 (f <$> a1 <*> a2)
psi23 f a1 a2                   = funzip3 (f <$> a1 <*> a2)
psi24 f a1 a2                   = funzip4 (f <$> a1 <*> a2)
psi25 f a1 a2                   = funzip5 (f <$> a1 <*> a2)
psi26 f a1 a2                   = funzip6 (f <$> a1 <*> a2)
psi27 f a1 a2                   = funzip7 (f <$> a1 <*> a2)
psi28 f a1 a2                   = funzip8 (f <$> a1 <*> a2)
psi31 f a1 a2 a3                =         (f <$> a1 <*> a2 <*> a3)
psi32 f a1 a2 a3                = funzip2 (f <$> a1 <*> a2 <*> a3)
psi33 f a1 a2 a3                = funzip3 (f <$> a1 <*> a2 <*> a3)
psi34 f a1 a2 a3                = funzip4 (f <$> a1 <*> a2 <*> a3)
psi35 f a1 a2 a3                = funzip5 (f <$> a1 <*> a2 <*> a3)
psi36 f a1 a2 a3                = funzip6 (f <$> a1 <*> a2 <*> a3)
psi37 f a1 a2 a3                = funzip7 (f <$> a1 <*> a2 <*> a3)
psi38 f a1 a2 a3                = funzip8 (f <$> a1 <*> a2 <*> a3)
psi41 f a1 a2 a3 a4             =         (f <$> a1 <*> a2 <*> a3 <*> a4)
psi42 f a1 a2 a3 a4             = funzip2 (f <$> a1 <*> a2 <*> a3 <*> a4)
psi43 f a1 a2 a3 a4             = funzip3 (f <$> a1 <*> a2 <*> a3 <*> a4)
psi44 f a1 a2 a3 a4             = funzip4 (f <$> a1 <*> a2 <*> a3 <*> a4)
psi45 f a1 a2 a3 a4             = funzip5 (f <$> a1 <*> a2 <*> a3 <*> a4)
psi46 f a1 a2 a3 a4             = funzip6 (f <$> a1 <*> a2 <*> a3 <*> a4)
psi47 f a1 a2 a3 a4             = funzip7 (f <$> a1 <*> a2 <*> a3 <*> a4)
psi48 f a1 a2 a3 a4             = funzip8 (f <$> a1 <*> a2 <*> a3 <*> a4)
psi51 f a1 a2 a3 a4 a5          =         (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5)
psi52 f a1 a2 a3 a4 a5          = funzip2 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5)
psi53 f a1 a2 a3 a4 a5          = funzip3 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5)
psi54 f a1 a2 a3 a4 a5          = funzip4 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5)
psi55 f a1 a2 a3 a4 a5          = funzip5 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5)
psi56 f a1 a2 a3 a4 a5          = funzip6 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5)
psi57 f a1 a2 a3 a4 a5          = funzip7 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5)
psi58 f a1 a2 a3 a4 a5          = funzip8 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5)
psi61 f a1 a2 a3 a4 a5 a6       =         (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6)
psi62 f a1 a2 a3 a4 a5 a6       = funzip2 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6)
psi63 f a1 a2 a3 a4 a5 a6       = funzip3 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6)
psi64 f a1 a2 a3 a4 a5 a6       = funzip4 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6)
psi65 f a1 a2 a3 a4 a5 a6       = funzip5 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6)
psi66 f a1 a2 a3 a4 a5 a6       = funzip6 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6)
psi67 f a1 a2 a3 a4 a5 a6       = funzip7 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6)
psi68 f a1 a2 a3 a4 a5 a6       = funzip8 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6)
psi71 f a1 a2 a3 a4 a5 a6 a7    =         (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7)
psi72 f a1 a2 a3 a4 a5 a6 a7    = funzip2 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7)
psi73 f a1 a2 a3 a4 a5 a6 a7    = funzip3 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7)
psi74 f a1 a2 a3 a4 a5 a6 a7    = funzip4 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7)
psi75 f a1 a2 a3 a4 a5 a6 a7    = funzip5 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7)
psi76 f a1 a2 a3 a4 a5 a6 a7    = funzip6 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7)
psi77 f a1 a2 a3 a4 a5 a6 a7    = funzip7 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7)
psi78 f a1 a2 a3 a4 a5 a6 a7    = funzip8 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7)
psi81 f a1 a2 a3 a4 a5 a6 a7 a8 =         (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8)
psi82 f a1 a2 a3 a4 a5 a6 a7 a8 = funzip2 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8)
psi83 f a1 a2 a3 a4 a5 a6 a7 a8 = funzip3 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8)
psi84 f a1 a2 a3 a4 a5 a6 a7 a8 = funzip4 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8)
psi85 f a1 a2 a3 a4 a5 a6 a7 a8 = funzip5 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8)
psi86 f a1 a2 a3 a4 a5 a6 a7 a8 = funzip6 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8)
psi87 f a1 a2 a3 a4 a5 a6 a7 a8 = funzip7 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8)
psi88 f a1 a2 a3 a4 a5 a6 a7 a8 = funzip8 (f <$> a1 <*> a2 <*> a3 <*> a4 <*> a5 <*> a6 <*> a7 <*> a8)


fanout2 x = (x, x)
fanout3 x = (x, x, x)
fanout4 x = (x, x, x, x)
fanout5 x = (x, x, x, x, x)
fanout6 x = (x, x, x, x, x, x)
fanout7 x = (x, x, x, x, x, x, x)
fanout8 x = (x, x, x, x, x, x, x, x)


infixl 5 #
class Filter c where
  (#) :: (a -> Bool) -> c a -> c a



demux2 sel = ffunzip2 (\f -> (f . sel #))
demux3 sel = ffunzip3 (\f -> (f . sel #))
demux4 sel = ffunzip4 (\f -> (f . sel #))
demux5 sel = ffunzip5 (\f -> (f . sel #))
demux6 sel = ffunzip6 (\f -> (f . sel #))
demux7 sel = ffunzip7 (\f -> (f . sel #))
demux8 sel = ffunzip8 (\f -> (f . sel #))
