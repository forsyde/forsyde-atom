{-# LANGUAGE PostfixOperators, TypeFamilies #-}

module ForSyDe.Atom.MoC.Cons where

import ForSyDe.Atom.MoC.Atom
import ForSyDe.Atom.Utility
import ForSyDe.Atom.Behavior

infixl 3 -&>-
delay i xs = i ->- (i -&- xs)
i -&>- xs = delay i xs

comb11 f s1                      = (f -$- s1 -¤)
comb21 f s1 s2                   = (f -$- s1 -*- s2 -¤)
comb31 f s1 s2 s3                = (f -$- s1 -*- s2 -*- s3 -¤)
comb41 f s1 s2 s3 s4             = (f -$- s1 -*- s2 -*- s3 -*- s4 -¤)
comb51 f s1 s2 s3 s4 s5          = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -¤)
comb61 f s1 s2 s3 s4 s5 s6       = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -¤)
comb71 f s1 s2 s3 s4 s5 s6 s7    = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -¤)
comb81 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*- s8 -¤)

comb12 f s1                      = (f -$- s1 -<)
comb22 f s1 s2                   = (f -$- s1 -*- s2 -<)
comb32 f s1 s2 s3                = (f -$- s1 -*- s2 -*- s3 -<)
comb42 f s1 s2 s3 s4             = (f -$- s1 -*- s2 -*- s3 -*- s4 -<)
comb52 f s1 s2 s3 s4 s5          = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -<)
comb62 f s1 s2 s3 s4 s5 s6       = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -<)
comb72 f s1 s2 s3 s4 s5 s6 s7    = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -<)
comb82 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s5 -*- s8 -<)

comb13 f s1                      = (f -$- s1 -<<)
comb23 f s1 s2                   = (f -$- s1 -*- s2 -<<)
comb33 f s1 s2 s3                = (f -$- s1 -*- s2 -*- s3 -<<)
comb43 f s1 s2 s3 s4             = (f -$- s1 -*- s2 -*- s3 -*- s4 -<<)
comb53 f s1 s2 s3 s4 s5          = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -<<)
comb63 f s1 s2 s3 s4 s5 s6       = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -<<)
comb73 f s1 s2 s3 s4 s5 s6 s7    = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -<<)
comb83 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s5 -*- s8 -<<)

comb14 f s1                      = (f -$- s1 -<<<)
comb24 f s1 s2                   = (f -$- s1 -*- s2 -<<<)
comb34 f s1 s2 s3                = (f -$- s1 -*- s2 -*- s3 -<<<)
comb44 f s1 s2 s3 s4             = (f -$- s1 -*- s2 -*- s3 -*- s4 -<<<)
comb54 f s1 s2 s3 s4 s5          = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -<<<)
comb64 f s1 s2 s3 s4 s5 s6       = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -<<<)
comb74 f s1 s2 s3 s4 s5 s6 s7    = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -<<<)
comb84 f s1 s2 s3 s4 s5 s6 s7 s8 = (f -$- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*- s8 -<<<)

scanl11 ns i s1          =        comb21 ns st s1 
  where st               = i -&>- comb21 ns st s1 
scanl21 ns i s1 s2       =        comb31 ns st s1 s2
  where st               = i -&>- comb31 ns st s1 s2
scanl31 ns i s1 s2 s3    =        comb41 ns st s1 s2 s3
  where st               = i -&>- comb41 ns st s1 s2 s3
scanl41 ns i s1 s2 s3 s4 =        comb51 ns st s1 s2 s3 s4
  where st               = i -&>- comb51 ns st s1 s2 s3 s4

scanl12 ns (i1,i2) s1          = let (ns1,ns2) = comb32 ns st1 st2 s1
                                     (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                 in  (ns1,ns2)
scanl22 ns (i1,i2) s1 s2       = let (ns1,ns2) = comb42 ns st1 st2 s1 s2
                                     (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                 in  (ns1,ns2)
scanl32 ns (i1,i2) s1 s2 s3    = let (ns1,ns2) = comb52 ns st1 st2 s1 s2 s3
                                     (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                 in  (ns1,ns2)
scanl42 ns (i1,i2) s1 s2 s3 s4 = let (ns1,ns2) = comb62 ns st1 st2 s1 s2 s3 s4
                                     (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                 in  (ns1,ns2)

scanl13 ns (i1,i2,i3) s1          = let (ns1,ns2,ns3) = comb43 ns st1 st2 st3 s1
                                        (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                    in  (ns1,ns2,ns3)
scanl23 ns (i1,i2,i3) s1 s2       = let (ns1,ns2,ns3) = comb53 ns st1 st2 st3 s1 s2
                                        (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                    in  (ns1,ns2,ns3)
scanl33 ns (i1,i2,i3) s1 s2 s3    = let (ns1,ns2,ns3) = comb63 ns st1 st2 st3 s1 s2 s3
                                        (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                    in  (ns1,ns2,ns3)
scanl43 ns (i1,i2,i3) s1 s2 s3 s4 = let (ns1,ns2,ns3) = comb73 ns st1 st2 st3 s1 s2 s3 s4
                                        (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                    in  (ns1,ns2,ns3)

scanl14 ns (i1,i2,i3,i4) s1          = let (ns1,ns2,ns3,ns4) = comb54 ns st1 st2 st3 st4 s1
                                           (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                       in  (ns1,ns2,ns3,ns4)
scanl24 ns (i1,i2,i3,i4) s1 s2       = let (ns1,ns2,ns3,ns4) = comb64 ns st1 st2 st3 st4 s1 s2
                                           (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                       in  (ns1,ns2,ns3,ns4)
scanl34 ns (i1,i2,i3,i4) s1 s2 s3    = let (ns1,ns2,ns3,ns4) = comb74 ns st1 st2 st3 st4 s1 s2 s3
                                           (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                       in  (ns1,ns2,ns3,ns4)
scanl44 ns (i1,i2,i3,i4) s1 s2 s3 s4 = let (ns1,ns2,ns3,ns4) = comb84 ns st1 st2 st3 st4 s1 s2 s3 s4
                                           (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                       in  (ns1,ns2,ns3,ns4)

scanld01 ns i             = st 
  where st                = i -&>- comb11 ns st 
scanld11 ns i s1          = st
  where st                = i -&>- comb21 ns st s1 
scanld21 ns i s1 s2       = st
  where st                = i -&>- comb31 ns st s1 s2
scanld31 ns i s1 s2 s3    = st
  where st                = i -&>- comb41 ns st s1 s2 s3
scanld41 ns i s1 s2 s3 s4 = st
  where st                = i -&>- comb51 ns st s1 s2 s3 s4

scanld02 ns (i1,i2)             = let (ns1,ns2) = comb22 ns st1 st2
                                      (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                  in  (st1,st2)
scanld12 ns (i1,i2) s1          = let (ns1,ns2) = comb32 ns st1 st2 s1
                                      (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                  in  (st1,st2)
scanld22 ns (i1,i2) s1 s2       = let (ns1,ns2) = comb42 ns st1 st2 s1 s2
                                      (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                  in  (st1,st2)
scanld32 ns (i1,i2) s1 s2 s3    = let (ns1,ns2) = comb52 ns st1 st2 s1 s2 s3
                                      (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                  in  (st1,st2)
scanld42 ns (i1,i2) s1 s2 s3 s4 = let (ns1,ns2) = comb62 ns st1 st2 s1 s2 s3 s4
                                      (st1,st2) = (i1 -&>- ns1, i2 -&>- ns2)
                                  in  (st1,st2)

scanld03 ns (i1,i2,i3)             = let (ns1,ns2,ns3) = comb33 ns st1 st2 st3
                                         (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                     in  (st1,st2,st3)
scanld13 ns (i1,i2,i3) s1          = let (ns1,ns2,ns3) = comb43 ns st1 st2 st3 s1
                                         (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                     in  (st1,st2,st3)
scanld23 ns (i1,i2,i3) s1 s2       = let (ns1,ns2,ns3) = comb53 ns st1 st2 st3 s1 s2
                                         (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                     in  (st1,st2,st3)
scanld33 ns (i1,i2,i3) s1 s2 s3    = let (ns1,ns2,ns3) = comb63 ns st1 st2 st3 s1 s2 s3
                                         (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                     in  (st1,st2,st3)
scanld43 ns (i1,i2,i3) s1 s2 s3 s4 = let (ns1,ns2,ns3) = comb73 ns st1 st2 st3 s1 s2 s3 s4
                                         (st1,st2,st3) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3)
                                     in  (st1,st2,st3)

scanld04 ns (i1,i2,i3,i4)             = let (ns1,ns2,ns3,ns4) = comb44 ns st1 st2 st3 st4
                                            (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                        in  (st1,st2,st3,st4)
scanld14 ns (i1,i2,i3,i4) s1          = let (ns1,ns2,ns3,ns4) = comb54 ns st1 st2 st3 st4 s1
                                            (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                        in  (st1,st2,st3,st4)
scanld24 ns (i1,i2,i3,i4) s1 s2       = let (ns1,ns2,ns3,ns4) = comb64 ns st1 st2 st3 st4 s1 s2
                                            (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                        in  (st1,st2,st3,st4)
scanld34 ns (i1,i2,i3,i4) s1 s2 s3    = let (ns1,ns2,ns3,ns4) = comb74 ns st1 st2 st3 st4 s1 s2 s3
                                            (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                        in  (st1,st2,st3,st4)
scanld44 ns (i1,i2,i3,i4) s1 s2 s3 s4 = let (ns1,ns2,ns3,ns4) = comb84 ns st1 st2 st3 st4 s1 s2 s3 s4
                                            (st1,st2,st3,st4) = (i1 -&>- ns1, i2 -&>- ns2, i3 -&>- ns3, i4 -&>- ns4)
                                        in  (st1,st2,st3,st4)

moore11 ns od i s1          =        comb11 od st
  where st                  = i -&>- comb21 ns st s1
moore12 ns od i s1          =        comb12 od st
  where st                  = i -&>- comb21 ns st s1
moore13 ns od i s1          =        comb13 od st
  where st                  = i -&>- comb21 ns st s1
moore14 ns od i s1          =        comb14 od st
  where st                  = i -&>- comb21 ns st s1
moore21 ns od i s1 s2       =        comb11 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore22 ns od i s1 s2       =        comb12 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore23 ns od i s1 s2       =        comb13 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore24 ns od i s1 s2       =        comb14 od st
  where st                  = i -&>- comb31 ns st s1 s2
moore31 ns od i s1 s2 s3    =        comb11 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore32 ns od i s1 s2 s3    =        comb12 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore33 ns od i s1 s2 s3    =        comb13 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore34 ns od i s1 s2 s3    =        comb14 od st
  where st                  = i -&>- comb41 ns st s1 s2 s3
moore41 ns od i s1 s2 s3 s4 =        comb11 od st
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
moore42 ns od i s1 s2 s3 s4 =        comb12 od st
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
moore43 ns od i s1 s2 s3 s4 =        comb13 od st
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
moore44 ns od i s1 s2 s3 s4 =        comb14 od st
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4

mealy11 ns od i s1          =        comb21 od st s1
  where st                  = i -&>- comb21 ns st s1
mealy12 ns od i s1          =        comb22 od st s1
  where st                  = i -&>- comb21 ns st s1
mealy13 ns od i s1          =        comb23 od st s1
  where st                  = i -&>- comb21 ns st s1
mealy14 ns od i s1          =        comb24 od st s1
  where st                  = i -&>- comb21 ns st s1
mealy21 ns od i s1 s2       =        comb31 od st s1 s2
  where st                  = i -&>- comb31 ns st s1 s2
mealy22 ns od i s1 s2       =        comb32 od st s1 s2
  where st                  = i -&>- comb31 ns st s1 s2
mealy23 ns od i s1 s2       =        comb33 od st s1 s2
  where st                  = i -&>- comb31 ns st s1 s2
mealy24 ns od i s1 s2       =        comb34 od st s1 s2
  where st                  = i -&>- comb31 ns st s1 s2
mealy31 ns od i s1 s2 s3    =        comb41 od st s1 s2 s3
  where st                  = i -&>- comb41 ns st s1 s2 s3
mealy32 ns od i s1 s2 s3    =        comb42 od st s1 s2 s3
  where st                  = i -&>- comb41 ns st s1 s2 s3
mealy33 ns od i s1 s2 s3    =        comb43 od st s1 s2 s3
  where st                  = i -&>- comb41 ns st s1 s2 s3
mealy34 ns od i s1 s2 s3    =        comb44 od st s1 s2 s3
  where st                  = i -&>- comb41 ns st s1 s2 s3
mealy41 ns od i s1 s2 s3 s4 =        comb51 od st s1 s2 s3 s4
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
mealy42 ns od i s1 s2 s3 s4 =        comb52 od st s1 s2 s3 s4
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
mealy43 ns od i s1 s2 s3 s4 =        comb53 od st s1 s2 s3 s4
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
mealy44 ns od i s1 s2 s3 s4 =        comb54 od st s1 s2 s3 s4
  where st                  = i -&>- comb51 ns st s1 s2 s3 s4
