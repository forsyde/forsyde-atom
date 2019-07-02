{-# LANGUAGE PostfixOperators, TypeFamilies #-}
module ForSyDe.Atom.MoC.SAXX where

import ForSyDe.Atom.MoC
import ForSyDe.Atom.MoC.Stream

kernel11 sf s1                      = (sf -*- s1 -*)
kernel21 sf s1 s2                   = (sf -*- s1 -*- s2 -*)
kernel31 sf s1 s2 s3                = (sf -*- s1 -*- s2 -*- s3 -*)
kernel41 sf s1 s2 s3 s4             = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*)
kernel51 sf s1 s2 s3 s4 s5          = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*)
kernel61 sf s1 s2 s3 s4 s5 s6       = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*)
kernel71 sf s1 s2 s3 s4 s5 s6 s7    = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*)
kernel81 sf s1 s2 s3 s4 s5 s6 s7 s8 = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*- s8 -*)
kernel12 sf s1                      = (sf -*- s1 -*<)
kernel22 sf s1 s2                   = (sf -*- s1 -*- s2 -*<)
kernel32 sf s1 s2 s3                = (sf -*- s1 -*- s2 -*- s3 -*<)
kernel42 sf s1 s2 s3 s4             = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*<)
kernel52 sf s1 s2 s3 s4 s5          = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*<)
kernel62 sf s1 s2 s3 s4 s5 s6       = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*<)
kernel72 sf s1 s2 s3 s4 s5 s6 s7    = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*<)
kernel82 sf s1 s2 s3 s4 s5 s6 s7 s8 = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s5 -*- s8 -*<)
kernel13 sf s1                      = (sf -*- s1 -*<<)
kernel23 sf s1 s2                   = (sf -*- s1 -*- s2 -*<<)
kernel33 sf s1 s2 s3                = (sf -*- s1 -*- s2 -*- s3 -*<<)
kernel43 sf s1 s2 s3 s4             = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*<<)
kernel53 sf s1 s2 s3 s4 s5          = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*<<)
kernel63 sf s1 s2 s3 s4 s5 s6       = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*<<)
kernel73 sf s1 s2 s3 s4 s5 s6 s7    = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*<<)
kernel83 sf s1 s2 s3 s4 s5 s6 s7 s8 = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s5 -*- s8 -*<<)
kernel14 sf s1                      = (sf -*- s1 -*<<<)
kernel24 sf s1 s2                   = (sf -*- s1 -*- s2 -*<<<)
kernel34 sf s1 s2 s3                = (sf -*- s1 -*- s2 -*- s3 -*<<<)
kernel44 sf s1 s2 s3 s4             = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*<<<)
kernel54 sf s1 s2 s3 s4 s5          = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*<<<)
kernel64 sf s1 s2 s3 s4 s5 s6       = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*<<<)
kernel74 sf s1 s2 s3 s4 s5 s6 s7    = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*<<<)
kernel84 sf s1 s2 s3 s4 s5 s6 s7 s8 = (sf -*- s1 -*- s2 -*- s3 -*- s4 -*- s5 -*- s6 -*- s7 -*- s8 -*<<<)

detector11 ns od i s1 = (od -$- g -*) 
  where g  = ns -$- st -*- s1
        st = i :- g
detector21 ns od i s1 s2 = (od -$- g -*) 
  where g  = ns -$- st -*- s1 -*- s2
        st = i :- g
detector31 ns od i s1 s2 s3 = (od -$- g -*) 
  where g  = ns -$- st -*- s1 -*- s2 -*- s3
        st = i :- g
detector41 ns od i s1 s2 s3 s4 = (od -$- g -*) 
  where g  = ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
        st = i :- g
detector12 ns od i s1 = (od -$- g -*<) 
  where g  = ns -$- st -*- s1
        st = i :- g
detector22 ns od i s1 s2 = (od -$- g -*<) 
  where g  = ns -$- st -*- s1 -*- s2
        st = i :- g
detector32 ns od i s1 s2 s3 = (od -$- g -*<) 
  where g  = ns -$- st -*- s1 -*- s2 -*- s3
        st = i :- g
detector42 ns od i s1 s2 s3 s4 = (od -$- g -*<) 
  where g  = ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
        st = i :- g
detector13 ns od i s1 = (od -$- g -*<<) 
  where g  = ns -$- st -*- s1
        st = i :- g
detector23 ns od i s1 s2 = (od -$- g -*<<) 
  where g  = ns -$- st -*- s1 -*- s2
        st = i :- g
detector33 ns od i s1 s2 s3 = (od -$- g -*<<) 
  where g  = ns -$- st -*- s1 -*- s2 -*- s3
        st = i :- g
detector43 ns od i s1 s2 s3 s4 = (od -$- g -*<<) 
  where g  = ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
        st = i :- g
detector14 ns od i s1 = (od -$- g -*<<<) 
  where g  = ns -$- st -*- s1
        st = i :- g
detector24 ns od i s1 s2 = (od -$- g -*<<<) 
  where g  = ns -$- st -*- s1 -*- s2
        st = i :- g
detector34 ns od i s1 s2 s3 = (od -$- g -*<<<) 
  where g  = ns -$- st -*- s1 -*- s2 -*- s3
        st = i :- g
detector44 ns od i s1 s2 s3 s4 = (od -$- g -*<<<) 
  where g  = ns -$- st -*- s1 -*- s2 -*- s3 -*- s4
        st = i :- g

