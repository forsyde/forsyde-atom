module ForSyDe.Atom.Behavior (  
  -- | The behavior layer is enabled by the 'Value' type constructor.
  
  Value(..),

  -- * Utility functions

  -- | These functions are provided for user convenience

  value, fromValue, unsafeFromValue, isPresent, isNotPresent, isAbsent, isUndefined,

  -- * Behavior atoms

  -- | These are the primitive (undivisible) blocks for implementing
  -- behavior wrappers. Each atom has a distinct behavioural semantic
  -- which reflects a real (analizable, implementable) behavior.
  
  Behavior(..),
  
  -- * Behavior wrappers
  
  -- ** @psi@
  
  -- | The @psi@/XY/ wrapper wraps a function with /X/ inputs and /Y/
  -- outputs in a default ForSyDe behavior. E.g.:
  --
  -- >>> let (v,u,a)=(Value 1, Undef, Abst)
  -- >>> (v,u,a)
  -- (1,?,⟂)
  -- >>> let f a b c = (a+b,b-c)
  -- >>> let wf = psi32 f
  -- >>> t: wf
  -- wf :: Num b => Value b -> Value b -> Value b -> (Value b, Value b)
  -- >>> wf v v v
  -- (2,0)
  -- >>> wf v u v
  -- (?,?)
  -- >>> wf v a v
  -- (*** Exception: Illegal occurrence of an absent and present event
  
  psi11, psi12, psi13, psi14, psi15, psi16, psi17, psi18,
  psi21, psi22, psi23, psi24, psi25, psi26, psi27, psi28,
  psi31, psi32, psi33, psi34, psi35, psi36, psi37, psi38,
  psi41, psi42, psi43, psi44, psi45, psi46, psi47, psi48,
  psi51, psi52, psi53, psi54, psi55, psi56, psi57, psi58,
  psi61, psi62, psi63, psi64, psi65, psi66, psi67, psi68,
  psi71, psi72, psi73, psi74, psi75, psi76, psi77, psi78,
  psi81, psi82, psi83, psi84, psi85, psi86, psi87, psi88,

  -- ** @store@

  -- | The @store@/X/ wrapper pushes the present defined values into a
  -- given list (e.g. FIFO) buffer.
  --
  -- >>> store5 (Value [1,2]) (Value 3) Undef (Value 4) Abst Undef
  -- [4,3,1,2]
  -- >>> store5 Undef         (Value 3) Undef (Value 4) Abst Undef
  -- ?
  -- >>> store5 Abst          (Value 3) Undef (Value 4) Abst Undef
  -- *** Exception: Illegal occurrence of an absent and present event
  
  store1, store2, store3, store4, store5, store6, store7, store8,

  -- ** @reduce@

  -- | The @reduce@/X/ merges the values belonging to multiple signals
  -- based on a given rule
  --
  -- >>> reduce5 (+) (Value 3) (Value 4) Abst (Value 1) Abst
  -- 8
  -- >>> reduce5 (+) (Value 3) (Value 4) Abst (Value 1) Undef
  -- ?

  reduce2, reduce3, reduce4, reduce5, reduce6, reduce7, reduce8,

  -- ** @replace@

  -- | The @replace@/X/ class of wrappers replaces a value with an
  -- @[ V - another value | U - undefined value | A - absent event ]@
  -- based on a boolean predicate.
  
  replaceV, replaceU, replaceA, unsafeReplaceV, unsafeReplaceU,
  
  ) where

import ForSyDe.Atom.Behavior.Atom
import ForSyDe.Atom.Behavior.Cons
import ForSyDe.Atom.Behavior.ValueExt
