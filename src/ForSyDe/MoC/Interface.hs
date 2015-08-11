module ForSyDe.MoC.Interface where

import ForSyDe.Core
import qualified ForSyDe.MoC.SY as SY
import qualified ForSyDe.MoC.SDF as SDF

sy2sdf :: SY.Signal a -> SDF.Signal a
sy2sdf = SDF.fromS . convertTokens . SY.toS
  where 
    convertTokens NullS        = NullS
    convertTokens (Abst  :-xs) = convertTokens xs
    convertTokens (Prst x:-xs) = x :- convertTokens xs

sdf2sy :: SDF.Signal a -> SY.Signal a
sdf2sy = SY.fromS . convertTokens . SDF.toS
  where 
    convertTokens = (<$>) pure
