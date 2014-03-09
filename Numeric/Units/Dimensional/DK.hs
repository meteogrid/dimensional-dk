{-
Numeric.Dimensional -- Statically checked physical dimensions
Bjorn Buckwalter, bjorn.buckwalter@gmail.com
License: BSD3


= Summary =

In this module we provide data types for performing arithmetic with
physical quantities and units. Information about the physical
dimensions of the quantities/units is embedded in their types and
the validity of operations is verified by the type checker at compile
time. The boxing and unboxing of numerical values as quantities is
done by multiplication and division of units, of which an incomplete
set is provided.

We limit ourselves to "Newtonian" physics. We do not attempt to
accommodate relativistic physics in which e.g. addition of length
and time would be valid.

As far as possible and/or practical the conventions and guidelines
of NIST's "Guide for the Use of the International System of Units
(SI)" [1] are followed. Occasionally we will reference specific
sections from the guide and deviations will be explained.


= Disclaimer =

Merely an engineer, the author doubtlessly uses a language and
notation that makes mathematicians and physicist cringe. He does
not mind constructive criticism (or darcs patches).

The sets of functions and units defined herein are incomplete and
reflect only the author's needs to date. Again, patches are welcome.

The author has elected to keep the module detached from the standard(?)
Haskell library hierarchy. In part because the module name space
layout seems to be an open issue and in part because he is unsure
where to fit it in.


= Preliminaries =

This module requires GHC 7.8 or later. We utilize Data Kinds, TypeNats,
Closed Type Families, etc. Clients of the module are generally not
required to use these extensions.

Clients probably will want to use the NegativeLiterals extension.
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NumDecimals #-}

{- |
   Copyright  : Copyright (C) 2006-2014 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn.buckwalter@gmail.com
   Stability  : Stable
   Portability: GHC only?

Please refer to the literate Haskell code for documentation of both API
and implementation.
-}

module Numeric.Units.Dimensional.DK
  ( (^), (^/), (**), (*), (/), (+), (-), (*~), (/~),
    Dimensional,
    Unit(..), Quantity, Dimension (Dim), Atomicity(..),
    DOne, DLength, DMass, DTime, DElectricCurrent, DThermodynamicTemperature, DAmountOfSubstance, DLuminousIntensity,
    Dimensionless, Length, Mass, Time, ElectricCurrent, ThermodynamicTemperature, AmountOfSubstance, LuminousIntensity,
    type (*), type (/), type (^), Root, Recip,
    negate, abs, nroot, sqrt, cbrt,
    (*~~), (/~~), sum, mean, dimensionlessLength,
    exp, log, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh, atan2,
    one, _0, _1, _2, _3, _4, _5, _6, _7, _8, _9, pi, tau,
    siUnit, showIn,
    Dimension' (Dim'), KnownDimension, toSIBasis, getSIBasis,
    prefix, alias, unit, dimensionless, unD, name,
    dmap, changeRep,
    deka, deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta,
    deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto,
    metre, meter, gram, second, ampere, kelvin, mole, candela
  )
  where

import Prelude
  ( Show, Eq, Ord, Enum, Num, Fractional, Floating, Real, RealFloat, Functor, fmap
  , (.), flip, show, (++), undefined, otherwise, (==), String, unwords
  , map, null, Integer, Int, ($), zipWith, uncurry, concat, realToFrac
  )
import qualified Prelude
import Data.List (genericLength)
import Data.Maybe (Maybe (Just, Nothing), catMaybes)
import Numeric.NumType.DK
  ( NumType (Zero, Pos1, Pos2, Pos3), (+)(), (-)()
  , pos2, pos3
  , KnownNumType, toNum
  )
import qualified Numeric.NumType.DK as N
import Data.Proxy (Proxy(..))
import Data.Foldable (Foldable(foldr, fold))
import Data.Monoid (Monoid(..))
import qualified Numeric.Units.Dimensional.DK.UnitNames as Name
import Numeric.Units.Dimensional.DK.UnitNames (NameAtom, UnitName, UnitNameTransformer, UnitNameTransformer2, atomic)

{-
We will reuse the operators and function names from the Prelude.
To prevent unpleasant surprises we give operators the same fixity
as the Prelude.
-}

infixr 8  ^, ^/, **
infixl 7  *, /
infixl 6  +, -
infixl 7  *~, /~

{-
= Dimensional =

Our primary objective is to define a data type that can be used to
represent (while still differentiating between) units and quantities.
There are two reasons for consolidating units and quantities in one
data type. The first being to allow code reuse as they are largely
subject to the same operations. The second being that it allows
reuse of operators (and functions) between the two without resorting
to occasionally cumbersome type classes.

We call this data type 'Dimensional' to capture the notion that the
units and quantities it represents have physical dimensions.
-}

newtype Quantity (d :: Dimension) (v :: *)
      = Quantity v deriving (Eq, Ord, Enum)

data Unit (a :: Atomicity) (d :: Dimension) (v :: *)
      = Unit UnitName v

data Atomicity = Atomic | Composite

class Dimensional (var :: Dimension -> * -> *) where
  extractValue :: var d v -> v
  extractName :: var d v -> Maybe UnitName
  injectValue :: (Maybe UnitName) -> v -> var d v
  dmap :: (v1 -> v2) -> var d v1 -> var d v2

instance Dimensional Quantity where
  extractValue (Quantity x) = x
  extractName _ = Nothing
  injectValue _ x = Quantity x
  dmap f (Quantity x) = Quantity (f x)

instance Dimensional (Unit a) where
  extractValue (Unit _ x) = x
  extractName (Unit n _) = Just n
  injectValue (Just n) x = Unit n x
  injectValue _ x = Prelude.error "Shouldn't be reachable. Needed to name a quantity."
  dmap f (Unit n x) = Unit n (f x)

type family DimensionalCombination (v1 :: Dimension -> * -> *) (v2 :: Dimension -> * -> *) :: Dimension -> * -> * where
  DimensionalCombination (Quantity) (Quantity) = Quantity
  DimensionalCombination (Quantity) (Unit a2)  = Quantity
  DimensionalCombination (Unit a1)  (Quantity) = Quantity
  DimensionalCombination (Unit a1)  (Unit a2)  = Unit Composite

type family DimensionalDropAtomicity (v1 :: Dimension -> * -> *) :: Dimension -> * -> * where
  DimensionalDropAtomicity (Unit a)   = Unit Composite
  DimensionalDropAtomicity (Quantity) = Quantity

{-

= The dimension 'd' of 'Dimensional' =

The phantom type variable d encompasses the physical dimension of
the 'Dimensional'. As detailed in [5] there are seven base dimensions,
which can be combined in integer powers to a given physical dimension.
We represent physical dimensions as the powers of the seven base
dimensions that make up the given dimension. The powers are represented
using NumTypes. For convenience we collect all seven base dimensions
in a data type 'Dim'.
-}

data Dimension = Dim NumType NumType NumType NumType NumType NumType NumType

{-
where the respective dimensions are represented by type variables
using the following convention.

    l  -- Length
    m  -- Mass
    t  -- Time
    i  -- Electric current
    th -- Thermodynamic temperature
    n  -- Amount of substance
    j  -- Luminous intensity

We could have chosen to provide type variables for the seven base
dimensions in 'Dimensional' instead of creating a new data type
'Dim'. However, that would have made any type signatures involving
'Dimensional' very cumbersome.  By encompassing the physical dimension
in a single type variable we can "hide" the cumbersome type arithmetic
behind convenient type classes as will be seen later.

Using our 'Dim' data type we define some type synonyms for convenience
and illustrative purposes. We start with the base dimensions.
-}

type DOne         = Dim Zero Zero Zero Zero Zero Zero Zero
type DLength      = Dim Pos1 Zero Zero Zero Zero Zero Zero
type DMass        = Dim Zero Pos1 Zero Zero Zero Zero Zero
type DTime        = Dim Zero Zero Pos1 Zero Zero Zero Zero
type DElectricCurrent          = Dim Zero Zero Zero Pos1 Zero Zero Zero
type DThermodynamicTemperature = Dim Zero Zero Zero Zero Pos1 Zero Zero
type DAmountOfSubstance        = Dim Zero Zero Zero Zero Zero Pos1 Zero
type DLuminousIntensity        = Dim Zero Zero Zero Zero Zero Zero Pos1

{-
Using the above type synonyms we can define type synonyms for
quantities of particular physical dimensions.

Quantities with the base dimensions.
-}

type Dimensionless            = Quantity DOne
type Length                   = Quantity DLength
type Mass                     = Quantity DMass
type Time                     = Quantity DTime
type ElectricCurrent          = Quantity DElectricCurrent
type ThermodynamicTemperature = Quantity DThermodynamicTemperature
type AmountOfSubstance        = Quantity DAmountOfSubstance
type LuminousIntensity        = Quantity DLuminousIntensity

{-

= Arithmetic on physical dimensions =

When performing arithmetic on units and quantities the arithmetics
must be applied to both the numerical values of the Dimensionals
but also to their physical dimensions. The type level arithmetic
on physical dimensions is governed by multi-parameter type classes
and functional dependences.

Multiplication of dimensions corresponds to adding of the base
dimensions' exponents.
-}

type family (a::Dimension) * (b::Dimension) where  -- constrain kinds??
  DOne * d = d
  d * DOne = d
  (Dim l  m  t  i  th  n  j) * (Dim l' m' t' i' th' n' j')
    = Dim (l + l') (m + m') (t + t') (i + i') (th + th') (n + n') (j + j')

{-
Division of dimensions corresponds to subtraction of the base
dimensions' exponents.
-}

type family (a::Dimension) / (d::Dimension) where
  d / DOne = d
  d / d = DOne
  (Dim l  m  t  i  th  n  j) / (Dim l' m' t' i' th' n' j')
    = Dim (l - l') (m - m') (t - t') (i - i') (th - th') (n - n') (j - j')

type Recip (d :: Dimension) = DOne / d

{-
We could provide the 'Mul' and 'Div' classes with full functional
dependencies but that would be of limited utility as there is no
obvious use for "backwards" type inference and would also limit
what we can achieve overlapping instances. (In particular, it breaks
the 'Extensible' module.)

We limit ourselves to integer powers of Dimensionals as fractional
powers make little physical sense. Since the value of the exponent
affects the type of the result the value of the exponent must be
visible to the type system, therefore we will generally represent
the exponent with a 'NumType'.

Powers of dimensions corresponds to multiplication of the base
dimensions' exponents by the exponent.
-}

type family (d::Dimension) ^ (x::NumType) where
  DOne ^ x = DOne
  d ^ Zero = DOne
  d ^ Pos1 = d
  (Dim l  m  t  i  th  n  j) ^ x
    = Dim (l N.* x) (m N.* x) (t N.* x) (i N.* x) (th N.* x) (n N.* x) (j N.* x)

{-
Roots of dimensions corresponds to division of the base dimensions'
exponents by order(?) of the root.
-}

type family Root (d::Dimension) (x::NumType) where
  Root DOne x = DOne
  Root d Pos1 = d
  Root (Dim l  m  t  i  th  n  j) x
    = Dim (l N./ x) (m N./ x) (t N./ x) (i N./ x) (th N./ x) (n N./ x) (j N./ x)

{-

= Arithmetic on units and quantities =

Thanks to the arithmetic on physical dimensions having been sorted
out separately a lot of the arithmetic on Dimensionals is straight
forward. In particular the type signatures are much simplified.

Multiplication, division and powers apply to both units and quantities.
-}

liftUntyped :: (Dimensional v1, Dimensional v2, v2 ~ DimensionalDropAtomicity v1) => (v -> v) -> UnitNameTransformer -> (v1 d1 v) -> (v2 d2 v)
liftUntyped f nt x = let x' = extractValue x
                         n = extractName x
                         n' = nt n
                      in injectValue n' (f x')

liftUntypedQ :: (Dimensional v1) => (v -> v) -> v1 d1 v -> Quantity d2 v
liftUntypedQ f x = let x' = extractValue x
                    in Quantity (f x')

liftUntyped2 :: (Dimensional v1, Dimensional v2, Dimensional v3, v3 ~ DimensionalCombination v1 v2) => (v -> v -> v) -> UnitNameTransformer2 -> v1 d1 v -> v2 d2 v -> v3 d3 v
liftUntyped2 f nt x1 x2 = let x1' = extractValue x1
                              x2' = extractValue x2
                              n1 = extractName x1
                              n2 = extractName x2
                              n' = nt n1 n2
                        in injectValue n' (f x1' x2') 

liftUntyped2Q :: (Dimensional v1, Dimensional v2) => (v -> v -> v) -> v1 d1 v -> v2 d2 v -> Quantity d3 v
liftUntyped2Q f x1 x2 = let x1' = extractValue x1
                            x2' = extractValue x2
                         in Quantity (f x1' x2') 

(*) :: (Dimensional v1, Dimensional v2, Dimensional p, p ~ (DimensionalCombination v1 v2), Num v) => v1 d1 v -> v2 d2 v -> p (d1 * d2) v
(*) = liftUntyped2 (Prelude.*) (Name.product')

(/) :: (Dimensional v1, Dimensional v2, Dimensional p, p ~ (DimensionalCombination v1 v2), Fractional v) => v1 d1 v -> v2 d2 v -> p (d1 / d2) v
(/) = liftUntyped2 (Prelude./) (Name.quotient')

(^) :: (KnownNumType i, Dimensional v1, Dimensional v2, v2 ~ DimensionalDropAtomicity v1, Fractional v)
    => v1 d1 v -> Proxy i -> v2 (d1 ^ i) v
x ^ n = let n' = (toNum n) :: Integer
            n'' = (toNum n) :: Int
         in liftUntyped (Prelude.^^ n') (Name.toPower' n'') x

{-

A special case is that dimensionless quantities are not restricted
to integer exponents. This is accommodated by the '**' operator
defined later.


= Quantity operations =

Some additional operations obviously only make sense for quantities.
Of these, negation, addition and subtraction are particularly simple
as they are done in a single physical dimension.
-}

negate :: (Dimensional v1, Num v) => v1 d v -> Quantity d v
negate = liftUntypedQ (Prelude.negate)

(+) :: (Dimensional v1, Dimensional v2, Dimensional p, p ~ (DimensionalCombination v1 v2), Num v) => v1 d v -> v2 d v -> Quantity d v
(+) = liftUntyped2Q (Prelude.+)

(-) :: (Dimensional v1, Dimensional v2, Dimensional p, p ~ (DimensionalCombination v1 v2), Num v) => v1 d v -> v2 d v -> Quantity d v
(-) = liftUntyped2Q (Prelude.-)

{-
Absolute value.
-}

abs :: (Dimensional v1, Num v) => v1 d v -> Quantity d v
abs = liftUntypedQ (Prelude.abs)


{-
Roots of arbitrary (integral) degree. Appears to occasionally be useful
for units as well as quantities.
-}

nroot :: (KnownNumType n, Dimensional v1, Floating v)
      => Proxy n -> v1 d v -> Quantity (Root d n) v
nroot n = let n' = 1 Prelude./ toNum n
           in liftUntypedQ (Prelude.** n')

{-
We provide short-hands for the square and cubic roots.
-}

sqrt :: (Dimensional v1, Floating v)
     => v1 d v -> Quantity (Root d Pos2) v
sqrt = nroot pos2
cbrt :: (Dimensional v1, Floating v)
     => v1 d v -> Quantity (Root d Pos3) v
cbrt = nroot pos3

{-
We also provide an operator alternative to nroot for those that
prefer such.
-}

(^/) :: (KnownNumType n, Dimensional v1, Floating v)
     => v1 d v -> Proxy n -> Quantity (Root d n) v
(^/) = flip nroot

{-
Since quantities form a monoid under addition, but not under multiplication unless they are dimensionless,
we will define a monoid instance that adds.
-}
instance (Num v) => Monoid (Quantity d v) where
  mempty = _0
  mappend = (+)

{-

= List functions =

Here we define operators and functions to make working with homogenuous
lists of dimensionals more convenient.

We define two convenience operators for applying units to all
elements of a functor (e.g. a list).
-}

(*~~) :: (Functor f, Num v) => f v -> Unit a d v -> f (Quantity d v)
xs *~~ u = fmap ((* u) . dimensionless) xs

(/~~) :: (Functor f, Fractional v) => f (Quantity d v) -> Unit a d v -> f v
xs /~~ u = fmap (/~ u) xs

infixl 7 *~~, /~~

{-
The sum of all elements in a list.
-}

sum :: (Num v, Foldable f) => f (Quantity d v) -> Quantity d v
sum = foldr (+) _0

{-
The arithmetic mean of all elements in a list.
-}

mean :: (Fractional v, Foldable f) => f (Quantity d v) -> Quantity d v
mean = uncurry (/) . foldr accumulate (_0, _0)
  where
    accumulate val (accum, count) = (accum + val, count + _1)

{-
The length of the list as a 'Dimensionless'. This can be useful for
purposes of e.g. calculating averages.
-}

dimensionlessLength :: (Dimensional v1, Num v) => [(v1 d v)] -> Dimensionless v
dimensionlessLength = dimensionless . genericLength

{-

= Dimensionless =

For dimensionless quantities pretty much any operation is applicable.
We provide this freedom by making 'Dimensionless' an instance of
'Functor'.
-}

instance Functor Dimensionless where
  fmap = dmap

{-
We continue by defining elementary functions on 'Dimensionless'
that may be obviously useful.
-}

exp, log, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh
  :: Floating a => Dimensionless a -> Dimensionless a
exp   = fmap Prelude.exp
log   = fmap Prelude.log
sin   = fmap Prelude.sin
cos   = fmap Prelude.cos
tan   = fmap Prelude.tan
asin  = fmap Prelude.asin
acos  = fmap Prelude.acos
atan  = fmap Prelude.atan
sinh  = fmap Prelude.sinh
cosh  = fmap Prelude.cosh
tanh  = fmap Prelude.tanh
asinh = fmap Prelude.asinh
acosh = fmap Prelude.acosh
atanh = fmap Prelude.atanh

(**) :: (Floating v) => Dimensionless v -> Dimensionless v -> Dimensionless v
(**) = liftUntyped2Q (Prelude.**)

{-
For 'atan2' the operands need not be dimensionless but they must be
of the same type. The result will of course always be dimensionless.
-}

atan2 :: (RealFloat v) => Quantity d v -> Quantity d v -> Dimensionless v
atan2 = liftUntyped2Q Prelude.atan2

{-
We add a polymorphic @siUnit@ which can be used in place a concrete
SI unit (combination of SI base units). This allows polymorphic
quantity creation and destruction without exposing the `Dimensional`
constructor.
-}

siUnit :: forall d v.(KnownDimension d, Num v) => Unit Composite d v
siUnit = let powers = asList $ toSIBasis (Proxy :: Proxy d)
             names = [name meter, name (kilo gram), name second, name ampere, name kelvin, name mole, name candela]
             powerNames = zipWith Name.toPower powers names
             n = fold powerNames
          in Unit n 1

{-
The unit one has dimension one and is the base unit of dimensionless values.
As detailed in 7.10 "Values of quantities expressed simply as numbers:
the unit one, symbol 1" of [1] the unit one generally does not
appear in expressions. However, for us it is necessary to use 'one'
as we would any other unit to perform the "boxing" of dimensionless values.
-}

one :: Num a => Unit Composite DOne a
one = Unit Name.nameOne 1

{-

= SI prefixes (section 4.4) =

Prefixes are used to form decimal multiples and submultiples of SI
Units as described in section 4.4. We will define the SI prefixes
in terms of the 'prefix' function which applies a scale factor to a
unit.

We define all SI prefixes from Table 5. Multiples first.

-}

deka, deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta
  :: Num v => Unit Atomic d v -> Unit Composite d v
deka  = prefix ("da", "deca")  1e1 -- International English.
deca  = deka                       -- American English.
hecto = prefix ("ha", "hecto") 1e2
kilo  = prefix ("k", "kilo")   1e3
mega  = prefix ("M", "mega")   1e6
giga  = prefix ("G", "giga")   1e9
tera  = prefix ("T", "tera")   1e12
peta  = prefix ("P", "peta")   1e15
exa   = prefix ("E", "eta")    1e18
zetta = prefix ("Z", "zetta")  1e21
yotta = prefix ("Y", "yotta")  1e24

{-

Then the submultiples.

-}

deci, centi, milli, micro, nano, pico, femto, atto, zepto, yocto
  :: Fractional v => Unit Atomic d v -> Unit Composite d v
deci  = prefix ("d", "deci")  1e-1
centi = prefix ("c", "centi") 1e-2
milli = prefix ("m", "milli") 1e-3
micro = prefix ("μ", "micro") 1e-6
nano  = prefix ("n", "nano")  1e-9
pico  = prefix ("p", "pico")  1e-12
femto = prefix ("f", "femto") 1e-15
atto  = prefix ("a", "atto")  1e-18
zepto = prefix ("z", "zepto") 1e-21
yocto = prefix ("y", "yocto") 1e-24

{-

By defining SI prefixes as functions applied to a 'Unit' we satisfy
section 6.2.6 "Unacceptability of stand-alone prefixes".

-}

{-

= SI base units (section 4.1) =

Now we will define the SI base unitsi from section 4.1. To avoid a
myriad of one-letter functions that would doubtlessly cause clashes
and frustration in users' code we spell out all unit names in full,
as we did for prefixes. We also elect to spell the unit names in
singular form, as allowed by section 9.7 "Other spelling conventions".

We define the SI base units in the order of table 1.

-}

metre, meter :: Num v => Unit Atomic DLength v
metre = Unit (atomic ("m", "meter")) 1 -- International English.
meter = metre         -- American English.

{-

For mass the SI base unit is kilogram. For sensible prefixes we
define gram here (see section 6.2.7 "Prefixes and the kilogram").
The drawback is that we are forced to use 'Fractional'.

-}

gram    :: Fractional v => Unit Atomic DMass v
gram    = Unit (atomic ("g", "gram")) 1e-3
second  :: Num v => Unit Atomic DTime v
second  = Unit (atomic ("s", "second")) 1
ampere  :: Num v => Unit Atomic DElectricCurrent v
ampere  = Unit (atomic ("A", "ampere")) 1
kelvin  :: Num v => Unit Atomic DThermodynamicTemperature v
kelvin  = Unit (atomic ("K", "kelvin")) 1
mole    :: Num v => Unit Atomic DAmountOfSubstance v
mole    = Unit (atomic ("mol", "mole")) 1
candela :: Num v => Unit Atomic DLuminousIntensity v
candela = Unit (atomic ("cd", "candela")) 1


{-
For convenience we define some constants for small integer values
that often show up in formulae. We also throw in 'pi' and 'tau' for
good measure.

The constant for zero is polymorphic as proposed by Douglas McClean
(http://code.google.com/p/dimensional/issues/detail?id=39) allowing
it to express zero Length or Capacitance or Velocity etc, in addition
to the dimensionless value zero.
-}

_0 :: Num a => Quantity d a
_0 = Quantity 0

dimensionless :: Num a => a -> Quantity DOne a
dimensionless = Quantity

unD :: (Dimensional v1) => v1 DOne v -> v
unD = extractValue

(*~) :: (Fractional v, Dimensional v1, DimensionalCombination Quantity v1 ~ Quantity) => v -> v1 d v -> Quantity d v
v *~ q = (dimensionless v) * q

(/~) :: (Dimensional v1, Dimensional v2, Dimensional (DimensionalCombination v1 v2), Fractional v) => v1 d v -> v2 d v -> v
x /~ y = unD $ x / y

_1, _2, _3, _4, _5, _6, _7, _8, _9 :: (Num a) => Dimensionless a
_1 = dimensionless 1
_2 = dimensionless 2
_3 = dimensionless 3
_4 = dimensionless 4
_5 = dimensionless 5
_6 = dimensionless 6
_7 = dimensionless 7
_8 = dimensionless 8
_9 = dimensionless 9

{-
For background on 'tau' see http://tauday.com/tau-manifesto (but also
feel free to review http://www.thepimanifesto.com).
-}

pi, tau :: Floating a => Dimensionless a
pi = dimensionless Prelude.pi
tau = _2 * pi

{-

= Conversion Between Number Representations =

We provide a convenience function for converting numerical types while retaining dimensional information.
Another flavor works with a user-supplied conversion function.

-}

changeRep :: (Real a, Fractional b, Dimensional v) => v d a -> v d b
changeRep = dmap realToFrac

{-

= Term Level Representation of Dimensions =

To facilitate parsing and pretty-printing functions that may wish to operate on term-level representations of dimension,
we provide a means for converting from type-level dimensions to term-level dimensions.

At the term level, Dimension' encodes a dimension as 7 integers, representing a factorization of the dimension into the
7 SI base dimensions.

-}

data Dimension' = Dim' !Int !Int !Int !Int !Int !Int !Int deriving (Show,Eq,Ord)

class KnownDimension (d::Dimension) where toSIBasis :: Proxy d -> Dimension'
instance ( KnownNumType l
         , KnownNumType m
         , KnownNumType t
         , KnownNumType i
         , KnownNumType th
         , KnownNumType n
         , KnownNumType j
         ) => KnownDimension (Dim l m t i th n j)
  where toSIBasis _ = Dim'
                (toNum (undefined :: Proxy l))
                (toNum (undefined :: Proxy m))
                (toNum (undefined :: Proxy t))
                (toNum (undefined :: Proxy i))
                (toNum (undefined :: Proxy th))
                (toNum (undefined :: Proxy n))
                (toNum (undefined :: Proxy j))

getSIBasis :: forall val d v.(KnownDimension d) => val d v -> Dimension'
getSIBasis _ = toSIBasis (Proxy :: Proxy d)

{-

= Instances of 'Show' =

We will conclude by providing a reasonable 'Show' instance for
quantities. The “normalized” unit of the quantity is inferred
from its dimension.

We neglect units since it is unclear how to represent them
in a way that distinguishes them from quantities, or whether that is
even a requirement.

-}

instance (KnownDimension d, Fractional v, Show v) => Show (Quantity d v) where
      show = showIn siUnit

instance (KnownDimension d, Fractional v, Show v) => Show (Unit a d v) where
      show = showIn siUnit

{-
The helper function asList converts a Dimension' value to a list of integers which may be easier to manipulate.
-}

asList :: Dimension' -> [Int]
asList (Dim' l m t i th n j) = [l, m, t, i, th, n, j]

{-

= The 'prefix' function =

We will define a 'prefix' function which applies a scale factor to
a unit. The 'prefix' function will be used by other modules to
define the SI prefixes and non-SI units.
-}

data UnitPrefix v = UnitPrefix NameAtom v

applyPrefix :: (Num v) => UnitPrefix v -> Unit Atomic d v -> Unit Composite d v
applyPrefix (UnitPrefix n1 v1) (Unit n2 v2) = let n2' = Name.asAtom n2
                                                  in case n2' of
                                                       Nothing -> Prelude.error "Atomic unit turned out to have a composite name!"
                                                       (Just n2'') -> Unit (Name.atomic $ Name.applyPrefix n1 n2'') (v1 Prelude.* v2)

prefix :: Num v => NameAtom -> v -> Unit Atomic d v -> Unit Composite d v
prefix name val = applyPrefix $ UnitPrefix name val

alias :: (Dimensional v1, Num v) => NameAtom -> v1 d v -> Unit Atomic d v
alias name x = Unit (Name.atomic name) (extractValue x)

unit :: (Num v) => UnitName -> Quantity d v -> Unit Composite d v
unit name x = Unit name (extractValue x)

name :: Unit a d v -> UnitName
name (Unit n _) = n

showIn :: (Dimensional v1, Fractional v, Show v, Dimensional (DimensionalCombination v1 (Unit a))) => Unit a d v -> v1 d v -> String
showIn u q = let q' = unD $ q / u
                 n' = show $ name u
              in (show q') ++ " " ++ n'

{-

= Conclusion and usage =

We have defined operators and units that allow us to define and
work with physical quantities. A physical quantity is defined by
multiplying a number with a unit (the type signature is optional).

] v :: Velocity Prelude.Double
] v = 90 *~ (kilo meter / hour)

It follows naturally that the numerical value of a quantity is
obtained by division by a unit.

] numval :: Prelude.Double
] numval = v /~ (meter / second)

The notion of a quantity as the product of a numerical value and a
unit is supported by 7.1 "Value and numerical value of a quantity" of
[1]. While the above syntax is fairly natural it is unfortunate that
it must violate a number of the guidelines in [1], in particular 9.3
"Spelling unit names with prefixes", 9.4 "Spelling unit names obtained
by multiplication", 9.5 "Spelling unit names obtained by division".

As a more elaborate example of how to use the module we define a
function for calculating the escape velocity of a celestial body
[2].

] escapeVelocity :: (Floating a) => Mass a -> Length a -> Velocity a
] escapeVelocity m r = sqrt (two * g * m / r)
]   where
]       two = 2 *~ one
]       g = 6.6720e-11 *~ (newton * meter ^ pos2 / kilo gram ^ pos2)

The following is an example GHC session where the above function
is used to calculate the escape velocity of Earth in kilometer per
second.

  *Numeric.Dimensional> :set +t
  *Numeric.Dimensional> let me = 5.9742e24 *~ kilo gram -- Mass of Earth.
  me :: Quantity DMass GHC.Float.Double
  *Numeric.Dimensional> let re = 6372.792 *~ kilo meter -- Mean radius of Earth.
  re :: Quantity DLength GHC.Float.Double
  *Numeric.Dimensional> let ve = escapeVelocity me re   -- Escape velocity of Earth.
  ve :: Velocity GHC.Float.Double
  *Numeric.Dimensional> ve /~ (kilo meter / second)
  11.184537332296259
  it :: GHC.Float.Double

For completeness we should also show an example of the error messages
we will get from GHC when performing invalid arithmetic. In the
best case GHC will be able to use the type synonyms we have defined
in its error messages.

] x = 1 *~ meter + 1 *~ second

    Couldn't match expected type `Pos1' against inferred type `Zero'
      Expected type: Unit DLength t
      Inferred type: Unit DTime a
    In the second argument of `(*~)', namely `second'
    In the second argument of `(+)', namely `1 *~ second'

In other cases the error messages aren't very friendly.

] x = 1 *~ meter / (1 *~ second) + 1 *~ kilo gram

    Couldn't match expected type `Zero'
           against inferred type `Neg Zero'
    When using functional dependencies to combine
      Sub Zero (Pos Zero) (Neg Zero),
        arising from use of `/' at Numeric/Dimensional.lhs:425:9-20
      Sub Zero (Pos Zero) Zero,
        arising from use of `/' at Numeric/Dimensional.lhs:532:5-30

It is the author's experience that the usefullness of the compiler
error messages is more often than not limited to pinpointing the
location of errors.


= Future work =

While there is an insane amount of units in use around the world
it is reasonable to provide at least all SI units. Units outside
of SI will most likely be added on an as-needed basis.

There are also plenty of elementary functions to add. The 'Floating'
class can be used as reference.

Another useful addition would be decent 'Show' and 'Read' instances.
The 'show' implementation could output the numerical value and the
unit expressed in (base?) SI units, along the lines of:

] instance (Fractional a, Show a) => Show (Length a)
]   where show x = show (x /~ meter) ++ " m"

Additional functions could be provided for "showing" with any unit
and prefix.  The 'read' implementation should be able to read values
with any unit and prefix. It is not clear to the author how to best
implement these.

Additional physics models could be implemented. See [3] for ideas.


= Related work =

Henning Thielemann numeric prelude has a physical units library,
however, checking of dimensions is dynamic rather than static.
Aaron Denney has created a toy example of statically checked
physical dimensions covering only length and time. HaskellWiki
has pointers [4] to these.

Also see Samuel Hoffstaetter's blog post [5] which uses techniques
similar to this library.

Libraries with similar functionality exist for other programming
languages and may serve as inspiration. The author has found the
Java library JScience [6] and the Fortress programming language [7]
particularly noteworthy.


= References =

[1] http://physics.nist.gov/Pubs/SP811/
[2] http://en.wikipedia.org/wiki/Escape_velocity
[3] http://jscience.org/api/org/jscience/physics/models/package-summary.html
[4] http://www.haskell.org/haskellwiki/Physical_units
[5] http://liftm.wordpress.com/2007/06/03/scientificdimension-type-arithmetic-and-physical-units-in-haskell/
[6] http://jscience.org/
[7] http://research.sun.com/projects/plrg/fortress.pdf

-}
