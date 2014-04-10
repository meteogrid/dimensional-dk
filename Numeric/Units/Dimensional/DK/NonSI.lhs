Numeric.Dimensional.NonSI
Bjorn Buckwalter, bjorn@buckwalter.se
License: BSD3


= Summary =

This module defines units that are not part of the SI, with the
exception of those defined in the 'SIUnits' module (units outside
of the SI accepted for use with the SI).

Any chapters, sections or tables referenced are from [1] unless
otherwise specified.

> {- |
>    Copyright  : Copyright (C) 2006-2014 Bjorn Buckwalter
>    License    : BSD3
>
>    Maintainer : bjorn@buckwalter.se
>    Stability  : Stable
>    Portability: GHC only?
>
> Please refer to the literate Haskell code for documentation of both API
> and implementation.
> -}

> {-# LANGUAGE DataKinds #-}
> module Numeric.Units.Dimensional.DK.NonSI where

> import Numeric.Units.Dimensional.DK.Prelude
> import qualified Prelude


= Neper, bel, shannon and the like =

The units of section 5.1.2 are purposefully (but not permanently)
omitted. In fact the logarithmic units (see section 8.7) are
problematic and it is not clear how to implement them. Perhaps with
a conversion function similar to for degrees Celsius.


= Table 7 =

"Units accepted for use with the SI whose values in SI units are
obtained experimentally."

When [1] was published The electronvolt had a standard combined
uncertainity of 0.00000049e-19 J and the unified atomic mass unit
had a combined uncertainty of 0.0000010e-27 kg.

> electronVolt :: Fractional a => Unit Atomic DEnergy a
> electronVolt = alias ("eV", "electron volt") (1.60217733e-19 *~ joule)
> unifiedAtomicMassUnit :: Fractional a => Unit Atomic DMass a
> unifiedAtomicMassUnit = alias ("u", "atomic mass unit") (1.6605402e-27 *~ (kilo gram))


= Standard gravity =

In order to relate e.g. pounds mass to pounds force we define the unit
'gee' equal to the standard gravity g_0: the nominal acceleration of a
body in free fall in a vacuum near the surface of the earth (note that
local values of acceleration due to gravity will differ from the standard
gravity). I.e. g_0 = 1 gee.

> gee :: Fractional a => Unit Atomic DAcceleration a
> gee = alias ("g", "gee") (9.80665 *~ meter / second ^ pos2)


= Inch-pound units =

Some US customary (that is, inch-pound) units.

> inch, foot, mil :: Fractional a => Unit Atomic DLength a
> inch = alias ("in", "inch") (2.54 *~ (centi meter))
> foot = alias ("ft", "foot") (12.0 *~ inch) -- 0.3048 m
> mil  = alias ("mil", "mil") (0.001 *~ inch)
> poundMass, ounce :: Fractional a => Unit Atomic DMass a
> poundMass = alias ("lbm", "pound mass") (0.45359237 *~ (kilo gram))
> ounce     = alias ("oz", "ounce") ((1 Prelude./ 16) *~ poundMass)

> poundForce :: Fractional a => Unit Atomic DForce a
> poundForce = alias ("lbf", "pound force") (poundMass * gee)  -- 4.4482 N

The slug is an alternative unit of mass defined in terms of the pound-force.

> slug :: Fractional a => Unit Atomic DMass a
> slug = alias ("slug", "slug") (poundForce * (second^pos2) / foot)

Pounds of force per square inch.

> psi :: Fractional a => Unit Atomic DPressure a
> psi = alias ("psi", "pound per square inch") (poundForce / inch ^ pos2)


= Various other (non inch-pound) units =

> yard, mile, nauticalMile :: (Fractional a) => Unit Atomic DLength a
> yard = alias ("yd", "yard") (3.0 *~ foot)
> mile = alias ("mi", "mile") (1760.0 *~ yard)
> nauticalMile = alias ("nm", "nautical mile") (1852.0 *~ meter)
> knot :: (Fractional a) => Unit Atomic DVelocity a
> knot = alias ("kt", "knot") (nauticalMile / hour)
> revolution :: (Floating a) => Unit Atomic DOne a
> revolution = alias ("r", "revolution") (tau * radian)
> solid :: (Floating a) => Unit Atomic DOne a
> solid = alias ("solid", "solid") (4.0 *~ pi * steradian)
> teaspoon :: (Fractional a) => Unit Atomic DVolume a
> teaspoon = alias ("tsp", "teaspoon") (5.0 *~ (milli liter))
> acre :: (Fractional a) => Unit Atomic DArea a
> acre = alias ("acre", "acre") (43560.0 *~ (square foot))

The IAU recommends[2] that:

  Although there are several different kinds of year (as there are
  several kinds of day), it is best to regard a year as a julian
  year of 365.25 days (31.5576 Ms) unless otherwise specified.

This aligns well with my needs so I'm happy to oblige. We define
the year in terms of seconds in order to avoid a 'Fractional'
constraint, and also provide a Julian century.

> year, century :: Fractional a => Unit Atomic DTime a
> year    = alias ("a", "year") (31557600 *~ second)
> century = alias ("c", "century") (100 *~ year)


= Pressure units =

Psi was defined earlier.

> bar :: (Fractional a) => Unit Atomic DPressure a
> bar = alias ("bar", "bar") (1.0e5 *~ pascal)

From Wikipedia[3]:

  The standard atmosphere (atm) is an established constant. It is
  approximately equal to typical air pressure at earth mean sea
  level.

> atmosphere :: (Fractional a) => Unit Atomic DPressure a
> atmosphere = alias ("atm", "atmosphere") (101325.0 *~ pascal)

From Wikipedia:

  A technical atmosphere (symbol: at) is a non-SI unit of pressure equal
  to one kilogram-force per square centimeter.

> technicalAtmosphere :: (Fractional a) => Unit Atomic DPressure a
> technicalAtmosphere = alias ("at", "technical atmosphere") (kilo gram * gee * centi meter ^ neg2)

Manometric pressure units:

Per Wikipedia[4] one mmHg (millimeter of mercury) is defined as:

  The pressure exerted at the base of a column of fluid exactly 1 mm high,
  when the density of the fluid is exactly 13.5951 g/cm^3, at a place
  where the acceleration of gravity is exactly 9.80665 m/s^2.

The chosen fluid density approximately corresponds to that of mercury
at 0 deg. Under most conditions, 1 mmHg is approximately equal to 1 torr.

> mmHg :: (Fractional a) => Unit Atomic DPressure a
> mmHg = alias ("mmHg", "millimeter of mercury") (13.5951 *~ gram * centi meter ^ neg3 * milli meter * gee)

One torr (symbol: Torr) is defined as 1/760 atm, which is approximately equal
to 1 mmHg.

> torr :: (Fractional a) => Unit Atomic DPressure a
> torr = alias ("Torr", "torr") ((1 Prelude./ 760) *~ atmosphere)


= Radiation =

> rad :: (Fractional a) => Unit Atomic DAbsorbedDose a
> rad = alias ("rad", "rad") (centi gray)


= Kinematic Viscosity =

> stokes :: (Fractional a) => Unit Atomic DKinematicViscosity a
> stokes = alias ("St", "stokes") (centi meter ^ pos2 / second)


= Temperature =

> degreeFahrenheit :: (Fractional a) => Unit Atomic DThermodynamicTemperature a
> degreeFahrenheit = alias ("degF", "degree Fahrenheit") ((5 Prelude./ 9) *~ degreeCelsius)

> degreeRankine :: (Fractional a) => Unit Atomic DThermodynamicTemperature a
> degreeRankine = alias ("degR", "degree Rankine") degreeFahrenheit

= Imperial Volumes =

Per http://en.wikipedia.org/wiki/Imperial_units and http://en.wikipedia.org/wiki/Cup_(unit)#Imperial_cup.

> imperialGallon, imperialQuart, imperialPint, imperialCup,
>                 imperialGill, imperialFluidOunce
>                 :: (Fractional a) => Unit Atomic DVolume a
> imperialGallon = alias ("gal", "gallon") (4.54609 *~ liter)
> imperialQuart  = alias ("qt", "quart") ((1 Prelude./ 4) *~ imperialGallon)
> imperialPint   = alias ("pt", "pint") ((1 Prelude./ 8) *~ imperialGallon)
> imperialCup    = alias ("cup", "cup") ((1 Prelude./ 2) *~ imperialPint) -- does this unit actually exist except by analogy with the US system? not in wiki table
> imperialGill   = alias ("gi", "gill") ((1 Prelude./ 4) *~ imperialPint)
> imperialFluidOunce = alias ("fl oz", "fluid ounce") ((1 Prelude./ 20) *~ imperialPint)

= US Customary Volumes =

Per http://www.nist.gov/pml/wmd/pubs/upload/2012-hb44-final.pdf page 452 and http://en.wikipedia.org/wiki/United_States_customary_units#Fluid_volume
Note that there exist rarely-used "dry" variants of units with overlapping names.

> usGallon, usQuart, usPint, usCup, usGill, usFluidOunce :: (Fractional a) => Unit Atomic DVolume a
> usGallon = alias ("gal", "gallon") (231 *~ (cubic inch))
> usQuart = alias ("qt", "quart") ((1 Prelude./ 4) *~ usGallon)
> usPint = alias ("pt", "pint") ((1 Prelude./ 8) *~ usGallon)
> usCup = alias ("cup", "cup") ((1 Prelude./ 2) *~ usPint)
> usGill = alias ("gi", "gill") ((1 Prelude./ 4) *~ usPint)
> usFluidOunce = alias ("fl oz", "fluid ounce") ((1 Prelude./ 16) *~ usPint) -- sic, does not match factor used in imperial system

= References =

[1] http://physics.nist.gov/Pubs/SP811/
[2] http://www.iau.org/science/publications/proceedings_rules/units/
[3] http://en.m.wikipedia.org/wiki/Pressure
[4] http://en.m.wikipedia.org/wiki/Torr
