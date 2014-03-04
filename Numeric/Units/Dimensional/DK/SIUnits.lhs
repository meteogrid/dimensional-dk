Numeric.Dimensional.SIUnits
Bjorn Buckwalter, bjorn@buckwalter.se
License: BSD3


= Summary =

This module defines the SI prefixes, the SI base units and the SI
derived units. It also defines the units outside of the SI that are
accepted for use with the SI. Any chapters, sections or tables
referenced are from [1] unless otherwise specified.

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
> module Numeric.Units.Dimensional.DK.SIUnits where

> import Numeric.Units.Dimensional.DK
> import Numeric.Units.Dimensional.DK.Quantities
> import Numeric.NumType.DK ( neg1, neg2, pos2, pos3 )
> import Prelude ( (.), Num, Real (toRational), fromInteger, ($), floor, Fractional(fromRational), Floating, recip )
> import qualified Prelude
> import Numeric.Units.Dimensional.DK.UnitNames (atomic)


= SI base units (section 4.1) =

Now we will define the SI base unitsi from section 4.1. To avoid a
myriad of one-letter functions that would doubtlessly cause clashes
and frustration in users' code we spell out all unit names in full,
as we did for prefixes. We also elect to spell the unit names in
singular form, as allowed by section 9.7 "Other spelling conventions".

We define the SI base units in the order of table 1.

> metre, meter :: Num v => Unit Atomic DLength v
> metre = Unit (atomic ("m", "meter")) 1 -- International English.
> meter = metre         -- American English.

For mass the SI base unit is kilogram. For sensible prefixes we
define gram here (see section 6.2.7 "Prefixes and the kilogram").
The drawback is that we are forced to use 'Fractional'.

> gram    :: Fractional v => Unit Atomic DMass v
> gram    = Unit (atomic ("g", "gram")) 1e-3
> second  :: Num v => Unit Atomic DTime v
> second  = Unit (atomic ("s", "second")) 1
> ampere  :: Num v => Unit Atomic DElectricCurrent v
> ampere  = Unit (atomic ("A", "ampere")) 1
> kelvin  :: Num v => Unit Atomic DThermodynamicTemperature v
> kelvin  = Unit (atomic ("K", "kelvin")) 1
> mole    :: Num v => Unit Atomic DAmountOfSubstance v
> mole    = Unit (atomic ("mol", "mole")) 1
> candela :: Num v => Unit Atomic DLuminousIntensity v
> candela = Unit (atomic ("cd", "candela")) 1


= SI derived units (section 4.2) =

Before defining the derived units themselves we provide type synonyms
for derived quantities and their dimensionalities. For lack of better
organization we provide definitions grouped by table in [1].


== Table 3a ==

"SI derived units with special names and symbols, including the
radian and steradian."

> radian :: Fractional a => Unit Atomic DPlaneAngle a
> radian = alias ("rad", "radian") one -- meter * meter ^ neg1
> steradian :: Fractional a => Unit Atomic DSolidAngle a
> steradian = alias ("sr", "steradian") one -- meter ^ pos2 * meter ^ neg2
> hertz :: Fractional a => Unit Atomic DFrequency a
> hertz = alias ("Hz", "hertz") (second ^ neg1)
> newton :: Fractional a => Unit Atomic DForce a
> newton = alias ("N", "newton") (kilo gram * meter * second ^ neg2)
> pascal :: Fractional a => Unit Atomic DPressure a
> pascal = alias ("Pa", "pascal") (newton / meter ^ pos2)
> joule :: Fractional a => Unit Atomic DEnergy a
> joule = alias ("J", "joule") (newton * meter)
> watt :: Fractional a => Unit Atomic DPower a
> watt = alias ("W", "watt") (joule / second)
> coulomb :: Fractional a => Unit Atomic DElectricCharge a
> coulomb = alias ("C", "coulomb") (second * ampere)
> volt :: Fractional a => Unit Atomic DElectricPotential a
> volt = alias ("V", "volt") (watt / ampere)
> farad :: Fractional a => Unit Atomic DCapacitance a
> farad = alias ("F", "farad") (coulomb / volt)
> ohm :: Fractional a => Unit Atomic DElectricResistance a
> ohm = alias ("Ω", "ohm") (volt / ampere)
> siemens :: Fractional a => Unit Atomic DElectricConductance a
> siemens = alias ("S", "siemens") (ampere / volt)
> weber :: Fractional a => Unit Atomic DMagneticFlux a
> weber = alias ("Wb", "weber") (volt * second)
> tesla :: Fractional a => Unit Atomic DMagneticFluxDensity a
> tesla = alias ("T", "tesla") (weber / meter ^ pos2)
> henry :: Fractional a => Unit Atomic DInductance a
> henry = alias ("H", "henry") (weber / ampere)

We defer the definition of Celcius temperature to the end (would
appear here if we stricly followed table 3a).

> lumen :: Fractional a => Unit Atomic DLuminousFlux a
> lumen = alias ("lm", "lumen") (candela / steradian)
> lux :: Fractional a => Unit Atomic DIlluminance a
> lux = alias ("lx", "lux") (lumen / meter ^ pos2)

=== Degree Celsius ===

A problematic area is units which increase proportionally to the
base SI units but cross zero at a different point. An example would
be degrees Celsius (see section 4.2.1.1). The author feels that it
is appropriate to define a unit for use with relative quantities
(taking only into account the proportionality) and complement the
unit with functions for converting absolute values.

> degreeCelsius :: Num a => Unit Atomic DCelsiusTemperature a
> degreeCelsius = alias ("°C", "degree Celsius") kelvin

The function 'fromDegreeCelsiusAbsolute' should be used in lieu of
"*~ degreeCelsius" when working with absolute temperatures. Similarily,
'toDegreeCelsiusAbsolute' should be used in lieu of "/~ degreeCelsius"
when working with absolute temperatures.

> fromDegreeCelsiusAbsolute :: Fractional a => a -> ThermodynamicTemperature a
> fromDegreeCelsiusAbsolute x = (dimensionless x) * degreeCelsius + (dimensionless 273.15) * degreeCelsius
> toDegreeCelsiusAbsolute :: Fractional a => ThermodynamicTemperature a -> a
> toDegreeCelsiusAbsolute x = (x - (dimensionless 273.15 * degreeCelsius)) /~ degreeCelsius


== Table 3b ==

"SI derived units with special names and symbols admitted for reasons
of safeguarding human health"

We use the same grouping as for table 3a.

> becquerel :: Fractional a => Unit Atomic DActivity a
> becquerel = alias ("Bq", "becquerel") (second ^ neg1)

Above we gave a new name to the dimensionality instead of reusing
'Frequency' in the quantity type definition. This will allow GHCi
be more specific when queried for the type of 'becquerel'. For
quantity types without a specific unit we don't bother doing this
(though perhaps we should in case there is a non-SI unit for the
quantity type?).

> gray :: Fractional a => Unit Atomic DAbsorbedDose a
> gray = alias ("Gy", "gray") (joule / kilo gram)
> sievert :: Fractional a => Unit Atomic DDoseEquivalent a
> sievert = alias ("Sv", "sievert") (joule / kilo gram)


= Units outside the SI =

There are several units that are not strictly part of the SI but
are either permanently or temporarily accepted for use with the SI.
We define the permanently accepted ones in this module.

== Table 6 ==

"Units accepted for use with the SI."

We start with time which we grant exclusive rights to 'minute' and
'second'.

> minute, hour, day :: Num a => Unit Atomic DTime a
> minute = alias ("min", "minute") (dimensionless 60 * second)
> hour   = alias ("h", "hour") (dimensionless 60 * minute)
> day    = alias ("d", "day") (dimensionless 24 * hour) -- Mean solar day.

Since 'minute' and 'second' are already in use for time we use
'arcminute' and 'arcsecond' [2] for plane angle instead.

> degree, arcminute, arcsecond :: Floating a => Unit Atomic DPlaneAngle a
> degree = alias ("°", "degree of arc") (pi * radian / dimensionless 180)
> arcminute = alias ("′", "arcminute") (degree / dimensionless 60)
> arcsecond = alias ("″", "arcsecond") (arcminute / dimensionless 60)

Alternate (longer) forms of the above. In particular 'degreeOfArc'
can be used if there is a percieved need to disambiguate from e.g.
temperature.

> degreeOfArc, minuteOfArc, secondOfArc :: Floating a => Unit Atomic DPlaneAngle a
> degreeOfArc = degree
> secondOfArc = arcsecond
> minuteOfArc = arcminute

> hectare :: Fractional a => Unit Atomic DArea a
> hectare = alias ("ha", "hectare") (square (hecto meter))

> litre, liter :: Fractional a => Unit Atomic DVolume a
> litre = alias ("L", "litre") (deci meter ^ pos3) -- International English. Capitalization of abbreviation is contentious but allowed, more readable in many fonts.
> liter = alias ("L", "liter") litre             -- American English.

> tonne, metricTon :: Fractional a => Unit Atomic DMass a
> tonne     = alias ("t", "tonne") (dimensionless 1000 * (kilo gram)) -- Name in original SI text.
> metricTon = alias ("t", "metric ton") tonne                   -- American name.

In 2012 the IAU redefined the astronomical unit as a conventional
unit of length directly tied to the meter, with a length of exactly
149,597,870,700 m and the official abbreviation of au[3].

> astronomicalUnit :: Num a => Unit Atomic DLength a
> astronomicalUnit = alias ("au", "astronomical unit") (dimensionless 149597870700 * meter)


= References =

[1] http://physics.nist.gov/Pubs/SP811/
[2] http://en.wikipedia.org/wiki/Minute_of_arc
[3] http://en.wikipedia.org/wiki/Astronomical_unit

