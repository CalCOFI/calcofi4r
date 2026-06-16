# NA

\#’ Oceanographic bottle data \#’ \#’ @description \#’ This dataset
contains oceanographic data. This table includes \#’ oceanographic
measurements for each bottle/sampling depth ever \#’ completed on a
CalCOFI cruise. There are additional data code \#’ and precision columns
describing the quality of each oceanographic \#’ measurement. Each row
is a unique bottle/sampling depth, numbered \#’ sequentially/indexed by
the “Btl_Cnt” column. \#’ \#’ @format A data frame with 889500 rows and
123 variables \#’ \#’ @source \#’ @concept data \#’ @details \#’ \###
Codes used in data set \#’ **Sta_Code (Station Codes)** \#’ - “ST” -
Standard CalCOFI Station \#’ - “SCO” - SCCOOS nearshore/20m Station
\#’ - “NRO” - Not Regularly Occupied Original CalCOFI Station \#’ -
“OCO” - Occasionally CalCOFI Occupied \#’ - “IMX” - IMECOCAL Occupied
\#’ - “NST” - Non-Standard Station \#’ - “MBR” - MBARI Occupied Station
\#’ \#’ **Data_Type (Data Type)** \#’ - “PR” - Productivity Cast \#’ -
“HY” - Hydrographic Cast \#’ - “10” - Ten-meter Cast \#’ - “CT” -
Compressed CTD Cast (Low Resolution) \#’ - “MX” - Mixed CTD and Bottle
Data \#’ \#’ \*\*\_qual, qua, or q (Quality Code; associated with
discrete samples; examples: “O_qual”, “Chlqua”, “PO4q”) **\#’ - Blank -
Data OK \#’ - “4” - Value zeroed due to value below detection limit
\#’ - “6” - Data taken from CTD sensor \#’ - “8” - Technician thinks
value is suspect \#’ - “9” - Missing Data \#’ \#’** RecInd (Record
Indicator)\*\* \#’ - “3” - Observed Data \#’ - “4” - Educated office
guess (ghost) \#’ - “5” - Data from STD or CTD sensor \#’ - “6” -
Duplicate Depth \#’ - “7” - Interpolated to a standard depth \#’ \#’
\### CalCOFI bottle sampling \#’ CalCOFI deploys a Seabird 911+ CTD with
a 24-place Seabird carousel \#’ mounted inside a 24-10L bottle,
epoxy-coated rosette frame. Bottle \#’ sampling is based on the
historical “bottles-hung-on-wire” sampling \#’ method utilized by
CalCOFI oceanographers from 1949-1993. Twenty Niskin \#’ (5L), Nansen
(1.25L), or Wally (2.5L) bottles with messengers were \#’ attached to a
winch wire and suspended vertically in (typically) 500m \#’ of ocean
depth. The spring-loaded bottles go into the ocean open so \#’ seawater
can easily flush through them. Once all 20 bottles were \#’ deployed and
after a 10+ minute soak for the reversing thermometers \#’ to
equilibrate. A messenger - 1-2lb metal weight that can clamp & \#’ slide
freely on the wire - was clamped to the wire and released, \#’ dropping
onto the first bottle submerged at the surface. When closed, \#’ the
bottles trap seawater from a particular depth with a water-tight \#’
seal. This is important since the characteristics of seawater such as
\#’ salinity and oxygen change with depth. It is critical to the \#’
integrity of the measurement that the bottles do not leak and become \#’
contaminated by seawater from different depths. \#’ \#’ - Nansen
bottles: the top end of the Nansen bottle would release from \#’ the
wire, inverting the bottle, releasing its messenger. The inversion \#’
of the thermometers would “break” the mercury in the reversing \#’
thermometers, locking in the temperature readings. When inverted, the
\#’ open bottle ends closed, trapping seawater from that depth for later
\#’ analyses. Three thermometers - two protected, one unprotected were
used \#’ on bottles 200m and deeper to derive corrected bottle depth;
two \#’ thermometers were mounted on bottles shallower than 200m. The
released \#’ messenger would slide down the wire to trip the next
bottle - this \#’ would continue until all 20 bottles had closed.
Calculating actual \#’ bottle depth by comparing protected and
unprotected reversing \#’ thermometer readings would correct for any
inaccuracy due to wire angle \#’ or winch-error. In use by CalCOFI
1949 - 1966, and occasionally after \#’ until CTD-Rosette implementation
in 1993. \#’ \#’ - Niskin bottles: the messenger would strike a plunger,
releasing a \#’ rotating thermometer rack that would spin 180°,
“breaking” the \#’ mercury to lock-in the temperature readings of the
reversing \#’ thermometers. An internal spring would close the o-ringed
end caps \#’ trapping seawater. CalCOFI Niskins were metal-free &
latex-free to \#’ allow 14C primary productivity incubation experiments
to be conducted \#’ with the seawater collected. Epoxy-coated springs
and viton o-rings \#’ were used to reduce latex toxicity and iron
fertilization. After the \#’ 1st bottle tripped, the messenger would
travel down the wire to the \#’ next bottle and so on. The person
dropping the messenger would often \#’ hold the wire to feel the
vibration up the wire of the bottles closing. \#’ Eight minutes would
elapse for all the bottles to close before the \#’ bottle array would be
recovered. In use by CalCOFI 1966-1993. \#’ \#’ - “Wally” bottles were
2.5L PVC/ABS bottles designed by CalCOFI techs \#’ George Anderson &
Walt Bryant, similar to Niskins but easier to deploy. \#’ They used a
single broad wire book-clamp and collected smaller water \#’ volumes so
were easier-quicker to attach and remove from the wire. \#’ They also
used epoxy-coated springs and Viton o-rings. During their \#’ use, 2.5L
of seawater was plenty of volume for the at-sea analyses: \#’ salts,
nutrients, chlorophylls, oxygens. Like Niskin bottles, after \#’ the
surface bottle tripped, the messenger would travel down the wire
striking the next bottle plunger, closing the bottle, and releasing the
next messenger. In use by CalCOFI 1988-1993. \#’ \#’ - CTD-rosette: 10L
PVC/ABS plastic bottles are equipped with \#’ epoxy-spring loaded with
Viton o-rings on all surfaces that contact \#’ the seawater samples.
Unlike a series of bottles clamped to a winch \#’ wire, the CTD-rosette
is tethered and deployed on an \#’ electrically-conductive winch wire.
24-10L bottles encircle the CTD \#’ electronics and are rigged open
using lanyards attached to the \#’ central carousel hub. The CTD sensors
display real-time seawater \#’ measurements to a computer screen on the
ship for the operator & \#’ other scientists to see. Based on the
downcast profiles of \#’ chlorophyll-a and mixed layer depth, as the
CTD-rosette is raised, \#’ bottles are closed electronically at specific
depths by the operator. \#’ Reversing thermometers have been replaced by
dual CTD temperature \#’ sensors. Bottles still trap seawater from
specific depths in \#’ leak-proof bottles for analyses on-board the
ship. In use by CalCOFI \#’ 1993-present. “bottle”

\#’ Oceanographic DIC data \#’ \#’ @description \#’ CalCOFI collects
samples for inorganic carbon and total alkalinity at \#’ specific
stations throughout the cruise track. Seawater samples are \#’ collected
from several depths, preserved and brought back to SIO for \#’ total
inorganic carbon and total alkalinity analysis. These \#’ measurements
allow the calculation of pH and pCO2. The objectives of \#’ these
measurements are: first, the long-term characterization of the \#’
inorganic carbon system and its response to changing ocean climate, \#’
and second, measurements of pH in the coastal zone in order to \#’
monitor the anthropogenic impact on coastal ecosystems in the \#’
Southern California Bight. \#’ \#’ @format A data frame with 35376 rows
and 82 variables \#’ \#’ @source \#’ @concept data \#’ @details \#’ DIC
samples for Dave Keeling were also collected on CalCOFI stations \#’
90.70, 90.90, & 90.120 from Sep 1986 (CalCOFI 8609NH) to Nov 2002 \#’
(CalCOFI 0211NH). In Aug 2008, DIC sampling restarted on 7 stations, \#’
grew to 10 stations in Jan 2011, and currently DIC has expanded \#’
substantially. Dissolved inorganic carbon \#’ (DIC) and total alkalinity
(TA) data have only been integrated into \#’ the CalCOFI hydrographic
dataset since Aug 2008, then Jan 2009 \#’ through presently. Similarly,
pH data have only been integrated into \#’ the CalCOFI hydrographic
dataset since April 2014. \#’ \#’ \### Codes used in data set \#’
**Sta_Code (Station Codes)** \#’ - “ST” - Standard CalCOFI Station \#’ -
“SCO” - SCCOOS nearshore/20m Station \#’ - “NRO” - Not Regularly
Occupied Original CalCOFI Station \#’ - “OCO” - Occasionally CalCOFI
Occupied \#’ - “IMX” - IMECOCAL Occupied \#’ - “NST” - Non-Standard
Station \#’ - “MBR” - MBARI Occupied Station \#’ \#’ **Data_Type (Data
Type)** \#’ - “PR” - Productivity Cast \#’ - “HY” - Hydrographic Cast
\#’ - “10” - Ten-meter Cast \#’ - “CT” - Compressed CTD Cast (Low
Resolution) \#’ - “MX” - Mixed CTD and Bottle Data \#’ \#’ \*\*\_qual,
qua, or q (Quality Code; associated with discrete samples; examples:
“O_qual”, “Chlqua”, “PO4q”) **\#’ - Blank - Data OK \#’ - “4” - Value
zeroed due to value below detection limit \#’ - “6” - Data taken from
CTD sensor \#’ - “8” - Technician thinks value is suspect \#’ - “9” -
Missing Data \#’ \#’** RecInd (Record Indicator)\*\* \#’ - “3” -
Observed Data \#’ - “4” - Educated office guess (ghost) \#’ - “5” - Data
from STD or CTD sensor \#’ - “6” - Duplicate Depth \#’ - “7” -
Interpolated to a standard depth \#’ “dic”
