#' Oceanographic bottle data
#'
#' @description
#' This dataset contains oceanographic data. This table includes
#' oceanographic measurements for each bottle/sampling depth ever
#' completed on a CalCOFI cruise. There are additional data code
#' and precision columns describing the quality of each oceanographic
#' measurement. Each row is a unique bottle/sampling depth, numbered
#' sequentially/indexed by the "Btl_Cnt" column.
#'
#' @format A data frame with 889500 rows and 123 variables
#' \describe{
#'   \item{Cst_Cnt}{Cast Count - All CalCOFI casts ever conducted, consecutively numbered}
#'   \item{Cruise_ID}{Cruise identifier [Year]-[Month]-[Day]-C-[Ship Code]}
#'   \item{Cruise}{Cruise Name [Year][Month]}
#'   \item{Cruz_Sta}{Cruise Name and Station [Year][Month][Line][Station]}
#'   \item{DbSta_ID}{Line and Station}
#'   \item{Cast_ID}{Cast Identifier [Century] - [YY][MM][ShipCode] - [CastType][Julian Day] - [CastTime]-[Line][Sta]}
#'   \item{Sta_ID}{Line and Station}
#'   \item{Sta_ID_line}{Line data - the first half of the value from the Sta_ID field}
#'   \item{Sta_ID_station}{Station data - the second half of the value from the Sta_ID field}
#'   \item{Quarter}{Quarter of the year}
#'   \item{Sta_Code}{Station Designation (See Station_ID and 0-St_Code for codes)}
#'   \item{Distance}{Nautical miles from coast intercept, calculated from estimated latitude and longitude}
#'   \item{Date}{Date (Month Day Year)}
#'   \item{Year}{Year}
#'   \item{Month}{Month}
#'   \item{Julian_Date}{OA Date: days since December 30, 1899}
#'   \item{Julian_Day}{Julian Day of the year}
#'   \item{Time}{Time (UTC) CTD reached terminal depth}
#'   \item{Lat_Dec}{Observed Latitude in decimal degrees}
#'   \item{Lat_Deg}{Observed Latitude in degrees}
#'   \item{Lat_Min}{Observed Latitude in minutes}
#'   \item{Lat_Hem}{Observed Latitude Hemisphere}
#'   \item{Lon_Dec}{Observed Longitude in decimal degrees}
#'   \item{Lon_Deg}{Observed Longitude in degrees}
#'   \item{Lon_Min}{Observed Longitude in minutes}
#'   \item{Lon_Hem}{Observed Longitude Hemisphere}
#'   \item{Rpt_Line}{Reported Line Number}
#'   \item{St_Line}{Nearest Standard Line}
#'   \item{Ac_Line}{Calculated actual line from observed latitude and longitude}
#'   \item{Rpt_Sta}{Reported Station Number}
#'   \item{St_Station}{Nearest Standard Station Number}
#'   \item{Ac_Sta}{Calculated actual station from observed lat and long}
#'   \item{Bottom_D}{Bottom depth in meters}
#'   \item{Secchi}{Secchi disk depth in meters}
#'   \item{ForelU}{Water color; Only used in CalCOFI dataset from 1988-10 through 1998-04 (Forel-Ule Scale)}
#'   \item{Ship_Name}{Ship's Name}
#'   \item{Ship_Code}{Ship's NODC Code}
#'   \item{Data_Type}{Data Type}
#'   \item{Order_Occ}{Order station was occupied (within each cruise)}
#'   \item{Event_Num}{Event number; includes all research opperations within each cruise (e.g. nets, CTD)}
#'   \item{Cruz_Leg}{Leg of cruise, if multiple legs}
#'   \item{Orig_Sta_ID}{IEH Reported Station ID}
#'   \item{Data_Or}{Data Origin, punch card short code used to designate the institution that collected the data (e.g. Scripps)}
#'   \item{Cruz_Num}{Cruise Designation (Year Month)}
#'   \item{IntChl}{Integrated Chlorophyll per half light day (milligrams chl per square meter per half light day)}
#'   \item{IntC14}{Integrated Primary Productivity per half light day (milligrams C per square meter per half light day)}
#'   \item{Inc_Str}{Incubation Start Time (PST)}
#'   \item{Inc_End}{Incubation End Time (PST)}
#'   \item{PST_LAN}{Time of Local Apparent Noon (PST)}
#'   \item{Civil_T}{Time of Civil Twilight (PST)}
#'   \item{TimeZone}{Time Zone}
#'   \item{Wave_Dir}{Wave Direction reported using an abbreviated 360° azimuth circle with 00 representing True North, 18 represents 180°}
#'   \item{Wave_Ht}{Wave Height in feet}
#'   \item{Wave_Prd}{Wave Period in seconds}
#'   \item{Wind_Dir}{Reported using an abbreviated 360 degree azimuth circle with 0 representing True North, 18 representing 180 degrees}
#'   \item{Wind_Spd}{Wind Speed in knots}
#'   \item{Barometer}{Millibars to the tenths}
#'   \item{Dry_T}{Dry Air Temperature from a Sling Psychrometer in degrees Celsius}
#'   \item{Wet_T}{Wet Air Temperature from a Sling Psychrometer in degrees Celsius}
#'   \item{Wea}{1 Digit Code from The World Meteorological Organization, Code source WMO 4501}
#'   \item{Cloud_Typ}{1 Digit Code from The World Meteorological Organization, Code source WMO 0500}
#'   \item{Cloud_Amt}{1 Digit Code from The World Meteorological Organization, Code source WMO 2700, in oktas}
#'   \item{Visibility}{1 Digit Code from The World Meteorological Organization, Code source WMO 4300}
#'   \item{Btl_Cnt}{Bottle Count - All CalCOFI bottles ever sampled, consecutively numbered}
#'   \item{Depth_ID}{Uses the Cast_ID prefix ([Century]-[Year][Month][ShipCode]-[CastType][Julian Day]-[CastTime]-[Line][Sta]) but adds three additional variables: [Depth][Bottle]-[Rec_Ind]}
#'   \item{Depthm}{Bottle depth in meters}
#'   \item{T_degC}{Water temperature in degrees Celsius}
#'   \item{Salnty}{Salinity (Practical Salinity Scale 1978)}
#'   \item{O2ml_L}{Milliliters oxygen per liter of seawater}
#'   \item{STheta}{Potential Density (Sigma Theta), kilograms per cubic meter}
#'   \item{O2Sat}{Oxygen percent saturation}
#'   \item{Oxy_痠ol/Kg}{Oxygen micromoles per kilogram seawater}
#'   \item{BtlNum}{Niskin bottle sample was collected from}
#'   \item{RecInd}{Record Indicator (quality code that applies to the whole bottle, instead of just to a specific parameter)}
#'   \item{T_prec}{Temperature Precision}
#'   \item{T_qual}{Quality Code}
#'   \item{S_prec}{Salinity Precision}
#'   \item{S_qual}{Quality Code}
#'   \item{P_qual}{Quality Code}
#'   \item{O_qual}{Quality Code}
#'   \item{SThtaq}{Quality Code}
#'   \item{O2Satq}{Quality Code}
#'   \item{ChlorA}{Micrograms Chlorophyll-a per liter seawater, measured fluorometrically}
#'   \item{Chlqua}{Quality Code}
#'   \item{Phaeop}{Micrograms Phaeopigment per liter seawater, measured fluormetrically}
#'   \item{Phaqua}{Quality Code}
#'   \item{PO4uM}{Micromoles Phosphate per liter of seawater}
#'   \item{PO4q}{Quality Code}
#'   \item{SiO3uM}{Micromoles Silicate per liter of seawater}
#'   \item{SiO3qu}{Quality Code}
#'   \item{NO2uM}{Micromoles Nitrite per liter of seawater}
#'   \item{NO2q}{Quality Code}
#'   \item{NO3uM}{Micromoles Nitrate per liter of seawater}
#'   \item{NO3q}{Quality Code}
#'   \item{NH3uM}{Micromoles Ammonia per liter of seawater}
#'   \item{NH3q}{Quality Code}
#'   \item{C14As1}{14C Assimilation of Replicate 1 (milligrams carbon per cubic meter of seawater per half light day)}
#'   \item{C14A1p}{Precision of 14C Assimilation of Replicate 1}
#'   \item{C14A1q}{Quality Code}
#'   \item{C14As2}{14C Assimilation of Replicate 2 (milligrams carbon per cubic meter of seawater per half light day)}
#'   \item{C14A2p}{Precision of 14C Assimilation of Replicate 2}
#'   \item{C14A2q}{Quality Code}
#'   \item{DarkAs}{14C Assimilation of Dark/Control Bottle (milligrams carbon per cubic meter of seawater per half light day)}
#'   \item{DarkAp}{Precision of 14C Assimilation of Dark/Control Bottle}
#'   \item{DarkAq}{Quality Code}
#'   \item{MeanAs}{Mean 14C Assimilation of Replicates 1 and 2 (milligrams carbon per cubic meter of seawater per half light day)}
#'   \item{MeanAp}{Precision of Mean 14C Assimilation of Replicates 1 and 2}
#'   \item{MeanAq}{Quality Code}
#'   \item{IncTim}{Elapsed incubation time of the primary productivity experiment}
#'   \item{LightP}{Light intensities of the incubation tubes in the primary productivity experiment, expressed as percentages}
#'   \item{R_Depth}{Reported Depth (from pressure) in meters}
#'   \item{R_TEMP}{Reported (Potential) Temperature in degrees Celsius}
#'   \item{R_Sal}{Reported Salinity (from Specific Volume Anomoly, cubic meters per kilogram)}
#'   \item{R_DYNHT}{Reported Dynamic Height in units of dynamic meters (work per unit mass)}
#'   \item{R_Nuts}{Reported Ammonium concentration (micromoles per liter)}
#'   \item{R_Oxy_痠ol/Kg}{Reported Oxygen micromoles/kilogram}
#'   \item{DIC1}{Dissolved Inorganic Carbon micromoles per kilogram solution}
#'   \item{DIC2}{Dissolved Inorganic Carbon micromoles per kilogram solution (on a replicate sample)}
#'   \item{TA1}{Total Alkalinity micromoles per kilogram solution}
#'   \item{TA2}{Total Alkalinity micromoles per kilogram solution (on a replicate sample)}
#'   \item{pH1}{pH (the degree of acidity/alkalinity of a solution)}
#'   \item{pH2}{pH (the degree of acidity/alkalinity of a solution) on a replicate sample}
#'   \item{DIC Quality Comment}{Quality Comment}
#' }
#' @source \url{https://calcofi.org/data/oceanographic-data/bottle-database/}
#' @concept data
#' @details
#' ### Codes used in data set
#' **Sta_Code (Station Codes)**
#' - "ST" - Standard CalCOFI Station
#' - "SCO" - SCCOOS nearshore/20m Station
#' - "NRO" - Not Regularly Occupied Original CalCOFI Station
#' - "OCO" - Occasionally CalCOFI Occupied
#' - "IMX" - IMECOCAL Occupied
#' - "NST" - Non-Standard Station
#' - "MBR" - MBARI Occupied Station
#'
#' **Data_Type (Data Type)**
#' - "PR" - Productivity Cast
#' - "HY" - Hydrographic Cast
#' - "10" - Ten-meter Cast
#' - "CT" - Compressed CTD Cast (Low Resolution)
#' - "MX" - Mixed CTD and Bottle Data
#'
#' **_qual, qua, or q (Quality Code; associated with discrete samples; examples: "O_qual", "Chlqua", "PO4q")**
#' - Blank - Data OK
#' - "4" - Value zeroed due to value below detection limit
#' - "6" - Data taken from CTD sensor
#' - "8" - Technician thinks value is suspect
#' - "9" - Missing Data
#'
#' **RecInd (Record Indicator)**
#' - "3" - Observed Data
#' - "4" - Educated office guess (ghost)
#' - "5" - Data from STD or CTD sensor
#' - "6" - Duplicate Depth
#' - "7" - Interpolated to a standard depth
#'
#' ### CalCOFI bottle sampling
#' CalCOFI deploys a Seabird 911+ CTD with a 24-place Seabird carousel
#' mounted inside a 24-10L bottle, epoxy-coated rosette frame. Bottle
#' sampling is based on the historical "bottles-hung-on-wire" sampling
#' method utilized by CalCOFI oceanographers from 1949-1993. Twenty Niskin
#' (5L), Nansen (1.25L), or Wally (2.5L) bottles with messengers were
#' attached to a winch wire and suspended vertically in (typically) 500m
#' of ocean depth. The spring-loaded bottles go into the ocean open so
#' seawater can easily flush through them. Once all 20 bottles were
#' deployed and after a 10+ minute soak for the reversing thermometers
#' to equilibrate. A messenger - 1-2lb metal weight that can clamp &
#' slide freely on the wire - was clamped to the wire and released,
#' dropping onto the first bottle submerged at the surface. When closed,
#' the bottles trap seawater from a particular depth with a water-tight
#' seal. This is important since the characteristics of seawater such as
#' salinity and oxygen change with depth. It is critical to the
#' integrity of the measurement that the bottles do not leak and become
#' contaminated by seawater from different depths.
#'
#' - Nansen bottles: the top end of the Nansen bottle would release from
#' the wire, inverting the bottle, releasing its messenger. The inversion
#' of the thermometers would "break" the mercury in the reversing
#' thermometers, locking in the temperature readings. When inverted, the
#' open bottle ends closed, trapping seawater from that depth for later
#' analyses. Three thermometers - two protected, one unprotected were used
#' on bottles 200m and deeper to derive corrected bottle depth; two
#' thermometers were mounted on bottles shallower than 200m. The released
#' messenger would slide down the wire to trip the next bottle - this
#' would continue until all 20 bottles had closed. Calculating actual
#' bottle depth by comparing protected and unprotected reversing
#' thermometer readings would correct for any inaccuracy due to wire angle
#' or winch-error. In use by CalCOFI 1949 - 1966, and occasionally after
#' until CTD-Rosette implementation in 1993.
#'
#' - Niskin bottles: the messenger would strike a plunger, releasing a
#' rotating thermometer rack that would spin 180°, "breaking" the
#' mercury to lock-in the temperature readings of the reversing
#' thermometers. An internal spring would close the o-ringed end caps
#' trapping seawater. CalCOFI Niskins were metal-free & latex-free to
#' allow 14C primary productivity incubation experiments to be conducted
#' with the seawater collected. Epoxy-coated springs and viton o-rings
#' were used to reduce latex toxicity and iron fertilization. After the
#' 1st bottle tripped, the messenger would travel down the wire to the
#' next bottle and so on. The person dropping the messenger would often
#' hold the wire to feel the vibration up the wire of the bottles closing.
#' Eight minutes would elapse for all the bottles to close before the
#' bottle array would be recovered. In use by CalCOFI 1966-1993.
#'
#' - "Wally" bottles were 2.5L PVC/ABS bottles designed by CalCOFI techs
#' George Anderson & Walt Bryant, similar to Niskins but easier to deploy.
#' They used a single broad wire book-clamp and collected smaller water
#' volumes so were easier-quicker to attach and remove from the wire.
#' They also used epoxy-coated springs and Viton o-rings. During their
#' use, 2.5L of seawater was plenty of volume for the at-sea analyses:
#' salts, nutrients, chlorophylls, oxygens. Like Niskin bottles, after
#' the surface bottle tripped, the messenger would travel down the wire striking the next bottle plunger, closing the bottle, and releasing the next messenger. In use by CalCOFI 1988-1993.
#'
#' - CTD-rosette: 10L PVC/ABS plastic bottles are equipped with
#' epoxy-spring loaded with Viton o-rings on all surfaces that contact
#' the seawater samples. Unlike a series of bottles clamped to a winch
#' wire, the CTD-rosette is tethered and deployed on an
#' electrically-conductive winch wire. 24-10L bottles encircle the CTD
#' electronics and are rigged open using lanyards attached to the
#' central carousel hub. The CTD sensors display real-time seawater
#' measurements to a computer screen on the ship for the operator &
#' other scientists to see. Based on the downcast profiles of
#' chlorophyll-a and mixed layer depth, as the CTD-rosette is raised,
#' bottles are closed electronically at specific depths by the operator.
#' Reversing thermometers have been replaced by dual CTD temperature
#' sensors. Bottles still trap seawater from specific depths in
#' leak-proof bottles for analyses on-board the ship. In use by CalCOFI
#' 1993-present.
"bottle"

#' Oceanographic stations
#'
#' The geographic locations of every bottle sampling station utilized on a CalCOFI
#' cruise. This data set is an extraction and modification of the CalCOFI cast table.
#'
#' @format A data frame with 2634 rows and 11 variables
#' \describe{
#'   \item{Sta_ID}{Station ID}
#'   \item{Sta_ID_line}{Line component of the Station ID}
#'   \item{Sta_ID_station}{Station component of the Station ID}
#'   \item{lon}{Station longitude in decimal degrees}
#'   \item{lat}{Station latitude in decimal degrees}
#'   \item{offshore}{`Sta_ID_station` > 60}
#'   \item{is_cast}{In the bottle cast database as of 2022-04-01}
#'   \item{is_cce}{In the California Coastal Ecosystem (CCE) set of stations}
#'   \item{is_ccelter}{In the California Coastal Ecosystem (CCE) Long-Term Ecological Research (LTER) set of stations}
#'   \item{is_sccoos}{In the Southern California Coastal Ocean Observing (SCOOS) set of stations}
#'   \item{geometry}{Station latitude and longitude as a geographic projection (SIRD 4326)}
#' }
#' @source \url{https://calcofi.org/data/oceanographic-data/bottle-database/}
#' @concept data
"stations"

#' Oceanographic DIC data
#'
#' @description
#' CalCOFI collects samples for inorganic carbon and total alkalinity at
#' specific stations throughout the cruise track. Seawater samples are
#' collected from several depths, preserved and brought back to SIO for
#' total inorganic carbon and total alkalinity analysis. These
#' measurements allow the calculation of pH and pCO2. The objectives of
#' these measurements are: first, the long-term characterization of the
#' inorganic carbon system and its response to changing ocean climate,
#' and second, measurements of pH in the coastal zone in order to
#' monitor the anthropogenic impact on coastal ecosystems in the
#' Southern California Bight.
#'
#' @format A data frame with 35376 rows and 82 variables
#' \describe{
#'   \item{Cst_Cnt}{Cast Count - All CalCOFI casts ever conducted, consecutively numbered}
#'   \item{Cruise_ID}{Cruise identifier [Year]-[Month]-[Day]-C-[Ship Code]}
#'   \item{Cruise.x}{Cruise Name [Year][Month]}
#'   \item{Cruz_Sta}{Cruise Name and Station [Year][Month][Line][Station]}
#'   \item{DbSta_ID}{Line and Station}
#'   \item{Cast_ID}{Cast Identifier [Century] - [YY][MM][ShipCode] - [CastType][Julian Day] - [CastTime]-[Line][Sta]}
#'   \item{Sta_ID}{Line and Station}
#'   \item{Quarter}{Quarter of the year}
#'   \item{Sta_Code}{Station Designation (See Station_ID and 0-St_Code for codes)}
#'   \item{Distance}{Nautical miles from coast intercept, calculated from estimated latitude and longitude}
#'   \item{Date}{Date (Month Day Year)}
#'   \item{Year}{Year}
#'   \item{Month}{Month}
#'   \item{Julian_Date}{OA Date: days since December 30, 1899}
#'   \item{Julian_Day}{Julian Day of the year}
#'   \item{Time}{Time (UTC) CTD reached terminal depth}
#'   \item{Lat_Dec}{Observed Latitude in decimal degrees}
#'   \item{Lat_Deg}{Observed Latitude in degrees}
#'   \item{Lat_Min}{Observed Latitude in minutes}
#'   \item{Lat_Hem}{Observed Latitude Hemisphere}
#'   \item{Lon_Dec}{Observed Longitude in decimal degrees}
#'   \item{Lon_Deg}{Observed Longitude in degrees}
#'   \item{Lon_Min}{Observed Longitude in minutes}
#'   \item{Lon_Hem}{Observed Longitude Hemisphere}
#'   \item{Rpt_Line}{Reported Line Number}
#'   \item{St_Line}{Nearest Standard Line}
#'   \item{Ac_Line}{Calculated actual line from observed latitude and longitude}
#'   \item{Rpt_Sta}{Reported Station Number}
#'   \item{St_Station}{Nearest Standard Station Number}
#'   \item{Ac_Sta}{Calculated actual station from observed lat and long}
#'   \item{Bottom_D}{Bottom depth in meters}
#'   \item{Secchi}{Secchi disk depth in meters}
#'   \item{ForelU}{Water color; Only used in CalCOFI dataset from 1988-10 through 1998-04 (Forel-Ule Scale)}
#'   \item{Ship_Name}{Ship's Name}
#'   \item{Ship_Code}{Ship's NODC Code}
#'   \item{Data_Type}{Data Type}
#'   \item{Order_Occ}{Order station was occupied (within each cruise)}
#'   \item{Event_Num}{Event number; includes all research opperations within each cruise (e.g. nets, CTD)}
#'   \item{Cruz_Leg}{Leg of cruise, if multiple legs}
#'   \item{Orig_Sta_ID}{IEH Reported Station ID}
#'   \item{Data_Or}{Data Origin, punch card short code used to designate the institution that collected the data (e.g. Scripps)}
#'   \item{Cruz_Num}{Cruise Designation (Year Month)}
#'   \item{IntChl}{Integrated Chlorophyll per half light day (milligrams chl per square meter per half light day)}
#'   \item{IntC14}{Integrated Primary Productivity per half light day (milligrams C per square meter per half light day)}
#'   \item{Inc_Str}{Incubation Start Time (PST)}
#'   \item{Inc_End}{Incubation End Time (PST)}
#'   \item{PST_LAN}{Time of Local Apparent Noon (PST)}
#'   \item{Civil_T}{Time of Civil Twilight (PST)}
#'   \item{TimeZone}{Time Zone}
#'   \item{Wave_Dir}{Wave Direction reported using an abbreviated 360° azimuth circle with 00 representing True North, 18 represents 180°}
#'   \item{Wave_Ht}{Wave Height in feet}
#'   \item{Wave_Prd}{Wave Period in seconds}
#'   \item{Wind_Dir}{Reported using an abbreviated 360 degree azimuth circle with 0 representing True North, 18 representing 180 degrees}
#'   \item{Wind_Spd}{Wind Speed in knots}
#'   \item{Barometer}{Millibars to the tenths}
#'   \item{Dry_T}{Dry Air Temperature from a Sling Psychrometer in degrees Celsius}
#'   \item{Wet_T}{Wet Air Temperature from a Sling Psychrometer in degrees Celsius}
#'   \item{Wea}{1 Digit Code from The World Meteorological Organization, Code source WMO 4501}
#'   \item{Cloud_Typ}{1 Digit Code from The World Meteorological Organization, Code source WMO 0500}
#'   \item{Cloud_Amt}{1 Digit Code from The World Meteorological Organization, Code source WMO 2700, in oktas}
#'   \item{Visibility}{1 Digit Code from The World Meteorological Organization, Code source WMO 4300}
#'   \item{Cast_Index}{CalCOFI Hydrographic Database Cast Table Index; 1 = CalCOFI database's 1st cast record}
#'   \item{Btl_Cnt}{CalCOFI Hydrographic Database Bottle Table Index; 1 = CalCOFI database's 1st bottle record}
#'   \item{Cruise.y}{CalCOFI Cruise Designation with century prefix; CalCOFI 0901 = 200901}
#'   \item{Depthm}{Sample Depth in meters}
#'   \item{Depth_ID}{Unique database index built from:Century-CruiseShip-Casttype-Julian DOY-UTC Time-LineSta-DepthDup-Record Indicator}
#'   \item{DIC1}{DIC Analysis - Dissolved Inorganic Carbon Measurement, first sample}
#'   \item{DIC2}{DIC Analysis - Dissolved Inorganic Carbon Measurement from a duplicate sample drawn from same depth as DIC 1}
#'   \item{TA1}{DIC Analysis - Total Alkalinity Measurement, first sample; same DIC bottle as DIC 1}
#'   \item{TA2}{DIC Analysis - Total Alkalinity Measurement from a duplicate sample drawn from same depth as TA 1}
#'   \item{pH1}{Spectrophotometrically measured pH per kilogram seawater}
#'   \item{pH2}{Spectrophotometrically measured pH per kilogram seawater from a duplicate sample drawn from same depth as pH 1}
#'   \item{Salinity1}{DIC Analysis - Salinity value from DIC 1 bottle}
#'   \item{Salinity2}{DIC Analysis - Salinity value from DIC 2 bottle}
#'   \item{Temperature_degC}{CTD Temperature from Bottle Sample Depth}
#'   \item{Bottle.Salinity}{CalCOFI Salinity Measurement from DIC Depth}
#'   \item{Bottle_O2_ml_L}{CalCOFI Oxygen Measurement in milliliters/liter, from DIC Depth}
#'   \item{Bottle_O2_µmol_kg}{CalCOFI Oxygen Measurement in umoles/Kg, from DIC Depth}
#'   \item{Sigma.theta}{Sigma-theta Density calculated from CalCOFI data}
#'   \item{DIC.Bottle_ID1}{DIC 1 Bottle Index Number}
#'   \item{DIC.Bottle_ID2}{DIC 2 Bottle Index Number}
#'   \item{DIC.Quality.Comment}{Quality Control Comments on DIC samples after processing}
#' }
#' @source \url{https://calcofi.org/data/oceanographic-data/dic/}
#' @concept data
#' @details
#' DIC samples for Dave Keeling were also collected on CalCOFI stations
#' 90.70, 90.90, & 90.120 from Sep 1986 (CalCOFI 8609NH) to Nov 2002
#' (CalCOFI 0211NH). In Aug 2008, DIC sampling restarted on 7 stations,
#' grew to 10 stations in Jan 2011, and currently DIC has expanded
#' substantially. Dissolved inorganic carbon
#' (DIC) and total alkalinity (TA) data have only been integrated into
#' the CalCOFI hydrographic dataset since Aug 2008, then Jan 2009
#' through presently. Similarly, pH data have only been integrated into
#' the CalCOFI hydrographic dataset since April 2014.
#'
#' ### Codes used in data set
#' **Sta_Code (Station Codes)**
#' - "ST" - Standard CalCOFI Station
#' - "SCO" - SCCOOS nearshore/20m Station
#' - "NRO" - Not Regularly Occupied Original CalCOFI Station
#' - "OCO" - Occasionally CalCOFI Occupied
#' - "IMX" - IMECOCAL Occupied
#' - "NST" - Non-Standard Station
#' - "MBR" - MBARI Occupied Station
#'
#' **Data_Type (Data Type)**
#' - "PR" - Productivity Cast
#' - "HY" - Hydrographic Cast
#' - "10" - Ten-meter Cast
#' - "CT" - Compressed CTD Cast (Low Resolution)
#' - "MX" - Mixed CTD and Bottle Data
#'
#' **_qual, qua, or q (Quality Code; associated with discrete samples; examples: "O_qual", "Chlqua", "PO4q")**
#' - Blank - Data OK
#' - "4" - Value zeroed due to value below detection limit
#' - "6" - Data taken from CTD sensor
#' - "8" - Technician thinks value is suspect
#' - "9" - Missing Data
#'
#' **RecInd (Record Indicator)**
#' - "3" - Observed Data
#' - "4" - Educated office guess (ghost)
#' - "5" - Data from STD or CTD sensor
#' - "6" - Duplicate Depth
#' - "7" - Interpolated to a standard depth
#'
"dic"
