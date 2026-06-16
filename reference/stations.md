# Oceanographic stations

The geographic locations of every bottle sampling station utilized on a
CalCOFI cruise. This data set is an extraction and modification of the
CalCOFI cast table.

## Usage

``` r
stations
```

## Format

A data frame with 2634 rows and 11 variables

- sta_id:

  Station ID

- sta_id_line:

  Line component of the Station ID

- sta_id_station:

  Station component of the Station ID

- lon:

  Station longitude in decimal degrees

- lat:

  Station latitude in decimal degrees

- is_offshore:

  `Sta_ID_station` \> 60

- is_cce:

  In the California Coastal Ecosystem (CCE) set of stations

- is_ccelter:

  In the California Coastal Ecosystem (CCE) Long-Term Ecological
  Research (LTER) set of stations

- is_sccoos:

  In the Southern California Coastal Ocean Observing (SCOOS) set of
  stations

- geometry:

  Station latitude and longitude as a geographic projection (SRID 4326)

## Source

<https://calcofi.org/data/oceanographic-data/bottle-database/>
