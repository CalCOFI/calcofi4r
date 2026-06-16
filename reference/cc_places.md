# CalCOFI Places

A set of places for commonly extracting CalCOFI data.

## Usage

``` r
cc_places
```

## Format

A `sf` spatial feature set with

- key:

  character key uniquely identifying the record

- category:

  character key

- name:

  name of the place, given the category

- geom:

  polygon geometry in geographic coordinates (SRID 4326)

## Details

Here are the categories and names \[key\]:

1.  BOEM Wind Planning Areas

    - California Call Area - Diablo Canyon \[boem-wpa_NI10-03\]

    - California Call Area - Morro Bay \[boem-wpa_NI10-01\]

    - Oregon Call Area - Brookings \[boem-wpa_NK10-04\]

    - Oregon Call Area - Coos Bay \[boem-wpa_NK10-01\]

2.  CalCOFI Zones

    - Extended Nearshore \[cc_nearshore-extended\]

    - Extended Offshore \[cc_offshore-extended\]

    - Historical Nearshore \[cc_nearshore-historical\]

    - Historical Offshore \[cc_offshore-historical\]

    - Standard Nearshore \[cc_nearshore-standard\]

    - Standard Offshore \[cc_offshore-standard\]

3.  Integrated Ecosystem Assessment

    - California Current \[iea_ca\]

4.  NOAA Aquaculture Opportunity Areas

    - Central North: CN1-A \[noaa-aoa_CN1-A\]

    - Central North: CN1-B \[noaa-aoa_CN1-B\]

    - North: N1-A \[noaa-aoa_N1-A\]

    - North: N1-B \[noaa-aoa_N1-B\]

    - North: N1-C \[noaa-aoa_N1-C\]

    - North: N2-A \[noaa-aoa_N2-A\]

    - North: N2-B \[noaa-aoa_N2-B\]

    - North: N2-C \[noaa-aoa_N2-C\]

    - North: N2-D \[noaa-aoa_N2-D\]

    - North: N2-E \[noaa-aoa_N2-E\]

5.  National Marine Sanctuaries

    - Channel Islands \[nms_ci\]

    - Chumash Proposed Action \[nms_cp\]

    - Cordell Bank \[nms_cb\]

    - Greater Farallones \[nms_gf\]

    - Monterey Bay \[nms_mb\]

    - Olympic Coast \[nms_oc\]
