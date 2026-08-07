# Stations along a CalCOFI line, ordered nearshore to offshore

A transect here is **a CalCOFI line, ordered by station number
ascending**, which is nearshore → offshore (`calcofi4r` treats station
\> 60 as offshore; station 100 on line 76.7 sits at roughly -124.3°
lon). That is well defined for every cruise without asking the user to
pick endpoints, which is what makes a whole archive of sections
pre-renderable.

## Usage

``` r
cc_transect_stations(
  con,
  line,
  cruise_key = NULL,
  dataset_key = "calcofi_ctd-cast",
  x = c("occupied", "line")
)
```

## Arguments

- con:

  DuckDB connection to a release (see
  [`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md)).

- line:

  CalCOFI line, e.g. `93.3`.

- cruise_key:

  cruise to restrict to; `NULL` for every cruise on the line.

- dataset_key:

  source dataset (default `"calcofi_ctd-cast"`).

- x:

  `"occupied"` (default) or `"line"` — see above.

## Value

Tibble: `cruise_key`, `grid_key`, `sta`, `lon`, `lat`, `datetime`,
`data_stage`, `dist_km`.

## Details

`order_occ` — the order stations were occupied during the cruise — is
deliberately NOT used: it is the ship's track, so its direction is
whichever way the ship steamed, and it is NULL on roughly half the
release's cast rows.

## The `x` argument

`x = "occupied"` (default) measures distance between the stations this
cruise actually occupied, so the section fills the plot and shows the
data at its largest. `x = "line"` measures each station's distance along
the FULL line geometry, so two cruises that sampled different subsets
are directly comparable in width — at the cost of blank space where a
cruise stopped short. That matters: line 93.3 has not been sampled past
station 90 since 2025-01, though 113 of the 130 cruises before that
reached station 120, so recent sections silently span a shorter distance
than historical ones.
