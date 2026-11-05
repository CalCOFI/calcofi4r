# The cruise's month and season every anomaly keys on

Since release v2026.11 (calcofi4db 4.34.0; Rasmus Swalethorp 2026-10-16,
Ben Best 2026-10-20) a cruise's month is the calendar month of the
**median datetime of its root samples** (`cruise.month_median`), not the
`YYYY-MM` its `cruise_key` names (59 of 868 cruises centre on the month
beside their key's), and the seasons are the meteorological **DJF, MAM,
JJA, SON**, numbered 1–4 (the `quarter` column), December counting
toward the following year's winter. `cc_cruise_month()` returns each
cruise's month from the connection's `cruise` table where it carries
`month_median`, else the key's `YYYY-MM`, else the month of `datetime`;
`cc_season()` maps a month to its season number (1–4), and
`cc_season_label()` to "Dec–Feb" … "Sep–Nov".

## Usage

``` r
cc_cruise_month(con, cruise_key, datetime = NULL)

cc_season(month)

cc_season_label(month)
```

## Arguments

- con:

  DuckDB connection to a release
  ([`cc_get_db()`](https://calcofi.io/calcofi4r/reference/cc_get_db.md)).

- cruise_key, datetime:

  vectors of equal length.

- month:

  integer month (1–12).

## Value

`cc_cruise_month()`: integer months; `cc_season()`: integer 1–4;
`cc_season_label()`: character.

## Examples

``` r
cc_season(c(12, 1, 6))          # 1 1 3
#> [1] 1 1 3
cc_season_label(cc_season(7))   # "Jun–Aug"
#> [1] "Mar–May"
```
