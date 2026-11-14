# The release's measurement type for an isopycnal variable

The release's `isopycnal` table and the isopycnal cells of its
`climatology` name each
[`cc_isopycnal()`](https://calcofi.io/calcofi4r/reference/cc_isopycnal.md)
variable `iso_` + the variable (`"depth_m"` -\> `"iso_depth_m"`,
`"temperature_ave"` -\> `"iso_temperature_ave"`), so a value on a
density surface is never read as the `obs` series of the same quantity
at a depth. `cc_isopycnal_variable()` is the inverse.

## Usage

``` r
cc_isopycnal_type(variable)

cc_isopycnal_variable(type)
```

## Arguments

- variable:

  [`cc_isopycnal()`](https://calcofi.io/calcofi4r/reference/cc_isopycnal.md)
  variable name(s).

- type:

  release measurement type(s) (`iso_*`).

## Value

character, the same length.

## Examples

``` r
cc_isopycnal_type(c("depth_m", "temperature_ave"))
#> [1] "iso_depth_m"         "iso_temperature_ave"
cc_isopycnal_variable("iso_spiciness0")
#> [1] "spiciness0"
```
