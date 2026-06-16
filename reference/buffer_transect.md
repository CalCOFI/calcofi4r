# Create Buffer Around Line Segment with Dateline Handling

Creates a buffered polygon around a line segment (transect), handling
dateline crossings and projecting to appropriate UTM zone for accurate
distance calculations.

## Usage

``` r
buffer_transect(coords, buffer_dist = 5000)
```

## Arguments

- coords:

  Matrix or data.frame of coordinates (longitude, latitude) defining the
  line segment

- buffer_dist:

  Numeric buffer distance in meters (default: 5000)

## Value

List containing:

- `utm_crs` - EPSG code for the UTM projection used

- `segment` - sf linestring object in WGS84 (EPSG:4326)

- `segment_utm` - sf linestring object in UTM projection

- `buffer` - sf polygon buffer in WGS84 (EPSG:4326)

- `buffer_utm` - sf polygon buffer in UTM projection

## Details

The function automatically detects the appropriate UTM zone based on the
centroid of the input segment. Dateline crossings are handled by
normalizing coordinates to 0-360 range when necessary.

## See also

[`fix_dateline_crossing`](https://calcofi.io/calcofi4r/reference/fix_dateline_crossing.md)
for dateline crossing detection

[`modal_depth_profile`](https://calcofi.io/calcofi4r/reference/modal_depth_profile.md)
for UI implementation

## Examples

``` r
# create transect across California Current
coords <- matrix(c(-120, 34, -118, 36), ncol = 2, byrow = TRUE)
result <- buffer_transect(coords, buffer_dist = 10000)
plot(result$buffer)

```
