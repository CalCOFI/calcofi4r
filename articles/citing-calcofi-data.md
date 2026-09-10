# Citing CalCOFI data

## The release cites itself; every dataset in it cites its own source

CalCOFI’s integrated database is not one thing to cite — it is a
**release** (a specific, versioned snapshot) built from **datasets**,
each contributed by its own program under its own license, and often its
own DOI. Citing “CalCOFI” and citing the CTD-bottle time series and
citing the Farallon seabird census are three different citations, and a
paper that used all three owes all three.

Every field involved — `citation_main`, `license`, `doi`,
`acknowledgement` — is checked at release time
(`calcofi4db::check_dataset_citation()`, the attribution contract): a
citation with a year and a locator, a license registered in
`metadata/license.csv`, a DOI that actually resolves.
[`cc_cite()`](https://calcofi.io/calcofi4r/reference/cc_cite.md) is the
one place that reads those fields back out and formats them, so you
never have to build a citation string by hand or guess whether a
`license_url` is required.

``` r

library(calcofi4r)

REL <- cc_latest_version()
cc_cite(version = REL)
#>  [1] "CalCOFI (2026). CalCOFI Integrated Database, release v2026.09.10 [Data set]. Scripps Institution of Oceanography, NOAA Fisheries, and California Department of Fish and Wildlife. https://calcofi.io/db-schema/?v=v2026.09.10\nPage: https://calcofi.io/datasets/release/"                                                                                                                                                                                                                                                                                                                                                                                                                           
#>  [2] "CalCOFI. (2023). CalCOFI Bottle Database 194903-202105. CalCOFI.org.\nPage: https://calcofi.io/datasets/calcofi_bottle/"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#>  [3] "CalCOFI. (2023). CalCOFI CTD Cast Files. CalCOFI.org.\nPage: https://calcofi.io/datasets/calcofi_ctd-cast/"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
#>  [4] "Keeling, C.D.; Lueker, T.J.; Emanuele, G.; Dickson, A.G.; Martz, T.R.; Wolfe, W.H.; Mau, A. (2025). Discrete profile dissolved inorganic carbon, total alkalinity, water temperature and salinity measurements for CalCOFI (NCEI Accession 0301029). NOAA NCEI. https://doi.org/10.25921/3w9f-jd72\nLicense: CC-BY-4.0\nDOI: https://doi.org/10.25921/3w9f-jd72\nPage: https://calcofi.io/datasets/calcofi_dic/"                                                                                                                                                                                                                                                                                     
#>  [5] "CalCOFI. Underway (METS) TSG/Meteorology Data. CalCOFI.org.\nPage: https://calcofi.io/datasets/calcofi_mets/"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
#>  [6] "CalCOFI - Scripps Institution of Oceanography and T. Koslow. 2017. Data pertaining to lobster phyllosoma, Panulirus interruptus, collection methods, locations, identification and staging (1951-2008, months of July and August) ver 4. Environmental Data Initiative. https://doi.org/10.6073/pasta/9e38121ebb26f1b59b7b39b2eff844fa\nLicense: custom (https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-cce.188.4)\nDOI: https://doi.org/10.6073/pasta/9e38121ebb26f1b59b7b39b2eff844fa\nPage: https://calcofi.io/datasets/calcofi_phyllosoma/"                                                                                                                              
#>  [7] "CalCOFI - Scripps Institution of Oceanography, California Current Ecosystem LTER, and E. Venrick. 2023. Temporal and spatial changes of the abundance and species composition of phytoplankton in the California Current from samples collected aboard CalCOFI cruises from summer 1996 through 2022. ver 4. Environmental Data Initiative. https://doi.org/10.6073/pasta/60edabfbfd85c623fce05822befaa071\nLicense: CC0-1.0\nDOI: https://doi.org/10.6073/pasta/60edabfbfd85c623fce05822befaa071\nPage: https://calcofi.io/datasets/calcofi_phytoplankton/"                                                                                                                                         
#>  [8] "Ohman, M.D. 2022. California Current Ecosystem Euphausiid data, Brinton and Townsend Euphausiid Database (BTEDB) ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/4a92a0044bcd1523a4f994ece874a57d\nLicense: custom (https://portal.edirepository.org/nis/metadataviewer?packageid=knb-lter-cce.313.1)\nDOI: https://doi.org/10.6073/pasta/4a92a0044bcd1523a4f994ece874a57d\nAcknowledgement: Data originate from the Brinton and Townsend Euphausiid Database of the Pelagic Invertebrate Collection, Scripps Institution of Oceanography. Supported in recent years by NSF grants to M.D. Ohman and the CCE-LTER site.\nPage: https://calcofi.io/datasets/cce-lter_euphausiids/"
#>  [9] "Landry, M. (2004-2023). Picoplankton and Bacteria Abundance (CalCOFI Cruise). CCE LTER.\nPage: https://calcofi.io/datasets/cce-lter_picoplankton-bacteria/"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
#> [10] "ZooDB Holoplankton Community [dataset].\nLicense: custom (https://oceaninformatics.ucsd.edu/zoodb/)\nAcknowledgement: Plankton sample analysis supported by NSF grants to M.D. Ohman and the CCE-LTER site, and by the SIO Pelagic Invertebrate Collection.\nPage: https://calcofi.io/datasets/cce-lter_zoodb/"                                                                                                                                                                                                                                                                                                                                                                                      
#> [11] "ZooScan PRPOOS Zooplankton [dataset].\nLicense: custom (https://oceaninformatics.ucsd.edu/zooscandb/)\nAcknowledgement: Plankton sample analysis supported by NSF grants to M.D. Ohman and the CCE-LTER site.\nPage: https://calcofi.io/datasets/cce-lter_zooscan/"                                                                                                                                                                                                                                                                                                                                                                                                                                  
#> [12] "Rogers-Bennett, L.; Jones, E.; Klemmedson, A. (2026). CDFW Dungeness Crab Megalopae from archived CalCOFI plankton samples (1949-2014). California Department of Fish and Wildlife, published through CalCOFI / Scripps Institution of Oceanography.\n\nLicense: CC-BY-4.0\nPage: https://calcofi.io/datasets/cdfw_dungeness-crab/"                                                                                                                                                                                                                                                                                                                                                                  
#> [13] "CalCOFI Bird & Mammal Census [dataset].\nLicense: custom (https://oceanview.pfeg.noaa.gov/CalCOFI/app/resources/docs/Data_Sharing_Agreement_FarallonInstitute.pdf)\nPage: https://calcofi.io/datasets/farallon_bird-mammal/"                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
#> [14] "Koslow, J. Anthony (2016). CalCOFI Trawl Data. In California Cooperative Oceanic Fisheries Investigations (CalCOFI): Acoustic and Trawl Data. UC San Diego Library Digital Collections. https://doi.org/10.6075/J0BZ64DH\n\nLicense: CC-BY-4.0\nDOI: https://doi.org/10.6075/J0BZ64DH\nPage: https://calcofi.io/datasets/sio_mesopelagic-fish/"                                                                                                                                                                                                                                                                                                                                                      
#> [15] "SIO PIC Zooplankton Net Tows [dataset].\nPage: https://calcofi.io/datasets/sio_pic-zooplankton/"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> [16] "CalCOFI Underway CUFES Fish Eggs [dataset].\nLicense: custom (https://coastwatch.pfeg.noaa.gov/erddap/tabledap/erdCalCOFIcufes.das)\nPage: https://calcofi.io/datasets/swfsc_cufes/"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#> [17] "NOAA Fisheries SWFSC. CalCOFI Ichthyoplankton Database.\nPage: https://calcofi.io/datasets/swfsc_ichthyo/"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
#> attr(,"source")
#> [1] "release"
```

The **first** entry is always the release itself — *CalCOFI Integrated
Database, release v2026.09.10* — cited under the concept DOI once Zenodo
has minted one for the tag, or a stable `db-schema` URL until then.
Every entry after it is one dataset, in alphabetical `dataset_key` order
by default.

## Citing just what you used

Pass the `dataset_key`(s) you actually queried — never everything, and
never whichever ones you remember typing.

``` r

cc_cite(c("calcofi_bottle", "calcofi_ctd-cast"), version = REL)
#> [1] "CalCOFI (2026). CalCOFI Integrated Database, release v2026.09.10 [Data set]. Scripps Institution of Oceanography, NOAA Fisheries, and California Department of Fish and Wildlife. https://calcofi.io/db-schema/?v=v2026.09.10\nPage: https://calcofi.io/datasets/release/"
#> [2] "CalCOFI. (2023). CalCOFI Bottle Database 194903-202105. CalCOFI.org.\nPage: https://calcofi.io/datasets/calcofi_bottle/"                                                                                                                                                  
#> [3] "CalCOFI. (2023). CalCOFI CTD Cast Files. CalCOFI.org.\nPage: https://calcofi.io/datasets/calcofi_ctd-cast/"                                                                                                                                                               
#> attr(,"source")
#> [1] "release"
```

Better still, hand
[`cc_cite()`](https://calcofi.io/calcofi4r/reference/cc_cite.md) the
**query result itself**. Any tibble carrying a `dataset_key` column
([`cc_read_obs()`](https://calcofi.io/calcofi4r/reference/cc_read_obs.md),
[`cc_read_sample()`](https://calcofi.io/calcofi4r/reference/cc_read_sample.md),
a `cc_match_*()` join, your own SQL) works directly — the distinct keys
the query actually touched are what gets cited, in the order they first
appear:

``` r

library(dplyr)

d <- cc_read_obs(
  datasets = c("calcofi_dic", "calcofi_bottle"),
  measurement_types = "temperature",
  version = REL) |>
  head(500)

cc_cite(d, version = REL)
#> [1] "CalCOFI (2026). CalCOFI Integrated Database, release v2026.09.10 [Data set]. Scripps Institution of Oceanography, NOAA Fisheries, and California Department of Fish and Wildlife. https://calcofi.io/db-schema/?v=v2026.09.10\nPage: https://calcofi.io/datasets/release/"
#> [2] "CalCOFI. (2023). CalCOFI Bottle Database 194903-202105. CalCOFI.org.\nPage: https://calcofi.io/datasets/calcofi_bottle/"                                                                                                                                                  
#> attr(,"source")
#> [1] "release"
```

An unrecognized `dataset_key` is an error naming it, rather than a
citation silently missing a dataset:

``` r

cc_cite("not_a_real_dataset", version = REL)
#> Error:
#> ! cc_cite(): unknown dataset_key(s): not_a_real_dataset
```

## Three formats

`format = "text"` (the default, above) is meant for a methods section or
an email. `format = "bibtex"` builds a `.bib`-ready entry per citation,
and `format = "csl"` returns
[CSL-JSON](https://citeproc-js.readthedocs.io/) items — what Zotero,
Pandoc citations and most reference managers import directly.

``` r

cat(cc_cite("calcofi_dic", version = REL, format = "bibtex"))
```

@misc{calcofi_release_v2026_09_10, title = {CalCOFI Integrated Database,
release v2026.09.10}, author = {CalCOFI}, year = {2026}, publisher =
{Scripps Institution of Oceanography, NOAA Fisheries, and California
Department of Fish and Wildlife}, url =
{<https://calcofi.io/db-schema/?v=v2026.09.10>} }

@misc{calcofi_dic, title = {CalCOFI DIC}, howpublished = {Keeling, C.D.;
Lueker, T.J.; Emanuele, G.; Dickson, A.G.; Martz, T.R.; Wolfe, W.H.;
Mau, A. (2025). Discrete profile dissolved inorganic carbon, total
alkalinity, water temperature and salinity measurements for CalCOFI
(NCEI Accession 0301029). NOAA NCEI.
<https://doi.org/10.25921/3w9f-jd72>}, year = {2025}, doi =
{10.25921/3w9f-jd72}, url = {<https://doi.org/10.25921/3w9f-jd72>}, note
= {License: CC-BY-4.0} }

``` r

str(cc_cite("calcofi_dic", version = REL, format = "csl")[[1]])
#> List of 7
#>  $ id       : chr "calcofi_release_v2026_09_10"
#>  $ type     : chr "dataset"
#>  $ title    : chr "CalCOFI Integrated Database, release v2026.09.10"
#>  $ author   :List of 1
#>   ..$ :List of 1
#>   .. ..$ literal: chr "CalCOFI"
#>  $ issued   :List of 1
#>   ..$ date-parts:List of 1
#>   .. ..$ :List of 1
#>   .. .. ..$ : int 2026
#>  $ publisher: chr "Scripps Institution of Oceanography, NOAA Fisheries, and California Department of Fish and Wildlife"
#>  $ URL      : chr "https://calcofi.io/db-schema/?v=v2026.09.10"
```

`format = "bibtex"` builds every entry **offline**, from the fields
already on the release’s `dataset` table and in its `catalog.json` —
nothing here calls the network by default. `resolve = TRUE` is the only
network path: for any entry with a DOI, it fetches the DOI’s own BibTeX
from `doi.org` instead (falling back to the offline entry if that fetch
fails), which some reference managers format slightly differently.

``` r

cc_cite("calcofi_dic", version = REL, format = "bibtex", resolve = TRUE)
```

## Citing an older release

A release frozen before the attribution contract landed (2026-09-03) has
no `citation` in its `catalog.json`.
[`cc_cite()`](https://calcofi.io/calcofi4r/reference/cc_cite.md)
computes the same wording `calcofi4db::release_citation()` would have
written rather than erroring, and marks the result so you can tell which
happened:

``` r

attr(cc_cite(character(0), version = REL), "source")
#> [1] "release"
```

`"release"` means the catalog carried its own citation; `"computed"`
means [`cc_cite()`](https://calcofi.io/calcofi4r/reference/cc_cite.md)
derived it.

## Citing the software, separately

[`cc_cite()`](https://calcofi.io/calcofi4r/reference/cc_cite.md) is for
the **data**. To cite the R package itself — appropriate alongside the
data citation when `calcofi4r` did real analytical work, not just I/O —
use base R’s own mechanism, which reads `DESCRIPTION`:

``` r

citation("calcofi4r")
#> To cite package 'calcofi4r' in publications use:
#> 
#>   Best B (2026). _calcofi4r: CalCOFI R helper functions_. R package
#>   version 1.24.2, <https://calcofi.io/calcofi4r>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {calcofi4r: CalCOFI R helper functions},
#>     author = {Ben Best},
#>     year = {2026},
#>     note = {R package version 1.24.2},
#>     url = {https://calcofi.io/calcofi4r},
#>   }
```

The Python sibling, [`calcofi4py`](https://calcofi.io/calcofi4py/),
mirrors every part of
[`cc_cite()`](https://calcofi.io/calcofi4r/reference/cc_cite.md) above
byte-for-byte under the same name (`calcofi4py.cc_cite()`) — the same
release and dataset citations, read the same way, for anyone working in
a notebook instead of R.
