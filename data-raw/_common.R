# libraries ----
if (!require("librarian")){
  install.packages("librarian")
  library(librarian)
}

# paths ----
dir_data <- switch(
  Sys.info()["nodename"],
  `Bens-MacBook-Pro.local`      =
    "/Users/bbest/My Drive/projects/calcofi/data",
  `Cristinas-MacBook-Pro.local` =
    "/Volumes/GoogleDrive/.shortcut-targets-by-id/13pWB5x59WSBR0mr9jJjkx7rri9hlUsMv/calcofi/data")
