librarian::shelf(
  here, hexSticker)

# downloaded

# https://calcofi.org/wp-content/uploads/2021/03/cropped-calcofirose_512_favicon.png
# to logo_calcofi.png

logo_calcofi   <- here("data-raw/logo_calcofi.png")
logo_calcofi4r <- here("man/figures/logo_calcofi4r.png")

sticker(
  logo_calcofi, package="calcofi4r",
  p_size   = 20,    # font size for package name
  s_x      =  1,    # x position for subplot
  s_y      =  0.75, # y position for subplot
  s_width  =  0.6,  # width for subplot
  filename = logo_calcofi4r)
