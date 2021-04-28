library(hsdar)
library(rhdf5)
# remotes::install_github("lbusett/prismaread")
library(prismaread)

## convert prisma to geotif
out_folder_L2D <- file.path("data/prisma/L2D")
dir.create(out_folder_L2D, recursive = TRUE)

# Save a full image, prioritizing the VNIR spectrometer and save in ENVI format
hfile="data/prisma/PRS_L2D_STD_20200104085130_20200104085135_0001.he5"
h5ls(hfile)

d=h5read(hfile,"/HDFEOS/SWATHS/PRS_L2D_HCO/Data Fields/VNIR_Cube")

str(d)

pr_convert(in_file    = hfile,
           out_folder = out_folder_L2D,
           out_format = "GTiff",
           VNIR       = TRUE, 
           SWIR       = TRUE,
           LATLON     = TRUE,
           ANGLES     = TRUE)

h5=H5Fopen("data/prisma/PRS_L2D_STD_20200104085130_20200104085135_0001.he5")

#d2=speclib(x=d)


cubePlot(x, r, g, b, ncol = 1, nrow = 1,
         sidecol = colorRamp(palette(heat.colors(100))),
         z_interpolate = FALSE, ...)