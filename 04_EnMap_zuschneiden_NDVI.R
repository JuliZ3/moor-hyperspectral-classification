# Biblioteken laden 
library(hsdar)
library(terra)
library(raster)
library(sf)

# Arbeitsverzeichnis setzen
# setwd()

# EnMAP-TIFF einlesen und auf Umrisse des Rehdener Geestmoores zuschneiden
enmap_rast <- rast("05_25_EnMAP.tif")

# Moor-Umriss laden 
umriss_moor <- st_read("../Umriss_Rehdener_Geestmoor.geojson")

# Koordinatenbezug anpassen
moor_poly <- st_transform(umriss_moor, crs(enmap_rast))  

# In terra-kompatibles Objekt umwandeln
moor_vect <- vect(moor_poly)

# Raster zuschneiden und maskieren
enmap_crop <- crop(enmap_rast, moor_vect)
enmap_mask <- terra::mask(enmap_crop,moor_vect)

plot(enmap_mask)

# Speicherort und Dateiname definieren
output_path <- "05_25_enmap_Rehdener_Geestmoor.tif"

# Abspeichern als GeoTIFF
# writeRaster(enmap_mask, filename = output_path, overwrite = TRUE)


# EnMAP-Daten vorbereiten

# Arbeitsverzeichnis setzen
# setwd()

# EnMAP-Daten einlesen
enmap_tif <- "Final EnMAP/05_25_enmap_Rehdener_Geestmoor.tif"
enmap_raster <- rast(enmap_tif)

# Wellenlängen einlesen
enmap_wavelengths <- read.csv("Final EnMAP/wavelengths.csv", header = TRUE)$wavelength

# Wellenlängenbereich 420–1070 nm extrahieren
keep_idx <- which(enmap_wavelengths >= 420 & enmap_wavelengths <= 1070)
enmap_cut <- enmap_raster[[keep_idx]]
wavelengths_cut <- enmap_wavelengths[keep_idx]

# In Spektralbibliothek umwandeln
spectra_matrix <- as.matrix(enmap_cut)
spec_lib_enmap_cut <- speclib(spectra = spectra_matrix, wavelength = wavelengths_cut)

# Ergebnis speichern: Spektralbibliothek als RDS-Datei
# saveRDS(spec_lib_enmap_cut, "enmap_speclib_420–1070.rds")


# Berechnung des NDVI
ndvi <- vegindex(spec_lib_enmap_cut, index = "NDVI")

# In Raster umwandeln
ndvi_raster <- enmap_cut[[1]]       
values(ndvi_raster) <- as.vector(ndvi)

# Plotten
plot(ndvi_raster, col = colorRampPalette(c("firebrick","yellow","green4"))(100), zlim = c(0,1))

# NDVI dem EnMAP-Raster hinzufügen
enmap_mask$ndvi <- ndvi_raster


# Standardabweichung des NDVI über verschiedenen Pixel-Umgebung
enmap_mask$ndvi_sd_3x3 <- focal(ndvi_raster, w = 3, fun = sd, na.rm = TRUE)
enmap_mask$ndvi_sd_5x5 <- focal(ndvi_raster, w = 5, fun = sd, na.rm = TRUE)
enmap_mask$ndvi_sd_7x7 <- focal(ndvi_raster, w = 7, fun = sd, na.rm = TRUE)


par(mfrow = c(1,3))
plot(enmap_mask$ndvi_sd_3x3, main = "NDVI SD 3x3")
plot(enmap_mask$ndvi_sd_5x5, main = "NDVI SD 5x5")
plot(enmap_mask$ndvi_sd_7x7, main = "NDVI SD 7x7")
par(mfrow = c(1,1))

# Endergebnis als GeoTIFF speichern
writeRaster(enmap_mask, "C:/Users/julia/Documents/Studium/6. Semester/Bachelorarbeit/Rehdener Moor Daten/predictors_ohne_weg_enmap.tif", overwrite=T)
