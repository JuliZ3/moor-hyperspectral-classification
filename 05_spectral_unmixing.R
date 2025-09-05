# Biblioteken laden 
library(hsdar)
library(terra)

# Arbeitsverzeichnis setzen
# setwd()

# EnMAP-Spektraldaten laden
spec_lib_enmap_cut <- readRDS("enmap_speclib_420–1070.rds")

# Feldspektralbibliothek laden
spec_lib_feld <- readRDS("feldspektrometer_speclib_mean_filter.rds")

# Auf Zielbereich (420–1070 nm) zuschneiden
keep_feld <- wavelength(spec_lib_feld) >= 420 & wavelength(spec_lib_feld) <= 1070
spectra_feld_cut <- spectra(spec_lib_feld)[, keep_feld]
spec_lib_feld_cut <- speclib(spectra = spectra_feld_cut, wavelength = wavelength(spec_lib_feld)[keep_feld])


# Feldspektren auf EnMAP-Bandgrenzen resamplen

# EnMAP-Wellenlängen 
wl_enmap <- wavelength(spec_lib_enmap_cut)

# Bandgrenzen berechnen (untere und obere Grenzen)
lower_bounds <- c(wl_enmap[1] - diff(wl_enmap)[1]/2, zoo::rollmean(wl_enmap, 2))
upper_bounds <- c(zoo::rollmean(wl_enmap, 2), wl_enmap[length(wl_enmap)] + diff(wl_enmap)[length(wl_enmap) - 1]/2)
band_limits <- data.frame(lower = lower_bounds, upper = upper_bounds)

# Resampling der Feldspektren auf EnMAP-Bandgrenzen
spec_lib_feld_resampled <- spectralResampling(spec_lib_feld_cut, band_limits, response_function = NA)


# EnMAP-Daten bereinigen (NAs entfernen)

# Spektraldaten extrahieren
enmap_spectra <- spectra(spec_lib_enmap_cut)
# jede Zeile der Matrix auf NA
valid_pixels <- complete.cases(enmap_spectra)
enmap_spectra_clean <- enmap_spectra[valid_pixels, ]
# bereinigte Matrix wird wieder in ein speclib-Objekt umwandeln
spec_lib_enmap_clean <- speclib(spectra = enmap_spectra_clean, wavelength = wl_enmap)


# Endmember definieren

# Extrahiere resampelte Feldspektren
feld_spectra_resampled <- spectra(spec_lib_feld_resampled)
enmap_wavelengths_cut <- wavelength(spec_lib_feld_resampled)

# Endmember-Spektren auswählen
endmembers <- speclib(spectra = feld_spectra_resampled[1:4, ],
                      wavelength = enmap_wavelengths_cut)


# EnMAP-Daten auf Endmember-Wellenlängen bringen

# Zielwellenlängen: Endmember
target_wl <- wavelength(endmembers)
enmap_wl <- wavelength(spec_lib_enmap_clean)

# Nächstliegende Bänder in EnMAP-Daten suchen
idx_match <- sapply(target_wl, function(wl) which.min(abs(enmap_wl - wl)))

# EnMAP-Spektren entsprechend extrahieren und Wellenlängen überschreiben
spec_lib_enmap_matched <- speclib(
  spectra = spectra(spec_lib_enmap_clean)[, idx_match],
  wavelength = target_wl
)

# Prüfen, ob die Wellenlängen exakt passen
all.equal(wavelength(spec_lib_enmap_matched), wavelength(endmembers))


# Lineares Spektrales Unmixing

unmix_result <- unmix(spec_lib_enmap_matched, endmembers)

# Ergebnisstruktur prüfen
str(unmix_result)


# Ergebnisse als Raster speichern

enmap_raster <- rast("Final EnMAP/05_25_enmap_Rehdener_Geestmoor.tif")

# Anzahl Gesamtpixel und Endmember
n_total <- ncell(enmap_raster)
n_endmembers <- nrow(unmix_result$fractions)

# Leere Matrix erstellen: Zeilen = Pixel, Spalten = Endmember
# Initial alle Werte mit NA, Platzhalter für ungültige Pixel
fraction_matrix <- matrix(NA, nrow = n_total, ncol = n_endmembers)

# Index der gültigen Pixel im Raster
valid_pixels <- which(!is.na(values(enmap_raster[[1]])))

# Unmixing-Ergebnisse in die entsprechende Rasterstruktur einfügen
fraction_matrix[valid_pixels, ] <- t(unmix_result$fractions)

# Für jeden Endmember ein eigenes Raster erzeugen
fraction_stack <- rast(lapply(1:n_endmembers, function(i) {
  r <- enmap_raster[[1]]
  values(r) <- fraction_matrix[, i]
  r
}))

# Endmember-Namen vergeben
names(fraction_stack) <- c("Heidekraut", "Pfeifengras", "Torfmoos", "Wollgras")

# Ergebnis-Raster plotten
plot(fraction_stack)

# abspeichern
# writeRaster(fraction_stack, filename = "linear_unmixing_hsdar.tif", overwrite = TRUE)

# Farbpalette 
pal <- colorRampPalette(c("gold","yellowgreen","forestgreen"))(100)

# Plotten in einem Rasterlayout (2x2, da 4 Endmember)
par(mfrow = c(2,2), mar = c(4,4,3,5))  # ränder: bottom, left, top, right

for(i in 1:n_endmembers){
  plot(fraction_stack[[i]],
       col = pal,
       main = names(fraction_stack)[i],
       legend.args = list(text='Fraktion', side=4, font=2, line=2.5))
}
