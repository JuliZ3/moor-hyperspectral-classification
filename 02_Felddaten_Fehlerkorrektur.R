# Biblioteken laden 
library(terra)
library(hsdar)

# Arbeitsverzeichnis setzen
setwd("C:/Users/julia/Documents/Studium/6. Semester/Bachelorarbeit/Rehdener Moor Daten/")

# Spektralbibliothek der Feldspektren laden
spec_lib <- readRDS("feldspektrometer_speclib_alle.rds")

# Wellenlängenvektor extrahieren
wl <- wavelength(spec_lib)

# entfernen der Wellenlängen 757 nm bis 770 nm
valid_idx <- which(wl < 757 | wl > 770)

# Subset der Spektralbibliothek bilden → kritische Wellenlängen ausgeschlossen
spec_lib_cleaned <- spec_lib[, valid_idx]

# Prüfen und plotten
wavelength(spec_lib_cleaned)
plot(spec_lib_cleaned, FUN = 1:4)

# Zwischenergebnis abspeichern
saveRDS(spec_lib_cleaned, file = "feldspektrometer_speclib_ohne757_770.rds")

# Smoothing der Spektren mit Mean-Filter-Funktion

meanflt <- noiseFiltering(spec_lib_cleaned, method = "mean", p = 5)

# plotten der geglätteten Spektren
farben <- c("mediumpurple3", "yellowgreen", "gold", "forestgreen")

plot(meanflt,
     FUN = 1:4,
     col = farben,
     lwd = 2,
     xlab = "Wellenlänge (nm)",
     ylab = "Reflexion")

# Legende hinzufügen
legend("topleft",
       legend = c("Heidekraut", "Pfeifengras", "Torfmoos", "Wollgras"), 
       col = farben,
       lwd = 2,
       bty = "n")

# Ergebnis abspeichern
saveRDS(spec_lib_cleaned, file = "feldspektrometer_speclib_mean_filter.rds")
