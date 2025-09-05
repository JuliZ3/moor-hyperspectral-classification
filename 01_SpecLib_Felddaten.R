# Biblioteken laden 
library(asdreader)
library(hsdar)
library(dplyr)

# Arbeitsverzeichnis setzen
# setwd()
data_path <- "Donnerstag/"  # z. B. "data/asd/"

# Hilfsfunktion zum Einlesen einer ASD-Datei
read_asd <- function(file) {
  get_spectra(file, type = "reflectance")
}

# Dateinummern der Messungen je Pflanzenart
files_heidekraut <- c(1:15, 31:45, 151:165)
files_pfeifengras <- c(16:30, 91:105, 136:150) # fehler?  
files_torfmoos <- c(46:60, 76:90, 121:135, 181:195)
files_wollgras <- c(61:75, 106:120, 166:180)

# Funktion lädt alle Spektren für eine Pflanzenart, prüft sie auf Fehler und bildet den Mittelwert
load_and_average <- function(file_ids, label) {
  spectra_list <- lapply(file_ids, function(id) {
    file <- sprintf("%s2BAJ%05d.asd", data_path, id)
    
    if (!file.exists(file)) {
      message(sprintf("Datei %s existiert nicht", file))
      return(NULL)  
    }
    
    spec <- read_asd(file)
    
    # Prüfen auf NAs/NaNs
    if (any(is.na(spec)) || any(is.nan(spec))) {
      message(sprintf("Warnung: Datei %s enthält NA/NaN-Werte und wird nicht genommen", file))
      return(NULL)
    }
    
    return(spec)
  })
  
  # Nur gültige Spektren
  valid_spectra <- Filter(Negate(is.null), spectra_list)
  
  if (length(valid_spectra) == 0) {
    warning(sprintf("Keine gültigen Spektren für %s gefunden!", label))
    return(list(spectrum = rep(NA, 751), label = label))  # Leeres Spektrum zurückgeben
  }
  
  # alle Spektren in eine MAtrix schreiben
  spectra_matrix <- do.call(rbind, valid_spectra)
  
  # Mittelwert bilden
  avg_spectrum <- colMeans(spectra_matrix, na.rm = TRUE)
  
  list(spectrum = avg_spectrum, label = label)
}

# Mittelwerte für jede Pflanzenart berechnen mit der obigen Funktion
avg_heide <- load_and_average(files_heidekraut, "Heidekraut")
avg_pfeifen <- load_and_average(files_pfeifengras, "Pfeifengras")
avg_torf <- load_and_average(files_torfmoos, "Torfmoos")
avg_woll <- load_and_average(files_wollgras, "Wollgras")

# alle Spektren zu einer Matrix kombinieren
all_spectra <- rbind(avg_heide$spectrum,
                     avg_pfeifen$spectrum,
                     avg_torf$spectrum,
                     avg_woll$spectrum)


# Wellenlängen definieren: 325–1075 nm mit 1 nm Schritt (für HandHeld2)
wavelengths <- seq(325, 1075, length.out = ncol(all_spectra))

# Spektralbibliothek erzeugen
spec_lib <- speclib(all_spectra, wavelengths)

# Labels zuweisen
SI(spec_lib) <- data.frame(art = c("Heidekraut", "Pfeifengras", "Torfmoos", "Wollgras"))


# Plot der mittleren Spektren

# Farben für die Pflanzenarten
farben <- c("darkgreen", "goldenrod", "red", "blue")

plot(spec_lib,
     FUN = 1:4,
     main = "Mittlere Spektren je Pflanzenart",
     col = farben,
     lwd = 2,
     xlab = "Wellenlänge (nm)",
     ylab = "Reflexion")

# Legende hinzufügen
legend("topright",
       legend = c("Heidekraut", "Pfeifengras", "Torfmoos", "Wollgras"),
       col = farben,
       lwd = 2,
       bty = "n")

# Ergebnis speichern: Spektralbibliothek als RDS-Datei
saveRDS(spec_lib, file = "feldspektrometer_speclib_alle.rds")
