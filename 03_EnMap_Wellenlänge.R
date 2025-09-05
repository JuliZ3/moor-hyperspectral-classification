# Biblioteken laden 
library(xml2)

# Pfad zur XML-Metadatendatei von EnMAP 
metadata_file <- "C:/Users/julia/Documents/Studium/6. Semester/Bachelorarbeit/ENMAP01-____L2A-DT0000129767_20250512T110717Z_009_V010502_20250520T173159Z/ENMAP01-____L2A-DT0000129767_20250512T110717Z_009_V010502_20250520T173159Z-METADATA.XML"

# XML-Datei einlesen
xml <- read_xml(metadata_file)


# Spezifisch alle "wavelengthCenterOfBand"-Knoten auslesen
# Enthalten die mittleren Wellenlängen der Bänder
wavelength_nodes <- xml_find_all(xml, ".//wavelengthCenterOfBand")

# Text extrahieren und in numerischen Vektor umwandeln
wavelength <- as.numeric(xml_text(wavelength_nodes))

# Prüfen, ob alle Bänder extrahiert wurden
length(wavelength) 

# Ausgabe prüfen
print(wavelength)

# Arbeitsverzeichnis setzen
# setwd("")

# Wellenlängen als Textdatei speichern
writeLines(as.character(wavelength), "wavelengths.txt")

# Wellenlängen als CSV-Datei speichern
write.csv(data.frame(wavelength = wavelength), "wavelengths.csv", row.names = FALSE)




