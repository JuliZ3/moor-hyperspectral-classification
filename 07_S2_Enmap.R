# Biblioteken laden 
library(terra)
library(sf)
library(hsdar)
library(CAST)
library(caret)
library(ggplot2)


# Arbeitsverzeichnis setzen
# setwd()

# Sentinel-2-Daten laden
sentinel <- rast("sentinel_combined_12_06_2025_ganze_Szene.tif")

# 2. Umriss des Moores einlesen
umriss_moor <- st_read("../Umriss_Rehdener_Geestmoor.geojson")
moor_vect <- vect(st_transform(umriss_moor, crs(sentinel)))

# EnMAP-Daten einlesen
enmap<- rast("../predictors_ohne_weg_enmap.tif")

# Sentinel-2-Daten auf Moorfläche zuschneiden und maskieren
sentinel_crop <- crop(sentinel, moor_vect)
sentinel_masked <- terra::mask(sentinel_crop, moor_vect)

# Sentinel-2 auf EnMAP-Gitter projizieren
sentinel_resampled <- resample(sentinel_masked, enmap, method = "bilinear")

# Sentinel-2 und EnMAP kombinieren
enmap_combined <- c(enmap, sentinel_resampled)
names(enmap_combined)  

# Ergebnis abspeichern
# writeRaster(enmap_combined, "C:/Users/julia/Documents/Studium/6. Semester/Bachelorarbeit/Rehdener Moor Daten/enmap_sentinel_2_combined.tif", overwrite=T)


# Spectral Unmixing Ergebnis einlesen
lu_stack <- rast("../linear_unmixing_hsdar.tif")
# Namen vergeben
names(lu_stack) <- c("lu_Heidekraut", "lu_Pfeifengras", "lu_Torfmoos", "lu_Wollgras")

# EnMAP + Sentinel-2 + Spectral Unmixing kombinieren
enmap_s2_lu_combined <- c(enmap_combined, lu_stack)

# Layernamen umbenennen
# Anzahl der Layer im Raster holen
num_layers <- nlyr(enmap_s2_lu_combined)

# Neue Namen erzeugen: Band1, Band2, ..., BandN
new_names <- paste0("Band", 1:num_layers)

# Namen im Raster setzen
names(enmap_s2_lu_combined) <- new_names

# die letzten Layer wieder richtig benennen
names(enmap_s2_lu_combined)[(num_layers - 17):num_layers] <- c("B05","B06", "B07", "B8A", "B11", "B12", "B04", "B03", "B02", "B08","ndvi", "ndvi_sd_3x3", "ndvi_sd_5x5", "ndvi_sd_7x7",
                                                        "lu_Heidekraut", "lu_Pfeifengras", "lu_Torfmoos", "lu_Wollgras")


# Trainingsdaten einlesen
trainingsites <- st_read("../Trainingsdaten_RM.gpkg")

# eindeutige ID für jedes Polygon
trainingsites$PolyID <- 1:nrow(trainingsites)

# Zufallsstichprobe innerhalb der Polygone (5000 Punkte)
set.seed(5)
sampleloc <- st_sample(trainingsites, size = 5000)

# Punkte mit Polygoneigenschaften verknüpfen
trainings_join <- st_join(st_sf(sampleloc), trainingsites)

# Pixelwerte extrahieren

pixel_data <- extract(enmap_s2_lu_combined, trainings_join, bind = TRUE)
pixel_data <- st_as_sf(pixel_data)
pixel_data


# Alle relevanten Prädiktoren definieren
predictors <- c(paste0("Band", 1:207),"B05","B06", "B07", "B8A", "B11", "B12", "B04", "B03", "B02", "B08", 
                "ndvi", "ndvi_sd_3x3", "ndvi_sd_5x5", "ndvi_sd_7x7",
                "lu_Heidekraut", "lu_Pfeifengras", "lu_Torfmoos", "lu_Wollgras")

# Problematische Bänder ausschließen
predictors <- predictors[!predictors %in% paste0("Band", 119:123)]

# Geometrie entfernen
trainDat <- st_drop_geometry(pixel_data)
trainDat$Label <- as.factor(trainDat$Label)

# Räumliche Folds für Kreuzvalidierung
spatial_folds <- CreateSpacetimeFolds(trainDat, spacevar = "PolyID", class = "Label", k = 5)

ctrl <- trainControl(
  method = "cv",
  index = spatial_folds$index,
  indexOut = spatial_folds$indexOut,
  savePredictions = TRUE
)

# Modell trainieren
set.seed(123)
model_enmap_s2 <- train(
  trainDat[, predictors],
  trainDat$Label,
  method = "rf",
  importance = TRUE,
  tuneGrid = data.frame(mtry = c(2,4,6,8,10,12,14)),
  trControl = ctrl,
  ntree = 100
)

model_enmap_s2


# Variablenwichitgkeit berechnen und plotten

imp <- varImp(model_enmap_s2, scale = TRUE)

# Gesamtranking berechnen: Mittelwert über alle Klassen bilden
imp_df <- data.frame(
  Variable = rownames(imp$importance),
  Importance = rowMeans(imp$importance, na.rm = TRUE)
)

# Top 15 Prädiktoren
top_imp <- imp_df[order(-imp_df$Importance), ][1:15, ]

# Plotten
ggplot(top_imp, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(aes(fill = Importance), width = 0.7) +
  scale_fill_gradient(low = "gold", high = "blue", name = "Importance") +
  coord_flip() +
  labs(
    x = "Prädiktor",
    y = "Variablenwichtigkeit (Mittelwert über Klassen)"
  ) +
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    axis.text.y = element_text(size = 8),  # Variablen
    axis.text.x = element_text(size = 8),                   # Zahlen auf Achse
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),  # Titel der Farbskala
    legend.text = element_text(size = 8),                   # Beschriftung der Farbskala
    legend.position = "right",
    panel.grid.major.y = element_blank()
  )

# Vorhersage auf Rasterdaten
prediction <- predict(enmap_s2_lu_combined, model_enmap_s2, na.rm = TRUE)

# Landbedeckungsklassifikation plotten
cols <- c("mediumpurple3", "yellowgreen", "gold", "blue", "forestgreen")
plot(prediction, col = cols)


# Klassifikation für eine reduzierte Variablenmenge auf Basis der Variablenwichitgkeit 

# Variablenwichitgkeit extrahieren
importance_df <- varImp(model_enmap_s2)$importance

# Mittelwert über Klassen
if (ncol(importance_df) > 1) {
  importance_df$Overall <- rowMeans(importance_df)
} else {
  importance_df$Overall <- importance_df[,1]
}

# 10 % der wichtigsten Variablen auswählen
n_top <- ceiling(0.10 * nrow(importance_df))

# Variablen nach Wichtigkeit sortieren (absteigend)
importance_sorted <- importance_df[order(importance_df$Overall, decreasing = TRUE), , drop=FALSE]
top_vars <- rownames(importance_sorted)[1:n_top]

# Zusätzliche Spectral Unmixing Ergebnisse und Sentinel-Bänder als Prädiktoren hinzufügen
additional_vars <- c("lu_Heidekraut","lu_Pfeifengras","lu_Torfmoos","lu_Wollgras",
                     "B05","B06","B07","B8A","B11","B12","B04","B03","B02","B08")
final_predictors <- unique(c(top_vars, additional_vars))

# Neues Modell mit den finalen Prädiktoren trainieren
set.seed(123)
model_top10 <- train(
  trainDat[, final_predictors],
  trainDat$Label,
  method = "rf",
  importance = TRUE,
  tuneGrid = data.frame(mtry = c(2,4,6,8,10,12,14)),
  trControl = ctrl,
  ntree = 100
)

model_top10

# Vorhersage mit reduziertem Modell
prediction_top10 <- predict(enmap_s2_lu_combined, model_top10, na.rm = TRUE)

# plotten
# plot(prediction_top10, col = cols)

