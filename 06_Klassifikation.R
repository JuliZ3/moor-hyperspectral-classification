# Biblioteken laden 
library(terra)
library(sf)
library(hsdar)
library(CAST)
library(caret)
library(ggplot2)
        
# -----------------------------
# Projektionsproblem lösen (falls nötig für terra)
# -----------------------------
plib <- Sys.getenv("PROJ_LIB")
prj <- system.file("proj", package = "terra")[1]
Sys.setenv("PROJ_LIB" = prj)

# Arbeitsverzeichnis setzen
setwd("C:/Users/julia/Documents/Studium/6. Semester/Bachelorarbeit/Rehdener Moor Daten/")

# EnMAP-Raster einlesen
enmap_mask <- rast("predictors_ohne_weg_enmap.tif")

# Ergebnisse Spectral Unmixing einlesen
lu_stack <- rast("linear_unmixing_hsdar.tif")

# Namen vergeben
names(lu_stack) <- c("lu_Heidekraut", "lu_Pfeifengras", "lu_Torfmoos", "lu_Wollgras")


# Zusammenführen von hyperspektralen EnMAP-Bändern + Spectral Unmixing-Ergebnis
enmap_combined <- c(enmap_mask, lu_stack)
names(enmap_combined)

# EnMAP-Bänder umbenennen

num_layers <- nlyr(enmap_combined)

# Neue Namen erzeugen: Band1, Band2, ..., BandN
new_names <- paste0("Band", 1:num_layers)

# Namen im Raster setzen
names(enmap_combined) <- new_names

# die letzten Layer wieder richtig benennen
names(enmap_combined)[(num_layers - 7):num_layers] <- c("ndvi", "ndvi_sd_3x3", "ndvi_sd_5x5", "ndvi_sd_7x7",
                                                        "lu_Heidekraut", "lu_Pfeifengras", "lu_Torfmoos", "lu_Wollgras")


# Trainingsdaten einlesen
trainingsites <- st_read("Trainingsdaten - Kopie.gpkg")

# eindeutige ID für jedes Polygon
trainingsites$PolyID <- 1:nrow(trainingsites)

# Zufallsstichprobe innerhalb der Polygone (5000 Punkte)
set.seed(5)
sampleloc <- st_sample(trainingsites, size = 5000)

# Punkte mit Polygoneigenschaften verknüpfen
trainings_join <- st_join(st_sf(sampleloc), trainingsites)


# Pixelwerte extrahieren

pixel_data <- extract(enmap_combined, trainings_join, bind = TRUE)
pixel_data <- st_as_sf(pixel_data)
pixel_data

# Zwischenergebnis abspeichern
# st_write(pixel_data, "pixel_data.gpkg", append=FALSE)


# Modelltraining mit Random Forest

# Alle relevanten Prädiktoren definieren
predictors <- c(paste0("Band", 1:207), 
                "ndvi", "ndvi_sd_3x3", "ndvi_sd_5x5", "ndvi_sd_7x7",
                "lu_Heidekraut", "lu_Pfeifengras", "lu_Torfmoos", "lu_Wollgras")

# Problematische Bänder ausschließen
predictors <- predictors[!predictors %in% paste0("Band", 119:123)]

# Geometrie entfernen
trainDat <- st_drop_geometry(pixel_data)

# Label als Faktor definieren
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
model_lu <- train(
  trainDat[, predictors],
  trainDat$Label,
  method = "rf",
  importance = TRUE,
  tuneGrid = data.frame(mtry = c(2,4,6,8,10,12,14)),
  trControl = ctrl,
  ntree = 100
)

model_lu


# Variablenwichitgkeit berechnen und plotten

# Variable Importance berechnen
imp <- varImp(model_lu, scale = TRUE)

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


# Konfusionsmatrix berechnen (Validierung)

# Beste mtry aus dem Modell
best_mtry <- model_lu$bestTune$mtry

# Vorhersagen aus den CV-Ergebnissen nur für bestes mtry filtern
pred_best <- model_lu$pred[model_lu$pred$mtry == best_mtry, ]

# Konfusionsmatrix berechnen
conf_mat <- confusionMatrix(pred_best$pred, pred_best$obs)
conf_mat


# Vorhersage auf Rasterdaten
prediction <- predict(enmap_combined, model_lu, na.rm = TRUE)

# Landbedeckungsklassifikation plotten

cols <- c("mediumpurple3", "yellowgreen", "gold", "blue", "forestgreen")

plot(prediction, col = cols, main = "Random Forest mit lu – Rehdener Geestmoor")

# Ergebnis abspeichern
# writeRaster(prediction, "prediction_RF_lu.tif", overwrite = TRUE)


# Klassifikation für eine reduzierte Variablenmenge auf Basis der Variablenwichitgkeit 

# Variablenwichitgkeit extrahieren
importance_df <- varImp(model_lu)$importance

# Mittelwert über Klassen
if (ncol(importance_df) > 1) {
  importance_df$Overall <- rowMeans(importance_df)
} else {
  importance_df$Overall <- importance_df[,1]
}

# 10 % der wichtigsten Variablen auswählen (Code ist für 10 %, kann aber schnell hier angepasst werden für 40 % und 20 %)
n_vars <- ceiling(0.1 * nrow(importance_df))

# Variablen nach Wichtigkeit sortieren (absteigend)
importance_sorted <- importance_df[order(importance_df$Overall, decreasing = TRUE), , drop=FALSE]
top_vars <- rownames(importance_sorted)[1:n_vars]

# Spectral Unmixing Ergebnisse als Prädiktoren hinzufügen
additional_vars <- c("lu_Heidekraut", "lu_Pfeifengras", "lu_Torfmoos", "lu_Wollgras")
final_predictors <- unique(c(top_vars, additional_vars))


# Neues Modell mit den finalen Prädiktoren trainieren
set.seed(123)
model_top10_plus <- train(
  trainDat[, final_predictors],
  trainDat$Label,
  method = "rf",
  importance = TRUE,
  tuneGrid = data.frame(mtry = c(2,4,6,8,10,12,14)),
  trControl = ctrl,
  ntree = 100
)

model_top10_plus

# Vorhersage mit reduziertem Modell
prediction_10 <- predict(enmap_combined, model_top10_plus, na.rm = TRUE)

# plotten
plot(prediction_10, col = cols)

