library(readxl)
library(patchwork)
library(tidyverse)

source("R/models.R")



################# PREPARE DATASET #################

path= "Data/DataCleanCurrent_mid_feb.xlsx" # alte daten ohne vollständiges Luxembourg

path = "Data/DataCleanCurrent_mid_april.xlsx" # neue daten 

PrepareNewDate = T

if(PrepareNewDate){

# 1. Daten einlesen
# ---------------------------------------------------------
# Excel-Datei einlesen
df <- read_excel(path)
metainformation = read.csv("MetaInfo/MetaDataComplete.csv")
# 2. Datenbereinigung
# ---------------------------------------------------------
# Identifizieren von Standorten, die in den Daten, aber nicht in den Metainformationen vorhanden sind
zusaetzliche_flaechen <- setdiff(unique(df$Site), metainformation$Location)
print("Standorte nur in df, nicht in Metainformationen:")
print(zusaetzliche_flaechen)

# Bereinigtes Dataframe erstellen ohne die zusätzlichen Flächen
df_bereinigt <- df[!(df$Site %in% zusaetzliche_flaechen), ]
print(paste("Originale Dimensionen:", paste(dim(df), collapse=" x ")))
print(paste("Bereinigte Dimensionen:", paste(dim(df_bereinigt), collapse=" x ")))

# 3. Metainformationen hinzufügen
# ---------------------------------------------------------
# Relevante Metadaten auswählen
meta_ausgewählt <- metainformation[, c("Location", "AgeName", "Portr_AGE", 
                                       "Portr_Petr", "AgeOldest", "AgeNewest", 
                                       "Era", "District")]
print(paste("Metainformationen Dimensionen:", paste(dim(metainformation), collapse=" x ")))

# Zusammenführen von bereinigten Daten mit Metainformationen
df_mit_meta <- merge(df_bereinigt, meta_ausgewählt, 
                     by.x = "Site", by.y = "Location", 
                     all.x = TRUE)

# 4. Analyse der fehlenden Werte
# ---------------------------------------------------------
# Gesamtzahl fehlender Werte
print(paste("Gesamtzahl der NAs:", sum(is.na(df_mit_meta))))

# Detaillierte Analyse fehlender Werte pro Spalte
na_counts <- sapply(df_mit_meta, function(x) sum(is.na(x)))
na_summary <- data.frame(
  Spalte = names(na_counts),
  Anzahl_NAs = na_counts
)
print("NA-Analyse pro Spalte:")
print(na_summary[order(-na_summary$Anzahl_NAs), ])

# 5. Spalten entfernen
# ---------------------------------------------------------
# Bestimmte Spalten entfernen
spalten_zu_entfernen <- c("CaCO3_prozent", "CaCO3_class", 
                          "Geologie_SP1", "Geologie_1_5Mio")
df_mit_meta_pro <- df_mit_meta[, -which(names(df_mit_meta) %in% spalten_zu_entfernen)]

print("Entfernte Spalten:")
print(spalten_zu_entfernen)
print(paste("Finale Dimensionen:", paste(dim(df_mit_meta_pro), collapse=" x ")))

# 6. Überprüfung der finalen Daten
# ---------------------------------------------------------
# Überprüfen der fehlenden Werte im finalen Dataframe
na_counts_final <- sapply(df_mit_meta_pro, function(x) sum(is.na(x)))
na_summary_final <- data.frame(
  Spalte = names(na_counts_final),
  Anzahl_NAs = na_counts_final
)
print("NA-Analyse im finalen Dataframe:")
print(na_summary_final[order(-na_summary_final$Anzahl_NAs), ])






# 7. clean_colnames funktion
# ---------------------------------------------------------
# 


clean_colnames = function(x) {
  x = gsub(" ", "_", x)  # Leerzeichen zu Unterstrich
  x = gsub(",", "", x)   # Kommas entfernen
  x = gsub("-", "_", x)  # Bindestriche zu Unterstrich
  x = make.names(x, unique=TRUE)  # R-konforme Namen erstellen
  x
}


# 7. dummy variablen erzeugen 
# ---------------------------------------------------------
# 


dummy = dummyVars(" ~ Geologie_1_5Mio_EU_Map + Geologie_1_5Mio_BGR + Geologie_SP2_Substrat + AgeName + Portr_Petr + Era + District", data = df_mit_meta_pro)
geology_dummies = predict(dummy, df_mit_meta_pro)
colnames(geology_dummies) = clean_colnames(colnames(geology_dummies))

df_transformed <- cbind(
  df_mit_meta_pro[, c("Sand","Ton", "K_percent", "Th_ppm", "K_Th_Ratio", "TC_merged_cps")],  # numerische Spalten
  geology_dummies  # One-Hot encoded Spalten
)

df_transformed[,c("Site","Labor-Nr Bobo_ID")] = df_mit_meta_pro[,c("Site","Labor-Nr Bobo_ID")]

features = c("K_percent", "Th_ppm", "K_Th_Ratio", "TC_merged_cps", colnames(geology_dummies))
#features = c("K_percent", "Th_ppm", "K_Th_Ratio", "TC_merged_cps")
write.csv(df_transformed, "/Users/phil/Documents/Universität/MA_Philipp_Mue/data.csv")
}



#df_transformed = read.csv("/Users/phil/Documents/Universität/MA_Philipp_Mue/Data/data.csv")

#END



#################################################################
#################  GENERAL SETTINGS #################
#################################################################


target = "Ton"
#features = c("K_percent", "Th_ppm", "K_Th_Ratio", "TC_merged_cps")
Viszualization = F
titel = target

#################################################################
#################  DATASET  VIZUALIZATION #################
#################################################################

if(Viszualization){

df_long = df_transformed %>%
  pivot_longer(cols = all_of(features),
               names_to = "Feature",
               values_to = "Value")



plt = ggplot(df_long, aes(x = .data[[target]], y = Value)) +
  geom_point() +
  facet_wrap( ~ Feature, scales = "free_y") +
  ggtitle(titel)

print(plt)

print(paste("Es sind aktuell", nrow(df), "Samples vorhanden"))
}

#END

################################################################################
#################  Machine Learning RANDOM FOREST #################
################################################################################



# Leave one side out Variation

results_rf_df = data.frame()
df_obvervedVSpred = data.frame()


for(site in unique(df_transformed$Site)){
  
  
  
  #Pipeline intialisieren: 
  rf_pipeline = RandomForestPipeline_New$new(
    data = df_transformed, 
    target = target, 
    features = features
  )
  
  rf_pipeline$split(test_site = site, split_method = "LOSO") # aktuelle site aus dem training auslassen
  
  rf_pipeline$train(mtry = NULL, cv_method = "repeatedcv", cv_folds = 5, cv_repeats = 5, implementation = "standard")  # modell trainieren
  
  rf_pipeline$predict() # vorhersage auf die aktuelle site erstellen
  
  loso_results = rf_pipeline$get_loso_predictions()
  
  site_results = rf_pipeline$evaluate(return_metric = TRUE) #metricen erstellen 
  
  site_results$Site = site 
  results_rf_df = rbind(results_rf_df, site_results)
  df_obvervedVSpred = rbind(df_obvervedVSpred, loso_results)
  
}

nrow(df_obvervedVSpred)
# Ergebnisse speichern 

write.csv(results_rf_df, "Ergebnisse/RFResults_AllLfdgOSO_longTraining.csv")

write.csv(df_obvervedVSpred, "Ergebnisse/predvsobsefdgdfrved_AllLOSO_longTraining.csv")




#########################
# Spikes
#######################

results_rf_df = data.frame()
df_obvervedVSpred = data.frame()

for(site in unique(df$Site)){
  
  
  if(nrow(df[df$Site == site,]) > 5 ){
  #Pipeline intialisieren: 
  rf_pipeline = RandomForestPipeline_New$new(
    data = df, 
    target = target, 
    features = features
  )
  
  rf_pipeline$split(test_site = site, split_method = "SPIKE") # aktuelle site aus dem training auslassen, diesmal spiken
  
  rf_pipeline$train(mtry = NULL, cv_method = "cv", cv_folds = 5)  # modell trainieren
  
  rf_pipeline$predict() # vorhersage auf die aktuelle site erstellen
  
  loso_results = rf_pipeline$get_loso_predictions()
  
  site_results = rf_pipeline$evaluate(return_metric = TRUE) #metricen erstellen 
  
  site_results$Site = site 
  results_rf_df = rbind(results_rf_df, site_results)
  df_obvervedVSpred = rbind(df_obvervedVSpred, loso_results)}
  
}

write.csv(results_rf_df, "Ergebnisse/RFResults_AllSPIKE.csv")
write.csv(df_obvervedVSpred, "Ergebnisse/predvsobserved_AllSPIKE.csv")


#########################
# Classic Test und Train Split Stefan Sites 
#######################



model34data = df_transformed[
  df_transformed$Site != "LuxM_FF" &
    df_transformed$Site != "Ascheberg" &
    df_transformed$Site != "Gent_JPA" &
    df_transformed$Site != "Gent_JPB" &
    df_transformed$Site != "Neustadt W - Ruppertsberg - 20210319", 
]

dim(model34data)

rf_pipeline = RandomForestPipeline_New$new(
  data = df_transformed, 
  target = target, 
  features = features
)


rf_pipeline$check_sites()
rf_pipeline$features

rf_pipeline$split(test_size = 0.20)

rf_pipeline$train(cv_method = "cv", cv_folds = 3, cv_repeats = 3)

rf_pipeline$predict()

ressss = rf_pipeline$get_predictions_dataframe()


rf_pipeline$plot_results()

metric = rf_pipeline$evaluate(return_metric = T)

rf_pipeline$evaluate()
rf_pipeline$save_model(path = "/Users/phil/Documents/Universität/MA_Philipp_Mue/FinalResultsRF/model34.RDS")

write.csv(metric,"/Users/phil/Desktop/Model/model38/Model38metric.csv")
write.csv(ressss,"/Users/phil/Desktop/Model/model38/Model38observed_pred.csv")



#########################
# Classic Test und Train Split 20/80
#######################
# "Labor.Nr.Bobo_ID"
# "Labor-Nr Bobo_ID"

# Load the data
df = read.csv("/Users/phil/Documents/Universität/MA_Philipp_Mue/Data/data.csv")

# get all names as features
features = names(df)

# exclude names. that are not needed in feature vector 
features = features[!features %in% c("X", "Sand", "Ton", "Site", "Labor.Nr.Bobo_ID")]

# set the target 
target = "Ton"

# Initialisiere die Pipeline 
rf_pipeline = RandomForestPipeline_New$new(
  data = df, 
  target = target, 
  features = features
)

# Ein paar checks 
rf_pipeline$check_sites()
rf_pipeline$features
rf_pipeline$target

#split
rf_pipeline$split(test_site = 0.2)

#train
rf_pipeline$train(mtry = seq(1,length(features)), cv_folds = 3, cv_repeats = 3)

#pred
rf_pipeline$predict()

#evaluate 
rf_pipeline$evaluate()

# Get feature Importance etc
diagnostics = rf_pipeline$diagnose()

view(diagnostics$feature_importance$importance)
nrow(diagnostics$feature_importance$importance)

feature_importance = diagnostics$feature_importance$importance

feature_importance_sorted = data.frame(
  Feature = rownames(feature_importance),
  Importance = feature_importance[, 1]
)

dim(feature_importance)
feature_importance_sorted = feature_importance_sorted[order(-feature_importance_sorted$Importance),]
diagnostics$feature_importance[order(-diagnostics$feature_importance$importance), ]
write.csv(feature_importance_sorted, "/Users/phil/Documents/Universität/MA_Philipp_Mue/FinalResultsRF/TonModel/Plots/model38Params/feat_imp.csv")


################################################################################
#################  Machine Learning XGBootsPipeline #################
################################################################################


xg_pipeline = XGBoostPipeline_New$new(data = df_transformed,
                                      target = target,
                                      features = features)

xg_pipeline$split(test_size = 0.25)

xg_pipeline$train()

xg_pipeline$predict()

xg_pipeline$plot_results()

xg_pipeline$evaluate(return_metric =TRUE)

results_xg = xg_pipeline$loso_predictions






################################################################################
#################  Machine Learning RANDOM FOREST RANGER #################
################################################################################


# Leave one side out Variation parallel erzeugen 

rf_pipeline = RandomForestPipeline_New$new(
  data = df_transformed, 
  target = target, 
  features = features
)


## Trainieren und speichern der Ergebisse 
#----------------------------------------------------------------------------------------------------------------------
##

# loso_results = rf_pipeline$run_parallel_loso(implementation = "schnell", split_method = "LOSO")


loso_results = rf_pipeline$run_parallel_loso(
  implementation = "schnell",        
  split_method = "LOSO",             
  cv_method = "repeatedcv",
  cv_folds = 3,
  cv_repeats = 3,                  
  num_trees = 1500,                  
  importance_method = "impurity",    
  mtry = unique(round(seq(5, length(rf_pipeline$features) * 0.7, length.out = 5)))
)


## Speicherroutine
#----------------------------------------------------------------------------------------------------------------------
##


results_rf_df = do.call(rbind, lapply(names(loso_results$site_results), function(site) {
  metrics = loso_results$site_results[[site]]
  metrics$Site = site
  return(metrics)
}))

df_obvervedVSpred = loso_results$all_predictions


write.csv(results_rf_df, "/Users/phil/Documents/Universität/MA_Philipp_Mue/FinalResults/TonModel/TonMetriken.csv")
write.csv(df_obvervedVSpred, "/Users/phil/Documents/Universität/MA_Philipp_Mue/FinalResults/TonModel/TonObsVSPredNed.csv")

# Ergebnisse initialisieren
evaluation_summary <- data.frame()
feature_importance_list <- list()
best_tune_list <- list()
cv_results_list <- list()
residuals_list <- list()

for (site_name in names(loso_results$site_results)) {
  metrics <- loso_results$site_results[[site_name]]
  model <- loso_results$best_parameters[[site_name]]
  var_imp <- loso_results$variable_importance[[site_name]]
  
  # Predictions und Residuen
  site_predictions <- subset(loso_results$all_predictions, Location == site_name)
  residuals <- site_predictions$Predicted - site_predictions$Observed
  
  # Speichern
  evaluation_summary <- rbind(evaluation_summary, cbind(Site = site_name, metrics))
  feature_importance_list[[site_name]] <- var_imp
  best_tune_list[[site_name]] <- model
  residuals_list[[site_name]] <- residuals
}




saveRDS(feature_importance_list, "/Users/phil/Documents/Universität/MA_Philipp_Mue/FinalResults/TonModel/ton_feature_importance_list.rds")
saveRDS(best_tune_list, "/Users/phil/Documents/Universität/MA_Philipp_Mue/FinalResults/TonModel/ton_best_tune_list.rds")
saveRDS(residuals_list, "//Users/phil/Documents/Universität/MA_Philipp_Mue/FinalResults/TonModel/ton_residuals_list.rds")

#End Random Forest 
# ============================================================================


################################################################################
#################  Machine Learning SUPPORT VECTOR MACHINE #################
################################################################################




svm_pipeline = SVMPipeline_New$new(data = df_transformed,
                                      target = target,
                                      features = features)

svm_pipeline$split(test_size = 0.15)

svm_pipeline$train()

svm_pipeline$predict()

svm_pipeline$plot_results()

svm_pipeline$evaluate()


################################################################################
#################  Machine Learning ANN #################
################################################################################



ann_pipeline = ANNPipeline$new(data = df_transformed,
                                   target = "Ton",
                                   features = features)

ann_pipeline$split(test_size = 0.15)

ann_pipeline$train(
  hidden_layers = c(256,128,64, 32),
  epochs = 150,
  batch_size = 16,
  learning_rate = 0.0005
)

ann_pipeline$predict()

ann_pipeline$plot_results()

ann_pipeline$evaluate()






ANNPipeline





model34data = df_transformed[
  df_transformed$Site != "LuxM_FF" &
    df_transformed$Site != "Ascheberg" &
    df_transformed$Site != "Gent_JPA" &
    df_transformed$Site != "Gent_JPB" &
    df_transformed$Site != "Neustadt W - Ruppertsberg - 20210319", 
]


#spike_samples = self$test_data[sample(nrow(self$test_data), 3), ] # hier anpassen wie viele spikes wir möchten


dfr = data.frame()
for(site in c("LuxM_FF", "Ascheberg", "Gent_JPA", "Gent_JPB","Neustadt W - Ruppertsberg - 20210319" )){


  sample_rows = df_transformed[df_transformed$Site == site, ]
  sample_rows = sample_rows[sample(nrow(sample_rows), 3), ]

  dfr = rbind(dfr, sample_rows)

}
dim(model34data)
model34data = rbind(model34data, dfr)
dim(model34data)

rf_pipeline = RandomForestPipeline_New$new(
  data = model34data, 
  target = target, 
  features = features
)


rf_pipeline$check_sites()
rf_pipeline$features

rf_pipeline$split(test_size = 0.20)

rf_pipeline$train(cv_method = "cv", cv_folds = 3, cv_repeats = 3)

rf_pipeline$predict()

rf_pipeline$plot_results()

metric = rf_pipeline$evaluate(return_metric = T)

rf_pipeline$evaluate()
rf_pipeline$save_model(path = "/Users/phil/Documents/Universität/MA_Philipp_Mue/FinalResultsRF/model34Spike.RDS")

write.csv(metric,"/Users/phil/Documents/Universität/MA_Philipp_Mue/FinalResultsRF/Model34Spike.csv")




rf_pipeline_34 = RandomForestPipeline_New$new(
  data = model34data, 
  target = target, 
  features = features
)



rf_pipeline_34$load_model("/Users/phil/Documents/Universität/MA_Philipp_Mue/FinalResultsRF/model34Spike.RDS")
LuxObPred = rf_pipeline_34$predict(df_transformed[df_transformed$Site == "LuxM_FF",])
LuxMetric = rf_pipeline_34$evaluate(return_metric = T,  data_eval= df_transformed[df_transformed$Site == "LuxM_FF",])

write.csv(LuxObPred,"/Users/phil/Desktop/Model/model34/LuxObservedPredSpike.csv" )
write.csv(LuxMetric,"/Users/phil/Desktop/Model/model34/LuxMetricSpike.csv" )

ascheberg = rf_pipeline_34$predict(df_transformed[df_transformed$Site == "Ascheberg",])
aschebergMetric = rf_pipeline_34$evaluate(return_metric = T ,data_eval= df_transformed[df_transformed$Site == "Ascheberg",])

write.csv(ascheberg,"/Users/phil/Desktop/Model/model34/AschebergObservedPredSpike.csv" )
write.csv(aschebergMetric,"/Users/phil/Desktop/Model/model34/AschebergMetricSpike.csv" )



gent = rf_pipeline_34$predict(df_transformed[df_transformed$Site == "Gent_JPA" | df_transformed$Site== "Gent_JPB",])
gent_met = rf_pipeline_34$evaluate(return_metric = T,  data_eval= df_transformed[df_transformed$Site == "Gent_JPA" | df_transformed$Site== "Gent_JPB",])


write.csv(gent,"/Users/phil/Desktop/Model/model34/GentObservedPredSpike.csv" )
write.csv(gent_met,"/Users/phil/Desktop/Model/model34/GentMetricSpike.csv" )


neusta = rf_pipeline_34$predict(df_transformed[df_transformed$Site == "Neustadt W - Ruppertsberg - 20210319",])
neumetric =rf_pipeline_34$evaluate(return_metric = T, data_eval= df_transformed[df_transformed$Site == "Neustadt W - Ruppertsberg - 20210319",])

write.csv(gent,"/Users/phil/Desktop/Model/model34/NeustadtObservedPredSpike.csv" )
write.csv(gent_met,"/Users/phil/Desktop/Model/model34/NeustadtMetricSpike.csv" )

df_transformed[,c("Site","Sand", "Ton", "Th_ppm", "K_Th_Ratio","TC_merged_cps")]









df_summary <- df_transformed[, c("Site", "Sand", "Ton", "Th_ppm", "K_Th_Ratio", "TC_merged_cps", "K_percent")] %>%
  na.omit() %>%
  group_by(Site) %>%
  summarise(
    n = n(),
    Sand_mean = mean(Sand),
    Sand_sd = sd(Sand),
    Sand_min = min(Sand),
    Sand_max = max(Sand),
    
    Ton_mean = mean(Ton),
    Ton_sd = sd(Ton),
    Ton_min = min(Ton),
    Ton_max = max(Ton),
    
    Th_ppm_mean = mean(Th_ppm),
    Th_ppm_sd = sd(Th_ppm),
    Th_ppm_min = min(Th_ppm),
    Th_ppm_max = max(Th_ppm),  # ← Achtung, war vorher TTh_ppm_max!
    
    K_percent_mean = mean(K_percent),
    K_percent_sd = sd(K_percent),
    K_percent_min = min(K_percent),
    K_percent_max = max(K_percent),
    
    K_Th_Ratio_mean = mean(K_Th_Ratio),
    K_Th_Ratio_sd = sd(K_Th_Ratio),
    K_Th_Ratio_min = min(K_Th_Ratio),
    K_Th_Ratio_max = max(K_Th_Ratio),
    
    TC_merged_cps_mean = mean(TC_merged_cps),
    TC_merged_cps_sd = sd(TC_merged_cps),
    TC_merged_cps_min = min(TC_merged_cps),
    TC_merged_cps_max = max(TC_merged_cps)
  )


write_xlsx(df_summary, "/Users/phil/Desktop/SiteStats.xlsx")

tempelberg = df_transformed[df_transformed$Site == "Tempelberg", ]

tempelberg[, c("Site","Sand", "Ton", "Th_ppm", "K_Th_Ratio","TC_merged_cps", "K_percent")]

view(df_summary)
