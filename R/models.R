library(caret)
library(xgboost) 
library(R6)
library(gridExtra)
library(randomForest)
library(Metrics)
library(moments)
library(ranger)
library(doParallel)
# library(keras).  # nur fürs ANN ein

# In disem Skript werden die verschiedenen ML
# Modelle definiert. OOP 


# Schnellere Implementation von RF um auf allen Kernen parallel zu laufen

num_cores = parallel::detectCores() - 1
registerDoParallel(cores = num_cores)
options(ranger.num.threads = num_cores) 


###################################################
# Random Forest Pipeline ####
###################################################



RandomForestPipeline_New = R6Class(
  "RandomForestPipeline",
  public = list(
    # Explizite Member-Deklaration
    data = NULL,
    train_data = NULL,
    test_data = NULL,
    model = NULL,
    predictions = NULL,
    site_list = NULL, 
    results = NULL,
    split_method = NULL,
    target = NULL,
    features = NULL,
    loso_predictions = NULL,
    original_data_size = NULL,
    cleaned_data_size = NULL,
    preprocess_info = NULL,  
    ensemble_models = NULL,  
    
    # Initialisierung
    initialize = function(data = NULL, target = NULL, features = NULL) {
      if (is.null(data) || is.null(target) || is.null(features)) {
        stop("Data, target und features müssen spezifiziert werden")
      }
      if (!all(c(target, features) %in% names(data))) {
        stop("Nicht alle spezifizierten Variablen im Datensatz vorhanden")
      }
      
      # Originalgröße speichern
      self$original_data_size = nrow(data)
      
      # NA Werte aus relevanten Spalten entfernen
      relevant_cols = c(target, features)
      complete_cases = complete.cases(data[, relevant_cols])
      cleaned_data = data[complete_cases, ]
      
      # Bereinigte Größe speichern
      self$cleaned_data_size = nrow(cleaned_data)
      
      # Bereinigungsergebnisse loggen
      self$log_info(sprintf("Datensatzbereinigung: %d von %d Zeilen behalten (%.1f%%)",
                            self$cleaned_data_size, 
                            self$original_data_size,
                            100 * self$cleaned_data_size / self$original_data_size))
      
      # Initialisierung aller Member
      self$data = cleaned_data
      self$train_data = NULL
      self$test_data = NULL
      self$model = NULL
      self$predictions = NULL
      self$site_list = unique(cleaned_data$Site)
      self$results = NULL
      self$split_method = NULL
      self$target = target
      self$features = features
      self$loso_predictions = NULL
      
      self$log_info("Neue Pipeline erzeugt.")
      self$log_info(paste("Zielvariable:", target))
      self$log_info(paste("Features:", 
                          paste(c(head(features, 5), 
                                  sprintf("... (%d weitere Features)", length(features) - 5)), 
                                collapse=", ")))
    },
    
    # Hilfsfunktion für Logging
    log_info = function(message) {
      cat(sprintf("[%s] %s\n", Sys.time(), message))
    },
    
    # Prüfen der Sites
    check_sites = function() {
      self$log_info("Aktuelle Sites im Datensatz:")
      print(self$site_list)
      invisible(self)
    },
    
    # Daten splitten
    split = function(test_site = NULL, test_size = 0.3, split_method = "random") {
      self$split_method = split_method
      
      if (split_method == "LOSO" && !is.null(test_site)) {
        if (!test_site %in% self$site_list) {
          stop("Spezifizierte Test-Site nicht im Datensatz vorhanden")
        }
        self$train_data = subset(self$data, Site != test_site)
        self$test_data = subset(self$data, Site == test_site)
        self$log_info(paste("LOSO Split durchgeführt. Test Site:", test_site))
      } else if(split_method == "SPIKE" && !is.null(test_site)){
        if (!test_site %in% self$site_list) {
          stop("Spezifizierte Test-Site nicht im Datensatz vorhanden")
        }
        self$train_data = subset(self$data, Site != test_site)
        self$test_data = subset(self$data, Site == test_site)
        
        # hier werden dem trainingsset punkte aus dem späteren testset hinzugefügt
        
        spike_samples = self$test_data[sample(nrow(self$test_data), 3), ] # hier anpassen wie viele spikes wir möchten 
        
        self$train_data = rbind(self$train_data, spike_samples)
        
        self$log_info(paste("SPIKE Split durchgeführt. Test Site:", test_site, 
                            "mit", nrow(spike_samples), "Samples zum Training hinzugefügt"))
        
  
        } else {
        set.seed(420)
        train_index = createDataPartition(self$data$Site, p = 1 - test_size, list = FALSE)
        self$train_data = self$data[train_index, ]
        self$test_data = self$data[-train_index, ]
        self$log_info(paste("Random Split durchgeführt.", 
                            "Trainset:", nrow(self$train_data),
                            "Testset:", nrow(self$test_data)))
      }
      invisible(self)
    },
    
    
    
    
    
    # Model Training
    train = function(mtry = NULL, cv_method = "repeatedcv", cv_folds = 5, cv_repeats = 5) {
      
      self$log_info(paste("training started!"))
      
      if (is.null(self$train_data)) stop("Bitte erst Daten splitten")
      
      n_features = length(self$features)
      
      # Verbesserte mtry-Auswahl
      if (is.null(mtry)) {
        mtry_grid = expand.grid(mtry = 1:min(n_features, 20))
      } else if (is.character(mtry) && mtry == "efficient") {
        # Effiziente Auswahl
        mtry_values = c(
          round(sqrt(n_features)),
          round(n_features/3),
          round(n_features/2)
        )
        mtry_grid = expand.grid(mtry = mtry_values)
      } else {
        mtry_grid = expand.grid(mtry = mtry)
      }
      
      formula = as.formula(paste(self$target, "~", paste(self$features, collapse = " + ")))
      
      ctrl_params = list(
        method = cv_method,
        number = cv_folds
      )
      if(cv_method == "repeatedcv") {
        ctrl_params$repeats = cv_repeats
      }
      
      ctrl = do.call(trainControl, ctrl_params)
      
      set.seed(420)
      self$model = train(
        formula,
        data = self$train_data,
        method = "rf",
        importance = TRUE,
        preProcess = c("center", "scale"),
        trControl = ctrl,
        tuneGrid = mtry_grid,
        na.action = "na.omit"
        # Du könntest hier ntree = 200 hinzufügen, aber das
        # wird über caret nicht direkt unterstützt (es wird über die tuneGrid gesteuert)
      )
      
      self$log_info("Modell erfolgreich trainiert")
      self$log_info(paste("Bester mtry Wert:", self$model$bestTune$mtry))
      invisible(self)
    },
    
    # Vorhersage
    
    predict = function(data_pred = NULL) {
      
      if (!is.null(data_pred)) {
        
        self$predictions = predict(self$model, data_pred[self$features])
        
        id_col = intersect(c("Labor-Nr Bobo_ID", "Labor.Nr.Bobo_ID"), names(data_pred))
        Labor_Nr_Bobo_ID = if (length(id_col) > 0) data_pred[[id_col]] else NA
        
        return(data.frame(
          Location = data_pred$Site,
          Observed = if ("Ton" %in% names(data_pred)) data_pred$Ton else NA,
          Predicted = self$predictions,
          Labor_Nr_Bobo_ID = Labor_Nr_Bobo_ID
        ))
      }
      
      # Sicherheitsabfragen (Modell und Testdaten vorhanden?)
      if (is.null(self$model)) stop("Bitte erst Modell trainieren")
      if (is.null(self$test_data)) stop("Keine Testdaten vorhanden")
      
      valid_indices = complete.cases(self$test_data[self$features])
      self$predictions = predict(self$model, self$test_data[valid_indices, ])
      
      # Loso-Prediction-Tracking
      if (self$split_method %in% c("LOSO", "SPIKE", "random")) {
        
        id_col = intersect(c("Labor-Nr Bobo_ID", "Labor.Nr.Bobo_ID"), names(self$test_data))
        Labor_Nr_Bobo_ID = if (length(id_col) > 0) self$test_data[[id_col]][valid_indices] else NA
        
        self$loso_predictions = data.frame(
          Location = self$test_data$Site[valid_indices],
          Observed = self$test_data[[self$target]][valid_indices],
          Predicted = self$predictions,
          Labor_Nr_Bobo_ID = Labor_Nr_Bobo_ID
        )
      }
      
      self$log_info("Vorhersage abgeschlossen")
      invisible(self)
    },

    # Evaluation
    evaluate = function(return_metric = FALSE, data_eval = NULL) {
      if (is.null(self$predictions)) stop("Bitte erst Vorhersagen durchführen")
      
      # Wenn manuelle Daten übergeben wurden: benutze sie
      if (!is.null(data_eval)) {
        if (!"Ton" %in% names(data_eval)) stop("Spalte 'Ton' fehlt in data_eval für Evaluation")
        obs <- data_eval$Ton
        pred <- self$predictions
      } else {
        if (is.null(self$test_data)) stop("Keine Testdaten vorhanden")
        valid_indices <- complete.cases(self$test_data[self$features])
        obs <- self$test_data[[self$target]][valid_indices]
        pred <- self$predictions
      }
      
      # RMSE berechnen (wird mehrfach verwendet)
      rmse_val <- rmse(obs, pred)
      
      self$results <- data.frame(
        RMSE = rmse_val,
        MAE = mae(obs, pred),
        R2 = summary(lm(obs ~ pred))$r.squared,
        RPD = sd(obs) / rmse_val,
        RPIQ = IQR(obs) / rmse_val,
        Bias = mean(pred - obs)
      )
      
      self$log_info("Evaluation abgeschlossen:")
      print(self$results)
      
      if (return_metric) return(self$results)
      invisible(self)
    }
    ,
    
    # Modelldiagnostik
    diagnose = function() {
      if (is.null(self$model)) stop("Kein trainiertes Modell vorhanden")
      
      diagnostics = list(
        feature_importance = varImp(self$model),
        residuals = self$predictions - self$test_data[[self$target]],
        model_summary = summary(self$model),
        cv_results = self$model$results
      )
      
      return(diagnostics)
    },
    
    # Standard Visualisierung
    plot_results = function() {
      if (is.null(self$predictions)) stop("Keine Vorhersagen vorhanden")
      
      library(ggplot2)
      library(gridExtra)  # Für die 2 Plots nebeneinander/untereinander
      
      df_plot <- data.frame(
        Observed = self$test_data[[self$target]],
        Predicted = self$predictions
      )
      
      df_plot$Residuals <- df_plot$Predicted - df_plot$Observed
      
      # Plot 1: Vorhersage vs Beobachtung
      p1 = ggplot(df_plot, aes(x = Observed, y = Predicted)) +
        geom_point(alpha = 0.6, color = "#2C3E50") +
        geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
        labs(title = "Vorhersage vs. Beobachtung",
             x = "Beobachtet",
             y = "Vorhergesagt") +
        theme_minimal()
      
      # Plot 2: Residuenplot
      p2 = ggplot(df_plot, aes(x = Predicted, y = Residuals)) +
        geom_point(alpha = 0.6, color = "#34495E") +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        labs(title = "Residuenplot",
             x = "Vorhergesagt",
             y = "Residuen") +
        theme_minimal()
      
      # Beide Plots untereinander anzeigen
      gridExtra::grid.arrange(p1, p2, nrow = 2)
      
      invisible(self)
    }
    ,
    
    # Neue Methode für standortspezifische Plots
    plot_site_results = function(site = NULL) {
      if (is.null(self$predictions)) stop("Keine Vorhersagen vorhanden")
      if (is.null(self$loso_predictions)) stop("Keine LOSO Vorhersagen vorhanden")
      
      # Wenn keine spezifische Site angegeben wurde, alle Sites plotten
      sites_to_plot = if(is.null(site)) unique(self$loso_predictions$Location) else site
      
      for(current_site in sites_to_plot) {
        site_data = subset(self$loso_predictions, Location == current_site)
        
        plot(site_data$Observed, site_data$Predicted,
             xlab = "Beobachtet", ylab = "Vorhergesagt",
             main = sprintf("Vorhersage vs. Beobachtung - Site: %s", current_site))
        abline(0, 1, col = "red")
        
        # R² zum Plot hinzufügen
        r2 = summary(lm(Predicted ~ Observed, data = site_data))$r.squared
        legend("topleft", sprintf("R² = %.3f", r2), bty = "n")
      }
      
      invisible(self)
    },
    
    
    get_predictions_dataframe = function() {
      if (is.null(self$predictions)) stop("Keine Vorhersagen vorhanden")
      df <- data.frame(
        Observed = self$test_data[[self$target]],
        Predicted = self$predictions
      )
      if ("Site" %in% names(self$test_data)) df$Site <- self$test_data$Site
      if ("Labor-Nr Bobo_ID" %in% names(self$test_data)) df$Labor_Nr_Bobo_ID <- self$test_data$`Labor-Nr Bobo_ID`
      return(df)
    },
    
    # Modell speichern
    save_model = function(path) {
      if (is.null(self$model)) stop("Kein Modell zum Speichern vorhanden")
      saveRDS(self$model, path)
      self$log_info(paste("Modell gespeichert unter:", path))
      invisible(self)
    },
    
    # Modell laden
    load_model = function(path) {
      self$model = readRDS(path)
      self$log_info(paste("Modell geladen von:", path))
      invisible(self)
    }
  )
)



#############################################################################
# XGBoostPipeline
#############################################################################

# XGBoostPipeline ####


XGBoostPipeline_New = R6Class(
  "XGBoostPipeline",
  
  # Von RandomForestPipeline erben
  inherit = RandomForestPipeline_New,
  
  public = list(
    # Nur die train-Methode überschreiben
    train = function(nrounds = c(50, 100, 200), eta = c(0.01, 0.1, 0.3), max_depth = c(3, 6, 9)) {
      if (is.null(self$train_data)) stop("Bitte erst Daten splitten")
      
      dtrain = xgb.DMatrix(
        data = as.matrix(self$train_data[, self$features]),
        label = self$train_data[[self$target]]
      )
      
      grid = expand.grid(nrounds = nrounds, eta = eta, max_depth = max_depth)
      
      best_model = NULL
      best_rmse = Inf
      best_params = NULL
      
      for (i in 1:nrow(grid)) {
        params = list(
          objective = "reg:squarederror",
          eta = grid$eta[i],
          max_depth = grid$max_depth[i]
        )
        
        model = xgb.train(params = params, data = dtrain, nrounds = grid$nrounds[i])
        pred = predict(model, dtrain)
        current_rmse = sqrt(mean((pred - self$train_data[[self$target]])^2))
        
        if (current_rmse < best_rmse) {
          best_rmse = current_rmse
          best_model = model
          best_params = list(
            nrounds = grid$nrounds[i],
            eta = grid$eta[i],
            max_depth = grid$max_depth[i]
          )
        }
      }
      
      self$model = best_model
      self$log_info("XGBoost Modell erfolgreich trainiert")
      self$log_info(paste("Beste Parameter - nrounds:", best_params$nrounds,
                          "eta:", best_params$eta,
                          "max_depth:", best_params$max_depth))
      invisible(self)
    },
    
    # Predict-Methode ebenfalls überschreiben, da XGBoost andere Prediction-Schnittstelle hat
    predict = function() {
      if (is.null(self$model)) stop("Bitte erst Modell trainieren")
      if (is.null(self$test_data)) stop("Keine Testdaten vorhanden")
      
      valid_indices = complete.cases(self$test_data[self$features])
      dtest = xgb.DMatrix(data = as.matrix(self$test_data[valid_indices, self$features]))
      self$predictions = predict(self$model, dtest)
      
      # LOSO predictions tracking erstellen
      if (self$split_method == "LOSO" || self$split_method == "SPIKE" || self$split_method == "random") {
        self$loso_predictions = data.frame(
          Location = self$test_data$Site[valid_indices],
          Observed = self$test_data[[self$target]][valid_indices],
          Predicted = self$predictions,
          Labor_Nr_Bobo_ID = self$test_data$`Labor-Nr Bobo_ID`[valid_indices]
        )
      }
      
      self$log_info("Vorhersage abgeschlossen")
      invisible(self)
    },
    
    # Diagnose-Methode überschreiben für XGBoost-spezifische Feature Importance
    diagnose = function() {
      if (is.null(self$model)) stop("Kein trainiertes Modell vorhanden")
      
      importance_matrix = xgb.importance(feature_names = self$features, model = self$model)
      
      diagnostics = list(
        feature_importance = importance_matrix,
        residuals = self$predictions - self$test_data[[self$target]],
        model = self$model
      )
      
      return(diagnostics)
    }
  )
)




#############################################################################
# SVMPipeline SUPPORT VECTOR  ####
#############################################################################

# SVMPipeline ####


# Auch hier erben wir wieder von der Klasse "RandomForestPipeline_New". Das 
# spart viel Code.. es muss nur die train predict und diganose methode überschrieben 
# werden 



SVMPipeline_New = R6Class(
  "SVMPipeline",
  
  # Von RandomForestPipeline erben
  inherit = RandomForestPipeline_New,
  
  public = list(
    # Train-Methode überschreiben
    train = function(sigma = c(0.1,0.5,1,5,10,25,50,100), 
                     C = c(0.1,0.5,1,5,10,25,50,100), 
                     cv_method = "repeatedcv", 
                     cv_folds = 5, 
                     cv_repeats = 5) {
      
      if (is.null(self$train_data)) stop("Bitte erst Daten splitten")
      
      # Grid für SVM Parameter
      myGrid_svm <- expand.grid(
        sigma = sigma,
        C = C
      )
      
      # Formel erstellen
      formula = as.formula(paste(self$target, "~", paste(self$features, collapse = " + ")))
      
      # Train Control Parameter
      ctrl_params = list(
        method = cv_method,
        number = cv_folds
      )
      if(cv_method == "repeatedcv") {
        ctrl_params$repeats = cv_repeats
      }
      
      ctrl = do.call(trainControl, ctrl_params)
      
      # Modell Training
      set.seed(420)
      self$model = train(
        formula,
        data = self$train_data,
        method = "svmRadial",
        preProcess = c("center", "scale"),
        trControl = ctrl,
        tuneGrid = myGrid_svm
      )
      
      self$log_info("SVM Modell erfolgreich trainiert")
      self$log_info(paste("Beste Parameter - sigma:", self$model$bestTune$sigma,
                          "C:", self$model$bestTune$C))
      invisible(self)
    },
    
    # Predict-Methode überschreiben (kann minimal bleiben, da caret's predict methode kompatibel ist)
    predict = function() {
      if (is.null(self$model)) stop("Bitte erst Modell trainieren")
      if (is.null(self$test_data)) stop("Keine Testdaten vorhanden")
      
      valid_indices = complete.cases(self$test_data[self$features])
      self$predictions = predict(self$model, self$test_data[valid_indices,])
      
      # LOSO predictions tracking erstellen
      if (self$split_method == "LOSO" || self$split_method == "SPIKE" || self$split_method == "random") {
        self$loso_predictions = data.frame(
          Location = self$test_data$Site[valid_indices],
          Observed = self$test_data[[self$target]][valid_indices],
          Predicted = self$predictions,
          Labor_Nr_Bobo_ID = self$test_data$`Labor-Nr Bobo_ID`[valid_indices]
        )
      }
      
      self$log_info("Vorhersage abgeschlossen")
      invisible(self)
    },
    
    # Diagnose-Methode überschreiben für SVM-spezifische Analysen
    diagnose = function() {
      if (is.null(self$model)) stop("Kein trainiertes Modell vorhanden")
      
      # Bei SVM haben wir andere Diagnostik-Möglichkeiten
      diagnostics = list(
        model_performance = self$model$results,  # Tuning-Ergebnisse
        best_tune = self$model$bestTune,        # Beste Parameter
        residuals = self$predictions - self$test_data[[self$target]],
        model = self$model
      )
      
      # Wenn verfügbar, Variable Importance hinzufügen
      if (!is.null(self$model$finalModel$coefs)) {
        diagnostics$feature_weights = abs(self$model$finalModel$coefs)
      }
      
      return(diagnostics)
    }
  )
)





ANNPipeline = R6Class(
  "ANNPipeline",
  inherit = RandomForestPipeline_New,
  public = list(
    # Einfache Parameter
    epochs = NULL,
    batch_size = NULL,
    
    # Initialisierung
    initialize = function(data = NULL, target = NULL, features = NULL,
                          epochs = 100L, batch_size = 32L) {
      # Parent initialize aufrufen
      super$initialize(data = data, target = target, features = features)
      
      # ANN Parameter setzen
      self$epochs = as.integer(epochs)
      self$batch_size = as.integer(batch_size)
      
      self$log_info("ANN Pipeline initialisiert")
    },
    
    # Training überschreiben
    train = function(hidden_layers = c(32L, 16L), 
                     learning_rate = 0.001, 
                     epochs = NULL, 
                     batch_size = NULL) {
    
      if (is.null(epochs)) epochs <- self$epochs
      if (is.null(batch_size)) batch_size <- self$batch_size
      
      
      epochs <- as.integer(if (is.null(epochs)) self$epochs else epochs)
      batch_size <- as.integer(if (is.null(batch_size)) self$batch_size else batch_size)
      hidden_layers <- as.integer(hidden_layers)
      
      if (is.null(self$train_data)) stop("Bitte erst Daten splitten")
      
      # Daten vorbereiten
      X_train = as.matrix(self$train_data[self$features])
      y_train = as.matrix(self$train_data[[self$target]])
      
      # Robuste Standardisierung
      means = colMeans(X_train, na.rm = TRUE)
      sds = apply(X_train, 2, sd, na.rm = TRUE)
      sds[sds < 0.001] = 0.001  # Vermeidet Division durch sehr kleine Zahlen
      
      # Standardisierung der Eingabedaten
      X_train = scale(X_train, center = means, scale = sds)
      
      # Standardisierung der Zielgröße
      y_mean = mean(y_train, na.rm = TRUE)
      y_sd = sd(y_train, na.rm = TRUE)
      if(y_sd < 0.001) y_sd = 0.001
      y_train = scale(y_train, center = y_mean, scale = y_sd)
      
      y_min = min(self$train_data[[self$target]], na.rm = TRUE)
      y_max = max(self$train_data[[self$target]], na.rm = TRUE)
      
      
      # NaN-Werte durch 0 ersetzen
      X_train[is.na(X_train)] = 0
      y_train[is.na(y_train)] = 0
      
      # Modell definieren
      model = keras_model_sequential()
      
      # Input Layer
      model$add(layer_dense(
        units = hidden_layers[1],
        input_shape = ncol(X_train),
        activation = "relu",
        kernel_initializer = "he_normal"
      ))
      
      # Hidden Layer
      model$add(layer_dense(
        units = hidden_layers[2],
        activation = "relu",
        kernel_initializer = "he_normal"
      ))
      
      # Output Layer
      model$add(layer_dense(
        units = 1L,
        activation = "linear",
        kernel_initializer = "he_normal"
      ))
      
      # Kompilieren
      model$compile(
        optimizer = optimizer_adam(learning_rate = learning_rate),
        loss = "mse",
        metrics = list("mae")
      )
      
      # Training
      history = model$fit(
        x = X_train,
        y = y_train,
        epochs = epochs,
        batch_size = batch_size,
        validation_split = 0.2,
        verbose = 1L
      )
      
      # Speichern des Modells und der Skalierungsparameter
      self$model = list(
        keras_model = model,
        x_means = means,
        x_sds = sds,
        y_mean = y_mean,
        y_sd = y_sd,
        y_min = y_min,  # <- neu!
        y_max = y_max   # <- neu!
      )
      
      self$log_info("Modell trainiert")
      invisible(self)
    },
    
    # Vorhersage überschreiben
    predict = function() {
      if (is.null(self$model)) stop("Bitte erst Modell trainieren")
      if (is.null(self$test_data)) stop("Keine Testdaten vorhanden")
      
      # Testdaten vorbereiten
      X_test = as.matrix(self$test_data[self$features])
      
      # Standardisierung der Testdaten
      X_test = scale(X_test, 
                     center = self$model$x_means, 
                     scale = self$model$x_sds)
      
      # NaN-Werte durch 0 ersetzen
      X_test[is.na(X_test)] = 0
      
      # Vorhersagen
      pred_scaled = self$model$keras_model$predict(X_test, verbose = 0)
      
      # Rücktransformation
      self$predictions = as.vector(pred_scaled * self$model$y_sd + self$model$y_mean)
      
      # Clipping – Vektor sicherstellen
      self$predictions = pmin(pmax(self$predictions, self$model$y_min), self$model$y_max)
      
      # LOSO predictions tracking
      if (self$split_method %in% c("LOSO", "SPIKE", "random")) {
        self$loso_predictions = data.frame(
          Location = self$test_data$Site,
          Observed = self$test_data[[self$target]],
          Predicted = as.vector(self$predictions),
          Labor_Nr_Bobo_ID = self$test_data$`Labor-Nr Bobo_ID`
        )
      }
      
      self$log_info("Vorhersage abgeschlossen")
      invisible(self)
    }
  )
)
