# =========================================================
# MACHINE LEARNING CLIMATE FORECASTING
# =========================================================

#' Machine Learning Forecasting for Climate Variables
#'
#' Uses machine learning algorithms to forecast
#' climate variables from simulated climate data.
#'
#' Supported methods:
#' \itemize{
#'   \item Random Forest ("rf")
#'   \item Linear Regression ("lm")
#'   \item Gradient Boosting ("gbm")
#'   \item ARIMA Time-Series ("arima")
#'   \item Extreme Gradient Boosting ("xgboost")
#'   \item Neural Network ("nnet")
#'   \item Run All Models ("all")
#' }
#'
#' @param climate_data Climate dataframe.
#'
#' @param target Variable to forecast.
#'
#' @param predictors Predictor variables.
#'
#' @param forecast_horizon Number of future periods.
#'
#' @param start_forecast Optional forecast start date.
#' Must be coercible to Date.
#'
#' @param end_forecast Optional forecast end date.
#' Must be coercible to Date.
#'
#' If supplied, forecast_horizon is
#' automatically calculated.
#'
#' @param method Forecasting method.
#'
#' @param frequency Forecast interval.
#' Options are:
#' \code{"day"},
#' \code{"month"},
#' \code{"year"}.
#'
#' @param train_fraction Fraction of data for training.
#'
#' @param include_lag Logical. Include lag predictor.
#'
#' @param lag_period Lag size.
#'
#' @param ntree Number of trees for RF and GBM.
#'
#' @param hidden_nodes Number of hidden nodes for neural network.
#'
#' @param digits Decimal places.
#'
#' @param seed Random seed for reproducibility.
#'
#' @return A list containing:
#'
#' \describe{
#'
#' \item{forecast_data}{
#' Forecasted future values.
#' }
#'
#' \item{model_performance}{
#' RMSE, MAE, and correlation.
#' }
#'
#' \item{importance}{
#' Variable importance table.
#' }
#'
#' \item{model}{
#' Trained ML model.
#' }
#'
#' \item{all_results}{
#' Returned only when method = "all".
#' }
#'
#' }
#'
#' @export

forecasting_ml <- function(
    climate_data,
    target = "Rainfall",
    predictors = c(
      "Tmin",
      "Tmax",
      "Avg.Temp",
      "RH",
      "WindSpeed",
      "Solar_Radiation",
      "ET0"
    ),
    forecast_horizon = 12,

    start_forecast = NULL,
    end_forecast = NULL,

    method = c(
      "rf",
      "lm",
      "gbm",
      "arima",
      "xgboost",
      "nnet",
      "all"
    ),
    frequency = c(
      "month",
      "day",
      "year"
    ),
    train_fraction = 0.8,
    include_lag = TRUE,
    lag_period = 1,
    ntree = 500,
    hidden_nodes = 5,
    digits = 2,
    seed = 123
){

  # -------------------------------------------------------
  # Match arguments
  # -------------------------------------------------------
  method <- match.arg(method)

  frequency <- match.arg(frequency)

  # -------------------------------------------------------
  # Run all models
  # -------------------------------------------------------
  if(method == "all"){

    methods_to_run <- c(
      "rf",
      "lm",
      "gbm",
      "arima",
      "xgboost",
      "nnet"
    )

    all_results <- lapply(
      methods_to_run,
      function(m){

        forecasting_ml(
          climate_data = climate_data,
          target = target,
          predictors = predictors,
          forecast_horizon = forecast_horizon,
          start_forecast = start_forecast,
          end_forecast = end_forecast,
          method = m,
          frequency = frequency,
          train_fraction = train_fraction,
          include_lag = include_lag,
          lag_period = lag_period,
          ntree = ntree,
          hidden_nodes = hidden_nodes,
          digits = digits,
          seed = seed
        )

      }
    )

    names(all_results) <- methods_to_run

    performance_table <- do.call(
      rbind,
      lapply(
        all_results,
        function(x){
          x$model_performance
        }
      )
    )

    best_model <- performance_table$Method[
      which.min(performance_table$RMSE)
    ]

    cat(
      "All forecasting models completed.\n",
      "Best model based on RMSE:",
      best_model,
      "\n"
    )

    return(
      list(
        best_model = best_model,
        model_performance = performance_table,
        all_results = all_results
      )
    )

  }

  # -------------------------------------------------------
  # Input checks
  # -------------------------------------------------------
  if(!is.data.frame(climate_data)){
    stop("climate_data must be a data.frame.")
  }

  if(!(target %in% names(climate_data))){
    stop("Target variable not found.")
  }

  # -------------------------------------------------------
  # Required packages
  # -------------------------------------------------------
  required_packages <- c()

  if(method == "rf"){
    required_packages <- "randomForest"
  }

  if(method == "gbm"){
    required_packages <- "gbm"
  }

  if(method == "xgboost"){
    required_packages <- "xgboost"
  }

  if(method == "arima"){
    required_packages <- "forecast"
  }

  if(method == "nnet"){
    required_packages <- "nnet"
  }

  # -------------------------------------------------------
  # Check required packages
  # -------------------------------------------------------
  for(pkg in required_packages){
    if(!requireNamespace(pkg, quietly = TRUE)){
      stop(
        paste("Package", pkg, "is required but not installed.")
      )
    }
  }

  # -------------------------------------------------------
  # Reproducibility
  # -------------------------------------------------------
  set.seed(seed)

  # -------------------------------------------------------
  # Working dataset
  # -------------------------------------------------------
  df <- climate_data

  # -------------------------------------------------------
  # DATE check
  # -------------------------------------------------------
  if(!("DATE" %in% names(df))){
    stop("DATE column not found in climate_data.")
  }

  df$DATE <- as.Date(df$DATE)

  # -------------------------------------------------------
  # Forecast year validation
  # -------------------------------------------------------
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  if(!is.null(start_forecast)){
    start_forecast <- as.Date(start_forecast)
    start_year <- as.numeric(format(start_forecast, "%Y"))
    if(start_year < current_year){
      stop(
        paste(
          "start_forecast year must be",
          current_year,
          "or later."
        )
      )
    }
  }
  if(!is.null(end_forecast)){
    end_forecast <- as.Date(end_forecast)
    end_year <- as.numeric(format(end_forecast, "%Y"))
    if(end_year < current_year){
      stop(
        paste(
          "end_forecast year must be",
          current_year,
          "or later."
        )
      )
    }
  }
  # -------------------------------------------------------
  # Automatic forecast horizon
  # -------------------------------------------------------
  if(
    !is.null(start_forecast) &&
    !is.null(end_forecast)
  ){

    start_forecast <- as.Date(start_forecast)

    end_forecast <- as.Date(end_forecast)

    if(end_forecast <= start_forecast){

      stop(
        "end_forecast must be greater than start_forecast."
      )

    }

    forecast_horizon <- switch(

      frequency,

      day = as.numeric(
        difftime(
          end_forecast,
          start_forecast,
          units = "days"
        )
      ),

      month = {

        (
          as.numeric(
            format(end_forecast, "%Y")
          ) -
            as.numeric(
              format(start_forecast, "%Y")
            )
        ) * 12 +

          (
            as.numeric(
              format(end_forecast, "%m")
            ) -
              as.numeric(
                format(start_forecast, "%m")
              )
          )

      },

      year = {

        as.numeric(
          format(end_forecast, "%Y")
        ) -

          as.numeric(
            format(start_forecast, "%Y")
          )

      }

    )

    forecast_horizon <- forecast_horizon + 1

  }

  # -------------------------------------------------------
  # Month creation
  # -------------------------------------------------------
  if(!("Month" %in% names(df))){

    df$Month <- as.numeric(
      format(df$DATE, "%m")
    )

  }

  # -------------------------------------------------------
  # Lag feature
  # -------------------------------------------------------
  if(include_lag){

    lag_name <- paste0(
      target,
      "_Lag"
    )

    df[[lag_name]] <- c(
      rep(NA, lag_period),
      head(
        df[[target]],
        -lag_period
      )
    )

    predictors <- c(
      predictors,
      lag_name
    )

  }

  # -------------------------------------------------------
  # Seasonal cyclical features
  # -------------------------------------------------------
  df$Month_Sin <- sin(
    2 * pi * df$Month / 12
  )

  df$Month_Cos <- cos(
    2 * pi * df$Month / 12
  )

  predictors <- unique(
    c(
      predictors,
      "Month_Sin",
      "Month_Cos"
    )
  )

  # -------------------------------------------------------
  # Predictor checks
  # -------------------------------------------------------
  missing_predictors <- predictors[
    !(predictors %in% names(df))
  ]

  if(length(missing_predictors) > 0){

    stop(
      paste(
        "Missing predictors:",
        paste(
          missing_predictors,
          collapse = ", "
        )
      )
    )

  }

  # -------------------------------------------------------
  # Remove incomplete rows
  # -------------------------------------------------------
  df <- df[complete.cases(df[, c(target, predictors)]), ]
  # -------------------------------------------------------
  # Minimum observations
  # -------------------------------------------------------
  if(nrow(df) < 30){
    stop("Insufficient observations for forecasting.")
  }
  # -------------------------------------------------------
  # Train-test split
  # -------------------------------------------------------
  n <- nrow(df)
  train_size <- floor(train_fraction * n)
  train_data <- df[1:train_size, ]
  test_data <- df[(train_size + 1):n, ]
  # -------------------------------------------------------
  # Formula
  # -------------------------------------------------------
  formula_ml <- as.formula(
    paste(
      target,
      "~",
      paste(
        predictors,
        collapse = " + "
      )
    )
  )
  # -------------------------------------------------------
  # Train models
  # -------------------------------------------------------
  if(method == "rf"){
    model <- randomForest::randomForest(
      formula_ml,
      data = train_data,
      ntree = ntree,
      importance = TRUE
    )
  }

  if(method == "lm"){
    model <- stats::lm(
      formula_ml,
      data = train_data
    )
  }

  if(method == "gbm"){
    model <- gbm::gbm(
      formula = formula_ml,
      data = train_data,
      distribution = "gaussian",
      n.trees = ntree,
      interaction.depth = 4,
      shrinkage = 0.01,
      cv.folds = 5,
      verbose = FALSE
    )
  }

  if(method == "xgboost"){
    x_train <- as.matrix(
      train_data[, predictors]
    )
    y_train <- train_data[[target]]
    model <- xgboost::xgboost(
      data = x_train,
      label = y_train,
      nrounds = 200,
      objective = "reg:squarederror",
      verbose = 0
    )
  }

  if(method == "nnet"){
    train_x <- scale(
      train_data[, predictors]
    )
    test_x <- scale(
      test_data[, predictors],
      center = attr(
        train_x,
        "scaled:center"
      ),
      scale = attr(
        train_x,
        "scaled:scale"
      )
    )
    target_mean <- mean(
      train_data[[target]],
      na.rm = TRUE
    )
    target_sd <- sd(
      train_data[[target]],
      na.rm = TRUE
    )
    train_y <- (
      train_data[[target]] -
        target_mean
    ) / target_sd
    train_nn <- data.frame(
      train_x,
      Target = train_y
    )
    formula_nn <- as.formula(
      paste(
        "Target ~",
        paste(
          colnames(train_x),
          collapse = " + "
        )
      )
    )
    model <- nnet::nnet(
      formula_nn,
      data = train_nn,
      size = hidden_nodes,
      linout = TRUE,
      trace = FALSE,
      maxit = 1000
    )
  }

  if(method == "arima"){
    ts_frequency <- switch(
      frequency,
      day = 365,
      month = 12,
      year = 1
    )
    ts_data <- ts(
      train_data[[target]],
      frequency = ts_frequency
    )
    model <- forecast::auto.arima(ts_data)
  }
  # -------------------------------------------------------
  # Predictions
  # -------------------------------------------------------
  if(method %in% c("rf", "lm")){
    preds <- predict(model, newdata = test_data)
  }
  if(method == "gbm"){
    preds <- predict(
      model,
      newdata = test_data,
      n.trees = ntree
    )
  }
  if(method == "xgboost"){
    preds <- predict(
      model,
      as.matrix(
        test_data[, predictors]
      )
    )
  }
  if(method == "nnet"){
    preds_scaled <- predict(
      model,
      newdata = as.data.frame(test_x)
    )
    preds <- (
      preds_scaled * target_sd
    ) + target_mean
  }
  if(method == "arima"){
    preds <- forecast::forecast(
      model,
      h = nrow(test_data)
    )$mean
  }
  actual <- test_data[[target]]
  # -------------------------------------------------------
  # Performance metrics
  # -------------------------------------------------------
  rmse <- sqrt(
    mean(
      (actual - preds)^2,
      na.rm = TRUE
    )
  )
  mae <- mean(
    abs(actual - preds),
    na.rm = TRUE
  )
  if(
    sd(preds, na.rm = TRUE) == 0 ||
    sd(actual, na.rm = TRUE) == 0
  ){
    cor_val <- NA
  } else {
    cor_val <- cor(
      actual,
      preds,
      use = "complete.obs"
    )
  }
  performance <- data.frame(
    Method = method,
    Target = target,
    RMSE = round(rmse, digits),
    MAE = round(mae, digits),
    Correlation = round(
      cor_val,
      digits
    )
  )
  # -------------------------------------------------------
  # Variable importance
  # -------------------------------------------------------
  importance_table <- NULL
  if(method == "rf"){
    imp <- randomForest::importance(model)
    importance_table <- data.frame(
      Variable = rownames(imp),
      Importance = round(
        imp[, 1],
        digits
      )
    )
  }

  if(method == "gbm"){
    imp <- summary(
      model,
      plotit = FALSE
    )
    importance_table <- data.frame(
      Variable = imp$var,
      Importance = round(
        imp$rel.inf,
        digits
      )
    )
  }

  if(method == "xgboost"){
    imp <- xgboost::xgb.importance(
      feature_names = predictors,
      model = model
    )
    importance_table <- data.frame(
      Variable = imp$Feature,
      Importance = round(
        imp$Gain,
        digits
      )
    )
  }

  if(method == "arima"){
    importance_table <- data.frame(
      Variable = "Time-Series Component",
      Importance = NA
    )
  }

  if(!is.null(importance_table)){
    rownames(importance_table) <- NULL
  }

  # -------------------------------------------------------
  # Future forecasting
  # -------------------------------------------------------
  last_rows <- tail(df, forecast_horizon)
  if(method %in% c("rf", "lm")){
    future_preds <- predict(
      model,
      newdata = last_rows
    )
  }
  if(method == "gbm"){
    future_preds <- predict(
      model,
      newdata = last_rows,
      n.trees = ntree
    )
  }
  if(method == "xgboost"){
    future_preds <- predict(
      model,
      as.matrix(
        last_rows[, predictors]
      )
    )
  }
  if(method == "nnet"){
    future_x <- scale(
      last_rows[, predictors],
      center = attr(
        train_x,
        "scaled:center"
      ),
      scale = attr(
        train_x,
        "scaled:scale"
      )
    )
    future_scaled <- predict(
      model,
      newdata = as.data.frame(
        future_x
      )
    )
    future_preds <- (
      future_scaled * target_sd
    ) + target_mean

  }
  if(method == "arima"){
    future_preds <- forecast::forecast(
      model,
      h = forecast_horizon
    )$mean
  }
  # -------------------------------------------------------
  # Future dates
  # -------------------------------------------------------
  if(!is.null(start_forecast) && !is.null(end_forecast)){
    future_dates <- seq(
      from = start_forecast,
      by = frequency,
      length.out = forecast_horizon
    )
  } else {
    future_dates <- seq(
      from = max(df$DATE),
      by = frequency,
      length.out = forecast_horizon + 1
    )[-1]
  }
  # -------------------------------------------------------
  # Forecast dataframe
  # -------------------------------------------------------
  forecast_data <- data.frame(
    DATE = future_dates,
    Forecast = round(
      as.numeric(future_preds),
      digits
    )
  )
  # -------------------------------------------------------
  # Console message
  # -------------------------------------------------------
  cat(
    "Machine learning forecasting complete using",
    method,
    "method.\n"
  )
  # -------------------------------------------------------
  # Return output
  # -------------------------------------------------------
  return(
    list(
      forecast_data = forecast_data,
      model_performance = performance,
      importance = importance_table,
      model = model
    )
  )
}
