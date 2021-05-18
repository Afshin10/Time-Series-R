library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(tidyverse)
library(data.table)


interactive <- TRUE

raw <- fread("AirPassengers (3).csv")
raw$Month <- raw$Month %>% paste0('-1') #%>% as.Date('%Y-%m-%d')

raw$Month <- raw$Month %>% as.Date('%Y-%m-%d')

raw %>% dim()

raw <- raw %>% rename(passengers=`#Passengers`,date= Month)

# Split Data 80/20
splits <- initial_time_split(raw, prop = 0.8)

#visualize
raw %>%
  plot_time_series(date, passengers, .interactive = interactive)


# Model 1: arima_boost ----
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(passengers ~ date + as.numeric(date) + factor(month(date), ordered = F),
      data = training(splits))

# Model 2: ets ----
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(passengers ~ date, data = training(splits))

# Model 3: prophet ----
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(passengers ~ date, data = training(splits))

#2. Compare RMSE scores on test set; 
#Step 3 - Add fitted models to a Model Table.----

models_tbl <- modeltime_table(
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet
)

models_tbl

#Step 4 - Calibrate the model to a testing set.----
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = raw
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, 
    .interactive      = interactive
  )
#5B - Accuracy Metrics----
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )
#RMSE=ARIMA has the lowest rmse

calibration_tbl <- model_fit_arima_boosted %>%
  modeltime_calibrate(new_data = testing(splits))


calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = raw
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, 
    .interactive      = interactive
  )


refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = raw)

refit_tbl %>%
  modeltime_forecast(h = "4 years", actual_data = raw) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, 
    .interactive      = interactive
  )
#pisss cox buz zzarafat ahah