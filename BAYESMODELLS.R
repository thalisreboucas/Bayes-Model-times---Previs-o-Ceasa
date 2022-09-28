library(readxl)
library(plotly)
library(tidymodels)
library(bayesmodels)
library(modeltime)
library(tidyverse)
library(timetk)
library(lubridate)
library(datawizard)


Dados_Ceasa_Preco <- read_excel("E:/edime/Thalis/MEU/Ceasa/Dados_Ceasa_Preco.xlsx", 
                                col_types = c("text", "text", "text", 
                                              "skip", "skip", "skip", "numeric", 
                                              "date"))

data <-  Dados_Ceasa_Preco %>% dplyr::select(ID_Produto,Data,`Preco Medio (R$)`) %>%
  set_names(c("id", "date", "value"))


df <- data %>% group_by(id) %>%
    filter(id == 24)
  

itens_abc <- c(24,7,8,27,34,44,33,16)


# Separação ----
  
splits <- df %>% initial_time_split(prop = 0.9)

# AED -----

graph <- function(id_i ){
  
  data %>%
    group_by(id) %>%
    filter(id == id_i) %>%
    plot_time_series(
      .date_var    = date,
      .value       = value
    )
  
}

graph(24)

table <- function(id_i ){
  
  data %>% filter(id == id_i) %>%  describe_distribution()
  
}

table(24)


## Teste


model_fit_arima_bayes<- sarima_reg() %>%
  set_engine(engine = "stan",tune()) %>%
  fit(value ~ date + month(date,label = T), data = training(splits))


plot(model_fit_arima_bayes$fit$models$model_1)


model_fit_garch <- garch_reg() %>% 
  set_engine("stan") %>% 
  fit(value ~ date + month(date,label = T), data = training(splits))

plot(model_fit_garch$fit$models$model_1)

model_fit_adps <- adaptive_spline() %>% 
  set_engine("stan") %>% 
  fit(value ~ date + month(date,label = T), data = training(splits))



models_tbl <- modeltime_table(
  model_fit_arima_bayes,
  model_fit_garch,
  model_fit_adps
)


calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))


calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = T
  )


calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )


refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = df)

refit_tbl %>%
  modeltime_forecast(h = "6 month", actual_data = df) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = T )
"# Bayes-Model-times---Previs-o-Ceasa" 
