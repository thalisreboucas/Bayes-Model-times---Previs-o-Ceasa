library(pacman)

pacman::p_load(readxl,# Open Data in Xl (Excel)
               plotly, # Making dynamic graphs
               tidymodels, # Model statistics
               bayesmodels, # Bayes times models 
               modeltime, # Times model classics 
               tidyverse, # All tidy for manipulation
               timetk, # Making a tables and graph's of timeseries
               lubridate, # Working if date
               datawizard, # making a spells on the datas.
               plyr,
               knitr)

# Open dataset
data <-  readxl::read_excel(
    "E:/edime/Thalis/MEU/Ceasa/Dados_Ceasa_Preco.xlsx",
    col_types = c("text", "skip", "skip",
                  "skip", "skip", "skip", "numeric",
                  "date") )
    
data <- data  %>% dplyr::select(ID_Produto, Data, `Preco Medio (R$)`) %>%
      set_names(c("id", "date", "value"))


# EDA and Graph function -----
itens_abc <- c(24,7,8,27,34,44,33,16)

EDA <- function(data,id_i){ 
 dt <- data %>%
     dplyr::group_by(id) %>%
     dplyr::filter(id == id_i)

  n <- length(plyr::count(id_i)$x)
  
  if (n > 3) {
    x = 3
  } else {
    x = 1
  }
  
graph <-  dt %>%
    timetk::plot_time_series(.facet_ncol = x,
                     .date_var    = date,
                     .value       = value)
  

table <-  dt %>%  datawizard::describe_distribution()
  
return(list(
  table <- table,
  garph <- graph
))  
  
}

eda <- EDA(data,itens_abc)

eda[[1]]
eda[[2]]


# :-)


ForCastBaYeS_Fit <- function(data,id_prod) {
  df <- data %>% 
    dplyr::group_by(id) %>%
    dplyr::filter(id == id_prod)
  
  splits <- df %>% initial_time_split(prop = 0.9)
  
  ## Making the models of timeseries
  # BAYEEESSS !!!!
  
  # Sarima: bayesmodels connects to the bayesforecast package .
  # Integrated AutoRegressive Moving Averages with Seasonality.
  model_fit_arima_bayes <- sarima_reg() %>%
    set_engine(engine = "stan") %>%
    fit(value ~ date + month(date, label = T), data = training(splits))
  
  # Garch: bayesmodels connects to the bayesforecast package.
  # volatility modeling
  model_fit_garch <- garch_reg() %>%
    set_engine("stan") %>%
    fit(value ~ date + month(date, label = T) , data = training(splits))
  
  #Adaptive Splines Surface: bayesmodels connects to the BASS package.
  model_fit_adps <- adaptive_spline() %>%
    set_engine("stan") %>%
    fit(value ~ date + month(date, label = T), data = training(splits))
  
  
  # Tables of the models
  models_tbl <- modeltime_table(model_fit_arima_bayes,
                                model_fit_garch,
                                model_fit_adps)
  
  # Calibration table
 calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = testing(splits))
  
  # Graph of the forecasting.
  graph <- calibration_tbl %>%
    modeltime_forecast(new_data    = testing(splits),
                       actual_data = df) %>%
    plot_modeltime_forecast(.legend_max_width = 25, # For mobile screens
                            .interactive      = T)
  
  # Calibration Table do see the R^2
  accuracy <- calibration_tbl %>%
    modeltime_accuracy() %>%
    table_modeltime_accuracy(.interactive = T)
  
  
  
  return(list(models_tbl,
              calibration_tbl,
              graph,
              accuracy))
  
}


laranja <- ForCastBaYeS_Fit(data,24)

# Graph
laranja[[3]]

###### Doing the Forecasting

ForCastBaYeS_4(Produto){
# Refti to do the Forecasting
refit_tbl <- Produto[[2]] %>%
  modeltime_refit(data = df)

# Forecasting Graph !!!
refit_tbl %>%
  modeltime_forecast(h = "6 month", actual_data = df) %>%
  plot_modeltime_forecast(.legend_max_width = 25, # For mobile screens
                          .interactive      = T)
}

# Done.
