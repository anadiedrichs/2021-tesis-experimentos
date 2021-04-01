library(here)
source(here("utils","dataset.R"))
library(keras)
library(tidymodels)
library(purrr)
# modelo keras 



build_model <- function(training_data){
  
  model = keras_model_sequential() %>% 
    layer_dense(units = 64, activation = "relu", input_shape = ncol(training_data)-1) %>% 
    layer_dense(units = 64, activation = "relu") %>% 
    layer_dense(units = 1)
  
  model %>% 
    compile(loss = "mse",
            optimizer = "rmsprop",
            metrics = list("mae","mse"))
  
  return(model)
}

train_predict <- function(dataset,weighted=FALSE){
  
  # train and testing split 
  
  data_split <- initial_split(dataset, prop = 4/5, strata = NULL)
  training_data <- training(data_split)
  testing_data <- testing(data_split)
  
  # receta 
  rec = recipe(tmin ~ ., data = training_data) %>% 
    step_center(all_predictors()) %>% 
    step_scale(all_predictors()) %>% 
    prep()
  set.seed(7894)
  # modelo keras
  model1 = build_model(training_data)
  
  training_baked = bake(rec, training_data, composition = "matrix")
  x = subset(training_baked, select = -tmin)
  y = training_baked[,"tmin"]
  
  history1 = NULL
  
  if(weighted==TRUE)
  {
    
    pesos <- case_when(
      training_data$tmin <= 0 ~ 1,
      TRUE ~ 0.1
    )
    
    history1 = model1 %>% fit(
      x = x,
      y = y,
      batch_size = 50,
      epoch = 100,
      sample_weight = pesos
    )
    
  }else{
    
    history1 = model1 %>% fit(
      x = x,
      y = y,
      batch_size = 50,
      epoch = 100
    )
  }
  
  #------------ regresar history1
  # predicciones 
  testing_baked = bake(rec,testing_data, composition = "matrix")
  xx = subset(testing_baked, select = -tmin)
  
  y_pred = predict(model1, xx)[,1]
  
  predicted = testing_data %>% 
    mutate(tmin_pred = y_pred,
           resid = tmin-tmin_pred)
  
  return(list(historia=history1,dataset=predicted))
}

experiment <- function(station,weighted=FALSE){
  
  data <- get_dataset_for_regression_all(station)
  
  lista <- train_predict(data,weighted)
  
  filas <- cbind(station,
                 rbind(mae(lista$dataset, truth = tmin, estimate = tmin_pred),
                       rmse(lista$dataset, truth = tmin, estimate = tmin_pred),
                       rsq(lista$dataset, truth = tmin, estimate = tmin_pred)))
  
  
  # regresar fila, history, predicted, truth, estimate
  
  valor_real <- cut(lista$dataset$tmin,breaks = c(-50,0,50))
  valor_predicho <- cut(lista$dataset$tmin_pred,breaks = c(-50,0,50))
  
  table(valor_real,valor_predicho)
  
  filas = rbind(filas,c(station,"bal_accuracy","standard",bal_accuracy_vec(valor_real, valor_predicho)))
  
  filas = rbind(filas,c(station,"f_meas","standard",f_meas_vec(valor_real, valor_predicho)))
  
  filas = rbind(filas,c(station,"sensitivity","standard",sens_vec(valor_real, valor_predicho)))
  
  filas = rbind(filas,c(station,"precision","standard",precision_vec(valor_real, valor_predicho)))
  
  return(list(metrics=filas,history=lista$history,tmin=lista$dataset$tmin,tmin_pred=lista$dataset$tmin_pred))
  
}


estaciones <- c("junin","tunuyan","agua_amarga","las_paredes","la_llave")

#resultado <- experiment("las_paredes")

resultado <- map(estaciones,experiment)

result_pesos <- map(estaciones,experiment,weighted=TRUE)
