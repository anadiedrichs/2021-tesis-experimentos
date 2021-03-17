library(tidyverse)
library(tidymodels)
library(here)
source(here("utils","dataset-processing.R"))

# global variables

dacc_daily_tmin <- get.dataset("dacc")$data

estaciones <- c("junin","tunuyan","agua_amarga","las_paredes","la_llave")

get_dataset_for_classification <- function(station_name)
{
  
  if(is.null(station_name) | !(station_name %in% estaciones) ) stop("station_name must be a valid name from estaciones")
  
  junin <- dacc_daily_tmin 
  # quita el junin. de los nombres de las columnas
  
  # Quiero predecir la temperatura mínima, si es o no es helada.
  
  tmin <- junin %>% 
    select( starts_with(station_name) & ends_with("temp_min")) 
  
  colnames(tmin) <- "tmin"
  
  tmin_helada <- tmin
  tmin_helada <- tmin_helada %>% mutate(tmin = case_when(
    tmin <= 0 ~ as.character("helada"),  # frost event
    TRUE ~ as.character("no-helada")   # no frost
  ))
  
  # para regresion
  # data_tmin <- cbind(junin[1:(nrow(junin)-1),],tmin[2:nrow(tmin),])
  
  data_clasificacion <- cbind(junin[1:(nrow(junin)-1),],tmin_helada[2:nrow(tmin_helada),])
  
  #Convierto a factor la columna tmin
  
  data_clasificacion$tmin <- as.factor(data_clasificacion$tmin)
  
  return(data_clasificacion)
}

data_split <- function(dataset,prop = 0.75) {
  
  base_split <- dataset %>%
    initial_split(prop=prop) # divido en prop %
  return(base_split)
}

receta_sin_smote <- function(un_split) {
  
  base_recipe <- training(un_split) %>%
    recipe(tmin~.) %>%
    step_corr(all_numeric()) %>%
    step_normalize(all_numeric(), -all_outcomes()) 
  
  base_recipe
  
}

base_training <- function(data_clasificacion){
  
  base_split <- data_split(data_clasificacion) 
  base_recipe <- receta_sin_smote(base_split)
  base_rf_spec <- rand_forest() %>% 
    set_engine("ranger") %>% 
    set_mode("classification")
  
  wf <- workflow() %>%
    add_recipe(base_recipe) %>% #agrego la receta
    add_model(base_rf_spec) #agrego el modelo
  
  final_fit <- last_fit(wf,split = base_split)
  # obtengo roc_auc
  final_fit %>% collect_metrics() %>% select(.estimate) %>% .[[1]] %>%  .[2] %>% round(4)
}


filtrar <- function(data,s,v=NULL,m=NULL){
  
  if(is.null(v) & is.null(m)) {
    dataset <- data %>% 
    select(starts_with(s) | starts_with("tmin"))
  }else if(is.null(m))
  {
    dataset <-data %>% 
      select(starts_with(s) | starts_with(v)| starts_with("tmin"))
  }else{
    dataset <-data %>% 
      select(starts_with(s) | starts_with(v) | starts_with(m) | starts_with("tmin"))
  }
  
}

for(s in estaciones){
  
  data <- get_dataset_for_classification(s)
  dataset <- filtrar(data,s)
  auc <- base_training(dataset)
  cat(s,",,",auc,"\n", file = "output.csv", append = TRUE)
  
  
  vecinos <- estaciones[!(estaciones %in% s)] # Quito s de estaciones para obtener sus vecinos
  
  for(v in vecinos){
    
    dataset <- filtrar(data,s,v)
    auc <- base_training(dataset)
    cat(s,",",v,",",auc,"\n", file = "output.csv", append = TRUE)
    
    tripletes <- vecinos[!(vecinos %in% v)] # Quito v de vecinos
    
    for(m in tripletes){
      
      dataset <- filtrar(data,s,v,m)
      auc <- base_training(dataset)
      cat(s,",",v,m,",",auc,"\n", file = "output.csv", append = TRUE)
    }
    
    vecinos <- vecinos[!(vecinos %in% v)]
    
  }
 #TODO  entrenar con todas las estaciones 
  auc <- base_training(data)
  cat(s,",","todos",",",auc,"\n", file = "output.csv", append = TRUE)
  
}
