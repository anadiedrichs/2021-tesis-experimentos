library(tidyverse)
library(tidymodels)


dacc_daily_tmin <- read_csv("data/dacc-daily-tmin.csv", 
                            col_types = cols(X1 = col_skip(), date = col_date(format = "%Y-%m-%d")))

estaciones <- c("junin","tunuyan","agua_amarga","las_paredes","la_llave")

get_dataset_for_classification <- function(station_name)
{
  
  if(is.null(station_name) | !(station_name %in% estaciones) ) stop("station_name must be a valid name from estaciones")
  
  junin <- dacc_daily_tmin %>% 
    select(starts_with(station_name)) %>% 
    rename_with( ~ tolower(gsub(paste0(station_name,"."), "", .x, fixed = TRUE))) 
  # quita el junin. de los nombres de las columnas
  
  # Quiero predecir la temperatura mínima, si es o no es helada.
  
  tmin <- junin %>% 
    select(temp_min) 
  
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

entrenar <- function(s){}
entrenar <- function(s,v){}
entrenar <- function(s,v,m){}

for(s in estaciones){
  
  vecinos <- estaciones[!(estaciones %in% s)] # Quito s de estaciones para obtener sus vecinos
  
  entrenar(s)
  print(s)
  
  for(v in vecinos){
    entrenar(s,v)
    cat(s,v,"\n")
    
    tripletes <- vecinos[!(vecinos %in% v)] # Quito v de vecinos
    
    for(m in tripletes){
      
      entrenar(s,v,m)
      cat(s,v,m,"\n")
    }
    
    vecinos <- vecinos[!(vecinos %in% v)]
    
  }
 #TODO  entrenar con todas las estaciones 
}
