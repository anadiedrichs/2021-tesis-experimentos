# get dataset for classification 
#     ya viene dataset desfasado a un dia, todos los datos menos radiacion
#     por lo tanto llama a get.dataset("dacc")
# 
library(readr)
library(here
        )
estaciones <- c("junin","tunuyan","agua_amarga","las_paredes","la_llave")

get_dataset_for_regression <- function(station_name)
{
  dacc_daily_tmin <- read_csv(here("data","dacc-daily-tmin.csv"), 
                              col_types = cols(X1 = col_skip(), date = col_date(format = "%Y-%m-%d")))
  
  
  if(is.null(station_name) | !(station_name %in% estaciones) ) stop("station_name must be a valid name from estaciones")
  junin <- dacc_daily_tmin %>% 
    select(-contains("radiacion_") & starts_with(station_name)) %>% 
    rename_with( ~ tolower(gsub(paste0(station_name,"."), "", .x, fixed = TRUE))) 
  # quita el junin. de los nombres de las columnas
  
  # Quiero predecir la temperatura mínima, si es o no es helada.
  tmin <- junin %>% 
    select(temp_min) 
  colnames(tmin) <- "tmin"
  # para regresion
  data_tmin <- cbind(junin[1:(nrow(junin)-1),],tmin[2:nrow(tmin),])
  return(data_tmin)
}

get_dataset_for_classification <- function(station_name)
{ 
  dacc_daily_tmin <- read_csv(here("data","dacc-daily-tmin.csv"), 
                              col_types = cols(X1 = col_skip(), date = col_date(format = "%Y-%m-%d")))
  
  if(is.null(station_name) | !(station_name %in% estaciones) ) stop("station_name must be a valid name from estaciones")
  
  junin <- dacc_daily_tmin %>% 
    select(-contains("radiacion") & starts_with(station_name)) %>% 
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