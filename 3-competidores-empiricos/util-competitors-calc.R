library(frost)
library(caret)
library(dplyr)
library(here)
source(here("utils","dataset.R"))
# global variables


# get dataset for classification 
#     ya viene dataset desfasado a un dia, todos los datos menos radiacion
#     por lo tanto llama a get.dataset("dacc")
# 
estaciones <- c("junin","tunuyan","agua_amarga","las_paredes","la_llave")

# https://gist.githubusercontent.com/charly06/91578196fc615c5a79c7174318be4349/raw/d96a98c2933f5af141eac91af83c7895062d68a5/ggrocs.R
ggrocs <- function(rocs, breaks = seq(0,1,0.1), legendTitel = "Leyenda") {
  if (length(rocs) == 0) {
    stop("No ROC objects available in param rocs.")
  } else {
    require(plyr)
    # Store all sensitivities and specifivities in a data frame
    # which an be used in ggplot
    RocVals <- plyr::ldply(names(rocs), function(rocName) {
      if(class(rocs[[rocName]]) != "roc") {
        stop("Please provide roc object from pROC package")
      }
      data.frame(
        fpr = rev(rocs[[rocName]]$specificities),
        tpr = rev(rocs[[rocName]]$sensitivities),
        names = rep(rocName, length(rocs[[rocName]]$sensitivities)),
        stringAsFactors = T
      )
    })
    
    aucAvg <- str_c(names(rocs),round(sapply(rocs, "[[", "auc"),3),"\n", sep = ' ', collapse = "")
    
    rocPlot <- ggplot(RocVals, aes(x = fpr, y = tpr, colour = names)) +
      geom_segment(aes(x = 0, y = 1, xend = 1,yend = 0), alpha = 0.5, colour = "gray") + 
      geom_step() +
      scale_x_reverse(name = "False Positive Rate (1 - Specificity)",limits = c(1,0), breaks = breaks) + 
      scale_y_continuous(name = "True Positive Rate (Sensitivity)", limits = c(0,1), breaks = breaks) +
      theme_bw() + 
      coord_equal() + 
      annotate("text", x = 0.1, y = 0.4, vjust = 0, label = paste0("AUC \n", aucAvg)) +
      guides(colour = guide_legend(legendTitel)) +
      theme(axis.ticks = element_line(color = "grey80"))
    
    rocPlot
  }
}
# train and test come from get.data.for.classification
experimento <- function(estacion)
{
  dataset <- get_dataset_for_classification(estacion)
  
  porc_train = 0.68
  until <- round(nrow(dataset)*porc_train)
  
  train_set = dataset[1:until-1, ] 
  test_set = dataset[until:nrow(dataset), ]
  
  # cálculo punto de rocío
  dw.train <- calcDewPoint(train_set$humedad_med,train_set$temp_med,mode = "C")
  dw.test <- calcDewPoint(test_set$humedad_med,test_set$temp_med,mode = "C")
  # buildMdz  
  model.mza <- buildMdz(dw=dw.train, tempMax=train_set$temp_max, tmin=train_set$temp_min)
  # espero un arreglo de valores. si da error, deberé usar sapply.
  predmza <- predMdz(dw = dw.test, tempMax = test_set$temp_max, model=model.mza)
  #plot(predmza,test_set$temp_min)
  
  model.FAO <- buildFAO(dw=dw.train,temp = train_set$temp_med,tmin=train_set$temp_min)
  # espero un arreglo de valores. si da error, deberé usar sapply.
  predfao <- predFAO(model=model.FAO,t=test_set$temp_med,dw=dw.test)
  # comparar resultados
  #plot(predfao,test_set$temp_min)
  
  set.seed(852)
  #logistic regresion
  glm.fit <- glm(tmin ~ ., data = train_set, family = binomial)
  pred.log <- predict(glm.fit,test_set)
  # random forest
  rf.fit <- ranger(tmin ~ ., data = train_set,probability = TRUE)
  pred.rf <- predict(rf.fit,test_set)
  
  
  require(pROC)
  lista <- list(buildMdz=roc(test_set$tmin,predmza),
                FAO=roc(test_set$tmin,predfao),
                RF=roc(test_set$tmin,pred.rf$predictions[,1]),
                LogReg=roc(test_set$tmin,pred.log))
  
  return(lista)
}

generar_plot <- function(lista,nombre)
{
  g <- ggrocs(lista)
  ggsave(filename = paste0(nombre,"-competidores-roc.pdf"),plot=g,device="pdf")
  g
}

#output <- experimento("junin")
#ggrocs(output)

xx <- map(estaciones,experimento)

map2(xx,estaciones,generar_plot) 



