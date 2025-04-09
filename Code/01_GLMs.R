
library(tidylog)
library(tidyverse)
library(sf)

out.dir <- "Figures/"


# Upload data -------------------------------------------------------------

load(file = "ignore/Output/00_bugs_algae_flow_joined_by_masterid.RData")
head(AllData)

# Models ------------------------------------------------------------------

## bio 
biol.endpoints<- unique(na.omit(AllData$Metric))
biol.endpoints

## flow
flow.endpoints<- unique(na.omit(AllData$hydro.endpoints))
flow.endpoints

## direction (negative or positive side of curve)
direction <- c("negative", "positive")

# Thresholds for index
index.thresholds <- c(0.86, 0.79) ## can add others in here but make sure they match the index 0.86 = asci, 0.79 = csc

## make a grid of all configurations
bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints, flow.endpoints=flow.endpoints, 
                             direction = direction, stringsAsFactors = F)
i=1
bio_h_summary
## model of each configuration
log.glm <-lapply(1:nrow(bio_h_summary), function(i)
{
  ## define each item of configuration
  fmet<-as.character(bio_h_summary[i,"flow.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  dmet<-as.character(bio_h_summary[i,"direction"])

  ## filter to model config
  mydat<-AllData %>%
    filter(Metric == bmet,
           hydro.endpoints == fmet) %>%
    select(MetricValue, deltah_final, comid, masterid) %>% ## only metrics needed
    drop_na(MetricValue, deltah_final) %>%
    filter_all(all_vars(!is.infinite(.))) %>% ## remove all missing values
    distinct()
  
  ## get direction of hydro 
  if(dmet == "negative") {
    
    ## filter out only negative hydro values
    mydat <- mydat[which(mydat$deltah_final<0 ),]
    
  } else {
    
    ## filter out only negative hydro values
    mydat <- mydat[which(mydat$deltah_final>=0 ),]
    
  }

  
  ## use different threshold for each index
  if(bmet == "CSCI") {
    
    mydat$Condition<-ifelse(mydat$MetricValue < 0.79 ,0, 1) ## convert to binary
    
  } else {
    
    mydat$Condition<-ifelse(mydat$MetricValue < 0.86 ,0, 1) ## convert to binary
    
  }
  
  head(mydat)
  mydat<-mydat[order(mydat$MetricValue),] ## order by csci value
  glm(Condition~deltah_final, family=binomial(link="logit"), data=mydat) ### glm
  
  
})

## save models
save(log.glm, file = "ignore/Output/01_glms_csci_asci_all_flow_metrics.RData")

### get rsqds and pvals
for(i in 1:length(log.glm)) {
  
  mod <- summary(log.glm[[i]])
  bio_h_summary$AIC[i] <- mod$aic ##1-mod$deviance/mod$null.deviance ## mcfaddens r2
  bio_h_summary$PValue[i] <- mod$coefficients[8]
  bio_h_summary$McFaddensR2[i] <- 1-mod$deviance/mod$null.deviance
  bio_h_summary$n[i] <- mod$df[2]+1
}
## save configs and r sqds
save(bio_h_summary, file="output_data/01_glm_rsqds.RData")
write.csv(bio_h_summary, "output_data/01_glm_rsqds.csv")
bio_h_summary

csci_coefs <- bio_h_summary


# Predictions -------------------------------------------------------------


## make df of predicted values to predict on - need to be different for each temp metric

## blank df
DF <- NULL
DF <- as.data.frame(DF)

### get predictions and fitted values
for(i in 1:length(log.glm)) {
  
  ## define each item of configuration
  fmet<-as.character(bio_h_summary[i,"flow.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  dmet<-as.character(bio_h_summary[i,"direction"])

  ## filter to model config
  mydat<-AllData %>%
    filter(Metric == bmet,
           hydro.endpoints == fmet) %>%
    select(MetricValue, deltah_final, comid, masterid) %>% ## only metrics needed
    drop_na(MetricValue, deltah_final) %>%
    filter_all(all_vars(!is.infinite(.))) %>% ## remove all missing values
    distinct()
  
  ## get direction of hydro 
  if(dmet == "negative") {
    
    ## filter out only negative hydro values
    mydat <- mydat[which(mydat$deltah_final<0 ),]
    ## new data
    flowvalues <- seq(range(mydat$deltah_final)[1], range(mydat$deltah_final)[2], 0.05) ## range of values to predict on
    flowvalues <- flowvalues[flowvalues < 0]
   
  } else {
    
    ## filter out only negative hydro values
    mydat <- mydat[which(mydat$deltah_final>=0 ),]
    ## new data
    flowvalues <- seq(range(mydat$deltah_final)[1],range(mydat$deltah_final)[2], 0.05) ## range of values to predict on
    flowvalues <- flowvalues[flowvalues >= 0]
  
  }

  ## get model, predict, extract all data and categories
  mod <- log.glm[[i]]
  predictedVals <- predict.glm(mod,list(deltah_final = flowvalues),  type = "response")
  DFX <- as.data.frame(predictedVals)
  DFX$DeltaH <- flowvalues
  DFX$Bio <- bmet
  DFX$Variable <- fmet
  DFX$Type <- dmet
  # DFX$BioThreshold <- 
  # DFX$Season <- seas
  DFX$MinVal <-  range(mydat$deltah_final)[1]
  DFX$MaxVal <-  range(mydat$deltah_final)[2]
  
  DF <- bind_rows(DF, DFX)
  
}

head(DFX)

## save 
save(DF, file = "ignore/Output/01_all_glms_predictions.RData")
