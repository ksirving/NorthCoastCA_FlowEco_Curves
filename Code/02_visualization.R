

library(tidylog)
library(tidyverse)

## directory for figures
out.dir <- "Figures/"


## full names for labels
labels <- read.csv("input_data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(hydro.endpoints = Flow.Metric.Code)
labels[25, 1] <- "Peak Flow Magnitude (Q99, cfs)"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow Magnitude"
labels


# Upload data -------------------------------------------------------------

load(file = "ignore/Output/01_all_glms_predictions.RData")
head(DF)

## scale probability and join to fancy labels
data <- DF %>%
  rename(hydro.endpoints = Variable) %>%
  group_by(Bio, hydro.endpoints, Type) %>%
  mutate(PredictedProbabilityScaled = (predictedVals-min(predictedVals))/
                  (max(predictedVals)-min(predictedVals))) %>%
  left_join(labels, by = c("hydro.endpoints"))

head(data)

## filter just csci

all_csci <- data %>%
  filter(Bio =="CSCI")

## filter just asci

all_asci <- data %>%
  filter(Bio =="ASCI")

# Figures CSCI ------------------------------------------------------------

## define FFM to loop through
HydroEnds <- unique(data$hydro.endpoints)

HydroEnds
m=2

for(m in 1:length(HydroEnds)) {
  
  ## title of FFM
  main.title <- all_csci %>%
    ungroup() %>%
    filter(hydro.endpoints == paste(HydroEnds[m])) %>%
    select(Flow.Metric.Name) %>%
    distinct(Flow.Metric.Name)
  
  ## subset data and put in order for geom.path
  all_cscix <- subset(all_csci,hydro.endpoints == paste(HydroEnds[m]))
  all_cscix <- all_cscix[order(all_cscix$PredictedProbabilityScaled, all_cscix$DeltaH),]
  
  ## plot
  p1 <- ggplot(all_cscix, aes(x=DeltaH, y=PredictedProbabilityScaled))+
    geom_path()+
    facet_wrap(~Type, scales = "free_x") +
    scale_y_continuous(limits=c(0,1))+
    theme_minimal()+
    # theme_classic()+
    theme(text = element_text(size=15),axis.text.x = element_text(angle = 60,  vjust = 0.5, hjust=0.5),
          strip.background = element_blank(),
          strip.text.x = element_blank()) +
    labs(title = paste(main.title),
         x = "Delta H (CFS)",
         y = "Probability of Good CSCI") #+ theme_bw(base_size = 15)
  p1
  out.filename <- paste0(out.dir,"02_csci_", paste(HydroEnds[m]), ".jpg")
  ggsave(p1, file = out.filename, dpi=300, height=4, width=6, bg = "white")
  
  
}



# Figures ASCI ------------------------------------------------------------


for(m in 1:length(HydroEnds)) {
  
  ## title of FFM
  main.title <- all_asci %>%
    ungroup() %>%
    filter(hydro.endpoints == paste(HydroEnds[m])) %>%
    select(Flow.Metric.Name) %>%
    distinct(Flow.Metric.Name)
  
  ## subset data and put in order for geom.path
  all_ascix <- subset(all_asci,hydro.endpoints == paste(HydroEnds[m]))
  all_ascix <- all_ascix[order(all_ascix$PredictedProbabilityScaled, all_ascix$DeltaH),]
  
  
  q3 <- ggplot(all_ascix, aes(x=DeltaH, y=PredictedProbabilityScaled))+
    geom_path()+
    facet_wrap(~Type, scales = "free_x") +
    scale_y_continuous(limits=c(0,1))+
    theme_minimal()+
    # theme_classic()+
    theme(text = element_text(size=15),axis.text.x = element_text(angle = 60,  vjust = 0.5, hjust=0.5),
          strip.background = element_blank(),
          strip.text.x = element_blank()) +
    labs(title = paste(main.title),
         x = "Delta H",
         y = "Probability of Good ASCI") #+ theme_bw(base_size = 15)
  q3
  
  out.filename <- paste0(out.dir,"02_asci_", paste(HydroEnds[m]), ".jpg")
  ggsave(q3, file = out.filename, dpi=300, height=4, width=6, bg = "white")
  
  
}
