
library(tidyverse)
library(sf)
library(tidylog)



# Flow data ---------------------------------------------------------------

## upload data - currently south coast - update with north coast data
dh_data <- read.csv("ignore/Input/2024-07-26_RFpred_output_alldata_chan.engCOMIDs_med_dlt_FFM_test12_test2.csv")
head(dh_data)

## pivot longer
dh_median <- dh_data %>%
  pivot_longer(d_ds_mag_50:delta_q99, names_to = "flow_metric", values_to = "deltah_final") %>%
  mutate(comid = as.character(comid))

dim(dh_median) # 13329
head(dh_median)
str(dh_median)

## full names for FFM labels
labels <- read.csv("input_data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(hydro.endpoints = Flow.Metric.Code) ## rename to match
## add Q99
labels[25, 1] <- "Magnitude of largest annual storm"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak flow"

## change all names to match dh_data
labels <- labels %>%
  mutate(flow_metric = case_when(hydro.endpoints == "DS_Mag_50" ~ "d_ds_mag_50",
                                 hydro.endpoints == "FA_Mag" ~ "d_fa_mag",
                                 hydro.endpoints == "Peak_10" ~ "d_peak_10",
                                 hydro.endpoints == "Peak_2" ~ "d_peak_2",
                                 hydro.endpoints == "Peak_5" ~ "d_peak_5",
                                 hydro.endpoints == "SP_Mag" ~ "d_sp_mag",
                                 hydro.endpoints == "Wet_BFL_Mag_10" ~ "d_wet_bfl_mag_10",
                                 hydro.endpoints == "Wet_BFL_Mag_50" ~ "d_wet_bfl_mag_50",
                                 hydro.endpoints == "Q99" ~ "delta_q99")) %>%
  drop_na(flow_metric)

## count sites with flow data

flowSites <- unique(dh_median$masterid)

length(flowSites) ## 1149


# Bio data ----------------------------------------------------------------

## CSCI data - all CA but will still need to be updated
load(file = "ignore/Input/CSCI_CA_Aug2024.RData")
bug_tax_ca <- csci

head(bug_tax_ca)

## how many flow in bug sites
sum(flowSites %in% bug_tax_ca$masterid) ## 1073

### remove reps format
csciScores <- bug_tax_ca %>%
  filter(fieldreplicate == 1 ) %>% ## taking rep number 1, can also take mean or rep at random
  mutate(Metric = "CSCI", csci = as.numeric(csci)) %>%
  rename(MetricValue = csci) %>%
  select(masterid, sampleyear, Metric, MetricValue, longitude, latitude, comid)

length(unique(csciScores$masterid)) ## 5729 sites (all ca)
head(csciScores)

## ASCI data - all CA - also needs updated
load(file = "ignore/Input/SMC_asci_cali_Mar2023_v3.RData")
head(alg_tax_ca) ## asci hybrid

asciScores <- alg_tax_ca %>%
  filter(replicate == 1) %>% ## taking rep number 1, can also take mean or rep at random
  rename(Metric = metric, MetricValue = result) %>% ## columns to match csci data
  separate(sampledate, into = c("sampleyear", "month", "day")) %>% ## get year from date
  mutate(sampleyear = as.integer(sampleyear),
         MetricValue = as.numeric(MetricValue)) %>% ## match data type to csci
  select(masterid, sampleyear, Metric, MetricValue, longitude, latitude, comid)

## check names
names(asciScores)
names(csciScores)

## join together

BioData <- bind_rows(asciScores, csciScores)
head(BioData)

# Join bio sites to flow data ---------------------------------------------

## join to gether by masterid and comid
AllData <- right_join(BioData, dh_median, by = c("masterid", "comid"), 
                      relationship = "many-to-many") %>%
  left_join(labels, by = "flow_metric") %>% ## join fancy flow metric labels
  drop_na(deltah_final) ## remove any sites with no flow value

head(AllData)

## save out
save(AllData, file = "ignore/Output/00_bugs_algae_flow_joined_by_masterid.RData")



