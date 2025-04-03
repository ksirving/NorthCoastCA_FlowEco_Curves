# NorthCoastCA_FlowEco_Curves

Curves for north coast cannabis project (type 1 curves)
CSCI and ASCI response to flow alteration (functional flow metrics)

all data currently April 2025 is for south coast, needs updating to north coast when available

# Script: 00_data_formatting.R

uploads, formats and joins bioassessment data with flow

outputs:
00_bugs_algae_flow_joined_by_masterid.RData

# Script: 01_GLMs.R

Automated code for all model configurations with CSCI and ASCI
all FFMs available
Note that GLMs are developed per dorection of hydro data (negative or positive) to 
depict the S curves properly where 0 delta should be the highest probabilty of achieveing a "good" bioassessment score

outputs:
01_glms_csci_asci_all_flow_metrics.RData # listed models
01_glm_rsqds.RData # coeficients
01_all_glms_predictions.RData" # predictions

# Script: 

Visualization