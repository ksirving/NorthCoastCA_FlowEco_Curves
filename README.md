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

# Script: 02_visualization

Visualization of GLMs for all metrics 
makes figures for FFMs and each respopnse separatley

Ex. figure title - 02_asci_FA_Mag.jpg

# Script: 03_CSCI_relative_importance

Runs boosted regression trees an CSCI
all metrics together
tunes brts and chooses the best model
saves relative importance

output - 
models/03_rel_imp_csci_labels.csv

# Script: 04_ASCI_relative_importance

Runs boosted regression trees an ASCI
all metrics together
tunes brts and chooses the best model
saves relative importance

output - 
models/04_rel_imp_asci_labels.csv

# Script: 05_relative_importance_figure

combines csci and asci relative importance to create figure

figure: 05_rel_imp_csci_asci_bar_plot_n1.jpg
data: output_data/05_relative_imp_table.csv

