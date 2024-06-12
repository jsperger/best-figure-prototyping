
library(dplyr)
library(scales)
library(ggplot2)

source("00_aesthetic_utils.R")
source("01_gen_fake_data.R")
source("02_plot_predictors.R")
source("04_plot_dtr_assignments.R")

# Define constants
seed_val <- 42





set.seed(seed_val)
dtr_assignments <- FakeDataForDTRAssignments()
PlotTreatSqn.BZ(dtr_assignments) %>% ThemePlot(.)



set.seed(seed_val)
pred_summary_df <- FakeDataForPredictorSummary()
PlotVarImportance.BZ(pred_summary_df) %>% ThemePlot(.)