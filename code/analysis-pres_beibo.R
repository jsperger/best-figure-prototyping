
library(dplyr)
library(scales)
library(ggplot2)



source("01_gen_fake_data.R")


source("101_plot_beibo.R")


# Define constants
seed_val <- 42



set.seed(seed_val)
dtr_assignments <- FakeDataForDTRAssignments()
PlotTreatSqn.BZ(dtr_assignments)



set.seed(seed_val)
pred_summary_df <- FakeDataForPredictorSummary()
PlotVarImportance.BZ(pred_summary_df)