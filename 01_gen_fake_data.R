
#' @title Generate Fake Data for Predictor Summary
#' @description This function generates fake data for the Predictor Summary
#' @param treatments A vector of strings containing the treatment names to generate data for
#'
FakeDataForPredictorSummary <- function(treatments = c("eSOC", "EBEM", "ACT", "Duloxetine")) {
        # Create a data frame
        test_data <- expand.grid(
                Predictor = c(
                        "Age", "Gender", "BMI", "Duration of Pain", "Pain Severity",
                        "Self-efficacy", "Baseline PEG",
                        "Functional Status", "Employment Status", "Opioid Use",
                        "Previous Surgery", "Smoking Status", "Alcohol Use",
                        "Exercise Frequency", "Depression Score", "Anxiety Score",
                        "Sleep Quality", "Diet Quality", "Social Support",
                        "Past Therapy Adherence", "Pain Coping Strategies"
                ),
                Treatment = treatments
        )

        # Assign random values for Magnitude, and calculate uniqueness as some function of magnitude
        test_data$Magnitude <- rexp(n = nrow(test_data), rate = 5)
        test_data$Uniqueness <- runif(n = nrow(test_data), min = 0, max = 1) * test_data$Magnitude

        # Assign a sign to the magnitude (+/-) to indicate positive or negative effects
        test_data$Sign <- ifelse(runif(nrow(test_data)) > 0.5, "Positive", "Negative")

        return(test_data)
}

# Define the function to generate fake data
FakeDataForValueComparison <- function(policy_names, point_estimates = NULL, ci_half_widths = NULL) {

  provided_est_flag <- all(is.null(point_estimates), is.null(ci_half_widths)) == FALSE

  if(provided_est_flag == TRUE){
    if (!is.null(point_estimates) && is.null(ci_half_widths)) {
      stop("If point estimates are provided, CI half-widths must also be provided.")
    }

    if (length(point_estimates) != length(policy_names) || length(ci_half_widths) != length(policy_names)){
      stop("Length of point estimates and CI half-widths must match the length of policy names.")
    }
  }

  if (is.null(point_estimates)) {
    # Generate random positive reductions between 0.1 and 1 if no point estimates are provided
    point_estimates <- runif(length(policy_names), min = 0.1, max = 1)
    # Generate random CI half-widths if not provided
    ci_half_widths <- runif(length(policy_names), min = 0.05, max = 0.15)
  }

  # Calculate the LowerCI and UpperCI
  lower_cis <- point_estimates - ci_half_widths
  upper_cis <- point_estimates + ci_half_widths

  # Create a data frame
  val_df <- data.frame(
    TreatmentPolicy = policy_names,
    PointEstimate = point_estimates,
    LowerCI = lower_cis,
    UpperCI = upper_cis
  )

  return(val_df)
}

