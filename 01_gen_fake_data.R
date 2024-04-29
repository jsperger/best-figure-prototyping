
#' @title Generate Fake Data for Predictor Summary
#' @description This function generates fake data for the Predictor Summary
#' @param treatments A vector of strings containing the treatment names to generate data for
#'
FakeDataForPredictorSummary <- function(treatments = c("eSOC", "EBEM", "ACT", "Duloxetine")) {
        # Create a data frame
        test_data <- expand.grid(
                Predictor = c(
                        "Age", "Gender", "BMI", "DurationOfPain", "PainSeverity",
                        "FunctionalStatus", "EmploymentStatus", "MedicationUse",
                        "PreviousSurgery", "SmokingStatus", "AlcoholUse",
                        "ExerciseFrequency", "DepressionScore", "AnxietyScore",
                        "SleepQuality", "DietQuality", "SocialSupport",
                        "PatientSatisfaction", "TherapyAdherence", "PainCopingStrategies"
                ),
                Treatment = treatments
        )

        # Assign random values for Magnitude, and calculate uniqueness as some function of magnitude
        test_data$Magnitude <- rexp(n = nrow(test_data), rate = 1 / 5)
        test_data$Uniqueness <- runif(n = nrow(test_data), min = 0, max = 1) * test_data$Magnitude

        # Assign a sign to the magnitude (+/-) to indicate positive or negative effects
        test_data$Sign <- ifelse(runif(nrow(test_data)) > 0.5, "Positive", "Negative")

        return(test_data)
}

# Define the function to generate fake data
FakeDataForValueComparison <- function(policy_names) {
  data <- data.frame(
    TreatmentPolicy = policy_names,
    PointEstimate = runif(length(policy_names), min = 0.1, max = 1),  # Random positive reductions between 0.1 and 1
    LowerCI = NA,
    UpperCI = NA
  )

  # Generate confidence intervals around the point estimates
  data <- data %>%
    mutate(LowerCI = PointEstimate - runif(n(), min = 0.01, max = 0.1),  # Ensuring all CIs are positive
           UpperCI = PointEstimate + runif(n(), min = 0.01, max = 0.1))

  return(data)
}
