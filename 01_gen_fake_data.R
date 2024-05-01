
#' @title Generate Fake Data for Predictor Summary
#' @description This function generates fake data for the Predictor Summary
#' @param treatments A vector of strings containing the treatment names to generate data for
#'
FakeDataForPredictorSummary <- function(treatments = c("ESC", "EBEM", "ACT", "Duloxetine")) {
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

# FakeDataForDTRAssignments generates a simulated dataset for Dynamic Treatment Regimes (DTR).
# It assigns stage 1 treatments and stage 2 actions based on predefined probabilities.
# 
# Args:
#   n_participants: An integer, the number of participants in the study (default 800).
#   stage1_treatments: A character vector specifying the treatments available in stage 1.
#   stage2_actions: A character vector specifying the possible actions in stage 2.
#   stage2_treatments: A character vector specifying the treatments available in stage 2 (not used in the function but might be used later).
#
# Returns:
#   A tibble with columns for stage1 allocation, x (randomly generated), and stage2 action allocation.
#
# Example:
#   FakeDataForDTRAssignments(100, c("A", "B", "C"), c('maintain', 'augment', 'switch'), c("X", "Y", "Z"))
#
# Needs to create arguments to control probabilites of assignment.
# 
FakeDataForDTRAssignments <- function(n_participants = 800,
                                      stage1_treatments = c("ACT", "Duloxetine", "EBEM", "ESC"), 
                                      stage2_actions = c("Maintain", "Augment", "Switch"), 
                                      stage2_treatments = c("ACT", "Duloxetine", "EBEM", "ESC"), 
                                      stage1_trt_probs = c(0.25, 0.25, 0.25, 0.25), 
                                      act_action_probs = c(0.1, 0.4, 0.5), 
                                      dulox_action_probs = c(0.1, 0.4, 0.5), 
                                      ebem_action_probs = c(0.1, 0.4, 0.5),
                                      esc_action_probs = c(0.2, 0.8, 0)){
  
  # Generate a tibble with stage 1 allocations and a random normal variable 'x'
  tibble(
    stage1_allocation = sample(stage1_treatments, size = n_participants, replace = TRUE, 
                               prob = stage1_trt_probs), 
    x = rnorm(n = n_participants)
  ) %>% 
    rowwise() %>% 
    # Assign stage 2 actions based on the stage 1 allocation and specific probabilities
    mutate(stage2_action_allocation = case_when(
      stage1_allocation == "ACT" ~ sample(stage2_actions, size = 1, replace = TRUE, 
                                          prob = act_action_probs), 
      stage1_allocation == "Duloxetine" ~ sample(stage2_actions, size = 1, replace = TRUE, 
                                                 prob = dulox_action_probs),
      stage1_allocation == "EBEM" ~ sample(stage2_actions, size = 1, replace = TRUE, 
                                           prob = ebem_action_probs),
      stage1_allocation == "ESC" ~ sample(stage2_actions, size = 1, replace = TRUE, 
                                          prob = esc_action_probs)
    ), 
    stage2_treatment_allocation = case_when(
      stage2_action_allocation == 'Maintain' ~ stage1_allocation, 
      stage2_action_allocation %in% c('Augment',
                                      "Switch") ~ sample(stage2_treatments[which(stage2_treatments != stage1_allocation)],
                                                     size = 1)
    ), 
    stage1_allocation = paste0("S1: ", stage1_allocation),
    stage2_treatment_allocation = paste0("S2: ", stage2_treatment_allocation))
}




