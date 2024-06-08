
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


#' Fake typed data using the `fabricatr` package
#'
#' @param n.subj.per.trt
FabricateTypedData <- function(n.subj.per.trt) {
    N <- 4 * n.subj.per.trt
    fake_data <- fabricatr::fabricate(N = 4*n.subj.per.trt,
  Age = round(runif(N, 18, 90)),  # Age as a continuous variable between 18 and 90
  Gender = draw_binary(N = N, prob = 0.5, outcome_labels = c("Male", "Female")),  # Gender as a binary variable
  BMI = rnorm(N, mean = 25, sd = 5),  # BMI as a continuous variable with a normal distribution
  DurationOfPain = runif(N, 0, 30),  # Duration of Pain in days as a continuous variable
  PainSeverity = draw_ordered(N = N, x = runif(N), outcome_labels = c("None", "Mild", "Moderate", "Severe")),  # Pain Severity as an ordered factor
  SelfEfficacy = rnorm(N, mean = 50, sd = 10),  # Self-efficacy as a continuous variable with a normal distribution
  BaselinePEG = runif(N, 0, 10),  # Baseline PEG score as a continuous variable between 0 and 10
  FunctionalStatus = draw_categorical(N = N, prob = c(0.3, 0.4, 0.3), outcome_labels = c("Low", "Medium", "High")),  # Functional Status as a categorical variable
  EmploymentStatus = draw_categorical(N = N, prob = c(0.6, 0.2, 0.2), outcome_labels = c("Employed", "Unemployed", "Retired")),  # Employment Status as a categorical variable
  OpioidUse = draw_binary(N = N, prob = 0.3, outcome_labels = c("No", "Yes")),  # Opioid Use as a binary variable
  PreviousSurgery = draw_binary(N = N, prob = 0.4, outcome_labels = c("No", "Yes")),  # Previous Surgery as a binary variable
  SmokingStatus = draw_categorical(N = N, prob = c(0.7, 0.2, 0.1), outcome_labels = c("Never", "Former", "Current")),  # Smoking Status as a categorical variable
  AlcoholUse = draw_categorical(N = N, prob = c(0.5, 0.3, 0.2), outcome_labels = c("None", "Moderate", "Heavy")),  # Alcohol Use as a categorical variable
  ExerciseFrequency = draw_ordered(N = N, x = runif(N), outcome_labels = c("Never", "Rarely", "Sometimes", "Often", "Always")),  # Exercise Frequency as an ordered factor
  DepressionScore = rnorm(N, mean = 5, sd = 3),  # Depression Score as a continuous variable with a normal distribution
  AnxietyScore = rnorm(N, mean = 5, sd = 3),  # Anxiety Score as a continuous variable with a normal distribution
  SleepQuality = draw_ordered(N = N, x = runif(N), outcome_labels = c("Very Poor", "Poor", "Fair", "Good", "Very Good")),  # Sleep Quality as an ordered factor
  DietQuality = draw_ordered(N = N, x = runif(N), outcome_labels = c("Very Poor", "Poor", "Fair", "Good", "Very Good")),  # Diet Quality as an ordered factor
  SocialSupport = draw_categorical(N = N, prob = c(0.3, 0.4, 0.3), outcome_labels = c("Low", "Medium", "High")),  # Social Support as a categorical variable
  PastTherapyAdherence = draw_categorical(N = N, prob = c(0.2, 0.3, 0.5), outcome_labels = c("None", "Partial", "Full")),  # Past Therapy Adherence as a categorical variable
  PainCopingStrategies = draw_ordered(N = N, x = runif(N), outcome_labels = c("None", "Some", "Moderate", "Good", "Excellent")),  # Pain Coping Strategies as an ordered factor
  PEGPain = runif(N, 0, 10),  # PEG Pain as a Likert scale item (0-10)
  PEGEnjoyment = runif(N, 0, 10),  # PEG Enjoyment as a Likert scale item (0-10)
  PEGActivity = runif(N, 0, 10),  # PEG Activity as a Likert scale item (0-10)
  PEGSum = PEGPain + PEGEnjoyment + PEGActivity  # PEG Sum as the sum of PEGPain, PEGEnjoyment, and PEGActivity
)
}

FakeDataDescriptiveAnalyses <- function(n.subj = 800) {
        treatments <- c("ACT", "EBEM", "ESC", "Duloxetine")
        treat_assignments <- rep(treatments, length.out = n.subj)[1:n.subj]

        avg_peg_weights <- c(rep(1, 3), rep(3, 3), rep(2, 3), rep(1, 2))
        peg_weights <- list(
                ACT = avg_peg_weights + rnorm(n = 11, sd = .5),
                EBEM = avg_peg_weights + rnorm(n = 11, sd = .5),
                ESC = avg_peg_weights + rnorm(n = 11, sd = .5),
                Duloxetine = avg_peg_weights + rnorm(n = 11, sd = .5)
        )

        peg_weights <- lapply(peg_weights, FUN = abs)

        avg_pgic_weights <- c(1, 2, 3, 4, 2, 2, 1)


  pgic_weights <- list(
          ACT = avg_pgic_weights + rnorm(n = 7, sd = .5),
          EBEM = avg_pgic_weights + rnorm(n = 7, sd = .5),
          ESC = avg_pgic_weights + rnorm(n = 7, sd = .5),
          Duloxetine = avg_pgic_weights + rnorm(n = 7, sd = .5)
  )

        pgic_weights <- lapply(pgic_weights, FUN = abs)

        wide_data <- tibble(
                Trt = treat_assignments,
                PEG = unlist(lapply(
                        treat_assignments,
                        function(t) sample(0:10, 1, prob = peg_weights[[t]], replace = TRUE)
                )),
                PGIC = unlist(lapply(treat_assignments, function(t) sample(1:7, 1, prob = pgic_weights[[t]], replace = TRUE)))
        ) %>%
                mutate(PEG = as.integer(PEG), PGIC = as.integer(PGIC))

        return(wide_data)
}

TransformWideToLong <- function(wide_data) {
      # Convert data to long format
      long_data <- wide_data %>%
              pivot_longer(
                      cols = c(PEG, PGIC),
                      names_to = "Outcome",
                      values_to = "Value"
              )

    return(long_data)
}

SummarizeDescriptiveOutcomes <- function(results_tbl_df, include_fivenum = TRUE) {

                                        # Create summary table
                                        grouped_tbl <- results_tbl_df %>%
                                            group_by(Outcome, Trt)
    if (include_fivenum == TRUE) {
            summary_tbl <- grouped_tbl %>%
                    summarize(
                            Mean = mean(Value),
                            SD = sd(Value),
                            Min = min(Value),
                            Q1 = quantile(Value, 0.25),
                            Median = median(Value),
                            Q3 = quantile(Value, 0.75),
                            Max = max(Value)
                    )
    } else {
                 summary_tbl <- grouped_tbl %>%
                         summarize(
                                 Mean = mean(Value),
                                 SD = sd(Value)
                         )
    }
    return(summary_tbl)
}
