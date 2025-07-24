## ----------------------------------------
## Plotting Utility Functions
## ----------------------------------------

#' Apply standard plot theme and colors
#'
#'
ThemeBESTPlot <- function(
  plot_obj,
  scale_colors = .ColorBlindPalette()
) {
  themed_plot <- plot_obj +
    ggplot2::scale_color_manual(values = scale_colors) +
    ggplot2::theme_minimal()
  return(themed_plot)
}

#' Colorblind friendly palette hex codes
.ColorBlindPalette <- function() {
  cbp <- c(
    "#999999",
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7"
  )

  return(cbp)
}

.CreateLabels <- function(variable_names) {
  labels <- gsub("^v_0_baseline_", "Baseline ", variable_names)
  labels <- gsub("^v_1_", "Week 12 ", labels)
  labels <- gsub("^v_2_", "Week 24 ", labels)
  labels <- gsub("^ph_1_wk_6_", "Week 6 ", labels)
  labels <- gsub("^ph_2_wk_18_", "Week 18 ", labels)
  labels <- gsub("^ph_4_wk_36_", "Week 36 ", labels)
  labels <- gsub("imprv_v_1_", "Week 12 Improvement in ", labels)
  labels <- gsub("imprv_v_2_", "Week 24 Improvement in ", labels)
  labels <- gsub("^wk_36_", "Week 36 ", labels)
  labels <- gsub("^d6wf1", "6 Week", labels)
  labels <- gsub("^d18wf1", "18 Week", labels)
  labels <- gsub("trt_", "Treatment ", labels)
  labels <- gsub("_x_", "_\\*_", labels)
  labels <- gsub("([a-z])([A-Z])", "\\1 \\2", labels)
  labels <- gsub("([A-Za-z])([0-9])", "\\1 \\2", labels)
  labels <- gsub("_", " ", labels)
  labels <- gsub("^([a-z])", "\\U\\1", labels, perl = TRUE) # Capitalize the first letter of each label
  labels <- gsub("(\\s)([a-z])", "\\1\\U\\2", labels, perl = TRUE) # Capitalize the first letter after the spaces
  labels <- gsub("Fabq", "FABQ", labels)
  labels <- gsub("Ksb", "KSB", labels)
  labels <- gsub("Odi", "ODI", labels)
  labels <- gsub("Peg", "PEG", labels)
  labels <- gsub("Pgic", "PGIC", labels)
  labels <- gsub("Pcs", "PCS", labels)
  labels <- gsub("Pain Gt 5", "cLBP for More than 5 Years", labels)
  labels <- gsub("Bmi", "BMI", labels)
  labels <- gsub("Spi ", "SPI ", labels)
  labels <- gsub("Phq", "PHQ", labels)
  labels <- gsub("Gad", "GAD", labels)
  labels <- gsub("Tx", "TX", labels)
  labels <- gsub(" Oc", " Outcome", labels)
  labels <- gsub(" Mot", " Motor", labels)
  labels <- gsub("Thc ", "THC ", labels)
  labels <- gsub("Yn", "Y/N", labels)
  labels <- gsub("Ssrisnri", "SSRIs/SNRIs ", labels)
  labels <- gsub("Lbp", "LBP", labels)
  labels <- gsub("Nsaid", "NSAID", labels)
  labels <- gsub("Prmanxt", "PROMIS Anxiety T-Score", labels)
  labels <- gsub("Prmanxr", "PROMIS Anxiety Raw Score", labels)
  labels <- gsub("Prmanx", "PROMIS Anxiety", labels)
  labels <- gsub("Prmdept", "PROMIS Depression T-Score", labels)
  labels <- gsub("Prmdepr", "PROMIS Depression Raw Score", labels)
  labels <- gsub("Prmdep", "PROMIS Depression", labels)
  labels <- gsub("Prmphft", "PROMIS Physical Function T-Score", labels)
  labels <- gsub("Prmphfr", "PROMIS Physical Function Raw Score", labels)
  labels <- gsub("Prmphf", "PROMIS Physical Function", labels)
  labels <- gsub("Prmpit", "PROMIS Pain Interference T-Score", labels)
  labels <- gsub("Prmpir", "PROMIS Pain Interference Raw Score", labels)
  labels <- gsub("Prmpi", "PROMIS Pain Interference", labels)
  labels <- gsub("Prmsdt", "PROMIS Sleep Disturbance T-Score", labels)
  labels <- gsub("Prmsdr", "PROMIS Sleep Disturbance Raw Score", labels)
  labels <- gsub("Prmsd", "PROMIS Sleep Disturbance", labels)
  labels <- gsub(
    "Prsr 4ar",
    "PROMIS Participation in Social Roles/Activities Raw Score",
    labels
  )
  labels <- gsub(
    "Prsr 4at",
    "PROMIS Participation in Social Roles/Activities T-Score",
    labels
  )
  labels <- gsub(
    "Prsr 4a",
    "PROMIS Participation in Social Roles/Activities",
    labels
  )
  labels <- gsub("Prcf 2at", "PROMIS Cognitive Function T-Score", labels)
  labels <- gsub("Prcf 2ar", "PROMIS Cognitive Function Raw Score", labels)
  labels <- gsub("Prcf 2a", "PROMIS Cognitive Function", labels)
  labels <- gsub("Prft 4at", "PROMIS Fatigue T-Score", labels)
  labels <- gsub("Prft 4a", "PROMIS Fatigue", labels)
  labels <- gsub("Posts", "HEAL Positive Outlook T-Score", labels)
  labels <- gsub("Posraw", "HEAL Positive Outlook Raw Score", labels)
  labels <- gsub("Pos ", "HEAL Positive Outlook ", labels)
  labels <- gsub("Psqi 4", "PSQI-4 Score", labels)
  labels <- gsub("Wk 36", "Week 36", labels)
  labels <- gsub("Painintensity", "Pain Intensity", labels)
  labels <- gsub("Opioid Tylnol", "Tylenol", labels)
  labels <- gsub(" Opioid Hydro", " Hydrocodone", labels)
  labels <- gsub(" Opioid Hydrom", " Hydromorphone", labels)
  labels <- gsub(" Opioid Tram", " Tramadol", labels)
  labels <- gsub(" Opioid Morph", " Morphine", labels)
  labels <- gsub(" Opioid Oxy", " Oxycodone", labels)
  labels <- gsub(" Opioid Percocet", " Percocet", labels)
  labels <- gsub(" Cbd", " Cannabidiol", labels)
  labels <- gsub(" Opioid Fentpatch", " Fentanyl", labels)
  labels <- gsub(" Opioid Mme", " Morphine Mg Equivalent", labels)
  labels <- gsub("ODIpct", "ODI Percent", labels)
  labels <- gsub("Law Suit", "Lawsuit", labels)
  labels <- gsub("Notreported", "Not Reported", labels)
  labels <- gsub("Avgpain", "Average Pain", labels)
  labels <- gsub("Receivedsurgery", "Received Surgery", labels)
  labels <- gsub("Mentalhealth", "Mental Health", labels)
  labels <- gsub("Exercisedays", "Exercise Days", labels)
  labels <- gsub("Dietnutrition", "Diet/Nutrition", labels)
  labels <- gsub("Dietnutri", "Diet/Nutrition", labels)
  labels <- gsub("Gabpreg", "Gabapentin/Pregabalin", labels)
  labels <- gsub("Gabapentin Pregabalin", "Gabapentin/Pregabalin", labels)
  labels <- gsub("Scoreraw", "Raw Score", labels)
  labels <- gsub("Qol", "Quality of Life", labels)
  labels <- gsub("Sms Painguide", "Count PainGuide Accesses After SMS", labels)
  labels <- gsub("Mand", "Mandatory Modules", labels)
  labels <- gsub("Supp", "Supplementary Modules", labels)
  labels <- gsub("Treatment Pt", "Treatment Physical Therapy", labels)
  labels <- gsub("Ther ", "Therapist ", labels)
  labels <- gsub("Ppt ", "Participant ", labels)
  labels <- gsub("Lin S", "Linear (4 Motions)", labels)
  labels <- gsub("Lin ", "Linear (6 Motions)", labels)
  labels <- gsub("Log S", "Logistic (4 Motions)", labels)
  labels <- gsub("Log ", "Logistic (6 Motions)", labels)
  labels <- gsub("W 12", "Visit 1", labels)
  labels <- gsub("W 0", "Baseline", labels)
  labels <- gsub("Escelig", "ESC Eligibility", labels)
  labels <- gsub("Ebemelig", "EBEM Eligibility", labels)
  labels <- gsub("Duloelig", "Duloxetine Eligibility", labels)
  labels <- gsub("Actelig", "ACT Eligibility", labels)
  labels <- gsub("Cbctx", "Choice Based Conjoint Pairs", labels)
  labels <- gsub("Mitt", "mITT", labels)
  labels <- gsub("Esc", "ESC", labels)
  labels <- gsub("Ebem", "EBEM", labels)
  labels <- gsub("Act ", "ACT ", labels)
  labels <- gsub("Sss", "SSS", labels)
  labels <- gsub("Modulesort", "Module Sort", labels)
  labels <- gsub("Timetype", "Time Type", labels)
  labels <- gsub("KSBraw", "KSB Raw Score", labels)
  labels <- gsub("KSBrisk", "KSB Risk Score", labels)
  labels <- gsub("KSB Rawsb", "KSB Raw Subscore", labels)
  labels <- gsub("GADraw", "GAD Raw Score", labels)
  labels <- gsub("FABQraw", "FABQ Raw Score", labels)
  labels <- gsub("PCSraw", "PCS Raw Score", labels)
  labels <- gsub("ODIraw", "ODI Raw Score", labels)
  labels <- gsub("Mbm 7region Sum", "Michigan Map-7 Region", labels)
  labels <- gsub("Aer", "AER Involving Duloxetine", labels)
  labels <- gsub("Pr ", "Patient Reported ", labels)
  labels <- gsub("Ella Ella ", "Ella ", labels)
  labels <- gsub("Mmp 9", "MMP-9", labels)
  labels <- gsub("Crp", "C-Reactive Protein", labels)
  labels <- gsub("Npy", "Neuropeptide Y", labels)
  labels <- gsub("Conc ", "Concentration ", labels)
  labels <- gsub(" Tid", " # of Times", labels)
  labels <- gsub("Q\\s+(\\d)", "Q\\1", labels)
  labels <- gsub("Gss\\s+8", "GSS-8", labels)
  labels <- gsub("Gssess", "GSS-ESS", labels)
  labels <- gsub("Gssint", "GSS-INT", labels)
  labels <- gsub("Gssraw", "GSS Raw Score", labels)
  labels <- gsub("Is ", "", labels)
  labels <- gsub("Taps ", "", labels)
  labels <- gsub("250116", "01/16/25", labels)
  labels <- gsub("V 0", "Baseline", labels)
  labels <- gsub("V 1", "Week 12", labels)
  labels <- gsub("V 2", "Week 24", labels)
  labels <- gsub("C LBP", "cLBP", labels)
  labels <- gsub("PH 1 Wk 6", "Week 6", labels)
  labels <- gsub("Baseline Baseline", "Baseline", labels)
  labels <- gsub("Motororor", "Motor", labels)
  labels <- gsub(
    "Supplementary Moduleslementary Moduleslementary Module",
    "Supplementary Modules",
    labels
  )
  labels <- gsub("([0-9]+)", "\\1", labels) # Keep numbers intact
  labels <- gsub("PHQ2raw", "PHQ-2 Raw Score", labels)
  labels <- gsub("PHQ2", "PHQ-2 Score", labels)
  labels <- gsub(" TX 1", " Initial", labels)
  labels <- gsub(" TX 2", " Week 12", labels)
  return(labels)
}

#' Rename the `generics::tidy` model coefficients output for plotting
#'
#' Also drops the intercept
.RenameTidyCoefsForPlotting <- function(
  tidy_coef_summary,
  drop_intercept = TRUE
) {
  renamed_coef_summary <- tidy_coef_summary |>
    dplyr::rename(Predictor = term) |>
    dplyr::mutate(
      Sign = factor(
        estimate >= 0,
        levels = c(TRUE, FALSE),
        labels = c("Positive", "Negative")
      ),
      Magnitude = abs(estimate)
    )

  if (drop_intercept) {
    renamed_coef_summary <- renamed_coef_summary |>
      dplyr::filter(Predictor != "(Intercept)")
  }

  return(renamed_coef_summary)
}

#' Strip prefixes from variable labels
#'
#' Removes wide format varaible prefixes e.g. "v_0_"
#'
#' @param tidy_mod_summary tibble result of \code{tidy(model_fit)}
#' @noRd
.PlotStripVarPrefixes <- function(
  tidy_mod_summary,
  prefix_vec = c("none_", "v_0_", "baseline_")
) {
  summary_for_plotting <- tidy_mod_summary

  for (prefix in prefix_vec) {
    summary_for_plotting <- dplyr::mutate(
      .data = summary_for_plotting,
      term = .CreateLabels(unlist(term))
    )
  }

  return(summary_for_plotting)
}
