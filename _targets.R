source("R/functions.R")
library(targets)
options(scipen = 999)
tar_option_set(packages = c(
  "tidyverse",
  "readxl",
  "arrow",
  "ggbeeswarm",
  "patchwork"
))

list(
  tar_target(exclusions, apply_exclusions()),
  tar_target(
    randomization,
    tribble(
      ~ID, ~randomization,
      "N01", "IPI enabled",
      "N02", "IPI disabled",
      "N03", "IPI disabled",
      "N04", "IPI enabled",
      "N05", "IPI disabled",
      "N06", "IPI disabled",
      "N07", "IPI enabled",
      "N08", "IPI enabled",
      "N09", "IPI disabled",
      "N10", "IPI enabled",
      "N11", "IPI disabled",
    )
  ),
  tar_target(summary_table, make_summary_table(randomization, exclusions)),
  tar_target(primary, primary_outcome()),
  tar_target(primary_plot, get_primary_plot(primary)),
  tar_target(zinb, GLMMadaptive::mixed_model(
    fixed = outcome ~ randomization, random = ~ 1 | nurse, data = primary,
    family = GLMMadaptive::zi.negative.binomial(), zi_fixed = ~randomization, zi_random = ~ 1 | nurse,
  )),
  tar_target(zinb_summary, summary(zinb)),
  tar_target(zinb_resid_plot, resids_plot(zinb, primary$outcome)),
  tar_target(zinb_nb_effects, exp(confint(zinb))),
  tar_target(zinb_zero_effects, exp(confint(zinb, "zero_part"))),
  tar_target(icc, get_icc(data = primary, outcome = "outcome")),
  # duration
  tar_target(duration, alarm_duration()),
  tar_target(zinb_duration, GLMMadaptive::mixed_model(
    fixed = outcome ~ randomization, random = ~ 1 | nurse, data = duration,
    family = GLMMadaptive::zi.negative.binomial(), zi_fixed = ~randomization, zi_random = ~ 1 | nurse
  )),
  tar_target(zinb_summary_duration, summary(zinb_duration)),
  tar_target(zinb_resid_plot_duration, resids_plot(zinb_duration, duration$outcome)),
  tar_target(zinb_nb_effects_duration, exp(confint(zinb_duration))),
  tar_target(zinb_zero_effects_duration, exp(confint(zinb_duration, "zero_part"))),
  tar_target(icc_alarm_duration, get_icc(data = duration, outcome = "outcome")),
  # spo2_auc
  tar_target(spo2_auc, spo2()),
  tar_target(zinb_spo2, GLMMadaptive::mixed_model(
    fixed = outcome ~ randomization, random = ~ 1 | nurse, data = spo2_auc,
    family = GLMMadaptive::hurdle.lognormal(), zi_fixed = ~randomization, zi_random = ~ 1 | nurse
  )),
  tar_target(zinb_summary_spo2_auc, summary(zinb_spo2)),
  tar_target(zinb_resid_plot_spo2_auc, resids_plot(zinb_spo2, spo2_auc$outcome)),
  tar_target(zinb_nb_effects_spo2_auc, confint(zinb_spo2)),
  tar_target(zinb_zero_effects_spo2_auc, exp(confint(zinb_spo2, "zero_part"))),
  tar_target(icc_spo2_auc, get_icc(data = spo2_auc, outcome = "outcome")),
  tar_target(spo2_plot, get_spo2_plot(spo2_auc)),
  tar_target(app_alarms, appropriate_alarms()),
  tar_target(zinb_app_alarms, GLMMadaptive::mixed_model(
    fixed = outcome ~ randomization, random = ~ 1 | nurse, data = app_alarms,
    family = GLMMadaptive::zi.negative.binomial(), control = list(max_phis_value = exp(15)), zi_fixed = ~randomization, zi_random = ~ 1 | nurse
  )),
  tar_target(zinb_summary_app_alarms, summary(zinb_app_alarms)),
  tar_target(zinb_resid_plot_app_alarms, resids_plot(zinb_app_alarms, app_alarms$outcome)),
  tar_target(zinb_nb_effects_app_alarms, exp(confint(
    zinb_app_alarms
    # , sandwich = TRUE
  ))),
  tar_target(zinb_zero_effects_app_alarms, exp(confint(zinb_app_alarms, "zero_part"))),
  tar_target(icc_app_alarms, get_icc(data = app_alarms, outcome = "outcome")),
  tar_target(app_alarms_plot, app_alarms |>
    filter(outcome != 0) |>
    ggplot(aes(y = outcome, x = randomization, color = randomization)) +
    geom_quasirandom(
      size = 1.5,
      alpha = 0.8,
      width = 0.4
    ) +
    theme_minimal() +
    labs(title = "app_alarms by randomization") +
    theme(plot.title = element_text(hjust = 0.5)) +
    #   remove axis labels
    theme(axis.title = element_blank()) +
    # remove color legend
    guides(color = FALSE)),
  tar_target(inapp_alarms, inappropriate_alarms()),
  tar_target(zinb_inapp_alarms, GLMMadaptive::mixed_model(
    fixed = outcome ~ randomization, random = ~ 1 | nurse, data = inapp_alarms,
    family = GLMMadaptive::zi.negative.binomial(), zi_fixed = ~randomization, zi_random = ~ 1 | nurse
  )),
  tar_target(zinb_summary_inapp_alarms, summary(zinb_inapp_alarms)),
  tar_target(zinb_resid_plot_inapp_alarms, resids_plot(zinb_inapp_alarms, inapp_alarms$outcome)),
  tar_target(zinb_nb_effects_inapp_alarms, exp(confint(zinb_inapp_alarms))),
  tar_target(zinb_zero_effects_inapp_alarms, exp(confint(zinb_inapp_alarms, "zero_part"))),
  tar_target(icc_inapp_alarms, get_icc(data = inapp_alarms, outcome = "outcome")),
  tar_target(inapp_alarms_plot, inapp_alarms |>
    filter(outcome != 0) |>
    ggplot(aes(y = outcome, x = randomization, color = randomization)) +
    geom_quasirandom(
      size = 1.5,
      alpha = 0.8,
      width = 0.3
    ) +
    theme_minimal() +
    labs(title = "inapp_alarms by randomization") +
    theme(plot.title = element_text(hjust = 0.5)) +
    #   remove axis labels
    theme(axis.title = element_blank()) +
    # remove color legend
    guides(color = FALSE)),
  tar_target(adverse_events_df, get_adverse_events_df()),
  tar_target(adverse_events_model, get_adverse_events_model(adverse_events_df)),
  tar_target(adverse_events_icc, get_adverse_events_icc(adverse_events_df)),
  tar_target(cluster_size_randomized, get_cluster_size_randomized()),
  tar_target(cluster_size_analyzed, get_cluster_size_analyzed())

)
