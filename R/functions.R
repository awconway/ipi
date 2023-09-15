apply_exclusions <- function() {
    data <- list.files(
        path = "observation_data",
        pattern = "*.xlsx",
        full.names = TRUE
    ) |>
        lapply(readxl::read_excel, sheet = "Patient") |>
        bind_rows()
    data |>
        select(patient, totalMidaz, totalOtherSedation, totalFent, procedure) |>
        filter(
            (totalMidaz == 0 | is.na(totalMidaz)) & is.na(totalOtherSedation)
        ) |>
        filter(patient != "E02") |> # sedation received but dose missing
        pull(patient)
}
make_summary_table <- function(randomization, exclusions) {
    ds <- open_dataset("monitor_data", format = "parquet", hive = TRUE)
    data <- list.files(
        path = "observation_data",
        pattern = "*.xlsx",
        full.names = TRUE
    ) |>
        lapply(readxl::read_excel, sheet = "Patient") |>
        bind_rows() |>
        # filter to exclude patients not in exclusion list
        filter(!patient %in% exclusions)

    nurse_data <- list.files(
        path = "observation_data",
        pattern = "*.xlsx",
        full.names = TRUE
    ) |>
        lapply(readxl::read_excel, sheet = "Nurse ID") |>
        bind_rows() |>
        left_join(randomization, by = c("nurse" = "ID")) |>
        select(-time) |>
        distinct(nurse, .keep_all = T)


    rand <- ds |>
        collect() |>
        distinct(id, .keep_all = T) |>
        select(nurse, id) |>
        right_join(nurse_data, by = "nurse") |>
        # E13 and E14 not in ds because missing the data from capnostream
        rbind(
            data.frame(
                nurse = "N09", id = "E13", randomization = "IPI disabled"
            )
        ) |>
        rbind(
            data.frame(
                nurse = "N09", id = "E14", randomization = "IPI disabled"
            )
        )

    data |>
        mutate(duration = mdy_hms(caseEnd) - mdy_hms(caseStart)) |>
        full_join(rand, by = c("patient" = "id")) |>
        filter(
            !patient %in% exclusions
        ) |> # filter to exclude patients not in exclusion list
        select(
            age,
            sex,
            totalMidaz,
            totalFent,
            oxygenDevice,
            duration,
            randomization,
            procedure
        ) |>
        mutate(procedure = case_when(
            grepl("tube", procedure, ignore.case = TRUE) ~ "Gastric tube",
            grepl("fistula", procedure, ignore.case = TRUE) ~ "Fistula",
            grepl("hemodialysis", procedure, ignore.case = TRUE) ~ "Hemodialysis line",
            grepl("cvc", procedure, ignore.case = TRUE) ~ "Central venous cannula",
            grepl("embolization", procedure, ignore.case = TRUE) ~ "Embolization",
            grepl("biopsy", procedure, ignore.case = TRUE) ~ "Biopsy",
            grepl("angiogram", procedure, ignore.case = TRUE) ~ "Angiogram",
            grepl("angioplasty", procedure, ignore.case = TRUE) ~ "Angioplasty",
            grepl("ablation", procedure, ignore.case = TRUE) ~ "Ablation",
            grepl("pulmonary", procedure, ignore.case = TRUE) ~ "Pulmonary angiogram",
            grepl("ptcd", procedure, ignore.case = TRUE) ~ "Percutaneous transhepatic cholangio drain",
            grepl("Abdominal drainage", procedure, ignore.case = TRUE) ~ "Abdominal drain",
            grepl("Abdominal drain", procedure, ignore.case = TRUE) ~ "Abdominal drain",
            grepl("Abdominal paracentesis", procedure, ignore.case = TRUE) ~ "Abdominal drain",
            TRUE ~ "Other"
        )) |>
        gtsummary::tbl_summary(
            by = "randomization",
            missing = "no",
            label = list(
                duration = "Procedure duration (minutes)",
                totalMidaz = "Total midazolam (mg)",
                totalFent = "Total fentanyl (mcg)",
                oxygenDevice = "Oxygen device",
                sex = "Sex",
                age = "Age (years)",
                procedure = "Procedure type"
            )
        )
}

get_adverse_events <- function() {
    data <- list.files(
        path = "observation_data",
        pattern = "*.xlsx",
        full.names = TRUE
    ) |>
        lapply(function(file) {
            df <- read_excel(
                file,
                sheet = "Adverse events",
                col_types = rep("text", 11)
            )
            # Remove extension from filename
            df$id <- file_path_sans_ext(basename(file))
            return(df)
        }) |>
        bind_rows()
}


primary_outcome <- function() {
    ds <- open_dataset("monitor_data/", format = "parquet", hive = TRUE) |>
        # filter to exclude patients not in exclusion list
        filter(!id %in% apply_exclusions())
    unitOfAnalysis <- ds |>
        # filter to exclude patients not in exclusion list
        filter(!id %in% apply_exclusions()) |>
        distinct(id, randomization, nurse) |>
        collect()
    primary <- ds |>
        mutate(allAlarms = case_when(
            pulseRateLow == 1 ~ 1,
            pulseRateHigh == 1 ~ 1,
            respRateLow == 1 ~ 1,
            respRateHigh == 1 ~ 1,
            noBreath == 1 ~ 1,
            spo2Low == 1 ~ 1,
            spo2High == 1 ~ 1,
            etco2Low == 1 ~ 1,
            etco2High == 1 ~ 1,
            ipiLow == 1 ~ 1,
            TRUE ~ 0
        )) |>
        collect() |>
        mutate(
            alarmStart = if_else(allAlarms == 1 & lag(allAlarms) == 0, datetime, NA)
        ) |>
        fill(alarmStart, .direction = "down") |>
        mutate(
            alarmEnd = if_else((allAlarms == 1 & lead(allAlarms) == 0) | allAlarms == 1 & !is.na(event), datetime, NA)
        ) |>
        filter(!is.na(alarmEnd)) |>
        filter(row_number() == 1, .by = c(alarmStart, id)) |>
        # select(id, datetime, allAlarms,alarmStart, alarmEnd, event, randomization, nurse) |>
        mutate(alarmDuration = difftime(alarmEnd, alarmStart, units = "secs")) |>
        mutate(outcome = sum(alarmDuration), .by = id) |>
        distinct(id, .keep_all = TRUE) |>
        # select(id, outcome, randomization, nurse) |>
        right_join(unitOfAnalysis, by = c("id", "randomization", "nurse")) |>
        replace_na(list(outcome = as.duration(0))) |>
        mutate(
            outcome = as.numeric(outcome),
            randomization = as.factor(randomization),
            id = as.factor(id), nurse = as.factor(nurse)
        ) |>
        # needs to be sorted by cluster for geeglm
        arrange(nurse) |>
        select(id, randomization, nurse, outcome) |>
        # E13 and E14 not in ds because missing the data from capnostream
        rbind(data.frame(
            nurse = "N09",
            id = "E13",
            randomization = "IPI disabled",
            outcome = 0
        )) |>
        rbind(
            data.frame(
                nurse = "N09",
                id = "E14",
                randomization = "IPI disabled",
                outcome = 0
            )
        )
}
alarm_duration <- function() {
    ds <- open_dataset("monitor_data/", format = "parquet", hive = TRUE) |>
        # filter to exclude patients not in exclusion list
        filter(!id %in% apply_exclusions())
    unitOfAnalysis <- ds |>
        # filter to exclude patients not in exclusion list
        filter(!id %in% apply_exclusions()) |>
        distinct(id, randomization, nurse) |>
        collect()
    duration <- ds |>
        mutate(allAlarms = case_when(
            pulseRateLow == 1 ~ 1,
            pulseRateHigh == 1 ~ 1,
            respRateLow == 1 ~ 1,
            respRateHigh == 1 ~ 1,
            noBreath == 1 ~ 1,
            spo2Low == 1 ~ 1,
            spo2High == 1 ~ 1,
            etco2Low == 1 ~ 1,
            etco2High == 1 ~ 1,
            ipiLow == 1 ~ 1,
            TRUE ~ 0
        )) |>
        collect() |>
        mutate(
            alarmStart = if_else(allAlarms == 1 & lag(allAlarms) == 0, datetime, NA)
        ) |>
        fill(alarmStart, .direction = "down") |>
        mutate(alarmEnd = if_else((allAlarms == 1 & lead(allAlarms) == 0), datetime, NA)) |>
        filter(!is.na(alarmEnd)) |>
        filter(row_number() == 1, .by = c(alarmStart, id)) |>
        # select(id, datetime, allAlarms,alarmStart, alarmEnd, event, randomization, nurse) |>
        mutate(alarmDuration = difftime(alarmEnd, alarmStart, units = "secs")) |>
        mutate(outcome = sum(alarmDuration), .by = id) |>
        distinct(id, .keep_all = TRUE) |>
        right_join(unitOfAnalysis, by = c("id", "randomization", "nurse")) |>
        replace_na(list(outcome = as.duration(0))) |>
        mutate(
            outcome = as.numeric(outcome),
            randomization = as.factor(randomization),
            id = as.factor(id),
            nurse = as.factor(nurse)
        ) |>
        # needs to be sorted by cluster for geeglm
        arrange(nurse) |>
        select(id, outcome, randomization, nurse) |>
        # E13 and E14 not in ds because missing the data from capnostream
        rbind(
            data.frame(
                nurse = "N09",
                id = "E13",
                randomization = "IPI disabled",
                outcome = 0
            )
        ) |>
        rbind(
            data.frame(
                nurse = "N09",
                id = "E14",
                randomization = "IPI disabled",
                outcome = 0
            )
        )
}

alarms <- function() {
    ds <- open_dataset("monitor_data/", format = "parquet", hive = TRUE) |>
        # filter to exclude patients not in exclusion list
        filter(!id %in% apply_exclusions())
    ds |>
        mutate(allAlarms = case_when(
            pulseRateLow == 1 ~ 1,
            pulseRateHigh == 1 ~ 1,
            respRateLow == 1 ~ 1,
            respRateHigh == 1 ~ 1,
            noBreath == 1 ~ 1,
            spo2Low == 1 ~ 1,
            spo2High == 1 ~ 1,
            etco2Low == 1 ~ 1,
            etco2High == 1 ~ 1,
            ipiLow == 1 ~ 1,
            TRUE ~ 0
        )) |>
        collect() |>
        # exclude the alarms that were only due to incorrectly fitting cannula
        mutate(event = case_when(
            event == "Fix equipment" ~ NA_character_,
            # event == "Alarm silenced" ~ NA_character_,
            TRUE ~ event
        )) |>
        # add a column to know if the condition allAlarms == 1 & !is.na(event) was met or not
        mutate(eventAlarm = if_else(allAlarms == 1 & !is.na(event), 1, 0)) |>
        select(id, datetime, eventAlarm, randomization, nurse) |>
        mutate(outcome = sum(eventAlarm), .by = "id") |>
        filter(row_number() == 1, .by = c(id, nurse)) |>
        mutate(
            outcome = as.numeric(outcome),
            randomization = as.factor(randomization),
            id = as.factor(id),
            nurse = as.factor(nurse)
        ) |>
        # needs to be sorted by cluster for geeglm
        arrange(nurse) |>
        select(id, randomization, nurse, outcome) |>
        # E13 and E14 not in ds because missing the data from capnostream
        rbind(
            data.frame(
                nurse = "N09",
                id = "E13",
                randomization = "IPI disabled",
                outcome = 0
            )
        ) |>
        rbind(
            data.frame(
                nurse = "N09",
                id = "E14",
                randomization = "IPI disabled",
                outcome = 0
            )
        )
}
appropriate_alarms <- function() {
    ds <- open_dataset("monitor_data/", format = "parquet", hive = TRUE) |>
        # filter to exclude patients not in exclusion list
        filter(!id %in% apply_exclusions())
    ds |>
        mutate(allAlarms = case_when(
            pulseRateLow == 1 ~ 1,
            pulseRateHigh == 1 ~ 1,
            respRateLow == 1 ~ 1,
            respRateHigh == 1 ~ 1,
            noBreath == 1 ~ 1,
            spo2Low == 1 ~ 1,
            spo2High == 1 ~ 1,
            etco2Low == 1 ~ 1,
            etco2High == 1 ~ 1,
            ipiLow == 1 ~ 1,
            TRUE ~ 0
        )) |>
        collect() |>
        mutate(event = case_when(
            event == "Fix equipment" ~ NA_character_,
            event == "Alarm silenced" ~ NA_character_,
            TRUE ~ event
        )) |>
        # add a column to know if the condition allAlarms == 1 & !is.na(event) was met or not
        mutate(eventAlarm = if_else(allAlarms == 1 & !is.na(event), 1, 0)) |>
        select(id, datetime, eventAlarm, randomization, nurse) |>
        mutate(outcome = sum(eventAlarm), .by = "id") |>
        filter(row_number() == 1, .by = c(id, nurse)) |>
        mutate(
            outcome = as.numeric(outcome),
            randomization = as.factor(randomization),
            id = as.factor(id),
            nurse = as.factor(nurse)
        ) |>
        # needs to be sorted by cluster for geeglm
        arrange(nurse) |>
        select(id, randomization, nurse, outcome) |>
        # E13 and E14 not in ds because missing the data from capnostream
        rbind(
            data.frame(
                nurse = "N09",
                id = "E13",
                randomization = "IPI disabled",
                outcome = 0
            )
        ) |>
        rbind(
            data.frame(
                nurse = "N09",
                id = "E14",
                randomization = "IPI disabled",
                outcome = 0
            )
        )
}

inappropriate_alarms <- function() {
    ds <- open_dataset("monitor_data/", format = "parquet", hive = TRUE) |>
        # filter to exclude patients not in exclusion list
        filter(!id %in% apply_exclusions())
    ds |>
        mutate(allAlarms = case_when(
            pulseRateLow == 1 ~ 1,
            pulseRateHigh == 1 ~ 1,
            respRateLow == 1 ~ 1,
            respRateHigh == 1 ~ 1,
            noBreath == 1 ~ 1,
            spo2Low == 1 ~ 1,
            spo2High == 1 ~ 1,
            etco2Low == 1 ~ 1,
            etco2High == 1 ~ 1,
            ipiLow == 1 ~ 1,
            TRUE ~ 0
        )) |>
        collect() |>
        mutate(event = case_when(
            event != "Alarm silenced" ~ NA_character_,
            TRUE ~ event
        )) |>
        # filter(!is.na(event)) |>
        # add a column to know if the condition allAlarms == 1 & !is.na(event) was met or not
        mutate(eventAlarm = if_else(allAlarms == 1 & !is.na(event), 1, 0)) |>
        select(id, datetime, eventAlarm, randomization, nurse) |>
        mutate(outcome = sum(eventAlarm), .by = "id") |>
        filter(row_number() == 1, .by = c(id, nurse)) |>
        mutate(outcome = as.numeric(outcome), randomization = as.factor(randomization), id = as.factor(id), nurse = as.factor(nurse)) |>
        # needs to be sorted by cluster for geeglm
        arrange(nurse) |>
        select(id, randomization, nurse, outcome) |>
        # E13 and E14 not in ds because missing the data from capnostream
        rbind(data.frame(nurse = "N09", id = "E13", randomization = "IPI disabled", outcome = 0)) |>
        rbind(data.frame(nurse = "N09", id = "E14", randomization = "IPI disabled", outcome = 0))
}

spo2 <- function() {
    ds <- open_dataset("monitor_data/", format = "parquet", hive = TRUE) |>
        # filter to exclude patients not in exclusion list
        filter(!id %in% apply_exclusions())
    unitOfAnalysis <- ds |>
        # filter to exclude patients not in exclusion list
        filter(!id %in% apply_exclusions()) |>
        distinct(id, randomization, nurse) |>
        collect()
    duration <- ds |>
        mutate(allAlarms = case_when(
            pulseRateLow == 1 ~ 1,
            pulseRateHigh == 1 ~ 1,
            respRateLow == 1 ~ 1,
            respRateHigh == 1 ~ 1,
            noBreath == 1 ~ 1,
            spo2Low == 1 ~ 1,
            spo2High == 1 ~ 1,
            etco2Low == 1 ~ 1,
            etco2High == 1 ~ 1,
            ipiLow == 1 ~ 1,
            TRUE ~ 0
        )) |>
        collect() |>
        mutate(alarmStart = if_else(allAlarms == 1 & lag(allAlarms) == 0, datetime, NA)) |>
        fill(alarmStart, .direction = "down") |>
        mutate(alarmEnd = if_else((allAlarms == 1 & lead(allAlarms) == 0), datetime, NA)) |>
        filter(!is.na(alarmEnd)) |>
        filter(row_number() == 1, .by = c(alarmStart, id)) |>
        # select(id, datetime, allAlarms,alarmStart, alarmEnd, event, randomization, nurse) |>
        mutate(alarmDuration = difftime(alarmEnd, alarmStart, units = "secs")) |>
        mutate(outcome = sum(ifelse(spo2 < 90, 90 - spo2, 0), na.rm = TRUE), .by = id) |>
        distinct(id, .keep_all = TRUE) |>
        # select(id, outcome, randomization, nurse) |>
        right_join(unitOfAnalysis, by = c("id", "randomization", "nurse")) |>
        replace_na(list(outcome = 0)) |>
        mutate(outcome = as.numeric(outcome), randomization = as.factor(randomization), id = as.factor(id), nurse = as.factor(nurse)) |>
        # needs to be sorted by cluster for geeglm
        arrange(nurse)
}

resids_plot <- function(object, y, nsim = 1000,
                        type = c("subject_specific", "mean_subject"),
                        integerResponse = NULL) {
    if (!inherits(object, "MixMod")) {
        stop("this function works for 'MixMod' objects.\n")
    }
    type <- match.arg(type)
    if (is.null(integerResponse)) {
        integer_families <- c(
            "binomial", "poisson", "negative binomial",
            "zero-inflated poisson", "zero-inflated negative binomial",
            "hurdle poisson", "hurdle negative binomial"
        )
        numeric_families <- c("hurdle log-normal", "beta", "hurdle beta", "Gamma")
        if (object$family$family %in% integer_families) {
            integerResponse <- TRUE
        } else if (object$family$family %in% numeric_families) {
            integerResponse <- FALSE
        } else {
            stop(
                "non build-in family object; you need to specify the 'integerResponse',\n",
                "\targument indicating whether the outcome variable is integer or not.\n"
            )
        }
    }
    sims <- simulate(object, nsim = nsim, type = type)
    fits <- fitted(object, type = type)
    dharmaRes <- DHARMa::createDHARMa(
        simulatedResponse = sims, observedResponse = y,
        fittedPredictedResponse = fits,
        integerResponse = integerResponse
    )
    DHARMa:::plotQQunif(dharmaRes)
}

get_icc <- function(data, outcome) {
    lmemod <- nlme::lme(fixed = outcome ~ 1, random = ~ 1 | nurse, data = data)
    s2b1 <- as.numeric(nlme::VarCorr(lmemod))[[1]]
    s2w1 <- as.numeric(nlme::VarCorr(lmemod))[[2]]
    rho1 <- s2b1 / (s2b1 + s2w1)
    # names(rho1)= 'ICC derived from random effects model'
    rho1
}

get_adverse_events_df <- function() {
    ds <- open_dataset("monitor_data/", format = "parquet", hive = TRUE)
    unitOfAnalysis <- ds |>
        # filter to exclude patients not in exclusion list
        filter(!id %in% apply_exclusions()) |>
        distinct(id, randomization, nurse) |>
        collect()

    data <- list.files(
        path = "observation_data",
        pattern = "*.xlsx",
        full.names = TRUE
    ) |>
        lapply(function(file) {
            df <- read_excel(file, sheet = "Adverse events", col_types = rep("text", 11))
            df$id <- tools::file_path_sans_ext(basename(file)) # Remove extension from filename
            return(df)
        }) |>
        bind_rows() |>
        right_join(unitOfAnalysis) |>
        #  variable to indicate if there was an adverse event
        mutate(adverseEvent = if_else(if_any(c(-id, -randomization, -nurse), ~ !is.na(.)), 1, 0))
}

get_adverse_events_model <- function(data) {
    parameters::parameters(
        lme4::glmer(
            factor(adverseEvent) ~ randomization + (1 | nurse),
            data = data,
            family = binomial(link = "logit")
        ),
        exponentiate = TRUE
    )
}

get_adverse_events_icc <- function(data) {
    icc <- ICCbin::iccbin(
        cid = nurse,
        y = adverseEvent,
        data = data,
        method = "stab"
    )

    icc$estimates$ICC
}

get_spo2_plot <- function(spo2_auc) {
    plot1 <- spo2_auc |>
        filter(outcome != 0) |>
        ggplot(aes(y = outcome, x = randomization)) +
        geom_quasirandom(size = 1.5, alpha = 0.8) +
        theme(
            axis.line = element_line(colour = "black"),
            axis.title.y = element_blank(),
            legend.position = "bottom",
            panel.background = element_blank(),
            plot.title.position = "plot",
            legend.title = element_blank()
        ) +
        labs(subtitle = str_wrap("Area under the oxygen desaturation curve for only participants whose SpO2 did drop below 90%", 45)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        # theme(axis.title = element_blank()) +
        guides(color = FALSE) +
        # y axis starts at 0
        scale_y_continuous(limits = c(0, NA)) +
        # remove x axis title
        labs(x = NULL)

    plot2 <- spo2_auc %>%
        mutate(zero = ifelse(outcome == 0, "Zero", "Not zero")) |>
        ggplot(aes(x = randomization, fill = factor(zero))) +
        geom_bar(position = "fill") +
        theme(
            axis.line = element_line(colour = "black"),
            axis.title.y = element_blank(),
            legend.position = "top",
            panel.background = element_blank(),
            plot.title.position = "plot",
            legend.title = element_blank()
        ) +
        labs(subtitle = "SpO2 <90%", x = NULL, y = NULL)

    combined <- plot2 + plot1 +
        plot_layout(ncol = 2, ) +
        plot_annotation(tag_levels = "A")

    ggplot2::ggsave(combined, filename = "spo2.svg", device = "svg")
}

get_primary_plot <- function(primary) {
    plot1 <- primary |>
        filter(outcome != 0) |>
        ggplot(aes(y = outcome, x = randomization)) +
        geom_quasirandom(size = 1.5, alpha = 0.8) +
        theme(
            axis.line = element_line(colour = "black"),
            axis.title.y = element_blank(),
            legend.position = "bottom",
            panel.background = element_blank(),
            plot.title.position = "plot",
            legend.title = element_blank()
        ) +
        labs(subtitle = str_wrap("Number of seconds in an alarm state without intervention for only participants with a score higher than zero", width = 50)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        guides(color = FALSE) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = NULL)

    plot2 <- primary %>%
        mutate(zero = ifelse(outcome == 0, "Zero", "Not zero")) |>
        ggplot(aes(x = randomization, fill = factor(zero))) +
        geom_bar(position = "fill") +
        theme(
            axis.line = element_line(colour = "black"),
            axis.title.y = element_blank(),
            legend.position = "top",
            panel.background = element_blank(),
            plot.title.position = "plot",
            legend.title = element_blank()
        ) +
        labs(subtitle = str_wrap("Number of patients who had an alarm", 50), x = NULL, y = NULL)

    combined <- plot2 + plot1 +
        plot_layout(ncol = 2, ) +
        plot_annotation(tag_levels = "A")

    ggplot2::ggsave(combined, filename = "primary.svg", device = "svg")
}

get_cluster_size_randomized <- function() {
    ds <- open_dataset("monitor_data/", format = "parquet", hive = TRUE)
    ds |>
        # filter(!id %in% apply_exclusions()) |>
        collect() |>
        distinct(id, .keep_all = T) |>
        select(nurse, id, randomization) |>
        # E13 and E14 not in ds because missing the data from capnostream
        rbind(data.frame(nurse = "N09", id = "E13", randomization = "IPI disabled")) |>
        rbind(data.frame(nurse = "N09", id = "E14", randomization = "IPI disabled")) |>
        # count the number of ids per nurse
        group_by(nurse, randomization) |>
        summarize(n = n())
}
get_cluster_size_analyzed <- function() {
    ds <- open_dataset("monitor_data/", format = "parquet", hive = TRUE)
    ds |>
        filter(!id %in% apply_exclusions()) |>
        collect() |>
        distinct(id, .keep_all = T) |>
        select(nurse, id, randomization) |>
        # E13 and E14 not in ds because missing the data from capnostream
        rbind(data.frame(nurse = "N09", id = "E13", randomization = "IPI disabled")) |>
        rbind(data.frame(nurse = "N09", id = "E14", randomization = "IPI disabled")) |>
        # count the number of ids per nurse
        group_by(nurse, randomization) |>
        summarize(n = n())
}
