rm(list = ls())

# Libraries ---------------------------------------------------------------
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(stringr)
library(psych)
library(plm)
library(stargazer)
library(corrplot)
library(lubridate)
library(scales)
library(lmtest)
library(lmtest)   # For robust standard errors
library(sandwich) # For clustered standard errors
library(fixest)   # Alternative for clustering
library(censReg) # Tobit


# Data --------------------------------------------------------------------
PTC_data_all <- read_excel("../DATA/PCT_data_all.xlsx")

#select
PTC_data <- PTC_data_all %>%
    select(school,
           session,
           date,
           participant.payoff,
           participant.typukolu,
           participant.kolikukolu,
           riskcharts.1.player.qrc1:riskcharts.1.player.qrc76,
           timepreferences2.1.player.datumq1:timepreferences2.1.player.datumq62,
           ExpDotaznik.1.player.rt1:ExpDotaznik.1.player.rt21,
           ExpDotaznik.1.player.mv1:ExpDotaznik.1.player.mv27,
           ExpDotaznik.1.player.WP5, ExpDotaznik.1.player.WP12,
           ExpDotaznik.1.player.StSkola,
           ExpDotaznik.1.player.Pohlavi,
           ExpDotaznik.1.player.Vek,
           ExpDotaznik.1.player.Utrata,
           ExpDotaznik.1.player.Narozeni,
           ExpDotaznik.1.player.DenNarozeni,
           ExpDotaznik.1.player.PrvniTrida,
           ExpDotaznik.1.player.StudiumS,
           ExpDotaznik.1.player.Porucha,
           o2.1.player.Treatmentgroup,
           o2.1.player.co2,
           o2.1.player.guess,
           o2.1.player.q0:o2.1.player.q20,
           ravenM.1.player.score,
           ravenM.1.player.curr_matrix,
           workingmemory.1.player.correct_answers,
           workingmemory.1.player.correct_answers2,
           dtask.1.player.correct_answers,
           dtask.1.player.wrong_answers,
           dtask.1.player.score,
           stroop2.1.player.Colour_blind,
           participant.stroop1,
           participant.stroop2,
           participant.stroop3,
           participant.stroopscore,
           stroop.1.player.CRTscore,
           dimension.Neuroticismus.raw,
           dimension.Extraverze.raw,
           dimension.Otevřenost_vůči_zkušenosti.raw,
           dimension.Přívětivost.raw,
           dimension.Svědomitost.raw,
           dimension.Neuroticismus.norm,
           dimension.Extraverze.norm,
           dimension.Otevřenost_vůči_zkušenosti.norm,
           dimension.Přívětivost.norm,
           dimension.Svědomitost.norm,
           `item.neo-ffistandard_Standard.1.1`:`item.neo-ffistandard_Standard.60.1`,
           date,
           participant.kolikukolu,
           participant.co2)

#rename
PTC_data <- PTC_data %>%
    rename(Drawn = participant.typukolu,
           Add_payoff = participant.kolikukolu,
           Date = date,
           Session = session,
           Payoff = participant.payoff,
           Altruism1 = ExpDotaznik.1.player.WP5,
           Altruism2 = ExpDotaznik.1.player.WP12,
           Type_Hschool = ExpDotaznik.1.player.StSkola,
           Female = ExpDotaznik.1.player.Pohlavi,
           Age = ExpDotaznik.1.player.Vek,
           Expenses = ExpDotaznik.1.player.Utrata,
           Birth_m = ExpDotaznik.1.player.Narozeni,
           Birth_w = ExpDotaznik.1.player.DenNarozeni,
           Enter_school = ExpDotaznik.1.player.PrvniTrida,
           High_school = ExpDotaznik.1.player.StudiumS,
           Learning_dis = ExpDotaznik.1.player.Porucha,
           CO2_Treatment = o2.1.player.Treatmentgroup,
           CO2_Contribution = o2.1.player.co2,
           CO2_Guess = o2.1.player.guess,
           Known_by_name = o2.1.player.q0,
           Acceptable_donation = o2.1.player.q1, ### dodělat o2.1.player.q2 až o2.1.player.q20
           Raven_score = ravenM.1.player.score,
           Raven_compl = ravenM.1.player.curr_matrix,
           Digit_span1 = workingmemory.1.player.correct_answers,
           Digit_span2 = workingmemory.1.player.correct_answers2,
           d2_correct = dtask.1.player.correct_answers,
           d2_mistake = dtask.1.player.wrong_answers,
           d2_score = dtask.1.player.score,
           CRT_score = stroop.1.player.CRTscore,
           stroop_W = participant.stroop1,
           stroop_C = participant.stroop2,
           stroop_CW = participant.stroop3,
           stroop_score = participant.stroopscore,
           NEU_raw = dimension.Neuroticismus.raw,
           NEU_norm = dimension.Neuroticismus.norm,
           EXT_raw = dimension.Extraverze.raw,
           EXT_norm = dimension.Extraverze.norm,
           OPN_raw = dimension.Otevřenost_vůči_zkušenosti.raw,
           OPN_norm = dimension.Otevřenost_vůči_zkušenosti.norm,
           ARG_raw = dimension.Přívětivost.raw,
           ARG_norm = dimension.Přívětivost.norm,
           CON_raw = dimension.Svědomitost.raw,
           CON_norm = dimension.Svědomitost.norm) %>%
    rename_with(~ paste0("rt_", 1:18), ExpDotaznik.1.player.rt1:ExpDotaznik.1.player.rt18)%>%
    rename_with(~ paste0("rtplus_", 1:3), ExpDotaznik.1.player.rt19:ExpDotaznik.1.player.rt21)%>%
    rename_with(~ paste0("RiskHL1_", 1:10), riskcharts.1.player.qrc1:riskcharts.1.player.qrc10)%>%
    rename_with(~ paste0("RiskHL2_", 1:10), riskcharts.1.player.qrc11:riskcharts.1.player.qrc20)%>%
    rename_with(~ paste0("RiskHL3_", 1:10), riskcharts.1.player.qrc21:riskcharts.1.player.qrc30)%>%
    rename_with(~ paste0("RiskOLS1_", 1:5), riskcharts.1.player.qrc31:riskcharts.1.player.qrc35)%>%
    rename_with(~ paste0("RiskOLS2_", 1:5), riskcharts.1.player.qrc36:riskcharts.1.player.qrc40)%>%
    rename_with(~ paste0("RiskOLS3_", 1:5), riskcharts.1.player.qrc41:riskcharts.1.player.qrc45)%>%
    rename_with(~ paste0("RiskOLS4_", 1:5), riskcharts.1.player.qrc46:riskcharts.1.player.qrc50)%>%
    rename_with(~ paste0("RiskOLS5_", 1:5), riskcharts.1.player.qrc51:riskcharts.1.player.qrc55)%>%
    rename_with(~ paste0("Amb1_", 1:7), riskcharts.1.player.qrc56:riskcharts.1.player.qrc62)%>%
    rename_with(~ paste0("Amb2_", 1:7), riskcharts.1.player.qrc63:riskcharts.1.player.qrc69)%>%
    rename_with(~ paste0("Amb3_", 1:7), riskcharts.1.player.qrc70:riskcharts.1.player.qrc76)%>%
    rename_with(~ paste0("Time1_", 1:8), timepreferences2.1.player.datumq1:timepreferences2.1.player.datumq8)%>%
    rename_with(~ paste0("Time2_", 1:8), timepreferences2.1.player.datumq9:timepreferences2.1.player.datumq16)%>%
    rename_with(~ paste0("Time3_", 1:8), timepreferences2.1.player.datumq17:timepreferences2.1.player.datumq24)%>%
    rename_with(~ paste0("Time4_", 1:8), timepreferences2.1.player.datumq25:timepreferences2.1.player.datumq32)%>%
    rename_with(~ paste0("Time5_", 1:8), timepreferences2.1.player.datumq33:timepreferences2.1.player.datumq40)%>%
    rename_with(~ paste0("Time6_", 1:8), timepreferences2.1.player.datumq41:timepreferences2.1.player.datumq48)%>%
    rename_with(~ paste0("Effort1_", 1:7), timepreferences2.1.player.datumq49:timepreferences2.1.player.datumq55)%>%
    rename_with(~ paste0("Effort2_", 1:7), timepreferences2.1.player.datumq56:timepreferences2.1.player.datumq62)%>%
    rename_with(~ paste0("Moral1_", 1:11), ExpDotaznik.1.player.mv1:ExpDotaznik.1.player.mv11)%>%
    rename_with(~ paste0("Moral2_", 1:11), ExpDotaznik.1.player.mv17:ExpDotaznik.1.player.mv27)%>%
    rename_with(~ paste0("NEP_", 1:15), o2.1.player.q2:o2.1.player.q16)%>%
    rename_with(~ paste0("Env_attitude", 1:4), o2.1.player.q17:o2.1.player.q20)%>%
    rename_with(~ paste0("NEU_", 1:12), c(`item.neo-ffistandard_Standard.1.1`,
                                          `item.neo-ffistandard_Standard.6.1`,
                                          `item.neo-ffistandard_Standard.11.1`,
                                          `item.neo-ffistandard_Standard.16.1`,
                                          `item.neo-ffistandard_Standard.21.1`,
                                          `item.neo-ffistandard_Standard.26.1`,
                                          `item.neo-ffistandard_Standard.31.1`,
                                          `item.neo-ffistandard_Standard.36.1`,
                                          `item.neo-ffistandard_Standard.41.1`,
                                          `item.neo-ffistandard_Standard.46.1`,
                                          `item.neo-ffistandard_Standard.51.1`,
                                          `item.neo-ffistandard_Standard.56.1`))%>%
    rename_with(~ paste0("EXT_", 1:12), c(`item.neo-ffistandard_Standard.2.1`,
                                          `item.neo-ffistandard_Standard.7.1`,
                                          `item.neo-ffistandard_Standard.12.1`,
                                          `item.neo-ffistandard_Standard.17.1`,
                                          `item.neo-ffistandard_Standard.22.1`,
                                          `item.neo-ffistandard_Standard.27.1`,
                                          `item.neo-ffistandard_Standard.32.1`,
                                          `item.neo-ffistandard_Standard.37.1`,
                                          `item.neo-ffistandard_Standard.42.1`,
                                          `item.neo-ffistandard_Standard.47.1`,
                                          `item.neo-ffistandard_Standard.52.1`,
                                          `item.neo-ffistandard_Standard.57.1`))%>%
    rename_with(~ paste0("OPN_", 1:12), c(`item.neo-ffistandard_Standard.3.1`,
                                          `item.neo-ffistandard_Standard.8.1`,
                                          `item.neo-ffistandard_Standard.13.1`,
                                          `item.neo-ffistandard_Standard.18.1`,
                                          `item.neo-ffistandard_Standard.23.1`,
                                          `item.neo-ffistandard_Standard.28.1`,
                                          `item.neo-ffistandard_Standard.33.1`,
                                          `item.neo-ffistandard_Standard.38.1`,
                                          `item.neo-ffistandard_Standard.43.1`,
                                          `item.neo-ffistandard_Standard.48.1`,
                                          `item.neo-ffistandard_Standard.53.1`,
                                          `item.neo-ffistandard_Standard.58.1`))%>%
    rename_with(~ paste0("AGR_", 1:12), c(`item.neo-ffistandard_Standard.4.1`,
                                          `item.neo-ffistandard_Standard.9.1`,
                                          `item.neo-ffistandard_Standard.14.1`,
                                          `item.neo-ffistandard_Standard.19.1`,
                                          `item.neo-ffistandard_Standard.24.1`,
                                          `item.neo-ffistandard_Standard.29.1`,
                                          `item.neo-ffistandard_Standard.34.1`,
                                          `item.neo-ffistandard_Standard.39.1`,
                                          `item.neo-ffistandard_Standard.44.1`,
                                          `item.neo-ffistandard_Standard.49.1`,
                                          `item.neo-ffistandard_Standard.54.1`,
                                          `item.neo-ffistandard_Standard.59.1`)) %>%
    rename_with(~ paste0("CON_", 1:12), c(`item.neo-ffistandard_Standard.5.1`,
                                          `item.neo-ffistandard_Standard.10.1`,
                                          `item.neo-ffistandard_Standard.15.1`,
                                          `item.neo-ffistandard_Standard.20.1`,
                                          `item.neo-ffistandard_Standard.25.1`,
                                          `item.neo-ffistandard_Standard.30.1`,
                                          `item.neo-ffistandard_Standard.35.1`,
                                          `item.neo-ffistandard_Standard.40.1`,
                                          `item.neo-ffistandard_Standard.45.1`,
                                          `item.neo-ffistandard_Standard.50.1`,
                                          `item.neo-ffistandard_Standard.55.1`,
                                          `item.neo-ffistandard_Standard.60.1`))

#recode
PTC_data <- PTC_data %>%
    mutate(school = ifelse(school == "high", 1, 0),
           Type_Hschool = ifelse(Type_Hschool == "Gymnázium", 1,
                                 ifelse(Type_Hschool == "Jinak zaměřenou střední škola", 3,
                                        ifelse(Type_Hschool == "Učiliště", 4,
                                               ifelse(Type_Hschool == "Jiné", 5, 2)))), # Type of High-school (grammar school = 1, high-school = 2, training institution = 3, other = 4)
           Female = ifelse(Female == "Žena", 1, 0),
           Expenses = ifelse(Expenses == "Méně než 1000 Kč", 500,
                             ifelse(Expenses == "1000 – 2000 Kč", 1500,
                                    ifelse(Expenses == "Více než 5000 Kč", 5000, 3500))),
           Birth_m = match(Birth_m, c("Leden", "Únor", "Březen", "Duben", "Květen", "Červen",
                                      "Červenec", "Srpen", "Září", "Říjen", "Listopad", "Prosinec")),
           Birth_w = recode(Birth_w,
                            "1-7" = 1, "8-15" = 2, "16-21" = 3, .default = 4),
           High_school = ifelse(High_school == "Ano", 1, 0), #high-school students = 1 if Yes, university students = 0
           Learning_dis = ifelse(Learning_dis == "Ne", 0, 1),
           CO2_Treatment = ifelse(CO2_Treatment == "1", 1, 0), #Treatment = 1, Control = 0
    )


#recode - risk, time, ambiguity, questionnaires
PTC_data <- PTC_data %>%
    mutate_at(vars(RiskHL1_1:Amb3_7), ~ ifelse(. == "A", 0, ifelse(. == "B", 1, .))) %>%
    mutate_at(vars(Time1_1:Time1_8), ~ ifelse(grepl("Dnes\\S+", .), 0, 1)) %>%
    mutate_at(vars(Time2_1:Time2_8), ~ ifelse(grepl("Za týden\\S+", .), 0, 1)) %>%
    mutate_at(vars(Time3_1:Time3_8), ~ ifelse(grepl("Za 2 měsíce\\S+", .), 0, 1)) %>%
    mutate_at(vars(Time4_1:Time4_8), ~ ifelse(grepl("Dnes\\S+", .), 0, 1)) %>%
    mutate_at(vars(Time5_1:Time5_8), ~ ifelse(grepl("Za týden\\S+", .), 0, 1)) %>%
    mutate_at(vars(Time6_1:Time6_8), ~ ifelse(grepl("Za 2 měsíce\\S+", .), 0, 1)) %>%
    mutate_at(vars(Effort1_1:Effort1_7), ~ ifelse(. == "Do 3 dnů", 0, ifelse(. == "Do 33 dnů", 1, .))) %>%
    mutate_at(vars(Effort2_1:Effort2_7), ~ ifelse(. == "Do 90 dnů", 0, ifelse(. == "Do 120 dnů", 1, .))) %>%
    mutate_at(vars(rt_1:rt_18), ~ ifelse(. == "Ano", 1, ifelse(. == "Ne", 0, .))) %>%
    mutate_at(vars(rtplus_1:rtplus_3), ~ recode(.,
                                                "Ano" = 1,
                                                "Ne" = 2,
                                                "Nevím" = 3,
                                                "Nechci odpovědět" = 4)) %>%
    mutate_at(vars(Moral1_1:Moral1_11), ~ recode(.,
                                                 "Zcela nepodstatné" = 0,
                                                 "Spíše nepodstatné" = 1,
                                                 "Mírně podstatné" = 2,
                                                 "Celkem podstatné" = 3,
                                                 "Velmi podstatné" = 4,
                                                 "Obzvlášť podstatné" = 5)) %>%
    mutate_at(vars(Moral2_1:Moral2_11), ~ recode(.,
                                                 "Silně nesouhlasím" = 0,
                                                 "Středně nesouhlasím" = 1,
                                                 "Trochu nesouhlasím" = 2,
                                                 "Trochu souhlasím" = 3,
                                                 "Středně souhlasím" = 4,
                                                 "Silně souhlasím" = 5)) %>% 
    mutate_at(vars(NEP_1:NEP_15), ~ recode(.,
                                             "rozhodně souhlasím" = 1,
                                             "souhlasím" = 2,
                                             "nejsem si jistý" = 3,
                                             "nesouhlasím" = 4,
                                             "rozhodně nesouhlasím" = 5)) %>% 
    # reverse-scored questions from NEP
    mutate_at(vars(NEP_2, NEP_4, NEP_6, NEP_8, NEP_10, NEP_12, NEP_14), ~ 6 - as.numeric(.)) %>% 
    mutate_at(vars(Env_attitude1:Env_attitude4), ~ recode(.,
                                                          "rozhodně souhlasím" = 1,
                                                          "souhlasím" = 2,
                                                          "nejsem si jistý" = 3,
                                                          "nesouhlasím" = 4,
                                                          "rozhodně nesouhlasím" = 5))

PTC_data <- PTC_data %>%
    mutate(
        MV_Care = rowSums(select(., Moral1_1, Moral1_7, Moral2_1, Moral2_7), na.rm = TRUE),
        MV_Fairness = rowSums(select(., Moral1_2, Moral1_8, Moral2_2, Moral2_8), na.rm = TRUE),
        MV_Loyalty = rowSums(select(., Moral1_3, Moral1_9, Moral2_3, Moral2_9), na.rm = TRUE),
        MV_Authority = rowSums(select(., Moral1_4, Moral1_10, Moral2_4, Moral2_10), na.rm = TRUE),
        MV_Purity = rowSums(select(., Moral1_5, Moral1_11, Moral2_5, Moral2_11), na.rm = TRUE)
    )

#Care: an ability to feel (and dislike) the pain of others. It underlies the virtues of kindness, gentleness, and nurturance.
#Fairness: reciprocal altruism. It underlies the virtues of justice and rights.
#Loyalty: to form shifting coalitions. It’s “one for all and all for one.” It underlies the virtues of patriotism and self-sacrifice for the group.
#Authority: it underlies virtues of leadership and followership, including deference to prestigious authority figures and respect for traditions.
#Purity: it underlies the virtues of self-discipline, self-improvement, naturalness, and spirituality.


PTC_data$NEU_norm <- as.numeric(as.character(PTC_data$NEU_norm))
PTC_data$EXT_norm <- as.numeric(as.character(PTC_data$EXT_norm))
PTC_data$OPN_norm <- as.numeric(as.character(PTC_data$OPN_norm))
PTC_data$ARG_norm <- as.numeric(as.character(PTC_data$ARG_norm))
PTC_data$CON_norm <- as.numeric(as.character(PTC_data$CON_norm))


# Switching point ------------------------------------------------

# Define columns for each MPL
RiskHL_columns <- list(
    paste0("RiskHL1_", 1:10),
    paste0("RiskHL2_", 1:10),
    paste0("RiskHL3_", 1:10)
)

RiskOLS_columns <- list(
    paste0("RiskOLS1_", 1:5),
    paste0("RiskOLS2_", 1:5),
    paste0("RiskOLS3_", 1:5),
    paste0("RiskOLS4_", 1:5),
    paste0("RiskOLS5_", 1:5)
)

Time_Pref_columns <- list(
    paste0("Time1_", 1:8),
    paste0("Time2_", 1:8),
    paste0("Time3_", 1:8),
    paste0("Time4_", 1:8),
    paste0("Time5_", 1:8),
    paste0("Time6_", 1:8)
)

Time_Effort_columns <- list(
    paste0("Effort1_", 1:7),
    paste0("Effort2_", 1:7)
)

Amb_Pref_columns <- list(
    paste0("Amb1_", 1:7),
    paste0("Amb2_", 1:7),
    paste0("Amb3_", 1:7)
)

# Function to find the switching point
find_switching_point <- function(data, column_list, prefix) {
    for (i in seq_along(column_list)) {
        col_names <- column_list[[i]]
        var_name <- paste0(prefix, i, "_switch")
        # Find the first occurrence of 1 in each row
        data[[var_name]] <- apply(data[col_names], 1, function(x) which(x == 1)[1])
        # If no 1 is found, set to the last option (max number of choices)
        data[[var_name]][is.na(data[[var_name]])] <- length(col_names)
    }
    return(data)
}

# Apply the function to all MPLs
PTC_data <- find_switching_point(PTC_data, RiskHL_columns, "RiskHL")
PTC_data <- find_switching_point(PTC_data, RiskOLS_columns, "RiskOLS")
PTC_data <- find_switching_point(PTC_data, Time_Pref_columns, "Time")
PTC_data <- find_switching_point(PTC_data, Time_Effort_columns, "Effort")
PTC_data <- find_switching_point(PTC_data, Amb_Pref_columns, "Amb")


# Risk Preferences --------------------------------------------------------

# RiskHL - Average across three MPLs
PTC_data$RiskHL_mean <- rowMeans(PTC_data[, c("RiskHL1_switch", "RiskHL2_switch", "RiskHL3_switch")], na.rm = TRUE)
# Higher values indicate lower risk aversion (switching later)
# Lower values indicate higher risk aversion (switching earlier)

# Risk Taking Questions (RT-18)
PTC_data <- PTC_data %>%
    mutate(across(c(rt_2, rt_9, rt_10, rt_11), ~ 5 - as.numeric(.))) # reverse-code coutions items

PTC_data <- PTC_data %>%
    mutate(across(rt_1:rt_18, as.numeric)) %>%  # Convert to numeric
    mutate(RT_18 = rowSums(select(., rt_1:rt_18), na.rm = TRUE)) # RT-18 total score

# Compute CRRA (risk aversion coefficient)


# RiskOLS - Average across five MPLs
PTC_data$RiskOLS_mean <- rowMeans(PTC_data[, c("RiskOLS1_switch", "RiskOLS2_switch", "RiskOLS3_switch", "RiskOLS4_switch", "RiskOLS5_switch")], na.rm = TRUE)
# Higher values indicate lower risk aversion (switching later)
# Lower values indicate higher risk aversion (switching earlier)


# Average of switching points HL and OLS
PTC_data <- PTC_data %>%
    mutate(Risk_HL_OLS_mean = rowMeans(select(., RiskHL_mean, RiskOLS_mean), na.rm = TRUE))


# Time Preferences --------------------------------------------------------

# Time_Pref - Average across six MPLs
PTC_data$Time_mean <- rowMeans(PTC_data[, c("Time1_switch", "Time2_switch", "Time3_switch", "Time4_switch", "Time5_switch", "Time6_switch")], na.rm = TRUE)
# A lower switching point (e.g., closer to 1) → More impatient (prefers immediate rewards)
# A higher switching point (e.g., closer to 8) → More patient (willing to wait for larger rewards)

PTC_data$Effort_mean <- rowMeans(PTC_data[, c("Effort1_switch", "Effort2_switch")], na.rm = TRUE)


# Ambiguity Preferences ---------------------------------------------------

# Amb_Pref - Average across three MPLs
PTC_data$Amb_mean <- rowMeans(PTC_data[, c("Amb1_switch", "Amb2_switch", "Amb3_switch")], na.rm = TRUE)
# Higher values indicate lower risk aversion (switching later)
# Lower values indicate higher risk aversion (switching earlier)



# Other  -----------------------------------------------------------------------

table(PTC_data$school) #(High_school == "Ano", 1, 0)


## big5 ##
big5_columns <- c("NEU_raw", "EXT_raw", "OPN_raw", "ARG_raw", "CON_raw")
# Convert columns to numeric (assuming they contain numbers)
PTC_data[big5_columns] <- lapply(PTC_data[big5_columns], as.numeric)

## CA ##
PTC_data$Raven_score <- as.numeric(PTC_data$Raven_score)
summary(PTC_data$Raven_score)
PTC_data$Digit_span2 <- as.numeric(PTC_data$Digit_span2)
summary(PTC_data$Digit_span2)
PTC_data$stroop_score <- as.numeric(PTC_data$stroop_score)
summary(PTC_data$stroop_score)
PTC_data$d2_score <- as.numeric(PTC_data$d2_score)
summary(PTC_data$d2_score)
PTC_data$CRT_score <- as.numeric(PTC_data$CRT_score)
summary(PTC_data$CRT_score)




# Relative Age ------------------------------------------------------------

PTC_data <- PTC_data %>%
    mutate(Birth_m_recode = case_when(
        Birth_m == 9  ~ 1,
        Birth_m == 10 ~ 2,
        Birth_m == 11 ~ 3,
        Birth_m == 12 ~ 4,
        Birth_m == 1  ~ 5,
        Birth_m == 2  ~ 6,
        Birth_m == 3  ~ 7,
        Birth_m == 4  ~ 8,
        Birth_m == 5  ~ 9,
        Birth_m == 6  ~ 10,
        Birth_m == 7  ~ 11,
        Birth_m == 8  ~ 12
    ))



### Reduced form RA ###

## Risk Preferences (HL and OLS combined) #
RA_ols_riskHL <- feols(RiskHL_mean ~ Birth_m_recode + Type_Hschool + Female + Age + Expenses,
                  data = PTC_data, cluster = ~Session)
summary(RA_ols_riskHL)

RA_ols_riskB <- feols(RiskOLS_mean ~ Birth_m_recode + Type_Hschool + Female + Age + Expenses,
                     data = PTC_data, cluster = ~Session)
summary(RA_ols_riskB)

RA_ols_rt18 <- feols(RT_18 ~ Birth_m_recode + Type_Hschool + Female + Age + Expenses,
                     data = PTC_data, cluster = ~Session)
summary(RA_ols_rt18)

# High-school Students
RA_ols_riskHL_hs <- feols(RiskHL_mean ~ Birth_m_recode + Female + Age + Expenses,
                        data = filter(PTC_data, school == 1), cluster = ~Session)
summary(RA_ols_riskHL_hs)

RA_ols_riskB_hs <- feols(RiskOLS_mean ~ Birth_m_recode + Female + Age + Expenses,
                        data = filter(PTC_data, school == 1), cluster = ~Session)
summary(RA_ols_riskB_hs)

RA_ols_rt18_hs <- feols(RT_18 ~ Birth_m_recode + Female + Age + Expenses,
                        data = filter(PTC_data, school == 1), cluster = ~Session)
summary(RA_ols_rt18_hs)

# University Students
RA_ols_riskHL_uni <- feols(RiskHL_mean ~ Birth_m_recode + Female + Age + Expenses,
                         data = filter(PTC_data, school == 0), cluster = ~Session)
summary(RA_ols_riskHL_uni)

RA_ols_riskB_uni <- feols(RiskOLS_mean ~ Birth_m_recode + Female + Age + Expenses,
                         data = filter(PTC_data, school == 0), cluster = ~Session)
summary(RA_ols_riskB_uni)

RA_ols_rt18_uni <- feols(RT_18 ~ Birth_m_recode + Female + Age + Expenses,
                         data = filter(PTC_data, school == 0), cluster = ~Session)
summary(RA_ols_rt18_uni)

# Male Students
RA_ols_riskHL_male <- feols(RiskHL_mean ~ Birth_m_recode + Type_Hschool + Age + Expenses,
                          data = filter(PTC_data, Female == 0), cluster = ~Session)
summary(RA_ols_riskHL_male)

RA_ols_riskB_male <- feols(RiskOLS_mean ~ Birth_m_recode + Type_Hschool + Age + Expenses,
                            data = filter(PTC_data, Female == 0), cluster = ~Session)
summary(RA_ols_riskB_male)

RA_ols_rt18_male <- feols(RT_18 ~ Birth_m_recode + Type_Hschool + Age + Expenses,
                          data = filter(PTC_data, Female == 0), cluster = ~Session)
summary(RA_ols_rt18_male)

# Female Students
RA_ols_riskHL_female <- feols(RiskHL_mean ~ Birth_m_recode + Type_Hschool + Age + Expenses,
                            data = filter(PTC_data, Female == 1), cluster = ~Session)
summary(RA_ols_riskHL_female)

RA_ols_riskB_female <- feols(RiskOLS_mean ~ Birth_m_recode + Type_Hschool + Age + Expenses,
                            data = filter(PTC_data, Female == 1), cluster = ~Session)
summary(RA_ols_riskB_female)

RA_ols_rt18_female <- feols(RT_18 ~ Birth_m_recode + Type_Hschool + Age + Expenses,
                            data = filter(PTC_data, Female == 1), cluster = ~Session)
summary(RA_ols_rt18_female)


## Time Preferences #
RA_ols_time <- feols(Time_mean ~ Birth_m_recode + Type_Hschool + Female + Age + Expenses,
                  data = PTC_data, cluster = ~Session)
summary(RA_ols_time)

RA_ols_effort <- feols(Effort_mean ~ Birth_m_recode + Type_Hschool + Female + Age + Expenses,
                     data = PTC_data, cluster = ~Session)
summary(RA_ols_effort)

# High School Students
RA_ols_time_hs <- feols(Time_mean ~ Birth_m_recode + Female + Age + Expenses,
                        data = filter(PTC_data, school == 1), cluster = ~Session)
summary(RA_ols_time_hs)

RA_ols_effort_hs <- feols(Effort_mean ~ Birth_m_recode + Female + Age + Expenses,
                        data = filter(PTC_data, school == 1), cluster = ~Session)
summary(RA_ols_effort_hs)

# University Students
RA_ols_time_uni <- feols(Time_mean ~ Birth_m_recode + Female + Age + Expenses,
                           data = filter(PTC_data, school == 0), cluster = ~Session)
summary(RA_ols_time_uni)

RA_ols_effort_uni <- feols(Effort_mean ~ Birth_m_recode + Female + Age + Expenses,
                         data = filter(PTC_data, school == 0), cluster = ~Session)
summary(RA_ols_effort_uni)

# Male Students
RA_ols_time_male <- feols(Time_mean ~ Birth_m_recode + Type_Hschool + Age + Expenses,
                          data = filter(PTC_data, Female == 0), cluster = ~Session)
summary(RA_ols_time_male)

RA_ols_effort_male <- feols(Effort_mean ~ Birth_m_recode + Type_Hschool + Age + Expenses,
                          data = filter(PTC_data, Female == 0), cluster = ~Session)
summary(RA_ols_effort_male)

# Female Students
RA_ols_time_female <- feols(Time_mean ~ Birth_m_recode + Type_Hschool + Age + Expenses,
                            data = filter(PTC_data, Female == 1), cluster = ~Session)
summary(RA_ols_time_female)

RA_ols_effort_female <- feols(Effort_mean ~ Birth_m_recode + Type_Hschool + Age + Expenses,
                            data = filter(PTC_data, Female == 1), cluster = ~Session)
summary(RA_ols_effort_female)


## Ambiguity Preferences #
RA_ols_amb <- feols(Amb_mean ~ Birth_m_recode + Type_Hschool + Female + Age + Expenses,
                 data = PTC_data, cluster = ~Session)
summary(RA_ols_amb)

# High School Students
RA_ols_amb_hs <- feols(Amb_mean ~ Birth_m_recode + Female + Age + Expenses,
                        data = filter(PTC_data, school == 1), cluster = ~Session)
summary(RA_ols_amb_hs)

# University Students
RA_ols_amb_uni <- feols(Amb_mean ~ Birth_m_recode + Female + Age + Expenses,
                         data = filter(PTC_data, school == 0), cluster = ~Session)
summary(RA_ols_amb_uni)

# Male Students
RA_ols_amb_male <- feols(Amb_mean ~ Birth_m_recode + Type_Hschool + Age + Expenses,
                          data = filter(PTC_data, Female == 0), cluster = ~Session)
summary(RA_ols_amb_male)

# Female Students
RA_ols_amb_female <- feols(Amb_mean ~ Birth_m_recode + Type_Hschool + Age + Expenses,
                            data = filter(PTC_data, Female == 1), cluster = ~Session)
summary(RA_ols_amb_female)



## Only those born in September - November and June - August
PTC_data_RDD <- PTC_data %>%
    mutate(Older = case_when(
        Birth_m %in% c(9, 10, 11) ~ 1,
        Birth_m %in% c(6, 7, 8) ~ 0,
        TRUE ~ NA_real_  # Exclude January-May and December from analysis
    ))

# RDD Model with 'Older' variable
RA_rdd_model_riskHL <- feols(RiskHL_mean ~ Older + Type_Hschool + Female + Birth_m_recode + Expenses,
                   data = PTC_data_RDD, cluster = ~Session)
summary(RA_rdd_model_riskHL)

RA_rdd_model_riskB <- feols(RiskOLS_mean ~ Older + Type_Hschool + Female + Birth_m_recode + Expenses,
                           data = PTC_data_RDD, cluster = ~Session)
summary(RA_rdd_model_riskB)

RA_rdd_model_riskRT <- feols(RT_18 ~ Older + Type_Hschool + Female + Birth_m_recode + Expenses,
                           data = PTC_data_RDD, cluster = ~Session)
summary(RA_rdd_model_riskRT)

RA_rdd_model_riskHL_high <- feols(RiskHL_mean ~ Older + Type_Hschool + Female + Birth_m_recode + Expenses,
                           data = filter(PTC_data_RDD, school == 1), cluster = ~Session)
summary(RA_rdd_model_riskHL_high)

RA_rdd_model_riskB_high <- feols(RiskOLS_mean ~ Older + Type_Hschool + Female + Birth_m_recode + Expenses,
                                  data = filter(PTC_data_RDD, school == 1), cluster = ~Session)
summary(RA_rdd_model_riskB_high)

RA_rdd_model_riskRT_high <- feols(RT_18 ~ Older + Type_Hschool + Female + Birth_m_recode + Expenses,
                                  data = filter(PTC_data_RDD, school == 1), cluster = ~Session)
summary(RA_rdd_model_riskRT_high)

RA_rdd_model_time <- feols(Time_mean ~ Older + Type_Hschool + Female + Birth_m_recode + Expenses,
                      data = PTC_data_RDD, cluster = ~Session)
summary(RA_rdd_model_time)

RA_rdd_model_effort <- feols(Effort_mean ~ Older + Type_Hschool + Female + Birth_m_recode + Expenses,
                             data = PTC_data_RDD, cluster =  ~Session)
summary(RA_rdd_model_effort)

RA_rdd_model_amb <- feols(Amb_mean ~ Older + Type_Hschool + Female + Birth_m_recode + Expenses,
                      data = PTC_data_RDD, cluster = ~Session)
summary(RA_rdd_model_amb)


## is the relative age effect stronger in males or females?
RA_ols_risk_gender <- feols(Risk_HL_OLS_mean ~ Birth_m_recode * Female + Type_Hschool + Age + Expenses,
                            data = PTC_data, cluster = ~Session)
summary(RA_ols_risk_gender)

RA_ols_time_gender <- feols(Time_mean ~ Birth_m_recode * Female + Type_Hschool + Age + Expenses,
                            data = PTC_data, cluster = ~Session)
summary(RA_ols_time_gender)

RA_ols_effort_gender <- feols(Effort_mean ~ Birth_m_recode * Female + Type_Hschool + Age + Expenses,
                            data = PTC_data, cluster = ~Session)
summary(RA_ols_effort_gender)

RA_ols_amb_gender <- feols(Amb_mean ~ Birth_m_recode * Female + Type_Hschool + Age + Expenses,
                            data = PTC_data, cluster = ~Session)
summary(RA_ols_amb_gender)



# Pro-enviro Behavior -----------------------------------------------------

# OLS model
BEP_ols_model <- lm(CO2_Contribution ~ CO2_Treatment + CO2_Treatment * CO2_Guess +
                    NEU_norm + EXT_norm + OPN_norm + ARG_norm + CON_norm +
                    Amb_mean + Effort_mean + Time_mean + RT_18 + Risk_HL_OLS_mean +
                    MV_Care + MV_Fairness + MV_Loyalty + MV_Authority + MV_Purity +
                    Female + Age + Expenses, data = PTC_data)

summary(BEP_ols_model)


# University Students
BEP_model_uni <- lm(CO2_Contribution ~ CO2_Treatment + CO2_Treatment * CO2_Guess +
                         NEU_norm + EXT_norm + OPN_norm + ARG_norm + CON_norm +
                         Amb_mean + Effort_mean + Time_mean + RT_18 + Risk_HL_OLS_mean +
                         MV_Care + MV_Fairness + MV_Loyalty + MV_Authority + MV_Purity +
                         Female + Age + Expenses,
                     data = subset(PTC_data, school == 0))

summary(BEP_model_uni)

# High School Students
BEP_model_high <- lm(CO2_Contribution ~ CO2_Treatment + CO2_Treatment * CO2_Guess +
                           NEU_norm + EXT_norm + OPN_norm + ARG_norm + CON_norm +
                           Amb_mean + Effort_mean + Time_mean + RT_18 + Risk_HL_OLS_mean +
                           MV_Care + MV_Fairness + MV_Loyalty + MV_Authority + MV_Purity +
                           Female + Age + Expenses,
                       data = subset(PTC_data, school == 1))

summary(BEP_model_high)


# Tobit model
BEP_tobit_model <- censReg(CO2_Contribution ~ CO2_Treatment + CO2_Treatment * CO2_Guess +
                           NEU_norm + EXT_norm + OPN_norm + ARG_norm + CON_norm +
                           Amb_mean + Effort_mean + Time_mean + RT_18 + Risk_HL_OLS_mean +
                           MV_Care + MV_Fairness + MV_Loyalty + MV_Authority + MV_Purity +
                           Female + Age + Expenses, left = 0, data = PTC_data)

summary(BEP_tobit_model)

# University Students
BEP_tobit_model_uni <- censReg(CO2_Contribution ~ CO2_Treatment + CO2_Treatment * CO2_Guess +
                               NEU_norm + EXT_norm + OPN_norm + ARG_norm + CON_norm +
                               Amb_mean + Effort_mean + Time_mean + RT_18 + Risk_HL_OLS_mean +
                               MV_Care + MV_Fairness + MV_Loyalty + MV_Authority + MV_Purity +
                               Female + Age + Expenses, left = 0, data = subset(PTC_data, school == 0))

summary(BEP_tobit_model_uni)

# High School Students
BEP_tobit_model_high <- censReg(CO2_Contribution ~ CO2_Treatment + CO2_Treatment * CO2_Guess +
                                   NEU_norm + EXT_norm + OPN_norm + ARG_norm + CON_norm +
                                   Amb_mean + Effort_mean + Time_mean + RT_18 + Risk_HL_OLS_mean +
                                   MV_Care + MV_Fairness + MV_Loyalty + MV_Authority + MV_Purity +
                                   Female + Age + Expenses, left = 0, data = subset(PTC_data, school == 1))

summary(BEP_tobit_model_high)
