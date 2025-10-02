# Stats for JAMA Ophthalmology paper
# setup
library(here)
library(tidyverse)
library(plotly)
library(ggplot2)
library(psych)
library(report)
library(rempsyc)

rm(list = ls()) 
load(here("dBdat.Rda"))

dBdat_flipped <- dBdat %>%
  mutate(x = as.numeric(x), y = as.numeric(y)) %>%
  mutate(x = case_when(
    eye == "L" & device == "retinalogik" ~ x*-1,
    TRUE ~ as.numeric(x)
  )) %>%
  # remove blind spots
  filter(!(x == 15 & y == -3)) %>%
  filter(!(x == 15 & y == +3)) %>%
  #remove first 2 visits
  filter(visit != 1 & visit != 2) %>%
  #remove reliability indices <30%
  filter(flPerc < 30 & fnPerc < 30 & fpPerc < 30) %>%
  group_by(id, device, eye) %>%
  mutate(meanmd = mean(md))

classification <- readxl::read_excel("RetinaLogik Data Classification.xlsx") %>%
  mutate(
    # Extract the numeric part after "RL"
    numeric_part = as.numeric(str_extract(ID, "\\d+")),
    # Format the numeric part to have at least two digits with leading zeros
    formatted_numeric = str_pad(numeric_part, width = 2, side = "left", pad = "0"),
    # Combine "RL" with the formatted number
    ID = paste0("RL", formatted_numeric)
  ) %>%
  # Remove the temporary columns
  select(-numeric_part, -formatted_numeric) %>%
  rename(id = ID, classification = Classifcation, eye = `Eye (OD/OS)`) %>%
  mutate(eye = case_when(eye == "OS" ~ "L",
                         eye == "OD" ~ "R",
                         eye == "OU" ~ "R"))

dBdat_filtered <- merge(dBdat_flipped, classification) %>%
  filter(id != "RL31")

df <- dBdat_filtered %>%
  distinct(id, device, visit, eye, x, y, dB) %>%
  group_by(id, device, eye, x, y) %>%
  mutate(meandB = mean(dB)) %>%
  distinct(id, device, eye, x, y, meandB) %>%
  pivot_wider(names_from = device, values_from = meandB) %>%
  mutate(retinalogik = as.numeric(retinalogik), hfa = as.numeric(hfa))

# ICC (1,3) Agreement between tests comparing "retinalogik" and "hfa" measurements.
dB_wide_agreement <- dBdat_filtered %>%
  group_by(id, device) %>%
  summarise(mean_dB = mean(dB, na.rm = TRUE)) %>%
  pivot_wider(names_from = device, values_from = mean_dB)

icc_data_agreement <- dB_wide_agreement %>%
  ungroup() %>%
  select(retinalogik, hfa) %>%
  mutate(
    retinalogik = as.numeric(retinalogik),
    hfa = as.numeric(hfa)
  ) %>%
  as.matrix() 

icc_result_1_3 <- ICC(icc_data_agreement, lmer = FALSE, missing=FALSE) # lmer=FALSE for base R stats
icc_result_1_3 <- ICC(icc_data_agreement, lmer = TRUE, missing=FALSE)
# use result from ICC3 Single_fixed_raters, e.g.,
# Single_fixed_raters      ICC3 0.93 27  32  32 1.4e-15        0.86        0.96

### For repeatability within a device
retinalogik_visits <- dBdat_filtered %>%
  filter(device == "retinalogik") %>%
  # If you have varying numbers of visits, you might need to pad with NA or take a consistent number.
  # For demonstration, let's assume we pivot directly.
  # It's crucial that 'visit' actually represents repeated measures of the *same thing* to be averaged.
  group_by(id, visit) %>%
  summarise(mean_dB_visit = mean(dB, na.rm = TRUE)) %>% # If multiple dB per id, visit, device
  pivot_wider(names_from = visit, values_from = mean_dB_visit, names_prefix = "Visit_") %>%
  ungroup() # Ungroup after pivot_wider

icc_data_retinalogik <- retinalogik_visits %>%
  select(starts_with("Visit_")) %>%
  as.matrix()

icc_result_1_2_retinalogik <- ICC(icc_data_retinalogik)
icc_result_1_2_retinalogik # use ICC2k results

# repeat for HFA
hfa_visits <- dBdat_filtered %>%
  filter(device == "hfa") %>%
  group_by(id, visit) %>%
  summarise(mean_dB_visit = mean(dB, na.rm = TRUE)) %>%
  pivot_wider(names_from = visit, values_from = mean_dB_visit, names_prefix = "Visit_") %>%
  ungroup()

icc_data_hfa <- hfa_visits %>%
  select(starts_with("Visit_")) %>%
  as.matrix()

icc_result_1_2_hfa <- ICC(icc_data_hfa)
icc_result_1_2_hfa # use ICC2k results

################
### test duration comparisons
################
# rtduration <- dBdat_filtered %>%
#   filter(device=="retinalogik") %>%
#   group_by(id) %>%
#   mutate(duration = mean(as.duration(duration))) %>%
#   distinct(id, classification, duration)
# 
# hfaduration <- dBdat_filtered %>%
#   filter(device=="hfa") %>%
#   group_by(id) %>%
#   mutate(duration = mean(as.duration(duration))) %>%
#   distinct(id, classification, duration)
# 
# t.test(as.numeric(rtduration$duration), as.numeric(hfaduration$duration), paired=T)

duration <- dBdat_filtered %>%
  group_by(id, device) %>%
  mutate(duration = mean(as.duration(duration))) %>%
  distinct(id, classification, duration) %>%
  pivot_wider(names_from = device, values_from = duration)

meanduration <- duration %>% drop_na()

t.test(Pair(retinalogik, hfa) ~ 1, data = meanduration)

t.test(Pair(retinalogik, hfa) ~ 1, data = duration)

#t.test(Pair(retinalogik, hfa) ~ 1, data = meanduration)

t.test(Pair(retinalogik, hfa) ~ 1, data = meanduration,
       subset = classification %in% "Mild")

t.test(Pair(retinalogik, hfa) ~ 1, data = meanduration,
       subset = classification %in% "Moderate")

t.test(Pair(retinalogik, hfa) ~ 1, data = meanduration,
       subset = classification %in% "Advanced")

md <- dBdat_filtered %>%
  group_by(id, device) %>%
  mutate(md = mean(md)) %>%
  distinct(id, classification, md) %>%
  pivot_wider(names_from = device, values_from = md)

meanmd <- md %>% drop_na()

t.test(Pair(retinalogik, hfa) ~ 1, data = md)

meanmd %>%
  filter(classification != "Control") %>%
  mutate(classification = factor(classification, levels = c("Control", "Mild", "Moderate", "Advanced"))) %>%
  group_by(classification) %>%
  summarise(
    n = n(),
    mean_retinalogik = mean(retinalogik, na.rm = TRUE),
    sd_retinalogik = sd(retinalogik, na.rm = TRUE),
    mean_hfa = mean(hfa, na.rm = TRUE),
    sd_hfa = sd(hfa, na.rm = TRUE)
  ) %>%
  nice_table()

t.test(Pair(retinalogik, hfa) ~ 1, data = meanmd,
       subset = classification %in% "Mild")

t.test(Pair(retinalogik, hfa) ~ 1, data = meanmd,
       subset = classification %in% "Moderate")

t.test(Pair(retinalogik, hfa) ~ 1, data = meanmd,
       subset = classification %in% "Advanced") %>% report(data = meanmd)

dBdat_filtered %>%
  select(id, classification, device, dB) %>%
  group_by(id, classification, device) %>%
  mutate(dB = mean(dB)) %>%
  group_by(classification, device) %>%
  mutate(mean = mean(dB), sd = sd(dB),) %>%
  distinct(classification, device, mean, sd) %>%
  pivot_wider(names_from = device, values_from = c(mean, sd)) %>%
  filter(classification != "Control") %>%
  nice_table()

ms <- dBdat_filtered %>%
  group_by(id, device) %>%
  mutate(mean = mean(dB)) %>%
  distinct(id, classification, mean, age, gender) %>%
  pivot_wider(names_from = device, values_from = mean) %>%
  filter(classification != "Control") %>%
  mutate(classification = factor(classification, levels = c("Mild", "Moderate", "Advanced"))) %>%
  drop_na()

ms %>%
  group_by(classification) %>%
  summarise(
    n = n(),
    mean_retinalogik = mean(retinalogik, na.rm = TRUE),
    sd_retinalogik = sd(retinalogik, na.rm = TRUE),
    mean_hfa = mean(hfa, na.rm = TRUE),
    sd_hfa = sd(hfa, na.rm = TRUE)
  ) %>%
  nice_table()

ms %>%
  group_by(classification) %>%
  summarise(
    n = n(),
    median_retinalogik = median(retinalogik, na.rm = TRUE),
    q1_retinalogik = quantile(retinalogik, .25, na.rm = TRUE),
    q3_retinalogik = quantile(retinalogik, .75, na.rm = TRUE),
    iqr_retinalogik = IQR(retinalogik, na.rm = TRUE),
    median_hfa = median(hfa, na.rm = TRUE),
    q1_hfa = quantile(hfa, .25, na.rm = TRUE),
    q3_hfa = quantile(hfa, .75, na.rm = TRUE),
    iqr_hfa = IQR(hfa, na.rm = TRUE)
  ) %>%
  nice_table()

# demographics
demo <- ms %>%
  group_by(id, gender, classification) %>%
  summarise(age = mean(age)) %>%
  mutate(id = as.factor(id),
         gender = as.factor(gender),
         classification = as.factor(classification))

summary(demo)

msmild <- ms %>%
  filter(classification == "Mild")
t.test(x=msmild$hfa, y=msmild$retinalogik, paired = TRUE)

msmod <- ms %>%
  filter(classification == "Moderate")
t.test(x=msmod$hfa, y=msmod$retinalogik, paired = TRUE)

msadv <- ms %>%
  filter(classification == "Advanced")
t.test(x=msadv$hfa, y=msadv$retinalogik, paired = TRUE)


