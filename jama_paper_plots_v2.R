# Plots for JAMA paper
# setup
library(here)
library(tidyverse)
library(plotly)
library(ggplot2)
library(hexbin)
library(RColorBrewer)
library(BlandAltmanLeh)
library(patchwork)
library(viridis)

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

dBdat_filtered <- merge(dBdat_flipped, classification)

df <- dBdat_filtered %>%
  distinct(id, device, visit, eye, x, y, dB) %>%
  group_by(id, device, eye, x, y) %>%
  mutate(meandB = mean(dB)) %>%
  distinct(id, device, eye, x, y, meandB) %>%
  pivot_wider(names_from = device, values_from = meandB) %>%
  mutate(retinalogik = as.numeric(retinalogik), hfa = as.numeric(hfa))

dBdat_filtered %>%
  distinct(id, device, visit, eye, md) %>%
  pivot_wider(names_from = device, values_from = md) %>%
  clipr::write_clip()

# Fig 1. MS and Variability Figures for Normal Eyes, Overall GON, Early, Mod, Adv GON
## Normal eyes
nRT <- dBdat_filtered %>%
  filter(classification == "Control" & device == "retinalogik") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

nHFA <- dBdat_filtered %>%
  filter(classification == "Control" & device == "hfa") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

meanSensitivity <- dBdat_filtered %>%
  filter(classification == "Control") %>%
  distinct(id, device, visit, eye, x, y, dB) %>%
  pivot_wider(names_from = device, values_from = dB) %>%
  group_by(x, y) %>%
  summarize(meanRT = mean(retinalogik, na.rm=T), sdRT = sd(retinalogik, na.rm=T),
            meanHFA = mean(hfa, na.rm=T), sdHFA = sd(hfa, na.rm=T)) %>%
  ungroup() %>%
  add_row(tibble_row(x = 15, y = -3, meanRT = NA, sdRT = NA)) %>%
  add_row(tibble_row(x = 15, y = +3, meanRT = NA, sdRT = NA)) 

rtplot_healthy <- meanSensitivity %>%
  ggplot(aes(x, y, meanRT)) +
  geom_raster(aes(x = x, y = y, fill = meanRT)) +
  geom_text(aes(label = ifelse(is.na(meanRT), "", paste0(round(meanRT, 2), "\n(", round(sdRT, 2), ")")),
                x = x, y = y), size = 3, colour="white") +
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours = viridis(51), limits = c(-1, 49),
                       na.value="darkred") +
  theme_bw() +
  ggtitle("Retinalogik", subtitle = paste0("(n = ", nRT, " normal eyes)")) +
  theme(legend.position="none")

hfaplot_healthy <- meanSensitivity %>%
  ggplot(aes(x, y, meanHFA)) +
  geom_raster(aes(x = x, y = y, fill = meanHFA)) +
  geom_text(aes(label = ifelse(is.na(meanHFA), "", paste0(round(meanHFA, 2), "\n(", round(sdHFA, 2), ")")),
                x = x, y = y), size = 3, colour="white") +
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours = viridis(51), limits = c(-1, 49),
                       na.value="darkred") +
  theme_bw() +
  ggtitle("HFA", subtitle = paste0("(n = ", nHFA, " normal eyes)")) +
  theme(legend.position="none")

## Overall GON
nRT <- dBdat_filtered %>%
  filter(classification != "Control" & device == "retinalogik") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

nHFA <- dBdat_filtered %>%
  filter(classification != "Control" & device == "hfa") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

meanSensitivity <- dBdat_filtered %>%
  filter(classification != "Control") %>%
  distinct(id, device, visit, eye, x, y, dB) %>%
  pivot_wider(names_from = device, values_from = dB) %>%
  group_by(x, y) %>%
  summarize(meanRT = mean(retinalogik, na.rm=T), sdRT = sd(retinalogik, na.rm=T),
            meanHFA = mean(hfa, na.rm=T), sdHFA = sd(hfa, na.rm=T)) %>%
  ungroup() %>%
  add_row(tibble_row(x = 15, y = -3, meanRT = NA, sdRT = NA)) %>%
  add_row(tibble_row(x = 15, y = +3, meanRT = NA, sdRT = NA)) 

rtplot_overallGON <- meanSensitivity %>%
  ggplot(aes(x, y, meanRT)) +
  geom_raster(aes(x = x, y = y, fill = meanRT)) +
  geom_text(aes(label = ifelse(is.na(meanRT), "", paste0(round(meanRT, 2), "\n(", round(sdRT, 2), ")")),
                x = x, y = y), size = 3, colour="white") +
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours = viridis(51), limits = c(-1, 49),
                       na.value="darkred") +
  theme_bw() +
  ggtitle("Retinalogik", subtitle = paste0("(n = ", nRT, " overall GON eyes)")) +
  theme(legend.position="none")

hfaplot_overallGON <- meanSensitivity %>%
  ggplot(aes(x, y, meanHFA)) +
  geom_raster(aes(x = x, y = y, fill = meanHFA)) +
  geom_text(aes(label = ifelse(is.na(meanHFA), "", paste0(round(meanHFA, 2), "\n(", round(sdHFA, 2), ")")),
                x = x, y = y), size = 3, colour="white") +
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours = viridis(51), limits = c(-1, 49),
                       na.value="darkred") +
  theme_bw() +
  ggtitle("HFA", subtitle = paste0("(n = ", nHFA, " overall GON eyes)")) +
  theme(legend.position="none")

## Early GON
nRT <- dBdat_filtered %>%
  filter(classification == "Mild" & device == "retinalogik") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

nHFA <- dBdat_filtered %>%
  filter(classification == "Mild" & device == "hfa") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

meanSensitivity <- dBdat_filtered %>%
  filter(classification == "Mild") %>%
  distinct(id, device, visit, eye, x, y, dB) %>%
  pivot_wider(names_from = device, values_from = dB) %>%
  group_by(x, y) %>%
  summarize(meanRT = mean(retinalogik, na.rm=T), sdRT = sd(retinalogik, na.rm=T),
            meanHFA = mean(hfa, na.rm=T), sdHFA = sd(hfa, na.rm=T)) %>%
  ungroup() %>%
  add_row(tibble_row(x = 15, y = -3, meanRT = NA, sdRT = NA)) %>%
  add_row(tibble_row(x = 15, y = +3, meanRT = NA, sdRT = NA)) 

rtplot_earlyGON <- meanSensitivity %>%
  ggplot(aes(x, y, meanRT)) +
  geom_raster(aes(x = x, y = y, fill = meanRT)) +
  geom_text(aes(label = ifelse(is.na(meanRT), "", paste0(round(meanRT, 2), "\n(", round(sdRT, 2), ")")),
                x = x, y = y), size = 3, colour="white") +
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours = viridis(51), limits = c(-1, 49),
                       na.value="darkred") +
  theme_bw() +
  ggtitle("Retinalogik", subtitle = paste0("(n = ", nRT, " early GON eyes)")) +
  theme(legend.position="none")

hfaplot_earlyGON <- meanSensitivity %>%
  ggplot(aes(x, y, meanHFA)) +
  geom_raster(aes(x = x, y = y, fill = meanHFA)) +
  geom_text(aes(label = ifelse(is.na(meanHFA), "", paste0(round(meanHFA, 2), "\n(", round(sdHFA, 2), ")")),
                x = x, y = y), size = 3, colour="white") +
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours = viridis(51), limits = c(-1, 49),
                       na.value="darkred") +
  theme_bw() +
  ggtitle("HFA", subtitle = paste0("(n = ", nHFA, " early GON eyes)")) +
  theme(legend.position="none")

## Moderate GON
nRT <- dBdat_filtered %>%
  filter(classification == "Moderate" & device == "retinalogik") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

nHFA <- dBdat_filtered %>%
  filter(classification == "Moderate" & device == "hfa") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

meanSensitivity <- dBdat_filtered %>%
  filter(classification == "Moderate") %>%
  distinct(id, device, visit, eye, x, y, dB) %>%
  pivot_wider(names_from = device, values_from = dB) %>%
  group_by(x, y) %>%
  summarize(meanRT = mean(retinalogik, na.rm=T), sdRT = sd(retinalogik, na.rm=T),
            meanHFA = mean(hfa, na.rm=T), sdHFA = sd(hfa, na.rm=T)) %>%
  ungroup() %>%
  add_row(tibble_row(x = 15, y = -3, meanRT = NA, sdRT = NA)) %>%
  add_row(tibble_row(x = 15, y = +3, meanRT = NA, sdRT = NA)) 

rtplot_moderateGON <- meanSensitivity %>%
  ggplot(aes(x, y, meanRT)) +
  geom_raster(aes(x = x, y = y, fill = meanRT)) +
  geom_text(aes(label = ifelse(is.na(meanRT), "", paste0(round(meanRT, 2), "\n(", round(sdRT, 2), ")")),
                x = x, y = y), size = 3, colour="white") +
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours = viridis(51), limits = c(-1, 49),
                       na.value="darkred") +
  theme_bw() +
  ggtitle("Retinalogik", subtitle = paste0("(n = ", nRT, " moderate GON eyes)")) +
  theme(legend.position="none")

hfaplot_moderateGON <- meanSensitivity %>%
  ggplot(aes(x, y, meanHFA)) +
  geom_raster(aes(x = x, y = y, fill = meanHFA)) +
  geom_text(aes(label = ifelse(is.na(meanHFA), "", paste0(round(meanHFA, 2), "\n(", round(sdHFA, 2), ")")),
                x = x, y = y), size = 3, colour="white") +
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours = viridis(51), limits = c(-1, 49),
                       na.value="darkred") +
  theme_bw() +
  ggtitle("HFA", subtitle = paste0("(n = ", nHFA, " moderate GON eyes)")) +
  theme(legend.position="none")

## Advanced GON
nRT <- dBdat_filtered %>%
  filter(classification == "Advanced" & device == "retinalogik") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

nHFA <- dBdat_filtered %>%
  filter(classification == "Advanced" & device == "hfa") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

meanSensitivity <- dBdat_filtered %>%
  filter(classification == "Advanced") %>%
  distinct(id, device, visit, eye, x, y, dB) %>%
  pivot_wider(names_from = device, values_from = dB) %>%
  group_by(x, y) %>%
  summarize(meanRT = mean(retinalogik, na.rm=T), sdRT = sd(retinalogik, na.rm=T),
            meanHFA = mean(hfa, na.rm=T), sdHFA = sd(hfa, na.rm=T)) %>%
  ungroup() %>%
  add_row(tibble_row(x = 15, y = -3, meanRT = NA, sdRT = NA)) %>%
  add_row(tibble_row(x = 15, y = +3, meanRT = NA, sdRT = NA)) 

rtplot_advancedGON <- meanSensitivity %>%
  ggplot(aes(x, y, meanRT)) +
  geom_raster(aes(x = x, y = y, fill = meanRT)) +
  geom_text(aes(label = ifelse(is.na(meanRT), "", paste0(round(meanRT, 2), "\n(", round(sdRT, 2), ")")),
                x = x, y = y), size = 3, colour="white") +
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours = viridis(51), limits = c(-1, 49),
                       na.value="darkred") +
  theme_bw() +
  ggtitle("Retinalogik", subtitle = paste0("(n = ", nRT, " advanced GON eyes)")) +
  theme(legend.position="none")

hfaplot_advancedGON <- meanSensitivity %>%
  ggplot(aes(x, y, meanHFA)) +
  geom_raster(aes(x = x, y = y, fill = meanHFA)) +
  geom_text(aes(label = ifelse(is.na(meanHFA), "", paste0(round(meanHFA, 2), "\n(", round(sdHFA, 2), ")")),
                x = x, y = y), size = 3, colour="white") +
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours = viridis(51), limits = c(-1, 49),
                       na.value="darkred") +
  theme_bw() +
  ggtitle("HFA", subtitle = paste0("(n = ", nHFA, " advanced GON eyes)")) +
  theme(legend.position="none")

(rtplot_healthy + hfaplot_healthy)/
  (rtplot_overallGON + hfaplot_overallGON)
rtplot_earlyGON + hfaplot_earlyGON 
rtplot_moderateGON + hfaplot_moderateGON 
rtplot_advancedGON + hfaplot_advancedGON

# Fig 2. Bland-Altman (RL/HFA Bland-Altman inverse so line goes up with lower dBs).
blandplot <- bland.altman.plot(df$retinalogik, df$hfa, graph.sys = "ggplot2", geom_count=T)
bias <- mean(df$retinalogik, na.rm=T) - mean(df$hfa, na.rm=T)

print(blandplot +
        geom_smooth(method = "lm", se = FALSE) +
        # geom_point(position = "jitter") +
        geom_hline(yintercept = 0, color = "black") +
        geom_hline(yintercept = bias, color = "red", linetype = "solid", linewidth = 1) +
        xlab("dB") +
        ylab("Difference in dB (Retinalogik-HFA)") +
        labs(subtitle=paste("Bias =", round(bias, 2), "dB")) +
        ggtitle("Bland-Altman plot for all locations"))

# Fig 2 Hexbin Plots (polynomial line through Hexbin)
df %>%
  ggplot(aes(x = df$hfa, y = df$retinalogik)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c() +
  geom_smooth(method = "loess", se = TRUE, colour = "red") +
  geom_abline(intercept = 0, slope = 1) +
  labs(title="Hexbin plot of threshold sensitivities for all locations",
       subtitle="Averaged across reliable datasets between visits 3 to 5\n(Size of bin = 40)") +
  xlab("HFA (dB)") +
  ylab("Retinalogik (dB)") +
  xlim(-2, 40) +
  ylim(-2, 40) +
  theme_bw()

df %>%
  ggplot(aes(x = df$hfa, y = df$retinalogik)) +
  geom_hex(bins = 30) +
  scale_fill_viridis_c() +
  geom_smooth(method = "loess", se = TRUE, colour = "red") +
  geom_abline(intercept = 0, slope = 1) +
  labs(title="Hexbin plot of threshold sensitivities for all locations",
       subtitle="Averaged across reliable datasets between visits 3 to 5\n(Size of bin = 30)") +
  xlab("HFA (dB)") +
  ylab("Retinalogik (dB)") +
  xlim(-2, 40) +
  ylim(-2, 40) +
  theme_bw()

## Table
# healthytable <- dBdat_filtered %>%
#   distinct(id, device, eye, meanmd) %>%
#   filter(classification == "Control") %>%
#   count(device)
  

# Fig 3. Scotoma Analysis Plots…


