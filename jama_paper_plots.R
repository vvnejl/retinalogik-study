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
  filter(!(x == 15 & y == +3))

df <- dBdat_flipped %>%
  distinct(id, device, visit, eye, x, y, dB) %>%
  pivot_wider(names_from = device, values_from = dB) %>%
  mutate(retinalogik = as.numeric(retinalogik), hfa = as.numeric(hfa)) 

# Fig 1. MS and Variability Figures for Normal Eyes, Overall GON, Early, Mod, Adv GON
## Normal eyes
nRT <- dBdat_flipped %>%
  filter(md > 0 & device == "retinalogik") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

nHFA <- dBdat_flipped %>%
  filter(md > 0 & device == "hfa") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

meanSensitivity <- dBdat_flipped %>%
  filter(md > 0) %>%
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
                x = x, y = y), size = 3) +
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
                x = x, y = y), size = 3) +
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours = viridis(51), limits = c(-1, 49),
                       na.value="darkred") +
  theme_bw() +
  ggtitle("HFA", subtitle = paste0("(n = ", nHFA, " normal eyes)")) +
  theme(legend.position="none")

## Overall GON
nRT <- dBdat_flipped %>%
  filter(md < 0 & device == "retinalogik") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

nHFA <- dBdat_flipped %>%
  filter(md < 0 & device == "hfa") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

meanSensitivity <- dBdat_flipped %>%
  filter(md < 0) %>%
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
                x = x, y = y), size = 3) +
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
                x = x, y = y), size = 3) +
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours = viridis(51), limits = c(-1, 49),
                       na.value="darkred") +
  theme_bw() +
  ggtitle("HFA", subtitle = paste0("(n = ", nHFA, " overall GON eyes)")) +
  theme(legend.position="none")

## Early GON
nRT <- dBdat_flipped %>%
  filter(md < 0 & md > -6 & device == "retinalogik") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

nHFA <- dBdat_flipped %>%
  filter(md < 0 & md > -6 & device == "hfa") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

meanSensitivity <- dBdat_flipped %>%
  filter(md < 0 & md > -6) %>%
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
nRT <- dBdat_flipped %>%
  filter(md < -6 & md > -12 & device == "retinalogik") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

nHFA <- dBdat_flipped %>%
  filter(md < -6 & md > -12 & device == "hfa") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

meanSensitivity <- dBdat_flipped %>%
  filter(md < -6 & md > -12) %>%
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
nRT <- dBdat_flipped %>%
  filter(md < -12 & device == "retinalogik") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

nHFA <- dBdat_flipped %>%
  filter(md < -12 & device == "hfa") %>%
  distinct(id, device, visit, eye) %>%
  n_distinct()

meanSensitivity <- dBdat_flipped %>%
  filter(md < -12) %>%
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
bias <- mean(df$retinalogik) - mean(df$hfa)

print(blandplot +
        geom_smooth(method = "lm", se = FALSE) +
        # geom_point(position = "jitter") +
        geom_hline(yintercept = 0, color = "black") +
        geom_hline(yintercept = bias, color = "red", linetype = "solid", size = 1) +
        xlab("dB") +
        ylab("Difference in dB (Retinalogik-HFA)") +
        labs(subtitle=paste("Bias =", round(bias, 2), "dB")) +
        ggtitle("Bland-Altman plot for all locations"))

# Fig 2 Hexbin Plots (polynomial line through Hexbin)
df %>%
  ggplot(aes(x = df$hfa, y = df$retinalogik)) +
  geom_hex(bins = 20) +
  scale_fill_viridis_c() +
  geom_smooth(method = "loess", se = TRUE, colour = "red") +
  labs(title="Hexbin plot of threshold sensitivities for all locations",
       subtitle="(Size of bin = 20)") +
  xlab("HFA (dB)") +
  ylab("Retinalogik (dB)") +
  theme_bw()

# Fig 3. Scotoma Analysis Plots…


