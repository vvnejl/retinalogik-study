# Stats for JAMA Ophthalmology paper
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
        geom_hline(yintercept = bias, color = "red", linetype = "solid", linewidth = 1) +
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
load(here("retinalogik_hfadatawpsd.Rda"))

# remove locations directly above and below blindspots
psdpvalues_hfa <- alldat %>%
  filter(!(x==15 & y==9) & !(x==15 & y==-9))

# remove reliability indices <30%
psdpvalues_hfa <- psdpvalues_hfa %>%
  filter((fl/flTest) < 0.3 & fnPerc < 0.3 & fpPerc < 0.3)

psdpvalues_hfa %>%
  filter(id=="RL24" & eye=="R")

my_data <- psdpvalues_hfa %>%
  filter(datetime==ymd_hms("2025-05-06 14:57:55") & eye=="R")

customcolors <- brewer.pal(4, "YlOrRd")

p <- my_data %>%
  ggplot(aes(x, y, psd_p)) +
  geom_raster(aes(x = x, y = y, fill = factor(psd_p))) +
  geom_text(aes(label = ifelse(is.na(psd_p), "", psd_p),
                x = x, y = y), size = 3) +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = customcolors, na.value="gray") +
  theme_bw() +
  ggtitle("HFA", subtitle = "PSD p-values") +
  theme(legend.position="none")

dBplot <- my_data %>%
  ggplot(aes(x, y, psd_p, dB)) +
  geom_raster(aes(x = x, y = y, fill = factor(psd_p))) +
  geom_text(aes(label = dB, x = x, y = y), size = 3) +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = customcolors, na.value="gray") +
  theme_bw() +
  ggtitle("HFA", subtitle = "Threshold sensitivity dB values") +
  theme(legend.position="none")
  
#library(sf)
library(spdep)
my_sf <- my_data %>%
  select(datetime, eye, id, x, y, dB, psd_p) %>%
  st_as_sf(coords = c("x", "y"), crs = NA)

# You can view the data
#plot(my_sf)

# Convert to sf object
# It's good practice to provide a CRS if your coordinates represent real-world locations.
# For simple relative coordinates, NA is fine.
points_sf <- st_as_sf(my_sf, wkt = "geometry", crs = NA)

# Create a spatial weights matrix based on distance for point data
# d1 = 0: minimum distance (points must be distinct)
# d2 = 9: maximum distance (captures vertical, horizontal, and diagonal neighbors
#         given your coordinate spacing of 6 units)
neighbors <- dnearneigh(points_sf, d1 = 0, d2 = 9)

# Convert the neighbor list to a spatial weights list (binary style is common)
weights_list <- nb2listw(neighbors, style = "B")

# Filter for points where psd_p <= 5
# Using is.finite() to handle NA values in psd_p, as NA <= 5 would also be NA, not FALSE.
filtered_points <- points_sf %>%
  filter(is.finite(psd_p) & psd_p <= 5)

if(nrow(filtered_points) > 0) {
  # Create a spatial weights matrix for the filtered points (re-evaluate neighbors)
  # This ensures we only consider contiguity among the *filtered* points.
  filtered_neighbors <- dnearneigh(filtered_points, d1 = 0, d2 = 9)
  
  # Check if there are any connections among the filtered points
  if (length(unlist(filtered_neighbors)) > 0) { # Check if any neighbors exist
    # Find connected components (contiguous groups)
    components <- n.comp.nb(filtered_neighbors)
    
    # Assign component IDs back to the filtered data
    filtered_points$component_id <- components$comp.id
    
    # Group by component and count points
    contiguous_groups <- filtered_points %>%
      group_by(component_id) %>%
      summarize(n_points = n()) %>%
      arrange(desc(n_points))
    
    # Print the results
    print("Contiguous groups where psd_p <= 5:")
    print(contiguous_groups)
    
    # Find the largest contiguous group
    largest_group_id <- contiguous_groups$component_id[1]
    
    largest_group <- filtered_points %>%
      filter(component_id == largest_group_id)
    
    print("Largest Contiguous Group:")
    print(largest_group)
    
    if (nrow(largest_group) >= 3) {
      print("Largest contiguous group has at least 3 points.")
      # Optionally, extract and print the coordinates of the largest group
      print("Coordinates of the largest contiguous group:")
      print(st_coordinates(largest_group))
    } else {
      print("No contiguous group with at least 3 points found that has at least 3 points.")
    }
  } else {
    print("No contiguous points found after filtering for psd_p <= 5.")
  }
  
} else {
  print("No points found where psd_p <= 5.")
}

# --- Visualization ---

# Plot all original points
# plot_all_points <- ggplot() +
#   geom_sf(data = points_sf, color = "grey", size = 1, alpha = 0.5) +
#   labs(title = "All Original Points (Grey) and Filtered Contiguous Groups (Colored)",
#        x = "X-coordinate", y = "Y-coordinate") +
#   theme_minimal()

plot_all_points <- ggplot() +
  geom_sf(data = points_sf, color = "grey", size = 1, alpha = 0.5) +
  labs(title = "Contiguous Groups (Coloured)",
       x = "x", y = "y") +
  theme_bw()

# Add the filtered and grouped points on top, colored by their component_id
# Use a specific color palette if you have many groups
plot_scotoma <- plot_all_points +
  geom_sf(data = filtered_points, aes(color = as.factor(component_id)), size = 3) +
  scale_color_viridis_d(option = "viridis", name = "Contiguous Group ID") # Viridis is a good perceptually uniform palette

# Highlight the largest contiguous group
# (Optional: you can make these points larger or a different shape)
plot_scotoma <- plot_scotoma +
  geom_sf(data = largest_group, color = "red", size = 5) # Red triangles for the largest group

# Print the plot
#print(plot_all_points)

rttmp <- dBdat %>%
  filter(id=="RL24", visit==1, eye=="R", device=="retinalogik") %>%
  select(id, eye, x, y, dB)

hfatmp <- largest_group %>%
  mutate(x = st_coordinates(.)[, "X"],
         y = st_coordinates(.)[, "Y"]) %>%
  st_drop_geometry() 

mergeddat <- left_join(rttmp, hfatmp, by=c("id","eye","x","y"))

rtplot <- mergeddat %>%
  ggplot(aes(x, y, psd_p, dB.x)) +
  geom_raster(aes(x = x, y = y, fill = factor(psd_p))) +
  geom_text(aes(label = dB.x, x = x, y = y), size = 3) +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = customcolors, na.value="gray") +
  theme_bw() +
  ggtitle("Retinalogik", subtitle = "Threshold sensitivity dB values") +
  theme(legend.position="none")

mergeddat %>%
  ggplot(aes(x, y, psd_p)) +
  geom_raster(aes(x = x, y = y, fill = factor(psd_p))) +
  geom_text(aes(label = psd_p, x = x, y = y), size = 3) +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = customcolors, na.value="gray") +
  theme_bw() +
  ggtitle("Retinalogik", subtitle = "Threshold sensitivity dB values") +
  theme(legend.position="none")

(p + plot_scotoma) / (dBplot + rtplot)
