# Script to create blur-spatial-uncertainty.png

# Create a simple 3x3 grid using sf
grid_size <- 1  # Define cell size
grid <- expand.grid(x = 1:3, y = 1:3) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_make_grid(cellsize = grid_size, n = c(3, 3), what = "polygons") %>%
  st_sf() %>%
  mutate(id = seq_len(n()))

centroids <- st_centroid(grid) %>%
  st_buffer(50000)

# Create data for estimates and uncertainty
data <- expand.grid(
  x = 1:3 + 0.5,  # Columns for estimate values
  y = 1:3 + 0.5   # Rows for uncertainty levels
) %>%
  mutate(
    # Low, Medium, High estimates
    estimate = rep(c(0.2, 0.5, 0.8), each = 3),
    # Low, Medium, High uncertainty
    uncertainty = rep(c("Low", "Medium", "High"), times = 3)
  )

# Assign visual properties for uncertainty representation
data <- data %>%
  mutate(
    blur_size = case_when(
      uncertainty == "Low" ~ NA,
      uncertainty == "Medium" ~ 25,
      uncertainty == "High" ~ 30
    ),
    blur_size2 = case_when(
      uncertainty == "Low" ~ NA,
      uncertainty == "Medium" ~ 15,
      uncertainty == "High" ~ 30
    ),
    alpha_value = case_when(
      uncertainty == "Low" ~ 1,
      uncertainty == "Medium" ~ 0.6,
      uncertainty == "High" ~ 0.3
    ),
    size = case_when(
      uncertainty == "Low" ~ 30,
      uncertainty == "Medium" ~ 15,
      uncertainty == "High" ~ 10
    ),
  )

# Convert data to sf points for plotting
data_sf <- cbind(centroids, data)

# Increasing blur with increasing uncertainty
library(ggblur)  # blurred points: https://github.com/coolbutuseless/ggblur

p_blur <- ggplot() +
  geom_sf(data = grid, color = "black", alpha = 0,
          linewidth = 1) +  # Grid background
  geom_point_blur(
    data = data_sf %>% filter(uncertainty == "High"),
    aes(x = x, y = y, colour = estimate, blur_size = blur_size),
    size = 1
  ) +
  geom_point_blur(
    data = data_sf %>% filter(uncertainty == "Medium"),
    aes(x = x, y = y, colour = estimate, blur_size = blur_size),
    size = 15
  ) +
  geom_point(
    data = data_sf %>% filter(uncertainty == "Low"),
    aes(x = x, y = y, colour = estimate),
    size = 25
  ) +
  scale_colour_viridis_c(option = "D") +
  scale_blur_size_continuous(range = c(10, 25)) +
  theme_minimal() +
  theme(panel.grid = element_line(linewidth = 1)) +
  labs(title = "Blur", x = "", y = "", colour = "Estimate") +
  guides(blur_size = "none") +
  annotate("segment",
           x = 1.5,
           y = 0.8,
           xend = 3.5,
           yend = 0.8,
           linewidth = 1,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")),
           colour = "black"
  ) +
  annotate("segment",
           x = 0.8,
           y = 1.5,
           xend = 0.8,
           yend = 3.5,
           linewidth = 1,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")),
           colour = "black"
  ) +
  annotate("text",
           label = c("Higher uncertainty"),
           x = 2.5,
           y = 0.65,
           size = 5, hjust = "center", vjust = "center", colour = "black"
  ) +
  annotate("text",
           label = c("Higher estimate"),
           x = 0.7,
           y = 2.5,
           size = 5, hjust = "center", vjust = "bottom", colour = "black", angle = 90
  )

# Increasing blur and descreasing size with increasing uncertainty
p_blur2 <- ggplot() +
  geom_sf(data = grid, color = "black", alpha = 0,
          linewidth = 1) +  # Grid background
  geom_point_blur(
    data = data_sf %>% filter(uncertainty == "High"),
    aes(x = x, y = y, colour = estimate, blur_size = blur_size2),
    size = 1
  ) +
  geom_point_blur(
    data = data_sf %>% filter(uncertainty == "Medium"),
    aes(x = x, y = y, colour = estimate, blur_size = blur_size2),
    size = 10
  ) +
  geom_point(
    data = data_sf %>% filter(uncertainty == "Low"),
    aes(x = x, y = y, colour = estimate),
    size = 25
  ) +
  scale_colour_viridis_c(option = "D") +
  scale_blur_size_continuous(range = c(15, 15)) +
  theme_minimal() +
  theme(panel.grid = element_line(linewidth = 1)) +
  labs(title = "Blur + Size", x = "", y = "", colour = "Estimate") +
  guides(blur_size = "none") +
  annotate("segment",
           x = 1.5,
           y = 0.8,
           xend = 3.5,
           yend = 0.8,
           linewidth = 1,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")),
           colour = "black"
  ) +
  annotate("segment",
           x = 0.8,
           y = 1.5,
           xend = 0.8,
           yend = 3.5,
           linewidth = 1,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")),
           colour = "black"
  ) +
  annotate("text",
           label = c("Higher uncertainty"),
           x = 2.5,
           y = 0.65,
           size = 5, hjust = "center", vjust = "center", colour = "black"
  ) +
  annotate("text",
           label = c("Higher estimate"),
           x = 0.7,
           y = 2.5,
           size = 5, hjust = "center", vjust = "bottom", colour = "black", angle = 90
  )

# Increasing transparency with increasing uncertainty
p_transparency <- ggplot() +
  geom_sf(data = grid, color = "black", alpha = 0,
          linewidth = 1) +  # Grid background
  geom_point(data = data_sf,
             aes(x = x, y = y, colour = estimate, alpha = alpha_value),
             size = 25) +
  scale_colour_viridis_c(option = "D") +
  scale_alpha_identity() +
  theme_minimal() +
  theme(panel.grid = element_line(linewidth = 1)) +
  labs(title = "Transparency", x = "", y = "", colour = "Estimate") +
  annotate("segment",
           x = 1.5,
           y = 0.8,
           xend = 3.5,
           yend = 0.8,
           linewidth = 1,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")),
           colour = "black"
  ) +
  annotate("segment",
           x = 0.8,
           y = 1.5,
           xend = 0.8,
           yend = 3.5,
           linewidth = 1,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")),
           colour = "black"
  ) +
  annotate("text",
           label = c("Higher uncertainty"),
           x = 2.5,
           y = 0.65,
           size = 5, hjust = "center", vjust = "center", colour = "black"
  ) +
  annotate("text",
           label = c("Higher estimate"),
           x = 0.7,
           y = 2.5,
           size = 5, hjust = "center", vjust = "bottom", colour = "black", angle = 90
  )

# Increasing transparency and descreasing size with increasing uncertainty
p_transparency2 <- ggplot() +
  geom_sf(data = grid, color = "black", alpha = 0,
          linewidth = 1) +  # Grid background
  geom_point(data = data_sf,
             aes(x = x, y = y, colour = estimate, alpha = alpha_value,
                 size = size)) +
  scale_colour_viridis_c(option = "D") +
  scale_alpha_identity() +
  scale_size_continuous(range = c(10, 25)) +
  theme_minimal() +
  theme(panel.grid = element_line(linewidth = 1)) +
  labs(title = "Transparency + Size", x = "", y = "", colour = "Estimate") +
  guides(size = "none") +
  annotate("segment",
           x = 1.5,
           y = 0.8,
           xend = 3.5,
           yend = 0.8,
           linewidth = 1,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")),
           colour = "black"
  ) +
  annotate("segment",
           x = 0.8,
           y = 1.5,
           xend = 0.8,
           yend = 3.5,
           linewidth = 1,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")),
           colour = "black"
  ) +
  annotate("text",
           label = c("Higher uncertainty"),
           x = 2.5,
           y = 0.65,
           size = 5, hjust = "center", vjust = "center", colour = "black"
  ) +
  annotate("text",
           label = c("Higher estimate"),
           x = 0.7,
           y = 2.5,
           size = 5, hjust = "center", vjust = "bottom", colour = "black", angle = 90
  )

# Create grid plot
library(cowplot)

# Extract the legend from one of the plots with larger key and text sizes
legend <- get_legend(
  p_blur +
    theme(
      legend.position = "right",
      legend.key.height = unit(1.2, "cm"),  # taller keys
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16)
    )
)

# Smaller margins
shrink_margins <- theme(plot.margin = margin(4, 1, 4, 1))

# Remove legends from the individual plots
p_blur_nolegend <- p_blur +
  theme(legend.position = "none") +
  shrink_margins
p_blur2_nolegend <- p_blur2 +
  theme(legend.position = "none") +
  shrink_margins
p_transparency_nolegend <- p_transparency +
  theme(legend.position = "none") +
  shrink_margins
p_transparency2_nolegend <- p_transparency2 +
  theme(legend.position = "none") +
  shrink_margins

# Combine the four panels
plots <- plot_grid(
  p_blur_nolegend, p_blur2_nolegend,
  p_transparency_nolegend, p_transparency2_nolegend,
  labels = c("A.", "B.", "C.", "D."),
  label_size = 18,
  ncol = 2
)

# Combine plots and legend side by side
plot_grid(
  plots, legend,
  rel_widths = c(5, 0.8),  # Adjust as needed for legend size
  nrow = 1
)
