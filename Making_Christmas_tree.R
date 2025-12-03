library(ggplot2)
library(dplyr)
library(tibble)
set.seed(1225) # Seed for reproducibility (Christmas date!)
n_points_tree <- 40000
n_points_trunk <- 5000 

# Function to generate the foliage (Cone shape distribution)
### We generate points where the spread of X depends on the height of Y
generate_foliage <- function(n) {
  ### Y values: height of the tree (0 to 10)
  y <- runif(n, min = 0, max = 10)
  ### X values: wider at bottom, narrow at top (Cone shape)
  ### Width decreases as Y increases. We add Gaussian noise (rnorm) to make it look organic (like cells)
  width_factor <- (10 - y) / 10 
  x <- rnorm(n, mean = 0, sd = width_factor * 1.5)
  return(tibble(UMAP_1 = x, UMAP_2 = y, type = "Foliage"))
}

# --- Create Foliage ---
tree_data <- generate_foliage(n_points_tree)

# --- Create Trunk (Rectangular cluster) ---
trunk_data <- tibble(
  UMAP_1 = runif(n_points_trunk, min = -0.5, max = 0.5) + rnorm(n_points_trunk, 0, 0.1),
  UMAP_2 = runif(n_points_trunk, min = -2, max = 0) + rnorm(n_points_trunk, 0, 0.1),
  type = "Trunk"
)

# --- Create Star (Dense cluster at the top) ---
star_data <- tibble(
  UMAP_1 = rnorm(700, mean = 0, sd = 0.25),
  UMAP_2 = rnorm(700, mean = 10.5, sd = 0.25),
  type = "Star"
)

# Add "Ornaments" 
# We randomly select points from the foliage and change their identity
tree_data <- tree_data %>%
  mutate(type = case_when(
    type == "Foliage" & runif(n()) > 0.87 ~ sample(c("Red Ball", "Gold Ball", "Blue Ball"), n(), replace = TRUE),
    TRUE ~ type
  ))

# Combine all datasets
final_data <- bind_rows(tree_data, trunk_data, star_data)

# Define custom colors to match the Christmas theme
cluster_colors <- c(
  "Foliage"   = "#228B22",   # Forest Green
  "Trunk"     = "#8B4513",   # Saddle Brown
  "Star"      = "#FFD700",   # Gold
  "Red Ball"  = "#FF0000",   # Red
  "Gold Ball" = "#FFA500",   # Orange/Gold
  "Blue Ball" = "#1E90FF"    # Dodger Blue
)

# Plot using ggplot2
p <- ggplot(final_data, aes(x = UMAP_1, y = UMAP_2, color = type)) +
  # Use small point size and slight transparency for that "single-cell" look
  geom_point(size = 1.2, alpha = 0.8, stroke = 0) + 
  scale_color_manual(values = cluster_colors) +
 theme_void() + 
  theme(
    plot.background = element_rect(fill = "white", color = NA), 
    legend.position = "none", 
    plot.title = element_text(color = "black", hjust = 0.5, size = 30, face = "bold"),
    plot.subtitle = element_text(color = "#9b4646", hjust = 0.5, size = 20),
    axis.title = element_text(color = "black", size = 12, face = "bold"),
    axis.text = element_text(color = "black", size = 15),        # Sets the number size
    axis.ticks = element_line(color = "black", linewidth = 0.5), # Sets the tick mark thickness
    axis.ticks.length = unit(0.5, "cm"),                         # Sets how long the ticks are
    # Axis Lines
    axis.line = element_line(
      color = "black", 
      linewidth = 2, 
      arrow = arrow(length = unit(0.3, "cm"), type = "closed")
    ),
    axis.title.y = element_text(angle = 90, vjust = 1)
  
  ) +
  labs(
    title = "OncOmics\n\nscRNA-seq Tree",
    subtitle = "n_cells = 45700 | Cluster: Merry Christmas",
    x = "umap_1",
    y = "umap_2")

ggsave(plot = p, filename = "tree.png", path = "/media/hd/pimenta2/natal_decoracao",
  width = 9, 
  height = 10, dpi = 300)
