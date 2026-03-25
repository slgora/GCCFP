########################## Global Food Plant Occurrence Complementarity Maps ##########################
# Author: Sarah Gora
# Date: 2025_09_09
# Updated: 2026_02_26
# Description:
#   - Generates overlays for 20km grid: occurrence and species richness heatmaps
#   - Overlays: Botanic Garden, Genebank, and both, using buffered polygons (10km radius)
#   - Output: PNG to specified path, color scales matching playground
#   - Fix 1: new_scale("fill") instead of new_scale_fill() [ggplot2 4.0.x compat]
#   - Fix 2: inst_type as real column on buffer sf objects, not hardcoded string in aes()
#   - Fix 3: show.legend = "polygon" removed [breaks guide processing in ggplot2 4.0.x]


# --- Libraries ---
library(dplyr)
library(readxl)
library(readr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(stringr)
library(scales)
library(grid)
library(cowplot)
library(ggnewscale)

# --- Parameters & Paths ---
output_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/FINAL_2026-03-20/Sp_richness_with_gb_bg_pts_maps"

# ------------------------------------------------------------------------------
# --- Data Loading ---
# ------------------------------------------------------------------------------

# occurrences: 5,489,253 rows
occurrences_data <- read_csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_occurrences_dataset_2026-03-13.csv')

# Note use: wcfp_name_match instead of genus_species

# ------------------------------------------------------------------------------
# --- Spatial Data Preparation ---
# ------------------------------------------------------------------------------

# 1. World land polygons (exclude Antarctica)
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")
world_4326 <- st_transform(world, 4326)

# 2. Convert occurrences to sf
occurrences_sf <- st_as_sf(
  occurrences_data,
  coords = c("longitude", "latitude"),
  crs = 4326, remove = FALSE
)

# 3. Filter to land only
occurrences_land <- st_join(occurrences_sf, world_4326, join = st_within, left = FALSE)

# 4. Grid / heatmap creation function
make_grid_and_tables <- function(occurrences_land, crs_equalarea, cell_size) {
  occurrences_proj <- st_transform(occurrences_land, crs = crs_equalarea)
  grid    <- st_make_grid(occurrences_proj, cellsize = c(cell_size, cell_size), what = "polygons")
  grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)
  
  point_grid_join <- st_join(occurrences_proj, grid_sf, left = FALSE)
  
  density_table <- point_grid_join %>%
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    tally(name = "count")
  
  richness_table <- point_grid_join %>%
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    summarise(richness = n_distinct(wcfp_name_match))
  
  grid_with_counts <- left_join(grid_sf, density_table, by = "grid_id")
  grid_with_counts$count[is.na(grid_with_counts$count)] <- 0
  
  grid_with_richness <- left_join(grid_sf, richness_table, by = "grid_id")
  grid_with_richness$richness[is.na(grid_with_richness$richness)] <- 0
  
  list(
    grid_sf            = grid_sf,
    grid_with_counts   = grid_with_counts,
    grid_with_richness = grid_with_richness
  )
}

# 5. Filter occurrence points by institution type
bg_points <- occurrences_land %>% filter(inst_type == "Botanic garden", data_source != "GBIF_observations")
gb_points <- occurrences_land %>% filter(inst_type == "Genebank")

# ------------------------------------------------------------------------------
# --- 20km Grid, Buffered Overlays ---
# ------------------------------------------------------------------------------

crs_equalarea <- 8857
cell_size     <- 20000    #20 x 20km grid cell

# 1. Build grid tables
grid_tables        <- make_grid_and_tables(occurrences_land, crs_equalarea, cell_size)
grid_with_counts   <- grid_tables$grid_with_counts
grid_with_richness <- grid_tables$grid_with_richness

# 2. Mask grid to land
world_proj         <- st_transform(world_4326, crs = crs_equalarea)
grid_land          <- st_filter(grid_with_counts,   world_proj)
grid_richness_land <- st_filter(grid_with_richness, world_proj)
world_plot         <- world_proj


# 3. Project, buffer, and add inst_type column to overlay points
pt_size <- 0.002                                                                 # <<<<<< added 
buffer_size   <- cell_size / 2  # 10,000m or 10km = 1/2 grid size

bg_points_proj <- st_transform(bg_points, crs = crs_equalarea)
gb_points_proj <- st_transform(gb_points, crs = crs_equalarea)

bg_points_buf  <- st_buffer(bg_points_proj, dist = buffer_size)
gb_points_buf  <- st_buffer(gb_points_proj, dist = buffer_size)

# FIX 2: inst_type must be a real column on the sf object for aes(fill = inst_type)   # <<<< added
bg_points_buf$inst_type <- "Botanic garden"
gb_points_buf$inst_type <- "Genebank"

# 4. Project grid for plotting
grid_land_proj          <- st_transform(grid_land,          crs = crs_equalarea)
grid_richness_land_proj <- st_transform(grid_richness_land, crs = crs_equalarea)

# ------------------------------------------------------------------------------
# --- Summary Stats ---
# ------------------------------------------------------------------------------

cat("Total mapped occurrences:", nrow(occurrences_land), "\n") # 5,094,982 

cat("Total Genebank points:", nrow(gb_points), "\n")  # 1,351,603 
cat("Unique Genebank points:", length(unique(gb_points$geometry)), "\n") # 368,921

cat("Total Botanic Garden points:", nrow(bg_points), "\n") # 13,550
cat("Unique Botanic Garden points:", length(unique(bg_points$geometry)), "\n") #8,410

cat("Max occurrence density (20km cell):", max(grid_land_proj$count,    na.rm = TRUE), "\n") # 17,111
cat("Max species richness (20km cell):",   max(grid_richness_land_proj$richness, na.rm = TRUE), "\n") # 1,366

# ------------------------------------------------------------------------------
# --- Helper Functions ---
# ------------------------------------------------------------------------------

label_comma_over_10000 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (abs(v) > 9999) scales::comma(v) else as.character(v)
  })
}

add_graticule_breaks <- function(
    lon_breaks = seq(-180, 180, by = 60),
    lat_breaks = seq(-60,   80, by = 20)
) {
  list(
    scale_x_continuous(breaks = lon_breaks),
    scale_y_continuous(breaks = lat_breaks)
  )
}

manual_graticule_theme <- function() {
  theme(
    axis.title       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
    panel.grid.minor = element_blank()
  )
}

manual_graticule_coord <- function() {
  coord_sf()
}

add_manual_graticule_labels <- function(
    lon_breaks    = seq(-180, 180, by = 60),
    lat_breaks    = seq(-60,   80, by = 20),
    text_size     = 3,
    text_color    = "grey35",
    lon_label_lat = -80,
    lat_label_lon = -179.5,
    lat_nudge_x   = -200000,
    lon_nudge_y   = -200000
) {
  lon_pts <- sf::st_as_sf(
    data.frame(lon = lon_breaks, lat = lon_label_lat),
    coords = c("lon", "lat"), crs = 4326
  )
  lon_pts$lab <- paste0(
    abs(lon_breaks), "\u00B0",
    ifelse(lon_breaks < 0, "W", ifelse(lon_breaks > 0, "E", ""))
  )
  lon_pts$lab[lon_breaks == 0] <- "0\u00B0"
  
  lat_pts <- sf::st_as_sf(
    data.frame(lon = lat_label_lon, lat = lat_breaks),
    coords = c("lon", "lat"), crs = 4326
  )
  lat_pts$lab <- paste0(
    abs(lat_breaks), "\u00B0",
    ifelse(lat_breaks < 0, "S", ifelse(lat_breaks > 0, "N", ""))
  )
  lat_pts$lab[lat_breaks == 0] <- "0\u00B0"
  
  list(
    geom_sf_text(
      data = lon_pts, aes(label = lab),
      size = text_size, color = text_color, vjust = 0, nudge_y = lon_nudge_y
    ),
    geom_sf_text(
      data = lat_pts, aes(label = lab),
      size = text_size, color = text_color, hjust = 1, nudge_x = lat_nudge_x
    )
  )
}


# UPDATED SAVE_MAP_BOTH
#save_map_both <- function(p, filename, output_dir,
#                          width = 14, height = 7, dpi = 600,
#                          fill_legend_position = c(0.18, 0.40)) {
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Bottom legend: institution type only
p_for_color_legend <- p +
  guides(
    fill   = "none",
    colour = guide_legend(
      direction      = "horizontal",
      title.position = "left",
      title.hjust    = 0.5,
      label.position = "right",
      override.aes   = list(shape = 16, size = 3, alpha = 1)
    )
  ) +
  theme(
    legend.position       = "bottom",
    legend.box            = "horizontal",
    legend.background     = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    legend.margin         = margin(6, 6, 6, 6)
  )
color_legend <- cowplot::get_legend(p_for_color_legend)

# Main plot: heatmap fill legend inset only
p_main <- p +
  guides(
    colour = "none",
    fill   = guide_colorbar(
      direction      = "vertical",
      title.position = "top",
      barheight      = unit(38, "mm"),
      barwidth       = unit(6,  "mm")
    )
  ) +
  theme(
    legend.position       = fill_legend_position,
    legend.justification  = c(0.5, 0.5),
    legend.background     = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    legend.margin         = margin(6, 6, 6, 6)
  )

combined <- cowplot::plot_grid(
  p_main, color_legend,
  ncol        = 1,
  rel_heights = c(1, 0.11),
  align       = "v"
) 
# Save high-resolution PNG
png_path <- file.path(output_dir, paste0(filename, ".png"))
ggsave(
  filename = png_path,
  plot     = combined,
  width    = width,
  height   = height,
  dpi      = dpi,
  bg       = "white")
cat(sprintf("Saved: %s.png\n", filename))

# Save PDF (vector, cairo_pdf for font/transparency support)
pdf_path <- file.path(output_dir, paste0(filename, ".pdf"))
ggsave(
  filename = pdf_path,
  plot     = combined,
  width    = width,
  height   = height,
  device   = cairo_pdf,
  bg       = "white")
cat(sprintf("Saved: %s.pdf\n", filename))
}




# UPDATED LEGEND PLACEMENT
save_map_both <- function(p, filename, output_dir,
                          width = 14, height = 7, dpi = 600) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # 1. Main map — NO legends
  p_main <- p +
    guides(fill = "none", colour = "none") +
    theme(legend.position = "none")
  
  # 2. Extract fill/colorbar legend
  p_fill_legend <- p +
    guides(
      fill = guide_colorbar(
        direction      = "vertical",
        title.position = "top",
        barheight      = unit(38, "mm"),
        barwidth       = unit(6,  "mm")
      ),
      colour = "none"
    ) +
    theme(legend.position = "left")
  fill_legend_grob <- cowplot::get_legend(p_fill_legend)
  
  # 3. Extract institution legend
  p_inst_legend <- p +
    guides(
      fill   = "none",
      colour = guide_legend(
        direction      = "vertical",
        title.position = "top",
        label.position = "right",
        override.aes   = list(shape = 16, size = 4, alpha = 1)
      )
    ) +
    theme(legend.position = "left")
  inst_legend_grob <- cowplot::get_legend(p_inst_legend)
  
  # 4. Place legends
  final_map <- cowplot::ggdraw() +
    cowplot::draw_plot(p_main, 0, 0, 1, 1) +
    cowplot::draw_plot(
      fill_legend_grob,
      x = 0.13, y = 0.34,
      width = 0.09, height = 0.28
    ) +
    cowplot::draw_plot(
      inst_legend_grob,
      x = 0.10, 
      #y = 0.26,   # << moved left?
      y = 0.22,                            #working <<<<<
      width = 0.14, height = 0.09
    )
  
  # Save png
  png_path <- file.path(output_dir, paste0(filename, ".png"))
  ggsave(
    filename = png_path,
    plot     = final_map,
    width    = width,
    height   = height,
    dpi      = dpi,
    bg       = "white")
  cat(sprintf("Saved: %s.png\n", filename))
  
  # Save pdf
  pdf_path <- file.path(output_dir, paste0(filename, ".pdf"))
  ggsave(
    filename = pdf_path,
    plot     = final_map,
    width    = width,
    height   = height,
    device   = cairo_pdf,
    bg       = "white")
  cat(sprintf("Saved: %s.pdf\n", filename))
}

# ------------------------------------------------------------------------------
# --- Map Definitions, SINGLE ---
# ------------------------------------------------------------------------------

# UPDATE: LAYER ORDER SWITCHED, BOTANIC GARDEN POINTS ARE NOW ON TOP OF GENEBANK POINTS FOR VISIBILITY

map_definitions <- list(
  list(
    type       = "richness",
    overlays   = list(
      list(name = c("Genebank"),      
           data = list(gb_points_buf), 
           color = c("orange"),      
           file = "FigSX.Speciesrichness-20kmgrid_genebank-pts-10kmbuffer_map")
    ),
    fill_aes   = aes(fill = richness),
    fill_title = "Number of species",
    grid_data  = grid_richness_land_proj ))



map_definitions <- list(
  list(
    type       = "richness",
    overlays   = list(
      list(name = c("Genebank", "Botanic garden"),      
           data = list(gb_points_buf, bg_points_buf), 
           color = c("orange", "purple"),      
           file = "FigSX.Speciesrichness-20kmgrid_botanicgarden-genebank-pts-10kmbuffer_map")
    ),
    fill_aes   = aes(fill = richness),
    fill_title = "Number of species",
    grid_data  = grid_richness_land_proj ))


# ------------------------------------------------------------------------------
# --- Map Definitions, ALL ---
# ------------------------------------------------------------------------------

map_definitions <- list(
  list(
    type       = "richness",
    overlays   = list(
      list(name = "Botanic garden",                     data = bg_points_buf,                    color = "purple",                       file = "map06_species_richness_bg_pts_20km"),
      list(name = "Genebank",                           data = gb_points_buf,                    color = "orange",                    file = "map07_species_richness_gb_pts_20km"),
      list(name = c("Botanic garden", "Genebank"),      data = list(bg_points_buf, gb_points_buf), color = c("purple", "orange"),      file = "map08_species_richness_bg_gb_pts_20km")
    ),
    fill_aes   = aes(fill = richness),
    fill_title = "Number of species",
    grid_data  = grid_richness_land_proj
  ),
  list(
    type       = "occurrence",
    overlays   = list(
      list(name = "Botanic garden",                     data = bg_points_buf,                    color = "purple",                       file = "map03_occ_density_bg_pts_20km"),
      list(name = "Genebank",                           data = gb_points_buf,                    color = "orange",                    file = "map04_occ_density_gb_pts_20km"),
      list(name = c("Botanic garden", "Genebank"),      data = list(bg_points_buf, gb_points_buf), color = c("purple", "orange"),      file = "map05_occ_density_bg_gb_pts_20km")
    ),
    fill_aes   = aes(fill = count),
    fill_title = "Number of occurrences",
    grid_data  = grid_land_proj
  )
)

lon_breaks <- seq(-180, 180, by = 60)
lat_breaks <- seq(-60,   80, by = 20)

# ------------------------------------------------------------------------------
# --- Map Generation Loop ---
# ------------------------------------------------------------------------------


# UPDATED
#for (map_def in map_definitions) {
for (overlay in map_def$overlays) {
  
  # ---- Base map and heatmap fill scale ----
  p <- ggplot() +
    geom_sf(data = world_plot, fill = "grey98", color = NA) +
    geom_sf(data = map_def$grid_data, mapping = map_def$fill_aes, color = NA) +
    scale_fill_gradient(
      low      = "lightgreen",
      high     = "darkgreen",
      na.value = "white",
      name     = map_def$fill_title,
      labels   = label_comma_over_10000
    ) +
    geom_sf(data = world_plot, fill = NA, color = "grey80", linewidth = 0.2) +
    new_scale("colour")   # Institution overlays use 'colour' not 'fill'!
  
  is_overlay       <- is.list(overlay$data) || !is.null(overlay$inst_type)
  has_two_overlays <- is.list(overlay$data) && length(overlay$data) == 2
  
  if (has_two_overlays) {
    name1 <- overlay$name[1]; col1 <- overlay$color[1]
    name2 <- overlay$name[2]; col2 <- overlay$color[2]
    inst_types  <- c(name1, name2)
    inst_colors <- c(col1, col2)
    # Buffered Inst polygons: don't show in legend
    p <- p +
      geom_sf(data = overlay$data[[1]], aes(colour = inst_type), fill = NA, alpha = 0.5, show.legend = FALSE) +
      geom_sf(data = overlay$data[[2]], aes(colour = inst_type), fill = NA, alpha = 0.5, show.legend = FALSE)
    # Dummy points for legend only
    df_legend <- data.frame(inst_type = inst_types, lon = Inf, lat = Inf)
    p <- p +
      geom_point(
        data = df_legend, aes(x = lon, y = lat, colour = inst_type),
        shape = 16, size = 3, inherit.aes = FALSE, show.legend = TRUE
      ) +
      scale_colour_manual(
        name   = "Institution type",
        values = setNames(inst_colors, inst_types)
      )
  } else if (is_overlay) {
    inst_name  <- overlay$name
    inst_color <- overlay$color
    p <- p +
      geom_sf(data = overlay$data, aes(colour = inst_type), fill = NA, alpha = 0.5, show.legend = FALSE)
    # Dummy point for legend only
    df_legend <- data.frame(inst_type = inst_name, lon = Inf, lat = Inf)
    p <- p +
      geom_point(
        data = df_legend, aes(x = lon, y = lat, colour = inst_type),
        shape = 16, size = 3, inherit.aes = FALSE, show.legend = TRUE
      ) +
      scale_colour_manual(
        name   = "Institution type",
        values = setNames(inst_color, inst_name)
      )
  }
  
  # ---- Shared theme & graticule ----
  p <- p +
    labs(title = NULL) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white", color = NA)) +
    add_graticule_breaks(lon_breaks = lon_breaks, lat_breaks = lat_breaks) +
    manual_graticule_theme() +
    manual_graticule_coord() +
    add_manual_graticule_labels(lon_breaks = lon_breaks, lat_breaks = lat_breaks)
  
  # ---- Save with correct legend logic ----
  save_map_both(p, overlay$file, output_dir)
}
}


# UPDATED WITH LEGEND *****
for (map_def in map_definitions) {
  for (overlay in map_def$overlays) {
    
    # ---- Base map and heatmap fill scale ----
    p <- ggplot() +
      geom_sf(data = world_plot, fill = "grey98", color = NA) +
      geom_sf(data = map_def$grid_data, mapping = map_def$fill_aes, color = NA) +
      scale_fill_gradient(
        low      = "lightgreen",
        high     = "darkgreen",
        na.value = "white",
        name     = map_def$fill_title,
        labels   = label_comma_over_10000
      ) +
      geom_sf(data = world_plot, fill = NA, color = "grey80", linewidth = 0.2) +
      new_scale("colour")   # Institution overlays use 'colour' not 'fill'!
    
    is_overlay       <- is.list(overlay$data) || !is.null(overlay$inst_type)
    has_two_overlays <- is.list(overlay$data) && length(overlay$data) == 2
    
    if (has_two_overlays) {
      name1 <- overlay$name[1]; col1 <- overlay$color[1]
      name2 <- overlay$name[2]; col2 <- overlay$color[2]
      inst_types  <- c(name1, name2)
      inst_colors <- c(col1, col2)
      # Buffered Inst polygons: don't show in legend
      p <- p +
        geom_sf(data = overlay$data[[1]], aes(colour = inst_type), fill = NA, alpha = 0.5, show.legend = FALSE) +
        geom_sf(data = overlay$data[[2]], aes(colour = inst_type), fill = NA, alpha = 0.5, show.legend = FALSE)
      # Dummy points for legend only
      df_legend <- data.frame(inst_type = inst_types, lon = Inf, lat = Inf)
      p <- p +
        geom_point(
          data = df_legend, aes(x = lon, y = lat, colour = inst_type),
          shape = 16, size = 3, inherit.aes = FALSE, show.legend = TRUE
        ) +
        scale_colour_manual(
          name   = "Institution type",
          values = setNames(inst_colors, inst_types)
        )
    } else if (is_overlay) {
      inst_name  <- overlay$name
      inst_color <- overlay$color
      # FIX: Extract sf object from single-element list if needed
      inst_sf <- overlay$data
      if (is.list(inst_sf) && length(inst_sf) == 1) inst_sf <- inst_sf[[1]]
      p <- p +
        geom_sf(data = inst_sf, aes(colour = inst_type), fill = NA, alpha = 0.5, show.legend = FALSE)
      # Dummy point for legend only
      df_legend <- data.frame(inst_type = inst_name, lon = Inf, lat = Inf)
      p <- p +
        geom_point(
          data = df_legend, aes(x = lon, y = lat, colour = inst_type),
          shape = 16, size = 3, inherit.aes = FALSE, show.legend = TRUE
        ) +
        scale_colour_manual(
          name   = "Institution type",
          values = setNames(inst_color, inst_name)
        )
    }
    
    # ---- Shared theme & graticule ----
    p <- p +
      labs(title = NULL) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white", color = NA)) +
      add_graticule_breaks(lon_breaks = lon_breaks, lat_breaks = lat_breaks) +
      manual_graticule_theme() +
      manual_graticule_coord() +
      add_manual_graticule_labels(lon_breaks = lon_breaks, lat_breaks = lat_breaks)
    
    # ---- Save with correct legend logic ----
    save_map_both(p, overlay$file, output_dir)
  }
}







# --- Crop white space from saved PNGs and PDFs ---
# Run this after Script has saved all maps

# --- Libraries ---
library(magick)   # install.packages("magick")

# --- Parameters ---
output_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/FINAL_2026-03-20/Sp_richness_with_gb_bg_pts_maps"

# Fuzz: how aggressively to trim near-white pixels (0-100%)
fuzz <- 2

# Border: white padding (in pixels) added back after trimming
# Increase for more breathing room around the map
border_px <- 20

# ------------------------------------------------------------------------------
# PNG cropping
# ------------------------------------------------------------------------------
png_files <- list.files(output_dir, pattern = "\\.png$", full.names = TRUE)

cat("--- Cropping PNGs ---\n")
for (f in png_files) {
  img         <- image_read(f)
  img_trimmed <- image_trim(img, fuzz = fuzz)
  img_padded  <- image_border(img_trimmed, color = "white",
                              geometry = paste0(border_px, "x", border_px))
  image_write(img_padded, path = f)
  info <- image_info(img_padded)
  cat(sprintf("Cropped: %s  [%d x %d px]\n", basename(f), info$width, info$height))
}

# ------------------------------------------------------------------------------
# PDF cropping
# ------------------------------------------------------------------------------
pdf_files <- list.files(output_dir, pattern = "\\.pdf$", full.names = TRUE)

cat("\n--- Cropping PDFs ---\n")
for (f in pdf_files) {
  img         <- image_read_pdf(f, density = 300)
  img_trimmed <- image_trim(img, fuzz = fuzz)
  img_padded  <- image_border(img_trimmed, color = "white",
                              geometry = paste0(border_px, "x", border_px))
  image_write(img_padded, path = f, format = "pdf")
  cat(sprintf("Cropped: %s\n", basename(f)))
}

cat("\nDone. All files cropped in place.\n")














################################################################################
################## OLD FUNTIONS BELOW ##########################################

# OLD SAVE_MAP_BOTH FUNCTION
save_map_both <- function(p, filename, output_dir,
                          width = 14, height = 7, dpi = 300,
                          fill_legend_position = c(0.18, 0.40)) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Bottom legend: institution type only
  p_for_color_legend <- p +
    guides(
      fill   = "none",
      colour = guide_legend(
        direction      = "horizontal",
        title.position = "left",
        title.hjust    = 0.5,
        label.position = "right",
        override.aes   = list(size = 3, alpha = 1)
      )
    ) +
    theme(
      legend.position       = "bottom",
      legend.box            = "horizontal",
      legend.background     = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      legend.margin         = margin(6, 6, 6, 6)
    )
  color_legend <- cowplot::get_legend(p_for_color_legend)
  
  # Main plot: heatmap fill legend inset only
  p_main <- p +
    guides(
      colour = "none",
      fill   = guide_colorbar(
        direction      = "vertical",
        title.position = "top",
        barheight      = unit(38, "mm"),
        barwidth       = unit(6,  "mm")
      )
    ) +
    theme(
      legend.position       = fill_legend_position,
      legend.justification  = c(0.5, 0.5),
      legend.background     = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      legend.margin         = margin(6, 6, 6, 6)
    )
  
  combined <- cowplot::plot_grid(
    p_main, color_legend,
    ncol        = 1,
    rel_heights = c(1, 0.11),
    align       = "v"
  )
  
  ggsave(
    filename = file.path(output_dir, paste0(filename, ".png")),
    plot     = combined,
    width    = width, height = height, dpi = dpi,
    bg       = "white"
  )
}


# OLD LOOP 1. HAS CORRECT POINTS BUT NOT CORRECT LEGEND
for (map_def in map_definitions) {
  for (overlay in map_def$overlays) {
    
    # Base map + heatmap fill scale
    p <- ggplot() +
      geom_sf(data = world_plot, fill = "grey98", color = NA) +
      geom_sf(data = map_def$grid_data, mapping = map_def$fill_aes, color = NA) +
      scale_fill_gradient(
        low      = "lightgreen",
        high     = "darkgreen",
        na.value = "white",
        name     = map_def$fill_title,
        labels   = label_comma_over_10000
      ) +
      geom_sf(data = world_plot, fill = NA, color = "grey80", linewidth = 0.2) +
      
      # FIX 1: reset fill scale for institution overlays
      new_scale("fill")
    
    # Overlay institution polygons with new fill scale
    if (is.list(overlay$data) && length(overlay$data) == 2) {
      # Dual overlay — freeze loop vars locally
      name1 <- overlay$name[1];  col1 <- overlay$color[1]
      name2 <- overlay$name[2];  col2 <- overlay$color[2]
      
      p <- p +
        geom_sf(data = overlay$data[[1]], aes(fill = inst_type),   # FIX 2: real column
                color = NA, alpha = 0.5) +                          # FIX 3: no show.legend
        geom_sf(data = overlay$data[[2]], aes(fill = inst_type),
                color = NA, alpha = 0.5) +
        scale_fill_manual(
          name   = "Institution type",
          values = c(setNames(col1, name1), setNames(col2, name2))
        )
    } else {
      inst_name  <- overlay$name
      inst_color <- overlay$color
      
      p <- p +
        geom_sf(data = overlay$data, aes(fill = inst_type),        # FIX 2: real column
                color = NA, alpha = 0.5) +                          # FIX 3: no show.legend
        scale_fill_manual(
          name   = "Institution type",
          values = setNames(inst_color, inst_name)
        )
    }
    
    # Shared theme + graticule
    p <- p +
      labs(title = NULL) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white", color = NA)) +
      add_graticule_breaks(lon_breaks = lon_breaks, lat_breaks = lat_breaks) +
      manual_graticule_theme() +
      manual_graticule_coord() +
      add_manual_graticule_labels(lon_breaks = lon_breaks, lat_breaks = lat_breaks)
    
    save_map_both(p, overlay$file, output_dir)
    cat(sprintf("Saved: %s.png\n", overlay$file))
  }
}

# OLD LOOP 2. HAS CORRRECT LEGEND BUT NOT CORRECT POINTS
for (map_def in map_definitions) {
  for (overlay in map_def$overlays) {
    
    p <- ggplot() +
      geom_sf(data = world_plot, fill = "grey98", color = NA) +
      geom_sf(data = map_def$grid_data, mapping = map_def$fill_aes, color = NA) +
      geom_sf(data = world_plot, fill = NA, color = "grey80", linewidth = 0.2)
    
    if (is.list(overlay$data) && length(overlay$data) == 2) {
      p <- p +
        geom_sf(data = overlay$data[[1]], aes(color = overlay$name[1]),
                size = pt_size, alpha = 0.4, show.legend = "point") +
        geom_sf(data = overlay$data[[2]], aes(color = overlay$name[2]),
                size = pt_size, alpha = 0.4, show.legend = "point") +
        scale_color_manual(
          name = "Institution type",
          values = setNames(overlay$color, overlay$name),
          guide = guide_legend(override.aes = list(size = 3, alpha = 1))
        )
    } else {
      p <- p +
        geom_sf(data = overlay$data, aes(color = overlay$name),
                size = pt_size, alpha = 0.4, show.legend = "point") +
        scale_color_manual(
          name = "Institution type",
          values = setNames(overlay$color, overlay$name),
          guide = guide_legend(override.aes = list(size = 3, alpha = 1))
        )
    }
    
    p <- p +
      scale_fill_gradient(
        low = "lightgreen", high = "darkgreen", na.value = "white",
        name = map_def$fill_title,
        labels = label_comma_over_10000
      ) +
      labs(title = NULL, fill = map_def$fill_title) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white", color = NA)) +
      add_graticule_breaks(lon_breaks = lon_breaks, lat_breaks = lat_breaks) +
      manual_graticule_theme() +
      manual_graticule_coord() +
      add_manual_graticule_labels(lon_breaks = lon_breaks, lat_breaks = lat_breaks)
    
    save_map_both(p, overlay$file, output_dir)
    cat(sprintf("Saved: %s.png\n", overlay$file))
  }
}



# END SCRIPT #

