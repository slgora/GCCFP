########################## Institution Locations Maps ##########################
# Author: Sarah Gora
# Description:
#   - Generates location maps for Genebanks, Botanic Gardens, and both combined
#   - Matches exact format and style of Script 1 (Global Food Plant Occurrence Maps)
#   - Output: High-resolution PNG (600 dpi) and PDF, saved to Script 1 output directory
#   - Point size: 0.8 for all institution points

# --- Libraries ---
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(readxl)
library(scales)
library(grid)
library(cowplot)

# --- Data ----
# Institution/Organization Locations
# Genebank locations from FAO WIEWS
# Botanic garden locations from BGCI

# all org: 22,276
org_geo_data <- read_excel("C:/Users/sarah/Downloads/all_institutions_locations_corrected_2026-03-09.xlsx")

#trim whitespace
org_geo_data <- org_geo_data %>%
  mutate(
    organization_type = str_trim(organization_type),
    latitude          = str_trim(latitude),
    longitude         = str_trim(longitude))

#select only GENEBANKS and BOTANIC GARDENS: 6,836
org_geo_data <- org_geo_data %>%
  filter(organization_type %in% c("Genebank", "Botanic garden"))

#drop organizations with no lat/longs: 4,893
org_geo_data <- org_geo_data %>%
  filter(
    !is.na(latitude) & latitude != "",
    !is.na(longitude) & longitude != "")

#convert lat/long to numeric
org_geo_data <- org_geo_data %>%
  mutate(
    latitude  = as.numeric(latitude),
    longitude = as.numeric(longitude) )

#check for invalid coords, zero invalid coords
org_geo_data_invalid <- org_geo_data %>%
  filter(!between(latitude, -90, 90) | !between(longitude, -180, 180))

#check specific coordinates if something looks off on map
check_org_geo_data_coords <- org_geo_data %>%
  filter(between(latitude, -20, 0) | between(longitude, 0, 60))

check_org_geo_data_coords <- org_geo_data %>%
  filter(between(latitude, 30, 40),
         between(longitude, -90, -60))

# --- Parameters ---
crs_equalarea <- 8857  # Equal Earth Projection
pt_size        <- 0.8  # Point size for all institution locations

output_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/FINAL_2026-03-20/Gb_bg_locations_maps"
#if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- Shared graticule definitions (matching Script 1) ---
lon_breaks <- seq(-180, 180, by = 60)
lat_breaks <- seq(-60,   80, by = 20)

# --- Helper: graticule scale breaks ---
add_graticule_breaks <- function(
    lon_breaks = seq(-180, 180, by = 60),
    lat_breaks = seq(-60, 80, by = 20)
) {
  list(
    scale_x_continuous(breaks = lon_breaks),
    scale_y_continuous(breaks = lat_breaks)
  )
}

# --- Helper: graticule theme (matches Script 1) ---
manual_graticule_theme <- function() {
  theme(
    axis.title       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
    panel.grid.minor = element_blank()
  )
}

# --- Helper: coord_sf ---
manual_graticule_coord <- function() {
  coord_sf()
}

# --- Helper: manual graticule labels (matches Script 1) ---
add_manual_graticule_labels <- function(
    lon_breaks     = seq(-180, 180, by = 60),
    lat_breaks     = seq(-60, 80, by = 20),
    text_size      = 3,
    text_color     = "grey35",
    lon_label_lat  = -80,
    lat_label_lon  = -179.5,
    lat_nudge_x    = -200000,
    lon_nudge_y    = -200000
) {
  lon_pts     <- sf::st_as_sf(
    data.frame(lon = lon_breaks, lat = lon_label_lat),
    coords = c("lon", "lat"), crs = 4326
  )
  lon_pts$lab <- paste0(
    abs(lon_breaks), "\u00B0",
    ifelse(lon_breaks < 0, "W", ifelse(lon_breaks > 0, "E", ""))
  )
  lon_pts$lab[lon_breaks == 0] <- "0\u00B0"
  
  lat_pts     <- sf::st_as_sf(
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
      size = text_size, color = text_color,
      vjust = 0, nudge_y = lon_nudge_y
    ),
    geom_sf_text(
      data = lat_pts, aes(label = lab),
      size = text_size, color = text_color,
      hjust = 1, nudge_x = lat_nudge_x
    )
  )
}

# --- Save function: high-res PNG (600 dpi) + PDF, colour legend at bottom ---
save_map_both <- function(p, filename, output_dir,
                          width = 14, height = 7, dpi = 600) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Extract colour legend for bottom strip
  p_for_color_legend <- p +
    guides(
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
  
  # Main plot: legend suppressed (shown via cowplot strip below)
  p_main <- p +
    guides(colour = "none") +
    theme(legend.position = "none")
  
  combined <- cowplot::plot_grid(
    p_main,
    color_legend,
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
    bg       = "white"
  )
  cat(sprintf("Saved: %s.png\n", filename))
  
  # Save PDF (vector, cairo_pdf for font/transparency support)
  pdf_path <- file.path(output_dir, paste0(filename, ".pdf"))
  ggsave(
    filename = pdf_path,
    plot     = combined,
    width    = width,
    height   = height,
    device   = cairo_pdf,
    bg       = "white"
  )
  cat(sprintf("Saved: %s.pdf\n", filename))
}

# --- Prepare World Data (shared) ---
world      <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")
world_plot <- st_transform(world, crs = crs_equalarea)

# ==============================================================================
# MAP 1 — Genebank Locations
# ==============================================================================

# filter org data for genebanks
org_geo_data_gb <- org_geo_data %>%
  filter(organization_type == "Genebank")

# --- Summary stats ---
cat("--- Map 1: Genebank Locations ---\n")
cat("Number of genebanks mapped:", nrow(org_geo_data_gb), "\n\n") # 1,421

org_geo_sf_gb   <- st_as_sf(
  org_geo_data_gb,
  coords = c("longitude", "latitude"),
  crs    = 4326,
  remove = FALSE
)
org_geo_proj_gb <- st_transform(org_geo_sf_gb, crs = crs_equalarea)



############# save map both updated #####3

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
      y = 0.40,                            #working <<<<<
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

# -----------

p_map01 <- ggplot() +
  geom_sf(data = world_plot, fill = "grey98", color = NA) +
  geom_sf(data = world_plot, fill = NA, color = "grey80", linewidth = 0.2) +
  geom_sf(
    data        = org_geo_proj_gb,
    aes(color   = "Genebank"),
    size        = pt_size,
    alpha       = 0.8,
    show.legend = "point"
  ) +
  scale_color_manual(
    name   = "Institution type",
    values = c("Genebank" = "orange"),
    guide  = guide_legend(override.aes = list(size = 3, alpha = 1))
  ) +
  labs(title = NULL) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  add_graticule_breaks(lon_breaks = lon_breaks, lat_breaks = lat_breaks) +
  manual_graticule_theme() +
  manual_graticule_coord() +
  add_manual_graticule_labels(lon_breaks = lon_breaks, lat_breaks = lat_breaks)

save_map_both(p_map01, "FigS2.Genebank-institution-locations_map", output_dir)

# ==============================================================================
# MAP 2 — Botanic Garden Locations
# ==============================================================================

# filter org data for Botanic gardens
org_geo_data_bg <- org_geo_data %>%
  filter(organization_type == "Botanic garden")

# --- Summary stats ---
cat("--- Map 2: Botanic Garden Locations ---\n")
cat("Number of botanic gardens mapped:", nrow(org_geo_data_bg), "\n\n")

org_geo_sf_bg   <- st_as_sf(
  org_geo_data_bg,
  coords = c("longitude", "latitude"),
  crs    = 4326,
  remove = FALSE
)
org_geo_proj_bg <- st_transform(org_geo_sf_bg, crs = crs_equalarea)

p_map02 <- ggplot() +
  geom_sf(data = world_plot, fill = "grey98", color = NA) +
  geom_sf(data = world_plot, fill = NA, color = "grey80", linewidth = 0.2) +
  geom_sf(
    data        = org_geo_proj_bg,
    aes(color   = "Botanic garden"),
    size        = pt_size,
    alpha       = 0.8,
    show.legend = "point"
  ) +
  scale_color_manual(
    name   = "Institution type",
    values = c("Botanic garden" = "purple"),
    guide  = guide_legend(override.aes = list(size = 3, alpha = 1))
  ) +
  labs(title = NULL) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  add_graticule_breaks(lon_breaks = lon_breaks, lat_breaks = lat_breaks) +
  manual_graticule_theme() +
  manual_graticule_coord() +
  add_manual_graticule_labels(lon_breaks = lon_breaks, lat_breaks = lat_breaks)

save_map_both(p_map02, "FigS3.Botanicgarden-institution-locations_map", output_dir)

# ==============================================================================
# MAP 3 — Botanic Gardens + Genebanks (combined, with jitter)
# ==============================================================================

org_geo_data_both <- org_geo_data %>%
  mutate(
    jittered_latitude  = jitter(latitude,  amount = 0.02),
    jittered_longitude = jitter(longitude, amount = 0.02)
  )

# --- Summary stats ---
cat("--- Map 3: Botanic Gardens + Genebanks (combined) ---\n")
cat("Number of botanic gardens mapped:", sum(org_geo_data_both$organization_type == "Botanic garden", na.rm = TRUE), "\n")
cat("Number of genebanks mapped:      ", sum(org_geo_data_both$organization_type == "Genebank",       na.rm = TRUE), "\n")
cat("Total institutions mapped:       ", nrow(org_geo_data_both), "\n\n")

org_geo_sf_both <- st_as_sf(
  org_geo_data_both,
  coords = c("jittered_longitude", "jittered_latitude"),
  crs    = 4326,
  remove = FALSE
)

botanic_gardens_proj <- st_transform(
  org_geo_sf_both %>% filter(organization_type == "Botanic garden"),
  crs = crs_equalarea
)
genebanks_proj <- st_transform(
  org_geo_sf_both %>% filter(organization_type == "Genebank"),
  crs = crs_equalarea
)

p_map03 <- ggplot() +
  geom_sf(data = world_plot, fill = "grey98", color = NA) +
  geom_sf(data = world_plot, fill = NA, color = "grey80", linewidth = 0.2) +
  geom_sf(
    data        = botanic_gardens_proj,
    aes(color   = "Botanic garden"),
    size        = pt_size,
    alpha       = 0.8,
    show.legend = "point"
  ) +
  geom_sf(
    data        = genebanks_proj,
    aes(color   = "Genebank"),
    size        = pt_size,
    alpha       = 0.8,
    show.legend = "point"
  ) +
  scale_color_manual(
    name   = "Institution type",
    values = c("Botanic garden" = "purple", "Genebank" = "orange"),
    guide  = guide_legend(override.aes = list(size = 3, alpha = 1))
  ) +
  labs(title = NULL) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  add_graticule_breaks(lon_breaks = lon_breaks, lat_breaks = lat_breaks) +
  manual_graticule_theme() +
  manual_graticule_coord() +
  add_manual_graticule_labels(lon_breaks = lon_breaks, lat_breaks = lat_breaks)

save_map_both(p_map03, "Fig2.Botanicgarden-genebank-locations_map", output_dir)



# --- Crop white space from saved PNGs and PDFs ---
# Run this after Script2_updated.R has saved all maps

# --- Libraries ---
library(magick)   # install.packages("magick")

# --- Parameters ---
output_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/FINAL_2026-03-20/Gb_bg_locations_maps"

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


## end script ##