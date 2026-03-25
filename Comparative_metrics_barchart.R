# Complementarity figure
# Figure 3
# Comparative metrics bar figure
# Save as both PDF and PNG.

# Numbers to be updated ************


# FINAL DATA: 2026-03-13
genebank_accessionlevel_dataset <- read_csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_genebank_accessionlevel_dataset_2026-03-13.csv')
botanicgarden_accessionlevel_dataset <- read_csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_botanicgarden_accessionlevel_dataset_2026-03-13.csv')
botanicgarden_specieslevel_dataset <- read_csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_botanicgarden_specieslevel_dataset_2026-03-13.csv')

# WCFP PLANT LIST CORRECTED 2026-03-04
plantlist <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/WCFP_plantlist/Standardized/WCFP_plantlist_standardized_2026-02-23_updated.xlsx")




# BAR 1
# Number of institutions = 2061
# Genebanks = 897
# Botanic gardens = 1,163


# BAR 2
# Total species =  26,632 
# Only genebanks = 1,003
# Only botanic gardens = 8,168
# Both = 11,086
# Neither = 6,375


# BAR 3
# Number of total accessions = 5,192,418 
# Number of accessions in genebanks = 4,755,837
# Number of accessions in botanic gardens = 436,581

library(grid)

# ---- helpers ----
rect_npc <- function(xl, xr, yb, yt, lwd = 0.9, col = "black", fill = NA) {
  grid.rect(
    x = (xl + xr) / 2, y = (yb + yt) / 2,
    width = xr - xl, height = yt - yb,
    default.units = "npc",
    gp = gpar(col = col, fill = fill, lwd = lwd, lineend = "butt")
  )
}

make_mapper <- function(pad = c(l = 0.018, r = 0.012, b = 0.020, t = 0.030)) {
  function(xl, xr, yb, yt) {
    list(
      xl = pad["l"] + xl * (1 - pad["l"] - pad["r"]),
      xr = pad["l"] + xr * (1 - pad["l"] - pad["r"]),
      yb = pad["b"] + yb * (1 - pad["b"] - pad["t"]),
      yt = pad["b"] + yt * (1 - pad["b"] - pad["t"])
    )
  }
}

# ---- draw_bar with rotated text for narrow bars ----
draw_bar <- function(map,
                     yb, yt,
                     top_title,
                     boxes,                 
                     bar_xl = 0.00,
                     bar_xr = 1.00,
                     title_frac = 0.16,    
                     bar_lwd = 1.1,
                     box_lwd = 0.9,
                     title_cex = 0.90,
                     text_cex = 0.88) {
  h <- yt - yb
  y_title_b <- yt - title_frac * h
  y_bar_t   <- y_title_b
  # Title strip
  bb_title <- map(bar_xl, bar_xr, y_title_b, yt)
  grid.text(
    top_title,
    x = bb_title$xl, y = bb_title$yt,
    default.units = "npc",
    just = c("left", "top"),
    gp = gpar(cex = title_cex, fontface = 2, lineheight = 0.95)
  )
  
  # Bar outline
  bb_bar <- map(bar_xl, bar_xr, yb, y_bar_t)
  rect_npc(bb_bar$xl, bb_bar$xr, bb_bar$yb, bb_bar$yt, lwd = bar_lwd, fill = NA)
  
  # Internal boxes
  for (b in boxes) {
    ib <- map(
      bar_xl + (bar_xr - bar_xl) * b$xl,
      bar_xl + (bar_xr - bar_xl) * b$xr,
      yb, y_bar_t
    )
    rect_npc(ib$xl, ib$xr, ib$yb, ib$yt, lwd = box_lwd, fill = b$fill)
    bar_width <- ib$xr - ib$xl
    dynamic_cex <- text_cex * max(0.75, min(1, bar_width * 5))
    
    if (bar_width < 0.10) {
      # Rotate text for narrow bars       <<< bar section= less than 10% of total
      grid.text(
        b$text,
        x = (ib$xl + ib$xr)/2, y = (ib$yb + ib$yt)/2,
        default.units = "npc",
        just = c("centre", "centre"),
        gp = gpar(cex = dynamic_cex, fontface = 2, lineheight = 0.95),
        rot = 90
      )
    } else {
      # Normal
      grid.text(
        b$text,
        x = (ib$xl + ib$xr)/2, y = (ib$yb + ib$yt)/2,
        default.units = "npc",
        just = c("centre", "centre"),
        gp = gpar(cex = dynamic_cex, fontface = 2, lineheight = 0.95)
      )
    }
  }
}

# ---- draw_figure ----
draw_figure <- function() {
  grid.newpage()
  map <- make_mapper()
  col_genebanks <- "#F28E2B"
  col_botanic   <- "#7B61FF"
  col_both      <- "#59A14F"
  col_neither   <- "#9D9D9D"
  
  gap <- 0.045
  band_h <- (1 - 2 * gap) / 3
  
  y1t <- 1.00; y1b <- y1t - band_h
  y2t <- y1b - gap; y2b <- y2t - band_h
  y3t <- y2b - gap; y3b <- y3t - band_h
  
  bar_xl <- 0.00; bar_xr <- 1.00
  
  # Bar 1
  draw_bar(
    map, yb = y1b, yt = y1t,
    top_title = "a) Number of institutions = 2061",
    boxes = list(
      list(xl = 0.00, xr = 0.435, text = "Genebanks\n897",         fill = col_genebanks),
      list(xl = 0.435, xr = 1.00, text = "Botanic gardens\n1164", fill = col_botanic)
    ),
    bar_xl = bar_xl, bar_xr = bar_xr
  )
  # Bar 2
  draw_bar(
    map, yb = y2b, yt = y2t,
    top_title = "b) Number of species = 26,632",
    boxes = list(
      list(xl = 0.00, xr = 0.0376, text = "Only genebanks\n1003",          fill = col_genebanks),
      list(xl = 0.0376, xr = 0.3439, text = "Only botanic gardens\n8168", fill = col_botanic),
      list(xl = 0.3439, xr = 0.7601, text = "Both\n11,086",               fill = col_both),
      list(xl = 0.7601, xr = 1.00, text = "Neither\n6375",                fill = col_neither)
    ),
    bar_xl = bar_xl, bar_xr = bar_xr
  )
  # Bar 3
  draw_bar(
    map, yb = y3b, yt = y3t,
    top_title = "c) Number of accessions = 5,192,418",
    boxes = list(
      list(xl = 0.00, xr = 0.9159, text = "Genebanks\n4,755,837",   fill = col_genebanks),
      list(xl = 0.9159, xr = 1.00, text = "Botanic gardens\n436,581", fill = col_botanic)
    ),
    bar_xl = bar_xl, bar_xr = bar_xr
  )
}

# ---- OUTPUT ----
out_pdf <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/FINAL_2026-03-20/Fig3.Comparative-metrics_barchart.pdf"
out_png <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/FINAL_2026-03-20/Fig3.Comparative-metrics_barchart.png"
dir.create(dirname(out_pdf), recursive = TRUE, showWarnings = FALSE)

# PDF
cairo_pdf(out_pdf, width = 8.5, height = 4.8)
draw_figure()
dev.off()

# PNG (600 dpi)
png(out_png, width = 8.5, height = 4.8, units = "in", res = 600, type = "cairo")
draw_figure()
dev.off()










############################### OLD ########################
library(grid)

# ---- helpers ----
rect_npc <- function(xl, xr, yb, yt, lwd = 0.9, col = "black", fill = NA) {
  grid.rect(
    x = (xl + xr) / 2, y = (yb + yt) / 2,
    width = xr - xl, height = yt - yb,
    default.units = "npc",
    gp = gpar(col = col, fill = fill, lwd = lwd, lineend = "butt")
  )
}

make_mapper <- function(pad = c(l = 0.018, r = 0.012, b = 0.020, t = 0.030)) {
  # Tight pads to reduce outer whitespace; t still nonzero to avoid clipping in PDF/PNG devices.
  function(xl, xr, yb, yt) {
    list(
      xl = pad["l"] + xl * (1 - pad["l"] - pad["r"]),
      xr = pad["l"] + xr * (1 - pad["l"] - pad["r"]),
      yb = pad["b"] + yb * (1 - pad["b"] - pad["t"]),
      yt = pad["b"] + yt * (1 - pad["b"] - pad["t"])
    )
  }
}

# One band: title strip (top) + bar area (bottom)
draw_bar <- function(map,
                     yb, yt,
                     top_title,
                     boxes,                 # list(list(xl,xr,text,fill))
                     bar_xl = 0.00,
                     bar_xr = 1.00,
                     title_frac = 0.16,     # slightly smaller title strip to save vertical space
                     bar_lwd = 1.1,
                     box_lwd = 0.9,
                     title_cex = 0.90,
                     text_cex = 0.88) {
  
  h <- yt - yb
  y_title_b <- yt - title_frac * h
  y_bar_t   <- y_title_b
  
  # Title (bold), inside title strip to avoid clipping
  bb_title <- map(bar_xl, bar_xr, y_title_b, yt)
  grid.text(
    top_title,
    x = bb_title$xl, y = bb_title$yt,
    default.units = "npc",
    just = c("left", "top"),
    gp = gpar(cex = title_cex, fontface = 2, lineheight = 0.95)
  )
  
  # Bar outline
  bb_bar <- map(bar_xl, bar_xr, yb, y_bar_t)
  rect_npc(bb_bar$xl, bb_bar$xr, bb_bar$yb, bb_bar$yt, lwd = bar_lwd, fill = NA)
  
  # Internal boxes
  for (b in boxes) {
    ib <- map(
      bar_xl + (bar_xr - bar_xl) * b$xl,
      bar_xl + (bar_xr - bar_xl) * b$xr,
      yb, y_bar_t
    )
    rect_npc(ib$xl, ib$xr, ib$yb, ib$yt, lwd = box_lwd, fill = b$fill)
    
    grid.text(
      b$text,
      x = (ib$xl + ib$xr)/2, y = (ib$yb + ib$yt)/2,
      default.units = "npc",
      just = c("centre", "centre"),
      gp = gpar(cex = text_cex, fontface = 2, lineheight = 0.95)
    )
  }
}

draw_figure <- function() {
  grid.newpage()
  map <- make_mapper()
  
  # ---- colors ----
  col_genebanks <- "#F28E2B"  # orange
  col_botanic   <- "#7B61FF"  # purple
  col_both      <- "#59A14F"  # green
  col_neither   <- "#9D9D9D"  # grey
  
  # ---- layout: tighter gaps between bands ----
  gap <- 0.045  # reduced further vs 0.065
  band_h <- (1 - 2 * gap) / 3
  
  y1t <- 1.00; y1b <- y1t - band_h
  y2t <- y1b - gap; y2b <- y2t - band_h
  y3t <- y2b - gap; y3b <- y3t - band_h
  
  # Bars fill full width now (no extra space for separate a)/b)/c))
  bar_xl <- 0.00
  bar_xr <- 1.00
  
  # ---- Bar 1 ----
  draw_bar(
    map, yb = y1b, yt = y1t,
    top_title = "a) Number of institutions = 2061",
    boxes = list(
      list(xl = 0.00, xr = 0.435, text = "Genebanks\n897",         fill = col_genebanks),
      list(xl = 0.435, xr = 1.00, text = "Botanic gardens\n1164", fill = col_botanic)
    ),
    bar_xl = bar_xl, bar_xr = bar_xr
  )
  
  # ---- Bar 2 ----
  draw_bar(
    map, yb = y2b, yt = y2t,
    top_title = "b) Number of species = 26,632",
    boxes = list(
      list(xl = 0.00, xr = 0.0376, text = "Only genebanks\n1003",          fill = col_genebanks),
      list(xl = 0.0376, xr = 0.3439, text = "Only botanic gardens\n8168", fill = col_botanic),
      list(xl = 0.3439, xr = 0.7601, text = "Both\n11,086",                  fill = col_both),
      list(xl = 0.7601, xr = 1.00, text = "Neither\n6375",               fill = col_neither)
    ),
    bar_xl = bar_xl, bar_xr = bar_xr
  )
  
  # ---- Bar 3 ----
  draw_bar(
    map, yb = y3b, yt = y3t,
    top_title = "c) Number of accessions = 5,192,418",
    boxes = list(
      list(xl = 0.00, xr = 0.9159, text = "Genebanks\n4,755,837",   fill = col_genebanks),
      list(xl = 0.9159, xr = 1.00, text = "Botanic gardens\n436,581", fill = col_botanic)
    ),
    bar_xl = bar_xl, bar_xr = bar_xr
  )
}

# -------------------------
# 1) VIEW
# -------------------------
draw_figure()

# -------------------------
# 2) SAVE (PDF + PNG)
# -------------------------
out_pdf <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/FINAL_2026-03-13/Fig3.Comparative_bar_figure_updated.pdf"
out_png <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/FINAL_2026-03-13/Fig3.Comparative_bar_figure_updated.png"

dir.create(dirname(out_pdf), recursive = TRUE, showWarnings = FALSE)

# PDF
cairo_pdf(out_pdf, width = 8.5, height = 4.8)
draw_figure()
dev.off()

# PNG (600 dpi)
png(out_png, width = 8.5, height = 4.8, units = "in", res = 600, type = "cairo")
draw_figure()
dev.off()
