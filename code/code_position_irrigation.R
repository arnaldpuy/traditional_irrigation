## ----setup, include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dev = "pdf", cache = TRUE)


## ----preliminary, warning=FALSE, message=FALSE, results="hide"-----------------------
# PRELIMINARY FUNCTIONS ########################################################

library(sensobol)
sensobol::load_packages(c("data.table", "ggplot2", "sf", "rnaturalearth",
                          "rnaturalearthdata", "ggrepel", "here", "readxl",
                          "ggwordcloud", "cowplot", "benchmarkme", "treemapify"))

# Source all .R files in the "functions" folder -------------------------------

r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))


## ----locations, dependson="preliminary"----------------------------------------------
# INTERVIEW LOCATIONS #########################################################

locations <- data.table(
  place = c("Dúrcal", "Huétor-Tajar", "Murcia", "Ricote", "Sudanell", "Arriate", "Oria"),
  lon   = c(-3.5694,  -4.0439,        -1.1307,  -1.4361,   0.6011,    -5.0606,   -2.2731),
  lat   = c(37.0022,   37.1964,        37.9922,  38.0714,  41.5261,    36.7914,   37.5225)
)


## ----boundary, dependson="preliminary"-----------------------------------------------
# SPAIN BOUNDARY ##############################################################

spain <- ne_countries(scale = "medium", country = "Spain", returnclass = "sf")


## ----plot, fig.width=3, fig.height=3, dependson=c("locations","boundary")------------
# PLOT ########################################################################

plot_locations <- ggplot() +
  geom_sf(data = spain, fill = "#f5f0e8", color = "#aaaaaa", linewidth = 0.4) +
  geom_point(data = locations,
             aes(x = lon, y = lat),
             shape = 21, fill = "#c0392b", color = "white",
             size = 3.5, stroke = 0.8) +
  geom_text_repel(data = locations,
                  aes(x = lon, y = lat, label = place),
                  size = 3.2, family = "serif", color = "#222222",
                  box.padding = 0.5, point.padding = 0.3,
                  segment.color = "#888888", segment.linewidth = 0.4,
                  min.segment.length = 0.2,
                  max.overlaps = Inf, seed = 42) +
  coord_sf(xlim = c(-9.5, 4.5), ylim = c(35.5, 44.5), expand = FALSE) +
  labs(title = NULL, x = NULL, y = NULL) +
  theme_AP() +
  theme(
    panel.background = element_rect(fill = "#dce8f0", color = NA),
    panel.grid       = element_line(color = "#cccccc", linewidth = 0.2),
    axis.text        = element_text(size = 8, color = "#555555"),
    plot.margin      = margin(10, 10, 10, 10)
  )

plot_locations


## ----load-vocab, dependson="preliminary"---------------------------------------------
# LOAD VOCABULARY DATA ########################################################

# Irrigators: factors sheet, general list, English translation ----------------

irrigators <- as.data.table(
  read_xlsx(here("datasets", "input", "carmen_dataset_2.xlsx"), sheet = "factors")
)
irrigators_wc <- irrigators[list == "general" & !is.na(factor_en) & place == "sudanell" ,
                             .(n = .N), by = .(word = factor_en)]
irrigators_wc[, group := "Traditional irrigators"]

# Scientists: Sheet2, general list, homogenized concepts ---------------------
# The raw `factor` column has many near-synonyms and a few typos. We map
# them to canonical concepts via a lookup table (raw -> canonical) so the
# wordclouds (and everything downstream) work on homogenized terms.
# See datasets/input/scientists_general_factor_lookup.csv for the mapping.

scientists <- as.data.table(
  read_xlsx(here("datasets", "input", "scientists_vocabulari.xlsx"), sheet = "Sheet2")
)
factor_lookup <- fread(here("datasets", "input",
                            "scientists_general_factor_lookup.csv"))
scientists[, factor_canonical := homogenize_factors_fun(factor, factor_lookup)]

scientists_wc <- scientists[list == "general" & !is.na(factor_canonical),
                             .(n = .N), by = .(word = factor_canonical)]
scientists_wc[, group := "Scientists"]

# Top 50 per group -----------------------------------------------------------

irrigators_wc <- setorder(irrigators_wc, -n)[1:50]
scientists_wc  <- setorder(scientists_wc,  -n)[1:50]

# Combine ---------------------------------------------------------------------

wc_data <- rbind(irrigators_wc, scientists_wc)


## ----plot-wordcloud, fig.width=6.4, fig.height=3.4, dependson="load-vocab"-----------
# WORDCLOUD ###################################################################

plot_wordcloud <- ggplot(wc_data, aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 42, color = "black") +
  scale_size_area(max_size = 6) +
  facet_grid(. ~ group) +
  theme_AP()

plot_wordcloud


## ----pyramid, dependson="load-vocab", fig.width=3.5, fig.height=3.3------------------

# PLOT PYRAMID / DIVERGING BAR #################################################

# Words on the y-axis, irrigators counts going left, scientists going right.
# Direct visual comparison: shared vs group-specific terms pop out at once.
wc_wide <- dcast(wc_data, word ~ group, value.var = "n", fill = 0)
setnames(wc_wide, c("Traditional irrigators", "Scientists"),
         c("irrigators", "scientists"))
wc_wide[, total := irrigators + scientists]
wc_pyr <- wc_wide[order(-total)][seq_len(min(.N, 40))]
wc_pyr_long <- melt(wc_pyr, id.vars = "word",
                    measure.vars = c("irrigators", "scientists"),
                    variable.name = "group", value.name = "n")
wc_pyr_long[group == "irrigators", n := -n]

plot_alt_pyramid <- ggplot(wc_pyr_long,
                           aes(x = n,
                               y = reorder(word, abs(n), sum),
                               fill = group)) +
  geom_col() +
  geom_vline(xintercept = 0, color = "grey40") +
  scale_x_continuous(labels = abs) +
  scale_fill_manual(values = c(irrigators = "#c0843d",
                               scientists = "#3d6fc0")) +
  labs(x = "Frequency", y = NULL, fill = NULL) +
  theme_AP() +
  theme(legend.position = c(0.8, 0.2))

plot_alt_pyramid


## ----alt_treemap, dependson="load-vocab", fig.width=6, fig.height=4------------------

# PLOT TREEMAP #################################################################

plot_alt_treemap <- ggplot(wc_data,
                           aes(area = n, label = word, fill = n)) +
  geom_treemap() +
  geom_treemap_text(color = "white", place = "centre",
                    grow = FALSE, reflow = TRUE, min.size = 1) +
  scale_fill_viridis_c(option = "mako", begin = 0.25, end = 0.85,
                       guide = "none") +
  facet_wrap(~ group) +
  theme_AP()

plot_alt_treemap


## ----spain_share---------------------------------------------------------------------

# READ IN DATASETS #############################################################

country_level_irrigated_areas <- fread(here("datasets", "input",
                                            "country_level_irrigated_areas.csv"))


# Calculate --------------------------------------------------------------------

country_level_irrigated_areas[continent == "Europe",
                              .(spain_mha = sum(mha[country == "Spain"]),
                                europe_mha = sum(mha),
                                spain_share_pct = 100 * sum(mha[country == "Spain"]) / 
                                  sum(mha)),
                              by = .(dataset, resolution)]


## ----session_information, dependson="preliminary"------------------------------------
# SESSION INFORMATION #########################################################

sessionInfo()

## Return the machine CPU -----------------------------------------------------

cat("Machine:     "); print(get_cpu()$model_name)

## Return number of true cores -------------------------------------------------

cat("Num cores:   "); print(parallel::detectCores(logical = FALSE))

## Return number of threads ----------------------------------------------------

cat("Num threads: "); print(parallel::detectCores(logical = FALSE))

