#' ---
#' title: "Map of Interview Locations in Spain"
#' subtitle: ""
#' author: "Arnald Puy"
#' header-includes:
#'   - \usepackage[font=footnotesize]{caption}
#'   - \usepackage{dirtytalk}
#'   - \usepackage{booktabs}
#'   - \usepackage{tabulary}
#'   - \usepackage{enumitem}
#'   - \usepackage{lmodern}
#'   - \usepackage{amsmath}
#'   - \usepackage{mathtools}
#'   - \usepackage[T1]{fontenc}
#'   - \usepackage{tikz}
#' output:
#'   pdf_document:
#'     fig_caption: yes
#'     number_sections: yes
#'     toc: yes
#'     toc_depth: 2
#'     keep_tex: true
#'   word_document:
#'     toc: no
#'     toc_depth: '2'
#'   html_document:
#'     keep_md: true
#' link-citations: yes
#' fontsize: 11pt
#' ---
#' 
## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dev = "pdf", cache = TRUE)

#' 
#' \newpage
#' 
## ----preliminary, warning=FALSE, message=FALSE, results="hide"----------------
# PRELIMINARY FUNCTIONS #######################################################

library(sensobol)
sensobol::load_packages(c("data.table", "ggplot2", "sf", "rnaturalearth",
                          "rnaturalearthdata", "ggrepel", "here", "readxl",
                          "ggwordcloud", "cowplot"))

# Source all .R files in the "functions" folder -------------------------------
r_functions <- list.files(path = here("functions"),
                          pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))

#' 
#' # Interview locations
#' 
## ----locations, dependson="preliminary"---------------------------------------
# INTERVIEW LOCATIONS #########################################################

locations <- data.table(
  place = c("Dúrcal", "Huétor-Tajar", "Murcia", "Ricote", "Sudanell", "Arriate", "Oria"),
  lon   = c(-3.5694,  -4.0439,        -1.1307,  -1.4361,   0.6011,    -5.0606,   -2.2731),
  lat   = c(37.0022,   37.1964,        37.9922,  38.0714,  41.5261,    36.7914,   37.5225)
)

#' 
#' # Spain boundary
#' 
## ----boundary, dependson="preliminary"----------------------------------------
# SPAIN BOUNDARY ##############################################################

spain <- ne_countries(scale = "medium", country = "Spain", returnclass = "sf")

#' 
#' # Plot
#' 
## ----plot, fig.width=3, fig.height=3, dependson=c("locations","boundary")-----
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

#' 
#' # Wordcloud
#' 
#' ## Load vocabulary data
#' 
## ----load-vocab, dependson="preliminary"--------------------------------------
# LOAD VOCABULARY DATA ########################################################

# Irrigators: factors sheet, general list, English translation ----------------
irrigators <- as.data.table(
  read_xlsx(here("datasets", "input", "carmen_dataset_2.xlsx"), sheet = "factors")
)
irrigators_wc <- irrigators[list == "general" & !is.na(factor_en),
                             .(n = .N), by = .(word = factor_en)]
irrigators_wc[, group := "Traditional irrigators"]

# Scientists: Sheet2, general list, cleaned terms -----------------------------
scientists <- as.data.table(
  read_xlsx(here("datasets", "input", "scientists_vocabulari.xlsx"), sheet = "Sheet2")
)
scientists_wc <- scientists[list == "general" & !is.na(factor_clean),
                             .(n = .N), by = .(word = factor_clean)]
scientists_wc[, group := "Scientists"]

# Top 50 per group -----------------------------------------------------------
irrigators_wc <- setorder(irrigators_wc, -n)[1:50]
scientists_wc  <- setorder(scientists_wc,  -n)[1:50]

# Combine ---------------------------------------------------------------------
wc_data <- rbind(irrigators_wc, scientists_wc)

#' 
#' ## Plot wordcloud
#' 
## ----plot-wordcloud, fig.width=5.5, fig.height=4, dependson="load-vocab"------
# WORDCLOUD ###################################################################

plot_wordcloud <- ggplot(wc_data, aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 42, color = "black") +
  scale_size_area(max_size = 4) +
  facet_grid(. ~ group) +
  theme_AP()

plot_wordcloud

# WORDCLOUD EXPERIMENTS #######################################################

# Experiment 1: circular spiral, tight packing --------------------------------
# Lower eccentricity (0.35) makes the cloud more circular, finer rstep/tstep
# packs words more densely — good when each panel is taller than wide.
plot_wc_exp1 <- ggplot(wc_data, aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 42, color = "black",
                      eccentricity = 0.35, rstep = 0.05, tstep = 0.05) +
  scale_size_area(max_size = 3.5) +
  facet_grid(. ~ group) +
  theme_AP()

plot_wc_exp1

# Experiment 2: rm_outside removes overflow, allows larger max_size -----------
# Words that cannot fit within the panel boundary are silently dropped rather
# than clipped, keeping all visible words fully legible.
plot_wc_exp2 <- ggplot(wc_data, aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 42, color = "black", rm_outside = TRUE) +
  scale_size_area(max_size = 5) +
  facet_grid(. ~ group) +
  theme_AP()

plot_wc_exp2

# Experiment 3: sqrt transform softens size differences ----------------------
# Compresses the size range so rare words remain readable alongside frequent
# ones without the layout being dominated by one or two large terms.
plot_wc_exp3 <- ggplot(wc_data, aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 42, color = "black") +
  scale_size_area(max_size = 4, trans = "sqrt") +
  facet_grid(. ~ group) +
  theme_AP()

plot_wc_exp3

# Experiment 4: stacked panels (facet_wrap ncol = 1) -------------------------
# Each group gets a wide, shallow band — suits a combined figure where the
# wordcloud sits below the map at full width.
plot_wc_exp4 <- ggplot(wc_data, aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 42, color = "black", eccentricity = 0.9) +
  scale_size_area(max_size = 3.5) +
  facet_wrap(~ group, ncol = 1) +
  theme_AP()

plot_wc_exp4

# Experiment 5: alternative seed + intermediate eccentricity ------------------
# Different spiral starting point often resolves awkward blank patches;
# eccentricity 0.55 is a compromise between circular and elliptical.
plot_wc_exp5 <- ggplot(wc_data, aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 99, color = "black",
                      eccentricity = 0.55, rstep = 0.07, tstep = 0.07) +
  scale_size_area(max_size = 4) +
  facet_grid(. ~ group) +
  theme_AP()

plot_wc_exp5

#'
#' # Combined figure
#' 
## ----combined, fig.width=5.5, fig.height=7, dependson=c("plot","plot-wordcloud")----
# COMBINED FIGURE #############################################################

# One column, two rows: map / faceted wordcloud -------------------------------
plot_combined <- plot_grid(
  plot_locations, plot_wordcloud,
  ncol       = 1,
  labels     = c("a)", "b)"),
  label_size = 7
)

plot_combined

#' 
#' # Export
#' 
## ----export, dependson=c("plot","plot-wordcloud","combined")------------------
# EXPORT ######################################################################

ggsave(here("datasets", "output", "interview_locations_spain.pdf"),
       plot = plot_locations, width = 7, height = 5.5, device = cairo_pdf)

ggsave(here("datasets", "output", "interview_locations_spain.png"),
       plot = plot_locations, width = 7, height = 5.5, dpi = 300)

ggsave(here("datasets", "output", "wordcloud_vocabularies.pdf"),
       plot = plot_wordcloud, width = 7, height = 4, device = cairo_pdf)

ggsave(here("datasets", "output", "wordcloud_vocabularies.png"),
       plot = plot_wordcloud, width = 7, height = 4, dpi = 300)

ggsave(here("datasets", "output", "combined_figure.pdf"),
       plot = plot_combined, width = 5.5, height = 7, device = cairo_pdf)

ggsave(here("datasets", "output", "combined_figure.png"),
       plot = plot_combined, width = 5.5, height = 7, dpi = 300)

############################################
############################################


# Irrigators: factors sheet, general list, English translation ----------------
cognitive_maps <- as.data.table(
  read_xlsx(here("datasets", "input", "carmen_dataset.xlsx"), sheet = "maps")
)

# Anonymize names and places --------------------------------------------------
  
  # Build lookup tables with anonymous IDs
  name_map  <- data.table(
    name      = unique(cognitive_maps$name),
    anon_name = paste0("person_", seq_along(unique(cognitive_maps$name)))
  )

place_map <- data.table(
  place      = unique(cognitive_maps$place),
  anon_place = paste0("place_", seq_along(unique(cognitive_maps$place)))
)

# Join and replace in place
setkey(cognitive_maps, name)
setkey(name_map, name)
cognitive_maps[name_map, name := anon_name]

setkey(cognitive_maps, place)
setkey(place_map, place)
cognitive_maps[place_map, place := anon_place]


unique(cognitive_maps$name)



# COUNTRY-LEVEL IRRIGATED AREAS ###############################################

country_level_irrigated_areas <- fread(here("datasets", "input",
                                            "country_level_irrigated_areas.csv"))

country_level_irrigated_areas[continent == "Europe",
                              .(spain_mha = sum(mha[country == "Spain"]),
                                europe_mha = sum(mha),
                                spain_share_pct = 100 * sum(mha[country == "Spain"]) / sum(mha)),
                              by = .(dataset, resolution)] %>%
  .[, mean(spain_share_pct)]

# WORDCLOUD EXPERIMENTS — VISUAL APPEAL #######################################

# Experiment 6: area-scaled glyphs --------------------------------------------
# geom_text_wordcloud_area sizes by glyph area instead of font height, so long
# words no longer dominate just because they have more letters. Usually the
# single biggest visual win over the default geom.
plot_wc_exp6 <- ggplot(wc_data, aes(label = word, size = n)) +
  geom_text_wordcloud_area(seed = 42, color = "black", rm_outside = TRUE) +
  scale_size_area(max_size = 11) +
  facet_grid(. ~ group) +
  theme_AP()

plot_wc_exp6

# Experiment 7: colour mapped to frequency ------------------------------------
# Viridis gradient turns the cloud into a heat-map of term frequency: rare
# words read cool, frequent ones warm, without changing the layout itself.
plot_wc_exp7 <- ggplot(wc_data, aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(seed = 42, rm_outside = TRUE) +
  scale_size_area(max_size = 5) +
  scale_color_viridis_c(option = "magma", end = 0.85, guide = "none") +
  facet_grid(. ~ group) +
  theme_AP()

plot_wc_exp7

# Experiment 8: circular silhouette + light rotation --------------------------
# Low eccentricity gives a rounder cloud; ~15% of words rotated 90° fills
# blank patches and adds rhythm without hurting readability.
set.seed(7)
wc_data_rot <- copy(wc_data)
wc_data_rot[, angle := sample(c(0, 90), .N, replace = TRUE,
                              prob = c(0.85, 0.15))]
plot_wc_exp8 <- ggplot(wc_data_rot,
                       aes(label = word, size = n, angle = angle)) +
  geom_text_wordcloud(seed = 42, color = "black",
                      eccentricity = 0.4, rm_outside = TRUE) +
  scale_size_area(max_size = 5) +
  facet_grid(. ~ group) +
  theme_AP()

plot_wc_exp8

# Experiment 9: combined — area + colour + circular + rotation ---------------
# Stack the previous three improvements: area scaling, viridis colour by
# frequency, low-eccentricity (circular) layout, and a small share of
# vertical words. This is the "best-of" candidate.
plot_wc_exp9 <- ggplot(wc_data_rot,
                       aes(label = word, size = n, color = n, angle = angle)) +
  geom_text_wordcloud_area(seed = 42, eccentricity = 0.4, rm_outside = TRUE) +
  scale_size_area(max_size = 12) +
  scale_color_viridis_c(option = "mako", begin = 0.15, end = 0.85,
                        guide = "none") +
  facet_grid(. ~ group) +
  theme_AP()

plot_wc_exp9


# ALTERNATIVES TO WORDCLOUDS ##################################################

sensobol::load_packages(c("treemapify", "tidytext"))

# Top-N words per group (used by lollipop) ------------------------------------
TOP_N <- 20
wc_top <- wc_data[order(group, -n), .SD[seq_len(min(.N, TOP_N))], by = group]

# Alternative 1: ranked lollipop ----------------------------------------------
# Top-N words per group, ordered by frequency. Boring but unambiguous: exact
# counts are read straight off the axis with no spatial-packing artefacts.
plot_alt_lollipop <- ggplot(wc_top,
                            aes(x = n,
                                y = reorder_within(word, n, group))) +
  geom_segment(aes(xend = 0,
                   yend = reorder_within(word, n, group)),
               color = "grey70") +
  geom_point(size = 1.8, color = "steelblue") +
  scale_y_reordered() +
  facet_wrap(~ group, scales = "free_y") +
  labs(x = "Frequency", y = NULL) +
  theme_AP()

plot_alt_lollipop

# Alternative 2: treemap ------------------------------------------------------
# Same area-as-frequency intuition as a wordcloud but with tidy rectangles.
# Labels stay inside their tile, so they remain legible at small sizes.
plot_alt_treemap <- ggplot(wc_data,
                           aes(area = n, label = word, fill = n)) +
  geom_treemap() +
  geom_treemap_text(color = "white", place = "centre",
                    grow = FALSE, reflow = TRUE, min.size = 4) +
  scale_fill_viridis_c(option = "mako", end = 0.85, guide = "none") +
  facet_wrap(~ group) +
  theme_AP()

plot_alt_treemap

# Alternative 3: pyramid / diverging bar --------------------------------------
# Words on the y-axis, irrigators counts going left, scientists going right.
# Direct visual comparison: shared vs group-specific terms pop out at once.
wc_wide <- dcast(wc_data, word ~ group, value.var = "n", fill = 0)
setnames(wc_wide, c("Traditional irrigators", "Scientists"),
         c("irrigators", "scientists"))
wc_wide[, total := irrigators + scientists]
wc_pyr <- wc_wide[order(-total)][seq_len(min(.N, 25))]
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
  theme_AP()

plot_alt_pyramid

# Alternative 4: slope chart --------------------------------------------------
# Rank within each group on left/right axes; lines connect each shared word.
# Reveals which terms shift in importance between the two vocabularies.
wc_rank <- copy(wc_data)
wc_rank[, rank := frank(-n, ties.method = "min"), by = group]
shared <- wc_rank[, .N, by = word][N == 2, word]
wc_rank <- wc_rank[word %in% shared & rank <= 20]

plot_alt_slope <- ggplot(wc_rank,
                         aes(x = group, y = -rank, group = word)) +
  geom_line(color = "grey70") +
  geom_point(size = 1.8) +
  geom_text(data = wc_rank[group == "Traditional irrigators"],
            aes(label = word), hjust = 1.1, size = 2.6) +
  geom_text(data = wc_rank[group == "Scientists"],
            aes(label = word), hjust = -0.1, size = 2.6) +
  scale_x_discrete(expand = expansion(mult = 0.4)) +
  labs(x = NULL, y = "Rank (1 = most frequent)") +
  theme_AP() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

plot_alt_slope

# Alternative 5: log-odds (keyness) plot --------------------------------------
# Smoothed log-odds-ratio per word across the two groups (Monroe et al. 2008).
# x: distinctive to scientists (right) vs irrigators (left); y: total
# frequency. Surfaces *characteristic* words, not just the most frequent.
wc_lo <- dcast(wc_data, word ~ group, value.var = "n", fill = 0)
setnames(wc_lo, c("Traditional irrigators", "Scientists"),
         c("n_irr", "n_sci"))
N_irr <- sum(wc_lo$n_irr)
N_sci <- sum(wc_lo$n_sci)
wc_lo[, log_odds := log(((n_sci + 0.5) / (N_sci - n_sci + 0.5)) /
                       ((n_irr + 0.5) / (N_irr - n_irr + 0.5)))]
wc_lo[, total := n_irr + n_sci]
wc_lo[, side := fifelse(log_odds >= 0, "Scientists",
                        "Traditional irrigators")]

labels_lo <- unique(rbind(
  wc_lo[order(-abs(log_odds))][seq_len(min(.N, 20))],
  wc_lo[order(-total)][seq_len(min(.N, 15))]
))

plot_alt_logodds <- ggplot(wc_lo, aes(x = log_odds, y = total, color = side)) +
  geom_vline(xintercept = 0, color = "grey60", linetype = "dashed") +
  geom_point(alpha = 0.6) +
  geom_text_repel(data = labels_lo, aes(label = word),
                  size = 2.6, max.overlaps = 30, show.legend = FALSE) +
  scale_color_manual(values = c("Traditional irrigators" = "#c0843d",
                                "Scientists" = "#3d6fc0")) +
  labs(x = "Log-odds  (← irrigators   ·   scientists →)",
       y = "Total frequency", color = NULL) +
  theme_AP()

plot_alt_logodds

# Alternative 6: dot-matrix heatmap -------------------------------------------
# words x groups, point size and fill encode frequency. Scales naturally if
# more groups are added later (regions, generations, age cohorts).
top_words <- wc_data[, .(s = sum(n)), by = word][order(s), word]
top30 <- tail(top_words, 30)
wc_heat <- wc_data[word %in% top30]
wc_heat[, word := factor(word, levels = top30)]

plot_alt_heatmap <- ggplot(wc_heat,
                           aes(x = group, y = word, size = n, color = n)) +
  geom_point() +
  scale_size_area(max_size = 6, guide = "none") +
  scale_color_viridis_c(option = "mako", end = 0.85, guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_AP()

plot_alt_heatmap

# Alternative 7: co-occurrence network (irrigators) --------------------------
# Two words are linked if at least one irrigator mentioned them both in their
# interview. Edge weight = number of interviewees citing the pair; node size
# = total mentions. Reveals which terms travel together in the discourse.
# Uses irrigators$name (interviewee id) from the raw factors sheet.
sensobol::load_packages(c("igraph", "ggraph", "tidygraph"))

# (person, word) pairs, deduplicated so each word counts once per interviewee
irrigators_pairs <- unique(
  irrigators[list == "general" & !is.na(factor_en) & !is.na(name),
             .(name, word = factor_en)]
)

# Edges: for each interviewee, take all unordered pairs of distinct words
# they mentioned; aggregate across interviewees to get co-occurrence weights.
co_pairs <- irrigators_pairs[, {
  w <- unique(word)
  if (length(w) < 2) NULL
  else {
    cb <- combn(sort(w), 2)
    data.table(from = cb[1, ], to = cb[2, ])
  }
}, by = name]
edges_co <- co_pairs[, .(weight = .N), by = .(from, to)]

# Nodes: total mentions per word. Column must be called "name" for tidygraph.
nodes_co <- irrigators_pairs[, .(n = .N), by = .(name = word)]

# Trim for legibility: top-30 words by frequency, edges with weight >= 2.
TOP_NODES <- 30
top_words_co <- nodes_co[order(-n)][seq_len(min(.N, TOP_NODES)), name]
edges_top <- edges_co[from %in% top_words_co & to %in% top_words_co
                      & weight >= 2]
nodes_top <- nodes_co[name %in% unique(c(edges_top$from, edges_top$to))]

g <- tbl_graph(nodes = nodes_top, edges = edges_top, directed = FALSE)

set.seed(42)
plot_alt_network <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), color = "grey70", alpha = 0.6) +
  geom_node_point(aes(size = n), color = "#c0843d") +
  geom_node_text(aes(label = name), repel = TRUE, size = 2.8) +
  scale_edge_width_continuous(range = c(0.2, 1.8), guide = "none") +
  scale_size_area(max_size = 8, guide = "none") +
  theme_void()

plot_alt_network

# Alternative 7b: co-occurrence network (scientists) -------------------------
# Same construction as the irrigator network, applied to the scientists sheet.
# Word column is factor_clean; interviewee id is again "name".
scientists_pairs <- unique(
  scientists[list == "general" & !is.na(factor_clean) & !is.na(name),
             .(name, word = factor_clean)]
)

co_pairs_sci <- scientists_pairs[, {
  w <- unique(word)
  if (length(w) < 2) NULL
  else {
    cb <- combn(sort(w), 2)
    data.table(from = cb[1, ], to = cb[2, ])
  }
}, by = name]
edges_co_sci <- co_pairs_sci[, .(weight = .N), by = .(from, to)]

nodes_co_sci <- scientists_pairs[, .(n = .N), by = .(name = word)]

top_words_co_sci <- nodes_co_sci[order(-n)][seq_len(min(.N, TOP_NODES)), name]
edges_top_sci <- edges_co_sci[from %in% top_words_co_sci
                              & to %in% top_words_co_sci
                              & weight >= 2]
nodes_top_sci <- nodes_co_sci[name %in% unique(c(edges_top_sci$from,
                                                 edges_top_sci$to))]

g_sci <- tbl_graph(nodes = nodes_top_sci, edges = edges_top_sci,
                   directed = FALSE)

set.seed(42)
plot_alt_network_sci <- ggraph(g_sci, layout = "fr") +
  geom_edge_link(aes(width = weight), color = "grey70", alpha = 0.6) +
  geom_node_point(aes(size = n), color = "#3d6fc0") +
  geom_node_text(aes(label = name), repel = TRUE, size = 2.8) +
  scale_edge_width_continuous(range = c(0.2, 1.8), guide = "none") +
  scale_size_area(max_size = 8, guide = "none") +
  theme_void()

plot_alt_network_sci

# Side-by-side: irrigators vs scientists co-occurrence networks --------------
plot_alt_network_combined <- plot_grid(
  plot_alt_network     + ggtitle("Traditional irrigators"),
  plot_alt_network_sci + ggtitle("Scientists"),
  ncol = 2, align = "hv"
)

plot_alt_network_combined

