

# Traditional irrigation is more than just a productive strategy 

[Arnald Puy](https://www.arnaldpuy.com/), Carmen Aguiló-Rivera, Seth N. Linga, 
Samuel Flinders, Olivia Richards, Samuel Flinders, Miguel Banegas García,
Gerlo Borghuis, Louise Busschaert, Antonio Candel Turpín, Giovanni de Grandis, 
Mona Liza F. Delos Reyes, Francisco Garrido Avilés, Pablo Garrido Guillamón,
Jose María García Avilés, Jerry Knox, Pierre Laluet, Bruce Lankford, Antonio Mirón, 
Nicola Paciolla, Robert Reinecke, Bich Tran, Saskia van der Kooij, Dominik Wisser, 
Victoria Reyes-García.

R code accompanying the position paper *Traditional irrigation is more than just a
production strategy*, whose abstract is the following: 

## Abstract

*Traditional irrigation systems sustain millions of people worldwide and are increasingly targeted by modernization policies aiming at enhancing their water and crop efficiency. While modernization may be desirable in some cases, most frameworks are blind to three relevant features that have kept these systems adaptive through contrasting social and climatic contexts: their capacity to foster belonging, their functional diversity and the traditional knowledge of irrigators. Drawing on a collaboration between Spanish traditional irrigators and scholars and on an extensive corpus of interviews, we argue that these features cannot be collapsed into efficiency metrics and cannot be rebuilt once lost. Modernization must be reframed to ensure that traditional irrigation systems move to new configurations without losing the social and ecological fabric that underpins them.*

---

## Contents

The analysis combines 1) a vocabulary comparison between
traditional irrigators and scientists drawn from semi-structured interviews in seven
Spanish regions, and 2) a quantification of Spain's share of European irrigated area
across ten global irrigated-area datasets and three spatial resolutions (<https://zenodo.org/records/19844960>).

- [Background](#background)
- [Repository structure](#repository-structure)
- [Dependencies](#dependencies)
- [How to run](#how-to-run)
- [Functions](#functions)
- [Outputs](#outputs)
- [License](#license)

---

## Background

The paper argues that traditional irrigation systems carry
cultural, ecological and institutional value that is invisible in efficiency-oriented
modernization projects. 

---

## Repository structure

```
code_position_irrigation/
├── code/                              # Analysis scripts (no function definitions)
│   ├── code_position_irrigation.Rmd   # Primary analysis notebook
│   ├── code_position_irrigation.R     # Spun R script (knitr::purl output)
│   └── code_position_irrigation.pdf   # Rendered notebook
│
├── functions/                         # One file per function (*_fun suffix convention)
│   └── theme_AP.R                     # ggplot2 house theme (theme_AP)
│
└── README.md                          # This file
```

> **Note:** raw input data and derived figures are kept locally in a `datasets/`
> folder (with `input/` and `output/` sub-folders) that is excluded from version
> control. The interview spreadsheets can be obtained from Arnald Puy or Carmen Aguiló-Rivera. The ten
> global irrigated-area datasets used to compute Spain's share are available from
> *Code and datasets of "Where irrigation exists is globally contested"* on
> Zenodo: <https://zenodo.org/records/19844960>.

---

## Dependencies

| Package | Purpose |
|---------|---------|
| `sensobol` | `load_packages()` helper used to install/load the dependency stack |
| `data.table` | Fast in-memory data manipulation |
| `ggplot2` | All plots |
| `sf`, `rnaturalearth`, `rnaturalearthdata` | Spain boundary and map projection |
| `ggrepel` | Non-overlapping site labels |
| `ggwordcloud` | Vocabulary wordclouds |
| `cowplot` | `plot_grid()` for combining panels |
| `readxl` | Reading the interview spreadsheets |
| `here` | Portable file paths relative to project root |
| `benchmarkme` | Session information at the end of the notebook |

R ≥ 4.1 is recommended.

---

## How to run

1. **Set the working directory** to the project root (`code_position_irrigation/`).
   In RStudio, use *Session → Set Working Directory → Choose Directory...*; from
   the R console, use `setwd("/path/to/code_position_irrigation")`. All paths in
   the analysis are resolved with the `here` package, so file references work
   from the project root regardless of where scripts are launched.

2. **Knit the notebook:**

   ```r
   rmarkdown::render(here::here("code", "code_position_irrigation.Rmd"))
   ```

   or run the spun script directly:

   ```bash
   Rscript code/code_position_irrigation.R
   ```

---

## Functions

All functions live in `functions/` and are loaded automatically at the top of the
notebook via:

```r
r_functions <- list.files(path = here("functions"), pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_functions, source))
```

| Function | Purpose |
|----------|---------|
| `theme_AP()` | House ggplot2 theme applied to every plot |

---

## Outputs

### Spain's share of European irrigated area

For each of the ten irrigated-area datasets and each spatial resolution
(0.2°, 0.4°, 1°), the notebook reports:

| Column | Description |
|--------|-------------|
| `dataset` | Name of the global irrigated-area dataset |
| `resolution` | Grid resolution (degrees) |
| `spain_mha` | Spain's irrigated area (million hectares) |
| `europe_mha` | Total European irrigated area (million hectares) |
| `spain_share_pct` | Spain's percentage of European irrigated area |

### Plot objects produced by `code_position_irrigation.Rmd`

| Object | Description |
|--------|-------------|
| `plot_locations` | Map of the seven interview sites in Spain |
| `plot_wordcloud` | Faceted wordclouds (irrigators vs scientists) |

---

## License

This project is released under the **MIT License**.

```
MIT License

Copyright (c) 2026 Arnald Puy

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
