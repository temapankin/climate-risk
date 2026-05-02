setwd("/Volumes/SSD/climate-risk/")

library(tidyverse)
library(arrow)
library(fixest)

# ── Load data ──────────────────────────────────────────────────────────────────
df <- read_parquet("data/parcels_shore_clean.parquet") 

# how many NAs in each variable?
sapply(df, function(x) sum(is.na(x)))

df_clean <- df |>
  filter(
    sale_price_2020 > 0,
    sale_year_1 >= 2000 & sale_year_1 <= 2025,
    dist_to_ocean_mi > 0,
    land_value > 0,
    improvement_value > 0,
    !is.na(pct_seasonal),
    !is.na(census_tract_geoid),
    !is.na(flood_risk) & flood_risk != "Undetermined",
    !is.na(pct_seasonal),
    !is.na(year_constructed),



  ) |>
  mutate(
    # log transformations of vairables of interest
    log_price    = log(sale_price_2020),
    log_dist     = log(dist_to_ocean_mi),
    log_land     = log(land_value),
    log_improve  = log(improvement_value),

    # controls
    age = 2026 - year_constructed,
    flood_risk = as.factor(flood_risk),
    # relevel flood risk so that "Low" is the reference category
    flood_risk = relevel(flood_risk, ref = "Low"),
    # bd_stories = as.factor(bd_stories),
    #bd_garage_type = as.factor(bd_garage_type),
    #bd_garage_stalls = as.factor(bd_garage_stalls),
    #bd_material = as.factor(bd_material),

    # if garage_type is a value or garage_stalls is a value, then we have a garage; otherwise, no garage
   #has_garage = ifelse(!is.na(bd_garage_type) | !is.na(bd_garage_stalls), 1, 0),
 
    # fixed effects
    year_fe      = as.factor(sale_year_1),
    tract_fe     = as.factor(census_tract_geoid),
    county_fe    = as.factor(mod_iv_county_name),

  )

cat(sprintf("Observations: %s\n", format(nrow(df_clean), big.mark = ",")))

# Fit the hedonic regression model
m1 <- feols(log_price ~ log_dist + flood_risk + pct_seasonal + log_dist:pct_seasonal + flood_risk:pct_seasonal + age + log_land + log_improve, data = df_clean)

# with year fixed effects
m2 <- feols(log_price ~ log_dist + flood_risk + pct_seasonal + log_dist:pct_seasonal + flood_risk:pct_seasonal + age + log_land + log_improve | year_fe,
cluster = ~mod_iv_county_name, data = df_clean)

# with year and county fixed effects
m3 <- feols(log_price ~ log_dist + flood_risk + pct_seasonal + log_dist:pct_seasonal + flood_risk:pct_seasonal + age + log_land + log_improve | year_fe + county_fe, 
cluster = ~mod_iv_county_name , data = df_clean)

# with year fixed effects and census tract fixed effects
m4 <- feols(log_price ~ log_dist + flood_risk + age + log_land + log_improve | year_fe + tract_fe,
cluster = ~tract_fe, data = df_clean)


# ── Table formatting ───────────────────────────────────────────────────────────
dict_console <- c(
  log_price                             = "log(Sale Price, 2020$)",
  log_dist                              = "log(Distance to Ocean)",
  flood_riskHigh                        = "Flood Risk: High",
  flood_riskModerate                    = "Flood Risk: Moderate",
  flood_riskMedium                      = "Flood Risk: Medium",
  pct_seasonal                          = "Pct. Seasonal Units",
  age                                   = "Age (years)",
  log_land                              = "log(Land Value)",
  log_improve                           = "log(Improvement Value)",
  "log_dist:pct_seasonal"               = "log(Distance) x Pct. Seasonal",
  "flood_riskHigh:pct_seasonal"         = "High Risk x Pct. Seasonal",
  "flood_riskModerate:pct_seasonal"     = "Moderate Risk x Pct. Seasonal",
  "flood_riskMedium:pct_seasonal"       = "Medium Risk x Pct. Seasonal",
  year_fe                               = "Year",
  county_fe                             = "County",
  mod_iv_county_name                    = "County",
  tract_fe                              = "Census Tract"
)

dict_tex <- replace(dict_console,
  c("log_dist:pct_seasonal", "flood_riskHigh:pct_seasonal",
    "flood_riskModerate:pct_seasonal", "flood_riskMedium:pct_seasonal"),
  c("log(Distance) $\\times$ Pct. Seasonal", "High Risk $\\times$ Pct. Seasonal",
    "Moderate Risk $\\times$ Pct. Seasonal", "Medium Risk $\\times$ Pct. Seasonal")
)

headers <- c("OLS", "Year FE", "Year + County FE", "Year + Tract FE")

etable(m1, m2, m3, m4,
       fitstat  = ~ n + r2 + ar2,
       se.below = TRUE,
       dict     = dict_console,
       headers  = headers,
       title    = "Hedonic Regression Results")

# ── Export as PNG ──────────────────────────────────────────────────────────────
if (!requireNamespace("tinytex", quietly = TRUE)) install.packages("tinytex")
if (!requireNamespace("magick",  quietly = TRUE)) install.packages("magick")
if (!tinytex::is_tinytex()) tinytex::install_tinytex()

dir.create("figures", showWarnings = FALSE)

tex_file <- "figures/hedonic_regression_results.tex"
if (file.exists(tex_file)) file.remove(tex_file)

capture.output(
  etable(m1, m2, m3, m4,
         fitstat   = ~ n + r2 + ar2,
         tex       = TRUE,
         se.below  = TRUE,
         dict      = dict_tex,
         headers   = headers,
         title     = "Hedonic Regression Results",
         style.tex = style.tex("aer"),
         file      = tex_file),
  file = NULL
)

tex_lines <- readLines(tex_file)
tex_lines <- tex_lines[!grepl(
  "^[[:space:]]*(\\\\begin\\{table\\}|\\\\end\\{table\\}|\\\\bigskip|\\\\centering|\\\\caption)",
  tex_lines
)]

# Wrap tabular in threeparttable so the sig note sits flush under the table
i_begin <- which(grepl("\\\\begin\\{tabular\\}", tex_lines))
i_end   <- which(grepl("\\\\end\\{tabular\\}",   tex_lines))
tex_lines <- c(
  tex_lines[seq_len(i_begin - 1)],
  "\\begin{threeparttable}",
  tex_lines[i_begin:i_end],
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{Signif.\\ codes:} 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  tex_lines[seq(i_end + 1, length(tex_lines))]
)

writeLines(c(
  "\\documentclass{article}",
  "\\usepackage[paperwidth=35cm, paperheight=28cm, margin=1.5cm]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{amssymb}",
  "\\usepackage{threeparttable}",
  "\\pagestyle{empty}",
  "\\begin{document}",
  "\\centering",
  tex_lines,
  "\\end{document}"
), "figures/hedonic_regression_results_doc.tex")

tinytex::pdflatex("figures/hedonic_regression_results_doc.tex",
                  pdf_file = "figures/hedonic_regression_results_doc.pdf")

magick::image_read_pdf("figures/hedonic_regression_results_doc.pdf", density = 300) |>
  magick::image_trim() |>
  magick::image_border("white", "30x30") |>
  magick::image_write("figures/hedonic_regression_results.png")

message("Saved → figures/hedonic_regression_results.png")
