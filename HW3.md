HW3
================
wd2311
2025-10-14

# Loading libraries

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.2
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(p8105.datasets)
library(knitr)
library(forcats)
library(patchwork)
library(scales)
```

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

# Problem 1

## Set up and brief description

The instacart dataset contains 1,384,617 observations and 15 variables,
each representing a single product added to a user’s grocery order on
the Instacart online shopping platform. Each row corresponds to one
product within one order, identified by variables such as: order_id
(unique order identifier), product_id and product_name (the specific
item purchased), add_to_cart_order (the position in which the item was
added), reordered (whether the product had been purchased before by the
same user), order_dow and order_hour_of_day (day of week and time the
order was placed), aisle and department (product category
classification).

``` r
data("instacart") 

instacart |>
  summarize(
    n_obs = n(),
    n_vars = ncol(across()),
    var_names = paste(names(across()), collapse = ", ")
  )
```

    ## Warning: There was 1 warning in `summarize()`.
    ## ℹ In argument: `n_vars = ncol(across())`.
    ## Caused by warning:
    ## ! Using `across()` without supplying `.cols` was deprecated in dplyr 1.1.0.
    ## ℹ Please supply `.cols` instead.

    ## # A tibble: 1 × 3
    ##     n_obs n_vars var_names                                                      
    ##     <int>  <int> <chr>                                                          
    ## 1 1384617     16 order_id, product_id, add_to_cart_order, reordered, user_id, e…

``` r
glimpse(instacart)
```

    ## Rows: 1,384,617
    ## Columns: 15
    ## $ order_id               <int> 1, 1, 1, 1, 1, 1, 1, 1, 36, 36, 36, 36, 36, 36,…
    ## $ product_id             <int> 49302, 11109, 10246, 49683, 43633, 13176, 47209…
    ## $ add_to_cart_order      <int> 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8,…
    ## $ reordered              <int> 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1,…
    ## $ user_id                <int> 112108, 112108, 112108, 112108, 112108, 112108,…
    ## $ eval_set               <chr> "train", "train", "train", "train", "train", "t…
    ## $ order_number           <int> 4, 4, 4, 4, 4, 4, 4, 4, 23, 23, 23, 23, 23, 23,…
    ## $ order_dow              <int> 4, 4, 4, 4, 4, 4, 4, 4, 6, 6, 6, 6, 6, 6, 6, 6,…
    ## $ order_hour_of_day      <int> 10, 10, 10, 10, 10, 10, 10, 10, 18, 18, 18, 18,…
    ## $ days_since_prior_order <int> 9, 9, 9, 9, 9, 9, 9, 9, 30, 30, 30, 30, 30, 30,…
    ## $ product_name           <chr> "Bulgarian Yogurt", "Organic 4% Milk Fat Whole …
    ## $ aisle_id               <int> 120, 108, 83, 83, 95, 24, 24, 21, 2, 115, 53, 1…
    ## $ department_id          <int> 16, 16, 4, 4, 15, 4, 4, 16, 16, 7, 16, 4, 16, 2…
    ## $ aisle                  <chr> "yogurt", "other creams cheeses", "fresh vegeta…
    ## $ department             <chr> "dairy eggs", "dairy eggs", "produce", "produce…

## Description on aisles

There are 134 aisles. The aisles with the greatest number of items
ordered are “fresh vegetables” (150609 orders) and “fresh fruits”
(150473 orders).

``` r
aisle_counts = 
  instacart |>
  count(aisle, sort = TRUE)

n_aisles = nrow(aisle_counts)
n_aisles
```

    ## [1] 134

``` r
# top aisles
aisle_counts |> 
  slice_head(n = 10) |> 
  kable()
```

| aisle                         |      n |
|:------------------------------|-------:|
| fresh vegetables              | 150609 |
| fresh fruits                  | 150473 |
| packaged vegetables fruits    |  78493 |
| yogurt                        |  55240 |
| packaged cheese               |  41699 |
| water seltzer sparkling water |  36617 |
| milk                          |  32644 |
| chips pretzels                |  31269 |
| soy lactosefree               |  26240 |
| bread                         |  23635 |

## Plot on aisles with more than 10000 items

``` r
aisle_counts |>
  filter(n > 10000) |>
  mutate(aisle = fct_reorder(aisle, n)) |>
  ggplot(aes(x = n, y = aisle)) +
  geom_col() +
  labs(
    x = "Number of items ordered",
    y = NULL,
    title = "Items ordered by aisle (aisles with > 10,000 items)"
  ) +
  theme_minimal(base_size = 12)
```

![](HW3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Table for top3 items by aisle

``` r
top3_by_aisle = 
  instacart |>
  filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) |>
  count(aisle, product_name, sort = TRUE) |>
  group_by(aisle) |>
  slice_max(n, n = 3, with_ties = FALSE) |>
  arrange(aisle, desc(n)) |>
  ungroup()

kable(top3_by_aisle, col.names = c("Aisle", "Product", "Times ordered"))
```

| Aisle | Product | Times ordered |
|:---|:---|---:|
| baking ingredients | Light Brown Sugar | 499 |
| baking ingredients | Pure Baking Soda | 387 |
| baking ingredients | Cane Sugar | 336 |
| dog food care | Snack Sticks Chicken & Rice Recipe Dog Treats | 30 |
| dog food care | Organix Chicken & Brown Rice Recipe | 28 |
| dog food care | Small Dog Biscuits | 26 |
| packaged vegetables fruits | Organic Baby Spinach | 9784 |
| packaged vegetables fruits | Organic Raspberries | 5546 |
| packaged vegetables fruits | Organic Blueberries | 4966 |

## Table for mean hours of the day

``` r
dow_labs = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")

mean_hour_tbl = 
  instacart |>
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) |>
  mutate(dow = factor(order_dow, levels = 0:6, labels = dow_labs)) |>
  group_by(product_name, dow) |>
  summarise(mean_hour = mean(order_hour_of_day), .groups = "drop") |>
  mutate(mean_hour = round(mean_hour, 2)) |>
  pivot_wider(names_from = dow, values_from = mean_hour) |>
  arrange(product_name)

kable(mean_hour_tbl, col.names = c("Product","Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
```

| Product          |   Sun |   Mon |   Tue |   Wed |   Thu |   Fri |   Sat |
|:-----------------|------:|------:|------:|------:|------:|------:|------:|
| Coffee Ice Cream | 13.77 | 14.32 | 15.38 | 15.32 | 15.22 | 12.26 | 13.83 |
| Pink Lady Apples | 13.44 | 11.36 | 11.70 | 14.25 | 11.55 | 12.78 | 11.94 |

# Problem 2

## Data Set-up

``` r
zip_meta = 
  read_csv("data/Zip Codes.csv") |> 
  clean_names() 
```

    ## Rows: 322 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): County, County Code, File Date, Neighborhood
    ## dbl (3): State FIPS, County FIPS, ZipCode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
zori_raw = 
  read_csv("data/Zip_zori_uc_sfrcondomfr_sm_month_NYC.csv", show_col_types = FALSE) |> 
  clean_names() 

# Check column names
names(zip_meta)
```

    ## [1] "county"       "state_fips"   "county_code"  "county_fips"  "zip_code"    
    ## [6] "file_date"    "neighborhood"

``` r
names(zori_raw)
```

    ##   [1] "region_id"   "size_rank"   "region_name" "region_type" "state_name" 
    ##   [6] "state"       "city"        "metro"       "county_name" "x2015_01_31"
    ##  [11] "x2015_02_28" "x2015_03_31" "x2015_04_30" "x2015_05_31" "x2015_06_30"
    ##  [16] "x2015_07_31" "x2015_08_31" "x2015_09_30" "x2015_10_31" "x2015_11_30"
    ##  [21] "x2015_12_31" "x2016_01_31" "x2016_02_29" "x2016_03_31" "x2016_04_30"
    ##  [26] "x2016_05_31" "x2016_06_30" "x2016_07_31" "x2016_08_31" "x2016_09_30"
    ##  [31] "x2016_10_31" "x2016_11_30" "x2016_12_31" "x2017_01_31" "x2017_02_28"
    ##  [36] "x2017_03_31" "x2017_04_30" "x2017_05_31" "x2017_06_30" "x2017_07_31"
    ##  [41] "x2017_08_31" "x2017_09_30" "x2017_10_31" "x2017_11_30" "x2017_12_31"
    ##  [46] "x2018_01_31" "x2018_02_28" "x2018_03_31" "x2018_04_30" "x2018_05_31"
    ##  [51] "x2018_06_30" "x2018_07_31" "x2018_08_31" "x2018_09_30" "x2018_10_31"
    ##  [56] "x2018_11_30" "x2018_12_31" "x2019_01_31" "x2019_02_28" "x2019_03_31"
    ##  [61] "x2019_04_30" "x2019_05_31" "x2019_06_30" "x2019_07_31" "x2019_08_31"
    ##  [66] "x2019_09_30" "x2019_10_31" "x2019_11_30" "x2019_12_31" "x2020_01_31"
    ##  [71] "x2020_02_29" "x2020_03_31" "x2020_04_30" "x2020_05_31" "x2020_06_30"
    ##  [76] "x2020_07_31" "x2020_08_31" "x2020_09_30" "x2020_10_31" "x2020_11_30"
    ##  [81] "x2020_12_31" "x2021_01_31" "x2021_02_28" "x2021_03_31" "x2021_04_30"
    ##  [86] "x2021_05_31" "x2021_06_30" "x2021_07_31" "x2021_08_31" "x2021_09_30"
    ##  [91] "x2021_10_31" "x2021_11_30" "x2021_12_31" "x2022_01_31" "x2022_02_28"
    ##  [96] "x2022_03_31" "x2022_04_30" "x2022_05_31" "x2022_06_30" "x2022_07_31"
    ## [101] "x2022_08_31" "x2022_09_30" "x2022_10_31" "x2022_11_30" "x2022_12_31"
    ## [106] "x2023_01_31" "x2023_02_28" "x2023_03_31" "x2023_04_30" "x2023_05_31"
    ## [111] "x2023_06_30" "x2023_07_31" "x2023_08_31" "x2023_09_30" "x2023_10_31"
    ## [116] "x2023_11_30" "x2023_12_31" "x2024_01_31" "x2024_02_29" "x2024_03_31"
    ## [121] "x2024_04_30" "x2024_05_31" "x2024_06_30" "x2024_07_31" "x2024_08_31"

## Data preparation

``` r
zip_meta = 
  zip_meta |>
  mutate(
    zip = str_pad(as.character(zip_code), 5, pad = "0"),
    county = str_to_title(county) |> str_trim()
  )

county_to_borough <- c(
  "Bronx"          = "Bronx",
  "Kings"          = "Brooklyn",
  "New York"       = "Manhattan",
  "Queens"         = "Queens",
  "Richmond"       = "Staten Island"
)

zip_meta = 
  zip_meta |>
  mutate(borough = recode(county, !!!county_to_borough)) |>
  filter(!is.na(borough)) |>
  select(zip, borough, county, neighborhood)
```

## Turning to long form

``` r
zip_lookup =
  zip_meta |>
  filter(!is.na(zip), !is.na(borough)) |>
  distinct(zip, borough) |>
  group_by(zip) |>
  slice(1) |>          
  ungroup()

zori_long = 
  zori_raw |>
  mutate(
    zip = str_pad(as.character(region_name), 5, pad = "0")
  ) |>
  pivot_longer(
    cols = matches("^x\\d{4}_\\d{2}_\\d{2}$"),
    names_to = "date_raw",
    values_to = "zori"
  ) |>
  mutate(
    date  = ymd(str_replace_all(str_remove(date_raw, "^x"), "_", "-")),
    year  = year(date),
    month = month(date)
  ) |>
  filter(!is.na(date))

nyc = 
  zori_long |>
  left_join(zip_lookup, by = "zip") |>
  filter(!is.na(borough))
```

## Monthly Observation

ZIP codes observed exactly 116 times: 149 ZIP codes observed fewer than
10 times: 0

``` r
nyc_target = 
  nyc |>
  filter(date >= ymd("2015-01-01"), date <= ymd("2024-08-31"))

months_per_zip = 
  nyc_target |>
  count(zip, name = "n_months")

n_116  = months_per_zip |> 
  filter(n_months == 116) |> 
  nrow()
n_lt10 = months_per_zip |> 
  filter(n_months < 10)   |> 
  nrow()

cat("ZIP codes observed exactly 116 times:", n_116, "\n")
```

    ## ZIP codes observed exactly 116 times: 149

``` r
cat("ZIP codes observed fewer than 10 times:", n_lt10, "\n")
```

    ## ZIP codes observed fewer than 10 times: 0

## Table on Borough × Year

Average rents rise from 2015 to 2024 with a brief drop in 2020/2021.
Manhattan demonstrates the most expensive rents and Bronx the lowest.

``` r
dir.create("results", showWarnings = FALSE)

borough_year_tbl = 
  nyc_target |>
  group_by(borough, year) |>
  summarize(avg_rent = mean(zori, na.rm = TRUE), .groups = "drop") |>
  arrange(borough, year)

borough_year_wide = 
  borough_year_tbl |>
  pivot_wider(names_from = year, values_from = avg_rent) |>
  mutate(across(-borough, ~ round(.x, 0))) 

kable(
  borough_year_wide |>
    mutate(across(-borough, ~ dollar(.x))),
  caption = "Average Rental Price by Borough and Year (ZORI)"
)
```

| borough | 2015 | 2016 | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| Bronx | \$1,760 | \$1,520 | \$1,544 | \$1,639 | \$1,706 | \$1,811 | \$1,858 | \$2,054 | \$2,285 | \$2,497 |
| Brooklyn | \$2,493 | \$2,520 | \$2,546 | \$2,547 | \$2,631 | \$2,555 | \$2,550 | \$2,868 | \$3,015 | \$3,126 |
| Manhattan | \$3,022 | \$3,039 | \$3,134 | \$3,184 | \$3,310 | \$3,107 | \$3,137 | \$3,778 | \$3,933 | \$4,078 |
| Queens | \$2,215 | \$2,272 | \$2,263 | \$2,292 | \$2,388 | \$2,316 | \$2,211 | \$2,406 | \$2,562 | \$2,694 |
| Staten Island | NA | NA | NA | NA | NA | \$1,978 | \$2,045 | \$2,147 | \$2,333 | \$2,536 |

Average Rental Price by Borough and Year (ZORI)

## Plot for all available years

Rents generally rise from 2015 to 2024, with a little drop in the year
of 2021. Manhattan is consistently the highest with most smooth curve,
demonstrate low short-time variation.

``` r
nyc_clean = 
  nyc_target |>
  filter(!is.na(zori), is.finite(zori))  

all_years_plot = 
  nyc_clean |>
  group_by(borough, date) |>
  mutate(borough_median = median(zori, na.rm = TRUE)) |>
  ungroup() |>
  ggplot(aes(x = date, y = zori, group = zip)) +
  geom_line(alpha = 0.08) +
  stat_summary(
    aes(group = borough),
    fun = median, geom = "line", linewidth = 1.1
  ) +
  facet_wrap(~ borough, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "NYC Rental Prices within ZIP Codes (All Years)",
    subtitle = "Thin lines: ZIP series · Thick line: Borough median ZORI",
    x = NULL, y = "ZORI ($)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())

ggsave("results/plot_all_years_by_borough.png", all_years_plot,
       width = 11, height = 7, dpi = 300)

all_years_plot
```

![](HW3_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Average rental price in 2023

In 2023, ZIP-level average rents are highest in Manhattan followed by
Brooklyn. Manhattan demonstrated widest spread and a right-skewed upper
tail, indicating substantial within-borough variation and high-end
ZIPs.Other areas also shows a generally right skewed figure.Staten
Island demonstrates lowest variation.

``` r
zip_2023 = 
  nyc |>
  filter(year(date) == 2023) |>
  group_by(zip, borough) |>
  summarize(avg_2023 = mean(zori, na.rm = TRUE), .groups = "drop") |>
  mutate(avg_2023 = ifelse(is.nan(avg_2023), NA_real_, avg_2023)) |>
  drop_na(borough, avg_2023)

dist_2023_plot <- ggplot(zip_2023, aes(x = borough, y = avg_2023)) +
  geom_violin(trim = TRUE, alpha = 0.5) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.3) +
  stat_summary(fun = median, geom = "point", size = 2) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "Distribution of ZIP-level Average Rental Prices (2023)",
    subtitle = "Violin = distribution · Box = IQR · Dot = median",
    x = "Borough", y = "Average ZORI in 2023 ($)"
  ) +
  theme_minimal(base_size = 12)

ggsave("results/plot_2023_distribution.png", dist_2023_plot,
       width = 9, height = 6, dpi = 300)

dist_2023_plot
```

![](HW3_files/figure-gfm/unnamed-chunk-13-1.png)<!-- --> \## Combine
graph

``` r
combined = 
  all_years_plot | dist_2023_plot +
  patchwork::plot_annotation(
    title = "NYC Rental Prices: Long-run Trends and 2023 Distributions"
  )

ggsave("results/combined_plots.png", combined,
                width = 17, height = 12, dpi = 300)
combined
```

![](HW3_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

# Problem 3

## Read-in

``` r
covar_raw = 
  read_csv("data/nhanes_covar.csv", skip = 4, show_col_types = FALSE) |>
  clean_names()
accel_raw = 
  read_csv("data/nhanes_accel.csv",  show_col_types = FALSE) |> 
  clean_names()
```

## Tidy demongraphic data

``` r
covar = 
  covar_raw |>
  clean_names() |>
  mutate(
    seqn = suppressWarnings(as.numeric(seqn)),
    age  = suppressWarnings(as.numeric(age)),
    bmi  = suppressWarnings(as.numeric(bmi)),
    sex = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female",
      str_to_lower(as.character(sex)) %in% c("m","male") ~ "Male",
      str_to_lower(as.character(sex)) %in% c("f","female") ~ "Female",
      TRUE ~ NA_character_
    ),
    education = case_when(
      education == 1 ~ "Less than HS",
      education == 2 ~ "High school",
      education == 3 ~ "More than HS",
      str_detect(str_to_lower(as.character(education)), "less") ~ "Less than HS",
      str_detect(str_to_lower(as.character(education)), "high") ~ "High school",
      str_detect(str_to_lower(as.character(education)), "more") ~ "More than HS",
      TRUE ~ NA_character_
    )
  ) |>
  filter(
    !is.na(seqn), !is.na(sex), !is.na(age), !is.na(bmi), !is.na(education),
    age >= 21
  ) |>
  mutate(
    sex = factor(sex, levels = c("Male","Female")),
    education = factor(education,
                       levels = c("Less than HS","High school","More than HS"),
                       ordered = TRUE)
  ) |>
  distinct(seqn, .keep_all = TRUE)
```

## Tidy accelerometer data

``` r
accel_core = 
  accel_raw |>
  clean_names() |>
  mutate(seqn = suppressWarnings(as.numeric(seqn))) |>
  distinct(seqn, .keep_all = TRUE)

minute_cols = 
  names(accel_core)[grepl("^min\\d{1,4}$", names(accel_core))]

accel_core = 
  accel_core |>
  mutate(
    across(
      all_of(minute_cols),
      ~ suppressWarnings(as.numeric(.x))
    )
  ) |>
  mutate(
    across(
      all_of(minute_cols),
      ~ ifelse(!is.na(.x) & .x < 0, NA_real_, .x)
    )
  )

accel_long = 
  accel_core |>
  pivot_longer(
    cols = all_of(minute_cols),
    names_to  = "minute_var",
    values_to = "mims"
  ) |>
  mutate(
    minute = suppressWarnings(as.integer(stringr::str_remove(minute_var, "^min")))
  ) |>
  filter(!is.na(minute), minute >= 1, minute <= 1440) |>
  mutate(
    time_of_day = as.POSIXct("2000-01-01 00:00:00", tz = "UTC") + lubridate::dminutes(minute - 1L)
  ) |>
  select(seqn, minute, time_of_day, mims)
```

## Merge

``` r
nhanes_long = 
  covar |>
  inner_join(accel_long, by = "seqn")
```

## Table on Counts of Men and Women by Education

The table shows a balanced gender distribution across education levels.
Men slightly outnumber women in the High School group (60.3% vs. 39.7%),
while other groups are nearly equal.

``` r
sex_by_edu = 
  covar |>
  count(education, sex, name = "Count") |>
  group_by(education) |>
  mutate(Percent = Count / sum(Count)) |>
  ungroup() |>
  mutate(Percent = percent(Percent, accuracy = 0.1)) |>
  arrange(education, sex)

kable(
  sex_by_edu,
  caption = "Counts of Men and Women by Education (NHANES accelerometer sample, ≥21y)",
  align = c("l", "l", "r", "r")
)
```

| education    | sex    | Count | Percent |
|:-------------|:-------|------:|--------:|
| Less than HS | Male   |    27 |   49.1% |
| Less than HS | Female |    28 |   50.9% |
| High school  | Male   |    35 |   60.3% |
| High school  | Female |    23 |   39.7% |
| More than HS | Male   |    56 |   48.7% |
| More than HS | Female |    59 |   51.3% |

Counts of Men and Women by Education (NHANES accelerometer sample, ≥21y)

## Visualization of age distributions

The density plots indicate similar age distributions between men and
women within each education level. Most participants are between ages
50–70, with the More than High School group showing a slightly younger
range. Overall, no major age differences are observed between sexes.

``` r
p_age_density = 
  ggplot(covar, aes(x = age, fill = sex, color = sex)) +
  geom_density(alpha = 0.25) +
  facet_wrap(~ education, nrow = 1, scales = "free_y") +
  labs(
    title = "Age distributions by sex within education levels",
    x = "Age (years)", y = "Density", fill = "Sex", color = "Sex"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

p_age_density
```

![](HW3_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

## Total Daily activities vs. Age

Across all education levels, total daily activity tends to decline with
age. Men and women show similar overall patterns, though women appear
slightly more active in the High School and More than High School
groups. The wide spread of points indicates large individual variation
beyond age and sex differences.

``` r
totals = 
  nhanes_long %>%
  group_by(seqn) %>%
  summarize(total_activity = sum(mims, na.rm = TRUE), .groups = "drop") %>%
  inner_join(covar, by = "seqn")  

p_total_vs_age = 
  ggplot(totals, aes(x = age, y = total_activity, color = sex)) +
  geom_point(alpha = 0.6, size = 1.6) +
  geom_smooth(se = FALSE, method = "loess", span = 0.9) +
  facet_wrap(~ education, nrow = 1, scales = "free_y") +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(
    title = "Total daily activity vs. age",
    subtitle = "Colored by sex; faceted by education level",
    x = "Age (years)",
    y = "Total daily activity (sum of minute-level MIMS)",
    color = "Sex"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

p_total_vs_age
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](HW3_files/figure-gfm/unnamed-chunk-21-1.png)<!-- --> \## 24 hour
activity time courses by education

``` r
diurnal_mean = 
  nhanes_long %>%
  group_by(education, sex, minute) %>%
  summarize(mean_mims = mean(mims, na.rm = TRUE), .groups = "drop") %>%
  mutate(time_of_day = as.POSIXct("2000-01-01 00:00:00", tz = "UTC") + dminutes(minute - 1L))

p_diurnal = 
  ggplot(diurnal_mean, aes(x = time_of_day, y = mean_mims, color = sex)) +
  geom_line(alpha = 0.7, linewidth = 0.6) +
  geom_smooth(se = FALSE, method = "loess", span = 0.1, linewidth = 0.9) +
  facet_wrap(~ education, nrow = 1, scales = "free_y") +
  scale_x_datetime(
    labels = date_format("%H:%M"),
    breaks = date_breaks("4 hours"),
    minor_breaks = date_breaks("1 hour")
  ) +
  labs(
    title = "24-hour activity time courses by education",
    subtitle = "Mean minute-level MIMS; color indicates sex",
    x = "Time of day", y = "Mean MIMS", color = "Sex"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

p_diurnal
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](HW3_files/figure-gfm/unnamed-chunk-22-1.png)<!-- --> Across all
education groups, activity levels rise sharply in the morning, stay high
throughout daytime, and drop at night. This consistent 24-hour rhythm
reflects typical waking and sleeping patterns. Women show slightly
higher mean activity during daytime—especially in the High school and
More than HS groups—while men display a steeper evening decline in the
Less than HS group. Overall, diurnal patterns are similar across sexes
and education levels.
