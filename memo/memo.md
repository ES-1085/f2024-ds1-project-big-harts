Hart Isalnd Trap memo
================
Big Harts!

This document should contain a detailed account of the data clean up for
your data and the design choices you are making for your plots. For
instance you will want to document choices you’ve made that were
intentional for your graphic, e.g. color you’ve chosen for the plot.
Think of this document as a code script someone can follow to reproduce
the data cleaning steps and graphics in your handout.

``` r
library(tidyverse)
library(broom)
```

## Data Clean Up Steps for Overall Data

### Step 1: Load original dataset

``` r
lobsters <- read_csv(file = "../data/lobsters.csv")
```

    ## New names:
    ## Rows: 227 Columns: 28
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (12): DATE COLLECTED (m-d-y), STATE, Z: ZONE, AREA, TAG ID NUMBER, TRAP ... dbl
    ## (4): TAG COUNT, # TAGS IN BUNDLE, GPS COORDINATES-Lat, TAGYEAR lgl (12): ...17,
    ## ...18, ...19, ...20, ...21, ...22, ...23, ...24, ...25, ......
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...17`
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`

``` r
glimpse(lobsters)
```

    ## Rows: 227
    ## Columns: 28
    ## $ `TAG COUNT`              <dbl> 90, 206, 31, 161, 183, 191, 43, 111, 201, 123…
    ## $ `DATE COLLECTED (m-d-y)` <chr> "8/24/2023", "8/24/2023", "8/24/2023", "8/24/…
    ## $ `# TAGS IN BUNDLE`       <dbl> 1, 1, 4, 1, 5, 2, 6, 5, 9, 1, 2, 2, 4, 3, 4, …
    ## $ STATE                    <chr> "ME", "ME", "ME", "ME", "ME", "ME", "ME", "ME…
    ## $ `Z: ZONE`                <chr> "D", "D", "D", "D", "D", "D", "D", "D", "D", …
    ## $ AREA                     <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", …
    ## $ `TAG ID NUMBER`          <chr> "435", "607", "754", "754", "1033", "1033", "…
    ## $ `TRAP BATCH NUMBER`      <chr> "72", "621", "94", "174", "499", "185", "142"…
    ## $ `TAG COLOUR`             <chr> "orange", "yellow", "light pink", "yellow", "…
    ## $ `SHORELINE WHERE FOUND`  <chr> "HART ISLAND", "HART ISLAND", "HART ISLAND", …
    ## $ `GPS COORDINATES-Lat`    <dbl> 43.90538, 43.90538, 43.90538, 43.90538, 43.90…
    ## $ `GPS COORD-Long`         <chr> "-69.266659", "-69.266659", "-69.266659", "-6…
    ## $ TAGYEAR                  <dbl> 2007, 2020, 2010, 2020, 2018, 2021, 2022, 202…
    ## $ `ADDITIONAL INFO ON TAG` <chr> NA, "also reads NON-CAT", NA, NA, NA, NA, "DM…
    ## $ `NAMED INDIVIDUAL`       <chr> "NOT IN 2021 DMR LICENSE LIST", "THOMAS YOUNG…
    ## $ `HOME PORT`              <chr> "N/A", "WINTER HBR", "PORT CLYDE", "PORT CLYD…
    ## $ ...17                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...18                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...19                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...20                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...21                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...22                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...23                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...24                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...25                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...26                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...27                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ MISCELLANEOUS            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…

### Step 2: Data cleaning

Remove unnecessary text rows at bottom of dataset:

``` r
true_lobsters <- head(lobsters, -4)
glimpse(true_lobsters)
```

    ## Rows: 223
    ## Columns: 28
    ## $ `TAG COUNT`              <dbl> 90, 206, 31, 161, 183, 191, 43, 111, 201, 123…
    ## $ `DATE COLLECTED (m-d-y)` <chr> "8/24/2023", "8/24/2023", "8/24/2023", "8/24/…
    ## $ `# TAGS IN BUNDLE`       <dbl> 1, 1, 4, 1, 5, 2, 6, 5, 9, 1, 2, 2, 4, 3, 4, …
    ## $ STATE                    <chr> "ME", "ME", "ME", "ME", "ME", "ME", "ME", "ME…
    ## $ `Z: ZONE`                <chr> "D", "D", "D", "D", "D", "D", "D", "D", "D", …
    ## $ AREA                     <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", …
    ## $ `TAG ID NUMBER`          <chr> "435", "607", "754", "754", "1033", "1033", "…
    ## $ `TRAP BATCH NUMBER`      <chr> "72", "621", "94", "174", "499", "185", "142"…
    ## $ `TAG COLOUR`             <chr> "orange", "yellow", "light pink", "yellow", "…
    ## $ `SHORELINE WHERE FOUND`  <chr> "HART ISLAND", "HART ISLAND", "HART ISLAND", …
    ## $ `GPS COORDINATES-Lat`    <dbl> 43.90538, 43.90538, 43.90538, 43.90538, 43.90…
    ## $ `GPS COORD-Long`         <chr> "-69.266659", "-69.266659", "-69.266659", "-6…
    ## $ TAGYEAR                  <dbl> 2007, 2020, 2010, 2020, 2018, 2021, 2022, 202…
    ## $ `ADDITIONAL INFO ON TAG` <chr> NA, "also reads NON-CAT", NA, NA, NA, NA, "DM…
    ## $ `NAMED INDIVIDUAL`       <chr> "NOT IN 2021 DMR LICENSE LIST", "THOMAS YOUNG…
    ## $ `HOME PORT`              <chr> "N/A", "WINTER HBR", "PORT CLYDE", "PORT CLYD…
    ## $ ...17                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...18                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...19                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...20                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...21                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...22                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...23                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...24                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...25                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...26                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ...27                    <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ MISCELLANEOUS            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…

Add in coordinates to each homeport:

``` r
homeport_coords <- true_lobsters |>
  mutate(
    "HOMEPORT LAT" = case_when(
     `HOME PORT` == "PORT CLYDE" ~ 43.9271,
     `HOME PORT` == "WINTER HBR" ~ 44.3948,
     `HOME PORT` == "ROCKLAND" ~ 44.1037,
     `HOME PORT` == "TENANTS HBR" ~ 43.9682,
     `HOME PORT` == "CUSHING" ~ 44.0138,
     `HOME PORT` == "OWLS HEAD" ~ 44.0823,
     `HOME PORT` == "ST GEORGE" ~ 44.0165
    )) |>
  mutate(
    "HOMEPORT LONG" = case_when(
     `HOME PORT` == "PORT CLYDE" ~ -69.2583,
     `HOME PORT` == "WINTER HBR" ~ -68.0830,
     `HOME PORT` == "ROCKLAND" ~ -69.1089,
     `HOME PORT` == "TENANTS HBR" ~ -69.2088,
     `HOME PORT` == "CUSHING" ~ -69.2615,
     `HOME PORT` == "OWLS HEAD" ~ -69.0573,
     `HOME PORT` == "ST GEORGE" ~ -69.1989
    ))
```

Assign acronyms to each individual and then remove their names to
anonymise them:

``` r
lobsters_anon <- homeport_coords |>   
  mutate(
    "INDIVIDUALS" = case_when(
      `NAMED INDIVIDUAL` == "THOMAS YOUNG/LC2O/WINTER HBR" ~ "T.Y.",
      `NAMED INDIVIDUAL` == "DOUGLAS ANDERSON/LC3/PORT CLYDE" ~ "D.A.",
      `NAMED INDIVIDUAL` == "KEITH YORK/LC2/ROCKLAND" ~ "K.Y.",
      `NAMED INDIVIDUAL` == "PETER HENDERSON/LNC/TENANTS HBR" ~ "P.H.",                      
      `NAMED INDIVIDUAL` == "CHRISTOPHER ANDERSON/LC2/PORT CLYDE" ~ "C.A.",                  
      `NAMED INDIVIDUAL` == "GARY LIBBY/LC2/PORT CLYDE" ~ "G.L.",                            
      `NAMED INDIVIDUAL` == "CHRISTOPHER CHADWICK/LC3/PORT CLYDE" ~ "C.C.",
      `NAMED INDIVIDUAL` == "DAVID TALOR/LC3/CUSHING" ~ "D.T.",
      `NAMED INDIVIDUAL` == "ERICH CULVER/LC2/PORT CLYDE" ~ "E.C.",
      `NAMED INDIVIDUAL` == "WHITNEY HUPPER/LC1/PORT CLYDE" ~ "W.H.",
      `NAMED INDIVIDUAL` == "GREG MORRIS/LC3/PORT CLYDE" ~ "G.M.",
      `NAMED INDIVIDUAL` == "JUSTIN THOMPSON/LC3/PORT CLYDE" ~ "J.T.",
      `NAMED INDIVIDUAL` == "DANIEL MORRIS/LC2/PORT CLYDE" ~ "D.M.",
      `NAMED INDIVIDUAL` == "DILLAN CUSHMAN/LC3/PORT CLYDE" ~ "D.C.",
      `NAMED INDIVIDUAL` == "SHANE HATCH/LC3/OWLS HEAD" ~ "S.H.",
      `NAMED INDIVIDUAL` == "WINSTON PEASE/LCO/PORT CLYDE" ~ "W.P.",
      `NAMED INDIVIDUAL` == "CHARLES CHRISTENSEN/LC1/PORT CLYDE" ~ "C.C.",
      `NAMED INDIVIDUAL` == "ANTHONY HOOPER/LC3/TENANTS HBR" ~ "A.H.",
      `NAMED INDIVIDUAL` == "BRIAN YORK/LC2/PORT CLYDE" ~ "B.Y.",
      `NAMED INDIVIDUAL` == "DYLAN LORD/STUDENT/ST GEORGE" ~ "D.L.",
      `NAMED INDIVIDUAL` == "JADEN PETERSDORF/LC2/PORT CLYDE" ~ "J.P.",
      `NAMED INDIVIDUAL` == "6691 IS LCS IN 2021; THESE TAGS ARE TOO OLD TO BE LCS" ~ "N/A",
      `NAMED INDIVIDUAL` == "NOT IN 2021 DMR LICENSE LIST" ~ "N/A"
    )) |>
  select(-`NAMED INDIVIDUAL`, -c(17:27), -`MISCELLANEOUS`)
```

## Plots

### ggsave example for saving plots

``` r
p1 <- starwars |>
  filter(mass < 1000, 
         species %in% c("Human", "Cerean", "Pau'an", "Droid", "Gungan")) |>
  ggplot() +
  geom_point(aes(x = mass, 
                 y = height, 
                 color = species)) +
  labs(x = "Weight (kg)", 
       y = "Height (m)",
       color = "Species",
       title = "Weight and Height of Select Starwars Species",
       caption = paste("This data comes from the starwars api: https://swapi.py43.com"))


ggsave("example-starwars.png", width = 4, height = 4)

ggsave("example-starwars-wide.png", width = 6, height = 4)
```

### Plot 1: Preliminary data visualisation

``` r
ggplot(data = true_lobsters, mapping = aes(x = `NAMED INDIVIDUAL`)) +
  geom_bar() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

<img src="memo_files/figure-gfm/prelim_vis_testing-1.png" alt="Bar plot of lobster traps collected on Hart Island that shows the number of traps belonging to each individual. The plot tells us that Brian York had the most traps, over 80"  />

### Plot 2: Test plot of trap ages

``` r
ggplot(data = true_lobsters, mapping = aes(x = `# TAGS IN BUNDLE`)) +
  geom_bar() +
  labs(x = "Age of Traps (Number of Tags in Bundle)", y = "Number of Traps",
  title = "Age of Traps Collected on Hart Island") +
  theme_linedraw() +
  scale_x_continuous(breaks = seq(0, 10, by = 1))
```

<img src="memo_files/figure-gfm/age_graph-1.png" alt="Bar plot showing the ages of lobster traps collected on Hart Island, displaying the amount of traps at each age. 1 year of age has the highest amount, with trap count decreasing as age increases, showing that fewer traps survive as time passes"  />

### Plot 3: Trap IDs Found on Hart Island

#### Data cleanup steps specific to plot 3

This graph is showing the correlation of number of lobster traps to
individuals and their home ports. We can see the majority of traps found
came from Port Clyde. While most of the individuals are linked to 30 or
less traps, one outlier from Port Clyde has closer to 85 lost traps. To
tidy the data, we arranged the graph in descending order using
arrange(desc(n)) function, and mutated the N/A variable to read as
“Unknown” using the mutate() function.

``` r
  trap_data <- true_lobsters %>%
  count(`TAG ID NUMBER`, `HOME PORT`) %>%
  arrange(desc(n)) %>%
  mutate(`HOME PORT` = if_else(`HOME PORT` == "N/A", "UNKNOWN", `HOME PORT`))

trap_data$`TAG ID NUMBER` <- fct_reorder(trap_data$`TAG ID NUMBER`, trap_data$n, .desc = TRUE)
```

``` r
ggplot(data = trap_data, aes(x = `TAG ID NUMBER`, y = n, fill = `HOME PORT`)) +
  geom_bar(stat = "identity") +
  labs(x = "Tag ID Number", y = "Number of Traps",
       title = "Trap IDs Found on Hart Island") +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  scale_fill_viridis_d(option = "H", name = "Home Port", direction = -1)
```

<img src="memo_files/figure-gfm/trap_ids-1.png" alt="Bar plot showing the number of lobster traps belonging to each individual collected on Hart Island. The plot shows that an overwhelming majority belonged to one person, with the ID 9899, having over 80 lost traps"  />

#### Final Plot 1

Add more plot sections as needed. Each project should have at least 3
plots, but talk to me if you have fewer than 3.

### Plot 4: \_\_\_\_\_\_\_\_\_\_\_
