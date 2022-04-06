CIEE Living Data Project
================
Living Data Project, Maxime Fraser Franco
05 April, 2022

## 3 datasets:

  - Secondary insects and fungi post-hemlock looper on Anticosti in 1973
  - Ground beetle and lepidoptera biodiversity on Anticosti in 1993
  - Stanley Park Winter Waterbird Surveys, British Columbia, Canada
    (1995-2019)

## Ecological concepts for Stanley Park:

  - change in state ?
  - beta diversity across years

## 1\. Setup working environment

``` r
# Load libraries
library(data.table)
library(ggplot2)
library(ggpubr)
```

## 2\. Import the data

### 2.1 URL paths to datasets

``` r
# Seabirds data
url_birds <- "https://data-donnees.ec.gc.ca/data/species/scientificknowledge/stanley-park-winter-waterbird-surveys-british-columbia-canada-1995-2019"
csv_birds <- "dbs_stanley_park_winter_waterbirds_en_20200324_data_catalogue.csv"

# Tree condition data
url_tree <- "https://open.canada.ca/data/en/dataset/9dda09b0-649f-4002-b207-7b204eb81cbb/resource/a1606235-95aa-4c81-a8db-19b852be288c"
csv_tree <- "download/anticosti_1973_trees.csv"

# Beetle data
url_beetle <- "https://open.canada.ca/data/dataset/f55b476f-534f-4d4c-aacf-3088724cc5fd/resource/3643d857-8f8f-4f9b-9bac-8fde79ea8aa2"
csv_beetle <- "download/microsigebready_carabeslepidoptera_anticosti_1993_en.csv"
```

### 2.2 Import data in R environment

``` r
# Seabirds data
dat_birds <- fread(file.path(url_birds, csv_birds),
                   select = c("season", "year", "month",
                              "day", "date_ddmmyyyy", "species_scientific_name",
                              "species_code", "order", "family",
                              "nasal_disk", "females", "males", "juveniles",
                              "unknown_sex", "pairs", "disturbance",
                              "number_individuals"))
# All variables
#dat <- fread(file.path(website_url, csv_link))

# Tree data
dat_tree <- fread(file.path(url_tree, csv_tree))

# Beetle data
dat_beetle <- fread(file.path(url_beetle, csv_beetle))
```

## 3\. Explore the beetle data

### 3.1 General data exploration

``` r
# Data structure ----------------------------

str(dat_beetle)
names(dat_beetle)
head(dat_beetle)



# Count factor levels -----------------------

# Check number of sites and stations
levels(as.factor(dat_beetle$Note_toponyme)) # topo
levels(as.factor(dat_beetle$Station_no)) # site + station (or treatment?)

# Check number of years
levels(as.factor(dat_beetle$Year))
# only 1993

# Check number of programs
levels(as.factor(dat_beetle$Code_program))
# two but they are the same

# Check number of persons in charge
levels(as.factor(dat_beetle$Code_PersonInCharge))
# 1 person in charge

# Check number of attractants
levels(as.factor(dat_beetle$Code_stimulus))
# only one attractant



# Count taxons ------------------------------

# How many species
length(unique(dat_beetle$Species)) #171

# How many generas
length(unique(dat_beetle$Genera)) #133

# How many families
length(unique(dat_beetle$Family)) # 22
```

### 3.2 Compute some variables

``` r
# Species richness by topo ------------------

# Compute the variable
dat_beetle[, richness_topo := length(unique(Species)),
           by = Note_toponyme]



# Species richness by station ---------------

# Separate the station variable
dat_beetle[, paste0("station_no", 1:2) := tstrsplit(Station_no, " - ")]

# site 1 variable (need to check what it is)
dat_beetle[, richness_station1 := length(unique(Species)),
           by = station_no1]

# site 2 variable (need to check what it is)
dat_beetle[, richness_station2 := length(unique(Species)),
           by = station_no2]

# Site 1 and 2 combined
dat_beetle[, richness_stations := length(unique(Species)),
           by = .(station_no1, station_no2)]
```

### 3.3 Plots for beetles

``` r
# Prepare plotting options ----------------------------------------------

# Create a custom theme
custom_theme <- theme(# axis values size
                      axis.text.x = element_text(face = "plain", 
                                                 size = 15,
                                                 color = "black"),
                      axis.text.y = element_text(face = "plain", 
                                                 size = 15,
                                                 color = "black"),
                      # axis ticks lenght
                      axis.ticks.length = unit(.15, "cm"),
                      # axis ticks width
                      axis.ticks = element_line(size = 0.90, 
                                                color = "black"),
                      # axis titles size
                      axis.title = element_text(size = 15, 
                                                face = "plain"),
                      axis.line = element_line(size = 0.95),
                      #  plot.margin = unit(c(2, 1.2, 2, 2), "lines"),
                      legend.position = "none",
                      panel.grid = element_blank(),
                      panel.background = element_blank())

# Prepare a dataset of unique observations for the plots
beetle_plots <- unique(dat_beetle[, .(station_no1, station_no2,
                                      richness_station1,
                                      richness_station2,
                                      richness_stations)])

# Change chr variable types to factors
beetle_plots[, ":=" (station_no1 = as.factor(station_no1),
                     station_no2 = as.factor(station_no2))]



# Compute plots ---------------------------------------------------------

# Barplot of richness by station2
with_la <- ggplot(beetle_plots,
                  aes(x = station_no2,
                      y = richness_stations)) +
               geom_boxplot(fill = "#A4A4A4",
                            color = "black") +
               geom_dotplot(binaxis = "y",
                            stackdir = "center",
                            dotsize = 0.4) +
               geom_jitter(shape = 16,
                           position = position_jitter(0.2)) +
               xlab("\nStation") +
               ylab("Species richness\n") +
               custom_theme

no_la <- ggplot(beetle_plots[station_no2 != "LA",],
                aes(x = station_no2,
                    y = richness_stations)) +
             geom_boxplot(fill = "#A4A4A4",
                          color = "black") +
             geom_dotplot(binaxis = "y",
                          stackdir = "center",
                          dotsize = 0.4) +
             geom_jitter(shape = 16,
                         position = position_jitter(0.2)) +
               xlab("\nStation") +
               ylab("Species richness\n") +
             custom_theme


# Distribution of species richness across all stations
hist <- ggplot(beetle_plots, aes(x = richness_stations)) +
            geom_histogram(color = "black",
                           fill = "#A4A4A4") +
            geom_vline(aes(xintercept = mean(richness_stations)),
                       color = "blue",
                       linetype = "dashed",
                       size = 1) +
            xlab("\nSpecies richness") +
            ylab("Frequency\n") +
            custom_theme


# Distribution of species richness by station
beetle_plots[,  means := mean(richness_stations),
             by = station_no2]

hist2 <- ggplot(beetle_plots, aes(x = richness_stations)) +
            geom_histogram(color = "black",
                           fill = "#A4A4A4") +
            geom_vline(aes(xintercept = means),
                      color = "blue",
                      linetype = "dashed",
                      size = 1) +
            facet_grid(station_no2 ~ .) +
            xlab("\nSpecies richness") +
            ylab("Frequency\n") +
            theme_bw()



# Save the plots --------------------------------------------------------

ggexport(no_la,
         filename = "./plots/plot_nola.png",
         width = 2000,
         height = 2000,
         res = 300)

ggexport(with_la,
         filename = "./plots/plot_la.png",
         width = 2000,
         height = 2000,
         res = 300)

ggexport(hist,
         filename = "./plots/hist.png",
         width = 2000,
         height = 2000,
         res = 300)

ggexport(hist2,
         filename = "./plots/hist2.png",
         width = 2000,
         height = 2000,
         res = 300)
```