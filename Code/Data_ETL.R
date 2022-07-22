############################################################################################################################################
# Author:   John Hinic
# Date:     27Jun2022
# Purpose:  Data ETL for ST 542 Consulting Project
############################################################################################################################################

library(tidyverse)
library(readxl)
library(lubridate)

# reading in raw files
absc2015_raw <- read_excel("../Data/2. Abscission data.xlsx", sheet = 2, range = "A1:N2768", na = ".")
absc2016_raw <- read_excel("../Data/2. Abscission data.xlsx", sheet = 3, range = "A1:P2153")

gdu2015_raw <- read_excel(
  "../Data/3. GDU profiling the tissues in a plants.xlsx",
  sheet = 1,
  range = "A1:Q7845"
)

gdu2016_raw <- read_excel(
  "../Data/3. GDU profiling the tissues in a plants.xlsx",
  sheet = 3,
  "A1:P2275"
)


############################################################################################################################################
# Writing helper functions
############################################################################################################################################

# helper function to parse the hole sizes column
parseHoles <- function(str) {
  as.numeric(unlist((str_split(str_remove_all(str, " "), ","))))
}

# helper function to create ovary hole columns
getHoleStats <- function(data) {
  max <- vector("double", nrow(data))
  sum <- vector("double", nrow(data))
  avg <- vector("double", nrow(data))
  num <- vector("integer", nrow(data))
  for(i in 1:nrow(data)) {
    parsed <- parseHoles(data$ovaryHolesSize[i])
    max[i] <- max(parsed)
    sum[i] <- sum(parsed)
    avg[i] <- mean(parsed)
    a <- near(length(parsed), 1) && parsed[1] == 0
    if(is.na(a)) {b <- FALSE} else {b <- a}
    if(b) {
      num[i] <- 0
    } else {
      num[i] <- length(parsed)
    }
  }
  list(max = max, sum = sum, avg = avg, num = num)
}

# helper function to derive variables
codeVars <- function(data, yr, absc) {
  stats <- getHoleStats(data)
  data %>% 
    mutate(
      year = yr, abscissed = absc,
      RF = if_else(var == 'RF', 1, 0),
      B2RF = if_else(var == 'B2RF', 1, 0),
      WRF = if_else(var == 'WRF', 1, 0),
      GLT = if_else(var == 'GLT', 1, 0),
      rep1 = if_else(rep == 1, 1, 0),
      rep2 = if_else(rep == 2, 1, 0),
      rep3 = if_else(rep == 3, 1, 0),
      rep4 = if_else(rep == 4, 1, 0),
      year2015 = if_else(yr == 2015, 1, 0),
      year2016 = if_else(yr == 2016, 1, 0),
      ovaryHolesMax = stats$max,
      ovaryHolesSum = stats$sum,
      ovaryHolesAvg = stats$avg,
      ovaryParsedCount = stats$num
    )
}


############################################################################################################################################
# Subsetting/renaming columns and deriving required variables
############################################################################################################################################

absc2015_pre <- absc2015_raw %>%
  select(plot = PLOT, date = DATE, rep, var, tissueID = "TISSUE ID", diameter = DIAMETER,
         sepal = SEPAL,  bract = BRACT, bollWall = "BOLL WALL",
         ovaryHolesCount = "number of holes", ovaryHolesSize = "OVARY HOLE(s) (mm)") %>%
  mutate(bract = as.numeric(bract))

absc2015 <- codeVars(absc2015_pre, yr = 2015, absc = 1)


absc2016_pre <- absc2016_raw %>%
  select(plot = PLOT, date = DATE, rep, var = Var, tissueID = "TISSUE ID", diameter = DIAMETER, sepal = SEPAL, bract = BRACT,
         bollWall = "BOLL WALL", ovaryHolesCount = "number of ovary hole", ovaryHolesSize = "OVARY HOLE(s) (mm)") %>%
  mutate(ovaryHolesCount = if_else(is.na(ovaryHolesCount), 0, ovaryHolesCount),
         ovaryHolesSize = as.character(ovaryHolesSize))

absc2016 <- absc2016_pre %>%
  codeVars(yr = 2016, absc = 1)


gdu2015_pre <- gdu2015_raw %>%
  select(plot = PLOT, date = DATE, rep, var = Var, tissueID = "TISSUE ID", diameter = DIAMETER, sepal = SEPAL, bract = BRACT, 
         bollWall = "BOLL WALL", ovaryHolesCount = "number of ovary hole", ovaryHolesSize = "OVARY HOLE(s) (mm)") %>%
  mutate(diameter = if_else(diameter != '.', as.numeric(trimws(diameter)), NA_real_))

gdu2015 <- gdu2015_pre  %>%
  codeVars(yr = 2015, absc = 0)


gdu2016_pre <- gdu2016_raw %>%
  select(plot = Plot, date = Date, rep, var, tissueID = "Tissue ID", diameter = Diameter, sepal = Sepal, bract = Bract,
         bollWall = "Boll wall", ovaryHolesSize = "Ovary Hole(s) (mm)") %>%
  mutate(
    var = if_else(
      plot %in% c(101, 202, 303, 401), "RF", 
        if_else(plot %in% c(103, 201, 302, 403), "B2RF",
          if_else(plot %in% c(104, 204, 301, 402), "WRF",
            if_else(plot %in% c(102, 203, 304, 404), "GLT", ".")
          )
        )
    )
  )

gdu2016 <- gdu2016_pre  %>%
  codeVars(yr = 2016, absc = 0)


############################################################################################################################################
# Verifying ovary hole columns are correct
############################################################################################################################################
absc2015 %>% select(starts_with("ovary")) %>%
  filter(ovaryHolesCount != ovaryParsedCount)

# it appears there are data entry issues with this sheet
print(absc2016 %>% select(starts_with("ovary")) %>%
  filter(ovaryHolesCount != ovaryParsedCount), n = 50)

gdu2015 %>% select(starts_with("ovary")) %>%
  filter(ovaryHolesCount != ovaryParsedCount)

print(gdu2016 %>% select(starts_with("ovary")) %>%
  filter(ovaryParsedCount != 0), n = 50)


############################################################################################################################################
# Combining all datasets into one
############################################################################################################################################
final <- bind_rows(absc2015, absc2016, gdu2015, gdu2016) %>%
  mutate(year = year(date))

# some seemingly problematic data points
final %>% slice_max(diameter, n = 5) %>%
  select(plot, rep, var, tissueID, diameter:bollWall, ovaryHolesCount, ovaryHolesSize, ovaryHolesSum, ovaryParsedCount, abscissed)

final %>% slice_max(bract, n = 5) %>%
  select(plot, rep, var, tissueID, diameter:bollWall, ovaryHolesCount, ovaryHolesSize, ovaryHolesSum, ovaryParsedCount, abscissed)

final %>% slice_max(bollWall, n = 5) %>%
  select(plot, rep, var, tissueID, diameter:bollWall, ovaryHolesCount, ovaryHolesSize, ovaryHolesSum, ovaryParsedCount, abscissed)

final %>% slice_max(ovaryHolesSum, n = 5) %>%
  select(plot, rep, var, tissueID, diameter:bollWall, ovaryHolesCount, ovaryHolesSize, ovaryHolesSum, ovaryParsedCount, abscissed)

final %>% filter(year == 2016, !is.na(ovaryHolesCount)) %>%
  slice_max(ovaryHolesCount, n = 5, with_ties = FALSE) %>%
  select(plot, rep, var, tissueID, diameter:bollWall, ovaryHolesCount, ovaryHolesSize, ovaryHolesSum, ovaryParsedCount, abscissed)

# "proportion" abscissed by variety (not a total proportion, more like a ratio since the number of non-abscissed tissues sampled was fixed)
final %>% group_by(var) %>% summarise(prop = mean(abscissed))

# abscission counts across varieties
final %>% group_by(var) %>%
  count(abscissed) %>%
  pivot_wider(id_cols = abscissed, names_from = var, values_from = n)

# basic summary of each predictor across the varieties (not including the record with diameter entry error)
final %>% filter(diameter < 50) %>%
  group_by(var) %>%
  summarise(diamMean = mean(diameter, na.rm = TRUE), diamSD = sd(diameter, na.rm = TRUE),
            sepalMean = mean(sepal, na.rm = TRUE), sepalSD = sd(sepal, na.rm = TRUE),
            bractMean = mean(bract, na.rm = TRUE), bractSD = sd(bract, na.rm = TRUE),
            bollWallMean = mean(bollWall, na.rm = TRUE), bollWallSD = sd(bollWall, na.rm = TRUE),
            ovarySumMean = mean(ovaryHolesSum, na.rm = TRUE), ovarySumSD = sd(ovaryHolesSum, na.rm = TRUE))

# the same summaries of each predictor, but only when the respective predictor is > 0
final %>% filter(diameter < 50) %>%
  group_by(var) %>%
  summarise(
    diamMean = mean(diameter[diameter > 0], na.rm = TRUE), diamSD = sd(diameter[diameter > 0], na.rm = TRUE),
    sepalMean = mean(sepal[sepal > 0], na.rm = TRUE), sepalSD = sd(sepal[sepal > 0], na.rm = TRUE),
    bractMean = mean(bract[bract > 0], na.rm = TRUE), bractSD = sd(bract[bract > 0], na.rm = TRUE),
    bollWallMean = mean(bollWall[bollWall > 0], na.rm = TRUE), bollWallSD = sd(bollWall[bollWall > 0], na.rm = TRUE),
    ovarySumMean = mean(ovaryHolesSum[ovaryHolesSum > 0], na.rm = TRUE), ovarySumSD = sd(ovaryHolesSum[ovaryHolesSum > 0], na.rm = TRUE)
  )


############################################################################################################################################
# Basic plots
# generally, the first plot shows some sort of issue, and the second plot is the data without the issue
############################################################################################################################################

### box plots of each predictor across varieties, not including observations with no damage
## diameter
final %>% filter(diameter < 50 & diameter > 0) %>%
  ggplot(aes(diameter)) +
  geom_boxplot() +
  facet_wrap(~var)

## sepal
# potential outliers for B2RF variety
final %>% filter(diameter < 50 & sepal > 0) %>%
  ggplot(aes(sepal)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free_x")
# stacked box plot with coordinates adjusted:
final %>% filter(diameter < 50 & sepal > 0) %>%
  ggplot(aes(sepal, var, fill = var)) +
  coord_cartesian(xlim = c(0, 30)) +
  geom_boxplot()

## bract
# probably data entry error for both B2RF and RF varieties
final %>% filter(diameter < 50 & bract > 0) %>%
  ggplot(aes(bract)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free_x")
# stacked box plots with the coordinates adjusted:
final %>% filter(diameter < 50 & bract > 0) %>%
  ggplot(aes(bract, var, fill = var)) +
  coord_cartesian(xlim = c(0, 30)) +
  geom_boxplot()

## bollWall
# another potential data entry issue with B2RF
final %>% filter(diameter < 50 & bollWall > 0) %>%
  ggplot(aes(bollWall)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free_x")
# stacked box plots with adjusted coords:
final %>% filter(diameter < 50 & bollWall > 0) %>%
  ggplot(aes(bollWall, var, fill = var)) +
  coord_cartesian(xlim = c(0, 50)) +
  geom_boxplot()

## total ovary hole sizes
# there doesn't appear to be any issues for this one
final %>% filter(diameter < 50 & ovaryHolesSum > 0) %>%
  ggplot(aes(ovaryHolesSum)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free_x")
# stacked with adjusted coords:
final %>% filter(diameter < 50 & ovaryHolesSum > 0) %>%
  ggplot(aes(ovaryHolesSum, var, fill = var)) +
  coord_cartesian(xlim = c(0, 30)) +
  geom_boxplot()


### same box plots but grouped across abscissed or not instead of variety
## diameter
final %>% filter(diameter < 50 & diameter > 0) %>%
  ggplot(aes(diameter)) +
  geom_boxplot() +
  facet_wrap(~abscissed)
final %>% filter(diameter < 50 & diameter > 0) %>%
  ggplot(aes(diameter, as.factor(abscissed), fill = as.factor(abscissed))) +
  coord_cartesian(xlim = c(0, 50)) +
  geom_boxplot()

## sepal
final %>% filter(diameter < 50 & sepal > 0) %>%
  ggplot(aes(sepal)) +
  geom_boxplot() +
  facet_wrap(~abscissed)
final %>% filter(diameter < 50 & sepal > 0) %>%
  ggplot(aes(sepal, as.factor(abscissed), fill = as.factor(abscissed))) +
  coord_cartesian(xlim = c(0, 50)) +
  geom_boxplot()

## bract
final %>% filter(diameter < 50 & bract > 0) %>%
  ggplot(aes(bract)) +
  geom_boxplot() +
  facet_wrap(~abscissed)
# this one seems... strange
final %>% filter(diameter < 50 & bract > 0) %>%
  ggplot(aes(bract, as.factor(abscissed), fill = as.factor(abscissed))) +
  coord_cartesian(xlim = c(0, 50)) +
  geom_boxplot()
final %>% filter(diameter < 50 & bract > 0 & bract < 600) %>%
  ggplot(aes(bract, fill = as.factor(abscissed))) +
  coord_cartesian(xlim = c(0, 50)) +
  geom_histogram(alpha = 0.6, stat = "density")

## bollWall
final %>% filter(diameter < 50 & bollWall > 0) %>%
  ggplot(aes(bollWall)) +
  geom_boxplot() +
  facet_wrap(~abscissed)
final %>% filter(diameter < 50 & bollWall > 0) %>%
  ggplot(aes(bollWall, as.factor(abscissed), fill = as.factor(abscissed))) +
  coord_cartesian(xlim = c(0, 50)) +
  geom_boxplot()

## total ovary hole sizes
final %>% filter(diameter < 50 & ovaryHolesSum > 0) %>%
  ggplot(aes(ovaryHolesSum)) +
  geom_boxplot() +
  facet_wrap(~abscissed)
final %>% filter(diameter < 50 & ovaryHolesSum > 0) %>%
  ggplot(aes(ovaryHolesSum, as.factor(abscissed), fill = as.factor(abscissed))) +
  coord_cartesian(xlim = c(0, 20)) +
  geom_boxplot()

## save clean dataset to folder  
write.csv(final, "../Data/cleandata.csv", row.names = FALSE)



