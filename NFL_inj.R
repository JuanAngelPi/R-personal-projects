
library(tidyverse)
library(dplyr)
library(nflverse)
library(GGally)
library(stats)
library(ggrepel)
library(broom)
library(kableExtra)
library(pscl)
library(car)
library(knitr)

# install if needed
# install.packages("nflreadr")

library(nflreadr)

# load all available injury data
inj <- load_injuries()

# or load specific seasons
inj_2023 <- load_injuries(seasons = 2023)

# view it
head(inj)

urls <- nflreadr:::.urls


