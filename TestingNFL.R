
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



data = load_injuries(2010:2024)
