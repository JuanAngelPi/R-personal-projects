#-------------------------------------------------------------------------
#  MULTIPLE REGRESSION OF WINS BASED ON DEFENSIVE STATS
#-------------------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(nflverse)
library(GGally)
library(stats)
library(ggrepel)
library(broom)
library(kableExtra)

data = load_pbp(2018:2022)

