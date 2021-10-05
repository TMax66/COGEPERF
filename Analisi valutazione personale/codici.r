library(tidyverse)
library(tidytext)
library(here)
library(readxl)

vp <- read_excel(sheet = "COMPARTO", here("data", "raw", "valutazione individuale anno 2020.xls"))
