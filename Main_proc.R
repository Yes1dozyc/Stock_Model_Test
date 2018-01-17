setwd("C:/Personal/Financial_Model/RF_Classification")

library(quantmod)
library(dplyr)

rm(list=ls())

source("script/data_retrieve.R")

data_retrieve()