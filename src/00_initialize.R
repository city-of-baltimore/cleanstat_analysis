library(here)
here <- here::here

suppressMessages(library(tidyverse))
suppressMessages(library(here))

suppressMessages(library(ggiteam))
suppressMessages(library(scales))
suppressMessages(library(lubridate))

VARS <- list()

VARS$EMAIL <- Sys.getenv("EMAIL")

# Initialize directories
VARS$SRC <- here("src")
VARS$RAW_DATA <- here("data/raw/")
VARS$PROCESSED_DATA <- here("data/processed/")
VARS$NOTEBOOKS <- here("notebooks/")
VARS$OUTPUT_FIGS <- here("output/figs/")
VARS$OUTPUT_TABLES <- here("output/tables/")
#VARS$GOOGLE_DRIVE <- Sys.getenv("GOOGLE_DRIVE")


# Server credentials in env variables
VARS$EGIS_SERVER <- Sys.getenv("EGIS_SERVER")
VARS$EGIS_SERVER_USER <- Sys.getenv("EGIS_SERVER_USERNAME")
VARS$EGIS_SERVER_PWD <-  Sys.getenv("EGIS_SERVER_PWD")

VARS$SQL_SERVER <- Sys.getenv("SQL_SERVER")

# Socrata credentials
VARS$SOCRATA_TOKEN <- Sys.getenv("SOCRATA_TOKEN")
VARS$SOCRATA_SECRET <- Sys.getenv("SOCRATA_SECRET")
VARS$SOCRATA_PWD <- Sys.getenv("SOCRATA_PWD")

# Google geocoding
VARS$GOOGLE_GEO_KEY <- Sys.getenv("GOOGLE_GEO_KEY")

source(here("src", "01_helper_functions.R"))