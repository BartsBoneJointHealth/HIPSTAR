# packages ----
library(DBI)
library(CDMConnector)
library(IncidencePrevalence)
library(testthat)
library(PatientProfiles)
library(here)
library(tidyselect)

# database metadata and connection details -----
# The name/ acronym for the database
db_name <- "....."

# Database connection details -----
# create a dbi database connection https://dbi.r-dbi.org/
# more examples can be seen at https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html
db <- dbConnect(".....")
# cdm schema contains the schema containing omop tables
cdm_schema <- "...."
# write schema we use for creating intermediate tables
write_schema <- "...."

# Table prefix -----
# any tables created in the database during the analysis will start with this prefix
# we provide the default here but you can change it
# note, any existing tables in your write schema starting with this prefix may
# be dropped during running this analysis
study_prefix <- "db_summary_"

# Run the study ------
source(here("RunStudy.R"), local = TRUE)

# After running the study you should have a set of csv files containing
# aggregated results in your "Results" folder ready to share




