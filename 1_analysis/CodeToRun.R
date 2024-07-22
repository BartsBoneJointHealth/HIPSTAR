
# Please restore the renv file first ----
# if you have not installed renv, please first install: install.packages("renv")
renv::activate()
renv::restore()

# Load required packages ----
library(DBI)
library(dplyr)
library(CDMConnector)
library(here)
library(CodelistGenerator)
library(readr)
library(omopgenerics)
library(CohortConstructor)
library(CohortCharacteristics)
library(PatientProfiles)
library(stringr)
library(RPostgres)
library(odbc)

# Create the database connection
db <- DBI::dbConnect("...")

# The name of the database
dbName <- "..."

# The name of the schema that contains the OMOP CDM with patient-level data
cdmSchema <- "..."

# The name of the schema where results tables will be created
writeSchema <- "..."

# A prefix to use when creating tables in the database
writePrefix <- "hipstar"

# minimum counts that can be displayed according to data governance
minCellCount <- 5

# create cdm reference -----
cdm <- cdmFromCon(
  con = db,
  cdmSchema = c(schema = cdmSchema),
  writeSchema = c(schema = writeSchema,
                  prefix = writePrefix),
  cdmName = dbName,
  .softValidation = TRUE
)

# Run the study code ----
source(here("RunStudy.R"))
