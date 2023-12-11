###########################################################
#
# This code downloads the latest quarterly rental data from 
# Queensland Rental Tenancies Authority and prepared it for 
# further graphical analyses in Tableau.
#
# Code written by : Darragh Murray 
# Contact: @thedatavist / thedatavist@gmail.com)
###########################################################


#############################################
# Load Libraries
#############################################
library(openxlsx)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(googlesheets4)


#############################################
# Data Import
#############################################


# IMPORT DATA FROM THE RTA
suburb_rents = read.xlsx("https://www.rta.qld.gov.au/sites/default/files/2023-04/rta-bond-statistics.xlsx", 
                    sheet=5,
                    startRow=6,
                    colNames = FALSE)

# READ IN BRISBANE SUBURB LIST
brisbane_suburbs = read.csv("https://www.data.brisbane.qld.gov.au/data/dataset/3a3392dd-4f23-43bc-b7fd-9606a611445e/resource/6fb89462-5ac5-4589-8576-cdca03652bc8/download/suburb-and-adjoining-suburb-november-2023.csv")


#############################################
# Data clean / preparation
#############################################

# CLEAN UP ROW HEADERS
# Separate off the first two rows of data
header_row = slice(suburb_rents,1)
subheader_row = slice(suburb_rents,2) 

# Concatenate the first two rows of data into a new variable
suburb_rents_header = paste0(header_row," ", subheader_row)

# remove the old headers (first two rows), then add in a new header row
suburb_rents <- tail(suburb_rents,-2)
names(suburb_rents) <- suburb_rents_header

# rename columns
colnames(suburb_rents)[1] <- "suburb"
colnames(suburb_rents)[2] <- "dwelling_type"


# ORGANISE RENT VALUES

suburb_rents <- suburb_rents %>%
  pivot_longer(!c(suburb, dwelling_type), names_to = "month_year", values_to = "rent", values_drop_na = TRUE) %>%
  separate(col = dwelling_type, into = c("dwelling_type", "bedrooms"), sep = " ", remove = FALSE) %>%
  mutate(dwelling_type = ifelse(dwelling_type == "All", "All Dwellings", dwelling_type)) %>%
  mutate(bedrooms = ifelse(bedrooms == "dwellings", "",bedrooms)) %>%
  mutate(month_year = as.Date(parse_date_time(month_year, order = "bY"))) %>%
  mutate(rent = as.numeric(rent)) %>%
  mutate(data_updated = Sys.Date())


# ORGANISE SUBURB DATA

brisbane_suburbs <- brisbane_suburbs %>%
  mutate(suburb = str_to_title(SUBURB_NAME, locale = "en")) %>%
  select(suburb) %>%
  distinct()


# JOIN RENT DATA TO SUBURB DATA
suburb_rents <- suburb_rents %>% inner_join(brisbane_suburbs, 
                              by=c('suburb'))

#############################################
# Google Drive Authentication
#############################################
gs4_auth(path = Sys.getenv('GSHEET_PAT'))

#############################################
# Output data
#############################################

# Output to google drive first
# find existing Google sheet
brd_g_sheet_meta_data <- gs4_find('rta_suburb_rents')
# Read in sheet data
brd_g_sheet <- read_sheet(brd_g_sheet_meta_data$id)
sheet_write(suburb_rents, ss = ss, sheet = "suburb_rents")

# Store a copy in github as well as a CSV
# Get it to overwrite the existing file - but also write a time stamped version as well for posterity
write.csv(suburb_rents, paste0('outputs/','rta_suburb_rents','.csv'), row.names=FALSE)
write.csv(suburb_rents, paste0('outputs/',Sys.Date(), '_rta_suburb_rents','.csv'), row.names=FALSE)


