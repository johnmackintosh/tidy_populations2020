library(here)
library(fs)
library(data.table)
library(dplyr)
library(janitor)
library(magrittr)

# download zip file
# create R project in new directory
# move zip file from downloads to project directory

setwd(here("CSV"))
# unzip files to this folder - couldn't get 'unzip' to work -- Windows issue?


allfiles <- dir(pattern = ".csv") # list of csv files
filelist <- dir(pattern = "*Data.csv") # list of csv files with DATA in filename

files_to_remove <- dplyr::setdiff(allfiles,filelist) # which files do not contain data
file_delete(files_to_remove) # remove non data files


# create a data.frame to store file metadata
files <- data.frame(sourcefile = filelist)  # files to read
files$filename <- path_ext_remove(files$sourcefile) # remove extension
files$meta <- "description" # placeholder for meta data
files$saved_as <- "filename.rds" # placeholder for the rdata file name
files$nrows <- NULL
files$ncols <- NULL

### Count of datazones by population size banding ##

temp <- fread(files[1,1],
              skip = 2,
              header = TRUE,
              drop = c(3:7),
              blank.lines.skip = TRUE)
temp <- temp[1:14,]
temp <- temp %>% clean_names()

dir.create(here("RDS"))

saveRDS(temp,
        here('RDS','Count-of-datazones-by-population-size-banding.RDS'))
files$meta[1] <- "Count of datazones by population size banding"
files$saved_as[1] <- 'Count-of-datazones-by-population-size-banding.RDS'
files$nrows[1] <- 14L
files$ncols[1] <- 2L
rm(temp)


## Median age by 6-fold Urban Rural Classification, mid-2020 ##

temp <- fread(files[2,1],
              skip = 2,
              header = TRUE,
              drop = c(5:16),
              blank.lines.skip = TRUE)

temp <- temp[1:6,]
temp <- temp %>% clean_names()

saveRDS(temp,
        here('RDS','Median-age-by-6-fold-Urban-Rural-Classification-mid-2020.RDS'))
files$meta[2] <- "Median age by 6-fold Urban Rural Classification, mid-2020"
files$saved_as[2] <- 'Median-age-by-6-fold-Urban-Rural-Classification-mid-2020.RDS'
files$nrows[2] <- 6L
files$ncols[2] <- 4L
rm(temp)

 ## Percentage of population in most deprived SIMD decile by council area, mid-2020 ##

temp <- fread(files[3,1],
              skip = 2,
              header = TRUE,
              drop = c(3:12),
              blank.lines.skip = TRUE)

temp <- temp[1:34,]
temp <- temp[,tail(.SD,32)]
temp <- temp %>% clean_names()

saveRDS(temp,
        here('RDS','council_area percentage_of_population_in_decile_1_most_deprived.RDS'))
files$meta[3] <- "Percentage of population in most deprived SIMD decile by council area, mid-2020"
files$saved_as[3] <- 'council_area percentage_of_population_in_decile_1_most_deprived.RDS'
files$nrows[3] <- 32L
files$ncols[3] <- 2L
rm(temp)


## Percentage of population in least deprived SIMD decile by council area, mid-2020 ##

temp <- fread(files[4,1],
              skip = 2,
              header = TRUE,
              drop = c(3:12),
              blank.lines.skip = TRUE)
temp <- temp[1:34,]
temp <- temp[3:34,] # drop first 2 empty rows
temp <- temp %>% clean_names()
saveRDS(temp,
        here('RDS','council_area percentage_of_population_in_decile_10_least_deprived.RDS'))
files$meta[4] <- "Percentage of population in least deprived SIMD decile by council area, mid-2020"
files$saved_as[4] <- 'council_area percentage_of_population_in_decile_10_least_deprived.RDS'
files$nrows[4] <- 32L
files$ncols[4] <- 2L
rm(temp)


##  Median age by SIMD decile, mid-2020 ##

temp <- fread(files[5,1],
              skip = 2,
              header = TRUE,
              drop = c(5:15),
              blank.lines.skip = TRUE)
temp <- temp[,head(.SD,10)] %>% clean_names()
temp <- temp %>% clean_names()
saveRDS(temp,
        here('RDS','median-age-by-SIMD-decile.RDS'))
files$meta[5] <- "Median age by SIMD decile, mid-2020 "
files$saved_as[5] <- 'median-age-by-SIMD-decile.RDS'
files$nrows[5] <- 10L
files$ncols[5] <- 4L
rm(temp)

##  Population count by Scottish Parliamentary Constituency, mid-2020 ##

temp <- fread(files[6,1],
              skip = 2,
              header = TRUE,
              drop = c(3:10),
              blank.lines.skip = TRUE)
temp <- temp[1:7,] %>% clean_names()

saveRDS(temp,
        here('RDS','population_count_by_number_of_constituencies.RDS'))
files$meta[6] <- "Population count by Scottish Parliamentary Constituency, mid-2020"
files$saved_as[6] <- 'population_count_by_number_of_constituencies.RDS'
files$nrows[6] <- 7L
files$ncols[6] <- 2L
rm(temp)

## Population count by UK Parliamentary Constituency, mid-2020 ##

temp <- fread(files[7,1],
              skip = 2,
              header = TRUE,
              drop = c(3:10),
              blank.lines.skip = TRUE)
temp <- temp[1:7,] %>% clean_names()

saveRDS(temp,
        here('RDS','population_count_by_number_of_UK_constituencies.RDS'))
files$meta[7] <- "Population count by UK Parliamentary Constituency, mid-2020"
files$saved_as[7] <- 'population_count_by_number_of_UK_constituencies.RDS'
files$nrows[7] <- 7L
files$ncols[7] <- 2L
rm(temp)


## Percentage of data zones in population ranges by council area, mid-2020 ##
temp <- fread(files[8,1],
              skip = 3,
              header = TRUE,
             # drop = c(3:10),
              blank.lines.skip = TRUE)
temp <- temp[1:32,]

# rename these columns as clean_names makes them worse
newnames <- c('council_name', 'total', 'pop_less_500', 'pop_500-999', 'pop_1000-1499',
              'pop_greater_1500', 'percent_less_500', 'percent_500-999' ,
              'percent_1000-1499', 'percent_greater_1500')

setnames(temp, new = newnames)

saveRDS(temp,
        here('RDS','percentage_of_data_zones_in_population_ranges_by_council_area.RDS'))
files$meta[8] <- "Percentage of data zones in population ranges by council area, mid-2020"
files$saved_as[8] <- 'percentage_of_data_zones_in_population_ranges_by_council_area.RDS'
files$nrows[8] <- 32L
files$ncols[8] <- 10L
rm(temp)


## Data zone population change, mid-2010 to mid-2020  ##

temp <- fread(files[9,1],
              skip = 2,
              header = TRUE,
              drop = c(3:6),
              blank.lines.skip = TRUE)

temp <- temp[1:13,]
temp <- temp %>% clean_names()

saveRDS(temp,
        here('RDS','data_zone_population_change_2010_to_2020.RDS'))
files$meta[9] <- "Data zone population change, mid-2010 to mid-2020"
files$saved_as[9] <- 'data_zone_population_change_2010_to_2020.RDS'
files$nrows[9] <- 13L
files$ncols[9] <- 2L
rm(temp)


# Percentage of data zones by population change and council area, mid-2010 to mid-2020 #

temp <- fread(files[10,1],
              skip = 3,
              header = TRUE,
              drop = c(9:12),
              blank.lines.skip = TRUE)
temp <- temp[1:32,]
newnames <- c('council_area', 'total', 'population_decrease', 'no_change' ,'population_increase',
              'percent_population_decrease', 'percent_no_change',
              'percent_population_increase')
setnames(temp, newnames)

saveRDS(temp,
        here('RDS','percent_datazones_by_population_change_and_council_area.RDS'))
files$meta[10] <- "Percentage of data zones by population change and council area, mid-2010 to mid-2020"
files$saved_as[10] <- 'percent_datazones_by_population_change_and_council_area.RDS'
files$nrows[10] <- 32L
files$ncols[10] <- 8L
rm(temp)


#  Percentage of data zones by change in median age and council area, mid-2010 to mid-2020 #

temp <- fread(files[11,1],
              skip = 3,
              header = TRUE,
              drop = c(9:14),
              blank.lines.skip = TRUE)
temp <- temp[1:32,] %>% clean_names()

oldnames <- c("median_age_decrease_2", "no_change_2", "median_age_increase_2")
newnames <- c("percent_median_age_decrease", "percent_no_change", "percent_median_age_increase")
setnames(temp, old = oldnames, new = newnames)

saveRDS(temp,
        here('RDS','percentage_of_data_zones_by_change_in_median_age_and_council_area.RDS'))
files$meta[11] <- "Percentage of data zones by change in median age and council area, mid-2010 to mid-2020"
files$saved_as[11] <- 'percentage_of_data_zones_by_change_in_median_age_and_council_area.RDS'
files$nrows[11] <- 32L
files$ncols[11] <- 8L
rm(temp)


# Average change in median age across data zones between mid-2010 and mid-2020, by council area #

temp <- fread(files[12,1],
              skip = 2,
              header = TRUE,
              drop = c(3:9),
              blank.lines.skip = TRUE)
temp <- temp[1:33,] %>% clean_names()

saveRDS(temp,
        here('RDS','average_change_in_median_age_across_datazones_by_council_area.RDS'))
files$meta[12] <- "average change in median age across data zones between mid-2010 and mid-2020, by council area"
files$saved_as[12] <- 'average_change_in_median_age_across_datazones_by_council_area.RDS'
files$nrows[12] <- 33L
files$ncols[12] <- 2L
rm(temp)


# Change in population by Urban Rural Classification, mid-2010 to mid-2020 #

temp <- fread(files[13,1],
              skip = 3,
              header = TRUE)
temp <- temp[1:6,]
saveRDS(temp,
        here('RDS','change_in_population_by_urban_rural_classification_mid_2010_to_mid_2020.RDS'))
files$meta[13] <- "Change in population by Urban Rural Classification, mid-2010 to mid-2020"
files$saved_as[13] <- 'change_in_population_by_urban_rural_classification_mid_2010_to_mid_2020.RDS'
files$nrows[13] <- 6L
files$ncols[13] <- 12L
rm(temp)

# Net migration by 6-fold Urban Rural Classification, 2009-10 to 2019-20 #

temp <- fread(files[14,1],
              skip = 2,
              header = TRUE,
              drop = c(8:9),
              blank.lines.skip = TRUE)

temp <- temp[1:19,] %>% clean_names()

saveRDS(temp,
        here('RDS','net_migration_by_6_fold_urban_rural_classification_2009_10_to_2019_20.RDS'))
files$meta[14] <- "Net migration by 6-fold Urban Rural Classification, 2009-10 to 2019-20"
files$saved_as[14] <- 'net_migration_by_6_fold_urban_rural_classification_2009_10_to_2019_20.RDS'
files$nrows[14] <- 19L
files$ncols[14] <- 7L
rm(temp)


setwd(here::here())

saveRDS(files,here("CSV-metadata.RDS"))

setDT(files)
untidy <- files[ncols > 2,.(saved_as)]
fwrite(untidy,here("untidy_files.csv"))





