library(here)
library(fs)
library(data.table)
library(janitor)
library(magrittr)
library(tidyr)
library(dplyr)


untidy <- fread(here("untidy_files.csv"))

setwd(here("RDS"))

allfiles <- dir(pattern = ".RDS") 
filelist <- untidy$saved_as

files_to_copy <- dplyr::setdiff(allfiles,filelist)

#fs::dir_delete(here("RDS","Tidy"))
dir.create(here("RDS","Tidy"))
fs::file_copy(path = files_to_copy, new_path = here("RDS","Tidy"))


setwd(here("RDS"))
temp <- readRDS("Median-age-by-6-fold-Urban-Rural-Classification-mid-2020.RDS")

setwd(here("RDS","Tidy"))
temp <- melt(temp,id.vars = 'urban_rural_classification', value.name = "median_age")
saveRDS(temp,'Median-age-by-6-fold-Urban-Rural-Classification-mid-2020.RDS')
rm(temp)



setwd(here("RDS"))
temp <- readRDS("median-age-by-SIMD-decile.RDS")
setwd(here("RDS","Tidy"))
temp <- melt(temp,id.vars = 'decile', value.name = "median_age")
saveRDS(temp,'median-age-by-SIMD-decile.RDS')
rm(temp)


setwd(here("RDS"))
temp <- readRDS("percentage_of_data_zones_in_population_ranges_by_council_area.RDS")
setwd(here("RDS","Tidy"))
temp1 <- temp %>%
  select(council_name, total, starts_with("pop")) %>%
  gather(measure, value, -c(total, council_name))
saveRDS(temp1,'percentage_of_data_zones_in_population_ranges_by_council_area_numbers.RDS')

temp2 <- temp %>%
  select(council_name, starts_with("percent")) %>%
  gather(measure, value, -c(council_name))
saveRDS(temp2,'percentage_of_data_zones_in_population_ranges_by_council_area_percent.RDS')

rm(temp)
rm(temp1)
rm(temp2)


setwd(here("RDS"))
temp <- readRDS("percent_datazones_by_population_change_and_council_area.RDS")
setwd(here("RDS","Tidy"))

temp1 <- temp %>%
  select(council_area, total, starts_with(c("pop","no_change"))) %>%
  gather(measure, value, -c(total, council_area))
saveRDS(temp1,'percent_datazones_by_population_change_and_council_area_numbers.RDS')


temp2 <- temp %>%
  select(council_area, total, starts_with("percent")) %>%
  gather(measure, value, -c(total, council_area))
saveRDS(temp2,'percent_datazones_by_population_change_and_council_area_percent.RDS')
rm(temp)
rm(temp1)
rm(temp2)


setwd(here("RDS"))
temp <- readRDS("percentage_of_data_zones_by_change_in_median_age_and_council_area.RDS")
setwd(here("RDS","Tidy"))
temp1 <- temp %>%
  select(council_name, total, starts_with(c("median","no_change"))) %>%
  gather(measure, value, -c(total, council_name))
saveRDS(temp1,'percentage_of_data_zones_by_change_in_median_age_and_council_area_numbers.RDS')

temp2 <- temp %>%
  select(council_name, total, starts_with("percent")) %>%
  gather(measure, value, -c(total, council_name))
saveRDS(temp2,'percentage_of_data_zones_by_change_in_median_age_and_council_area_percent.RDS')
rm(temp)
rm(temp1)
rm(temp2)


setwd(here("RDS"))
temp <- readRDS("change_in_population_by_urban_rural_classification_mid_2010_to_mid_2020.RDS")
setwd(here("RDS","Tidy"))
temp <-  melt(temp,id.vars = 'Classification', variable.name = 'year')
saveRDS(temp,'change_in_population_by_urban_rural_classification_mid_2010_to_mid_2020.RDS')



setwd(here("RDS"))
temp <- readRDS("net_migration_by_6_fold_urban_rural_classification_2009_10_to_2019_20.RDS")
setwd(here("RDS","Tidy"))
temp <-  melt(temp,id.vars = 'year', variable.name = 'classification')
saveRDS(temp,'net_migration_by_6_fold_urban_rural_classification_2009_10_to_2019_20.RDS')

rm(temp)

# check
assertthat::are_equal(length(dir()),17)
# should be 17

# write files out - one time only so commenting out

# filetable <- as.data.table(dir())
#
# write_files <- function(x){
#
# filetable <- as.data.table(dir())
#
#
# for (i in filetable$V1) {
#
#   temp <- readRDS(filetable$V1[i])
#   startstring  <- fs::path_ext_remove(path = filetable$V1[i])
#   ext <- ".csv"
#   filepath <- paste(startstring,ext, sep ='')
#   write.csv(temp,filepath)
# }
#
# }
