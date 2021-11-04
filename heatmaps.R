library(data.table)
library(ggplot2)
library(ggExtra)
library(viridis)
library(forcats)

setwd(here("time_series"))
DT <- fread("sape-2020-persons_Table 1a Persons.csv", skip = 3)

DT <- DT[-c(2:3),][,.SD, .SDcols = -c(96,97)][] # remove rows 2 & 3, and last 2 cols
names <- DT[,.SD,][1,]
names2 <- unlist(names)

old <- names(names)
new <- unlist(names)

setnames(DT, old, new, skip_absent = TRUE)
DT <- DT[-c(1,6976:6978),][]

# all the columns are currently character -  need to change the appropriate ones
# to integer

intcols <- names(DT[,c(5:95)]) # get the integer column names using col indexes

DT[,(intcols) := lapply(.SD, as.numeric), .SDcols = intcols]
DT <-  DT[`Area code` != '© Crown Copyright 2021']

DT_tidy <- melt(DT, id.vars = names2[c(1:4)])

# read in the datazone lookups and sub hscp data

sub_highland <- fread("highlandsubhscp.csv")
sub_argyll <- fread("argyllbutesubhscp.csv")

dz_lookup <- fread("datazone-to-locality-lookup.csv")

# get the hscp names we want to keep

high_hscp <- sub_highland$SubHSCPName
ab_hscp <- sub_argyll$SubHSCPName

# filter the lookup table for highland and AB
DZ <- dz_lookup[SubHSCPName  %in% c(high_hscp,ab_hscp)]

# create a smaller lookup with DZ and hscp name
DZ2 <- DZ[,.(DataZone,SubHSCPName)]


DZ2[,joinvar := DataZone]
DT_tidy[,joinvar := `Area code`][]

DT_tidy <- DZ2[DT_tidy, on = "joinvar"][, joinvar := NULL][]

#councils <- c('Highland', 'Argyll and Bute')

# highlands, without inverness

highland <- DT_tidy[`Council area` == "Highland" & SubHSCPName != "Inverness"]
highland$`Area name` <- factor(highland$`Area name`, ordered = TRUE) # make area name a factor
highland$variable <- gsub("AGE","",highland$variable)
highland[variable %in% c(0,1,2,3,4,5,6,7,8,9), variable := paste0(0,variable, sep = "")]
highland[variable == '90+', variable := 90][]
highland$variable <- as.numeric(highland$variable)


p <- ggplot(highland, aes(variable, forcats::fct_rev(`Area name`),fill = value, drop = TRUE)) +
  geom_tile(color = "white",size = 0.1) +
  facet_wrap(vars(SubHSCPName),scales = "free_y", drop = TRUE, ncol = 2) +
  scale_fill_viridis(name = "Population by Age Group", option = "cividis", direction = 1)

p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = "Population by Age Band and Council Area - North Highland, excluding Inverness",
              subtitle = "2020 Mid year population estimates",
              caption = "@_johnmackintosh \n Source : National Records Scotland \n https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series",
              x = "Age",
              y = "")
p <- p + theme(legend.position = "bottom") +
  theme(plot.title = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(strip.background = element_rect(colour = "white")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 8)) +
  theme(legend.text = element_text(size = 6)) +
  removeGrid()
p <- p + scale_x_continuous(breaks = seq(from = 0, to = 90, by = 5))
p

ggsave("north-highland-exclude-inverness.png", width = 11, height = 10)

# Inverness
inverness <- DT_tidy[`Council area` == "Highland" & SubHSCPName == "Inverness"]

inverness$`Area name` <- factor(inverness$`Area name`, ordered = TRUE) # make area name a factor
inverness$variable <- gsub("AGE","",inverness$variable)
inverness[variable %in% c(0,1,2,3,4,5,6,7,8,9), variable := paste0(0,variable, sep = "")]
inverness[variable == '90+', variable := 90][]
inverness$variable <- as.numeric(inverness$variable)


p2 <- ggplot(inverness, aes(variable, fct_rev(`Area name`), fill = value, drop = TRUE)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Population by Age Group",option = "plasma", direction = 1)

p2 <- p2 + theme_minimal(base_size = 8)
p2 <- p2 + labs(title = "Population by Age Band and Council Area - Inverness",
              subtitle = "2020 Mid year population estimates",
              caption = "@_johnmackintosh \n Source : National Records Scotland \n https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series",
              x = "Age",
              y = "")
p2 <- p2 + theme(legend.position = "bottom") +
  theme(plot.title = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(strip.background = element_rect(colour = "white")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 8)) +
  theme(legend.text = element_text(size = 6)) +
  ggExtra::removeGrid()
p2 <- p2 + scale_x_continuous(breaks = seq(from = 0, to = 90, by = 5))
p2

ggsave("inverness.png", width = 11, height = 10)


# Argyll & Bute
ab <- DT_tidy[`Council area` == "Argyll and Bute",]

ab$`Area name` <- factor(ab$`Area name`, ordered = TRUE) # make area name a factor
ab$variable <- gsub("AGE","",ab$variable)
ab[variable %in% c(0,1,2,3,4,5,6,7,8,9), variable := paste0(0,variable, sep = "")]
ab[variable == '90+', variable := 90][]
ab$variable <- as.numeric(ab$variable)


p <- ggplot(ab, aes(variable, forcats::fct_rev(`Area name`), fill = value, drop = TRUE)) +
  geom_tile(color = "white",size = 0.1) +
  facet_wrap(vars(SubHSCPName),scales = "free_y", drop = TRUE, ncol = 2) +
  scale_fill_viridis(name = "Population by Age Group",
                     begin = 0.2,
                     end = 0.6,
                     option = "mako",
                     direction = -1)

p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = "Population by Age Band and Council Area - Argyll & Bute",
              subtitle = "2020 Mid year population estimates",
              caption = "@_johnmackintosh \n Source : National Records Scotland \n https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series",
              x = "Age",
              y = "")
p <- p + theme(legend.position = "bottom") +
  theme(plot.title = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(strip.background = element_rect(colour = "white")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 8)) +
  theme(legend.text = element_text(size = 6)) +
  ggExtra::removeGrid()
p <- p + scale_x_continuous(breaks = seq(from = 0, to = 90, by = 5))
p

ggsave("ab.png", width = 11, height = 10)

