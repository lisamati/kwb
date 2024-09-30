### KWB data - by gemeente 
# join kwb data together and clean variables - by gemeente 
# coordinated with the script in the RA environment (output check july 2024). 


# INPUT: 
# - .xlsx files from KWB in the folder "Neighboorhood Stats" 

# OUTPUT:
# - .Rds files with kwb data by gemeente for all years (1995 -2022)

# reads in the KWB (kerncijfers wijken en buurten) from the CBS utilities folder and 
# binds it together by year and makes the types of variables consistent.

# As wijken and buurten are not necessarily comparable over the years, this should not be interpreted as a panel. 

## OUTPUT 
# - dataframe of kwb by year 

# set up
getwd()
setwd("~/Dropbox/Immigration & Housing Markets")

# packages & paths 
source("scripts/0_setup.R")

setwd("~/Dropbox/Immigration & Housing Markets/NeighborhoodStats")

# set up
getwd()
setwd("~/Dropbox/Immigration & Housing Markets")

# packages & paths 
source("scripts/0_setup.R")

setwd("~/Dropbox/Immigration & Housing Markets/NeighborhoodStats")

#### Load datasets #####
# this dataset includes a selection of variables on buurt level.
kwb <- readRDS("~/Dropbox/Immigration & Housing Markets/scripts/datasets/kwb_buurt_1995_2023.Rds")


#### number of gemeente codes ####
# number of distinct 4 digit classifiers of GWB_CODE_8 (note that "0000" is the entire Netherlands)
#n postcodes per year in all NL
kwb[, .(n_postcode = n_distinct(postcode), 
        n_wijkcode = n_distinct(wijkcode), 
        n_gemcode = n_distinct(gemcode)), by = year]
# number of distinct gemeentecodes is decreasing over time, 343 
# cross checking with CBS (https://www.cbs.nl/nl-nl/onze-diensten/methoden/classificaties/overig/gemeentelijke-indelingen-per-jaar), 
# this seems to be correct 

dim(kwb[is.na(gemcode)])
# no NA values for GWB_CODE_8 

#### subset for Gemeente ####
# as of 2006 (n= 8), the names are in order.
kwb_gem <- kwb[nchar(gwb_code_8) ==4 & gwb_code_8 != "0000"]
kwb_gem[, gemcode := gwb_code_8] 
# for a_inw and woningen, check if totals (from kwb_buurt) add up to the gemeente values
kwb_buurt <- kwb[nchar(gwb_code_8) == 8]

kwb_buurt_gem <- merge(kwb_gem[, .(gemcode, year, a_inw, woningen)], 
                       kwb_buurt[gemcode != "0000", .(a_inw_b = sum(a_inw, na.rm = T), 
                               woningen_b = sum(woningen, na.rm = T)), by = .(year, gemcode)], 
                       by = c("year", "gemcode"), all.x = TRUE, all.y = TRUE)

# same moments
summary(kwb_buurt_gem[, .(a_inw, a_inw_b)])
summary(kwb_buurt_gem[, .(woningen, woningen_b)])
summary(kwb_buurt_gem[, .(a_inw - a_inw_b, woningen - woningen_b)])

# some small differences
head(kwb_buurt_gem[woningen - woningen_b > 50, ])

rm(kwb_buurt_gem)

###### calculate stocks of migrants #####
#kwb_gem[, `:=`(a_west = (p_west/100) * a_inw, 
#               a_nonwest = (p_nonwest/100) * a_inw)][, a_migrant := a_west + a_nonwest]

kwb_gem[, `:=`(p_west = a_west / a_inw, 
               p_nonwest = a_nonwest / a_inw)][, a_migrant := a_west + a_nonwest]


###### save ####
saveRDS(kwb_gem, "~/Dropbox/Immigration & Housing Markets/scripts/datasets/kwb_gemeente.Rds")
