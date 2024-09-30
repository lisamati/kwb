### KWB data 
# join kwb data together and clean variables - by PC4 level
# coordinated with the script in the RA environment (output check july 2024). 

# INPUT: 
# - .Rds files with kwb data for all years (1995 -2022)

# OUTPUT:
# - .Rds file with info from kwb aggregated by pc4 and year. 

# As wijken and buurten are not necessarily comparable over the years, this should not be interpreted as a panel. 

# set up
getwd()
setwd("~/Dropbox/Immigration & Housing Markets")

# packages & paths 
source("scripts/0_setup.R")

setwd("~/Dropbox/Immigration & Housing Markets/NeighborhoodStats")

#### Load datasets #####
# this dataset includes a selection of variables on buurt level.
kwb <- readRDS("~/Dropbox/Immigration & Housing Markets/scripts/datasets/kwb_buurt_1995_2023.Rds")

big4code <- c("0363", "0518","0599", "0344")

#### number of postcodes ####
# Dataset from 2006
kwb <- kwb[year >= 2006]
kwb[, postcode := as.numeric(postcode)]

#n postcodes per year in all NL
kwb[, .(n_postcode = n_distinct(postcode), 
        n_wijkcode = n_distinct(wijkcode), 
        n_gemcode = n_distinct(gemcode)), by = year]
#most years have around 3800+ postcodes, which is nearly the totality of postal codes, good coverage

kwb[nchar(gwb_code_8) >= 8 & is.na(postcode), .(n_postcode = length(postcode)), by = year]
# some years have missing values for postcodes, but < 100 buurts per year

# classification for city size (stedelijkheid or dekkingspercentage)
dcast(kwb[nchar(gwb_code_8) >= 8, .(n_buurt = n_distinct(gwb_code_8)), keyby = .(year, sted)], 
      year ~ sted, value.var = c("n_buurt"))

# dekkingspercentage 1: >90% of addresses have the same postcode 
dcast(kwb[nchar(gwb_code_8) >= 8, .(n_buurt = n_distinct(gwb_code_8)), keyby = .(year, dek_perc)], 
      year ~ dek_perc, value.var = c("n_buurt"))

dcast(merge(kwb[nchar(gwb_code_8) >= 8, 
                .(n_buurt = n_distinct(gwb_code_8)), 
                keyby = .(year, dek_perc)], 
            kwb[nchar(gwb_code_8) >= 8, 
                .(n_codes = length(gwb_code_8)), 
                by = year], by = "year")[, frac := round(n_buurt / n_codes, 3)],
      year ~ dek_perc, value.var = c("n_buurt", "frac"))
# > 87% have dekkingspercentage of >90% (cat 1)

#### calculate avg WOZ ####
# before combining the dataset, check how many obs of WOZ we have and compute the average WOZ per pc4. 
# #include only buurts since WOZ refers to a buurt?
kwb_buurt <- kwb[nchar(gwb_code_8) == 8]

# how many buurts have non-NA entries for WOZ? - > 76% 
print(merge(kwb_buurt[!is.na(woz), .(n_buurt_woz = length(woz)), by = year],
      kwb_buurt[, .(all_buurt = length(woz)), by = year], by = "year")[, frac := round(n_buurt_woz / all_buurt, 3)][])

kwb_buurt[, all_woz_na := as.integer(all(is.na(woz))), by = .(year, postcode)]
kwb_buurt[, .(n_distinct(postcode)), by = .(all_woz_na)]

#### aggregate by pc4 
# first determine which cols need summation and weighted averages. Then do one operation per column type.
kwb_buurt[, a_woningen := woningen]

sum_cols <- c(grep("^a_", names(kwb_buurt), value = TRUE)) 
avg_woning_cols <- c(grep("^g_", names(kwb_buurt), value = TRUE), "sted", "avg_hh_size", 
                     "p_wont2000", "p_wonv2000")
avg_pop_cols <- c("income_person", "income_worker", "p_laag", "p_hoog")

# all vars that need a simple sum 
kwb_pc <- kwb_buurt[!is.na(postcode), lapply(.SD, sum), 
             by = .(year, postcode), .SDcols = sum_cols]

# vars weighted by number of houses
kwb_pc <- merge(kwb_pc, kwb_buurt[, lapply(.SD, function(var) weighted.mean(x = as.numeric(var), w = woningen)), 
                                  by = .(year, postcode), .SDcols = c(avg_woning_cols, "woningen")], 
                by = c("year", "postcode"), all.x = TRUE, all.y = FALSE)

# vars weighted by population
kwb_pc <- merge(kwb_pc, kwb_buurt[, lapply(.SD, function(var) weighted.mean(x = as.numeric(var), w = a_inw)), 
                                  by = .(year, postcode), .SDcols = c(avg_pop_cols)], 
                by = c("year", "postcode"), all.x = TRUE, all.y = FALSE)


kwb_pc <- merge(kwb_pc, kwb_buurt[!is.na(postcode), .(mean_woz = mean(woz, na.rm = T),
                                                      all_woz_na = all_woz_na[1],
                                                      mean_woz_weight_won = weighted.mean(x = woz, w = woningen, na.rm = T), 
                                                      gemcode = gemcode[1], 
                                                      wijkcode = wijkcode[1]), 
                    by = .(year, postcode)], by = c("year", "postcode"), all.x = TRUE, all.y = FALSE)



# calculate shares for relevant vars 
kwb_pc[, `:=`(share_rent = a_rent / a_woningen,
              share_koop = a_koop / a_woningen,
              share_corp = a_corp / a_woningen,
              share_west = a_west / a_inw, 
              share_nonwest = a_nonwest / a_inw, 
              share_mig = (a_west + a_nonwest) / a_inw)][a_inw == 0, `:=`(share_west = NA, 
                                                                          share_nonwest = NA, 
                                                                          share_mig = NA)][a_west > a_inw, share_west := 1] 

##### check ####
summary(kwb_pc[, .(share_west, share_nonwest)])
summary(kwb_pc[, .(share_rent, share_koop, share_corp)])

kwb_pc[is.na(a_west), .(n_distinct(postcode)), by = year]

#### save ####
saveRDS(kwb_pc, "~/Dropbox/Immigration & Housing Markets/scripts/datasets/kwb_pc4_2006_2023.Rds")



