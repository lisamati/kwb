## KWB data by Wijk 
# wijken en burten change every year. Take the postcode-wijken allocation from 2022 and then aggregate via postcodes. 


# set up
getwd()
setwd("~/Dropbox/Immigration & Housing Markets")

# packages & paths 
source("scripts/0_setup.R")
setwd("~/Dropbox/Immigration & Housing Markets")

#### load data ####
kwb <- readRDS("~/Dropbox/Immigration & Housing Markets/scripts/datasets/kwb_buurt_1995_2023.Rds")[year >= 2006]
kwb_pc <- readRDS("~/Dropbox/Immigration & Housing Markets/scripts/datasets/kwb_pc4_2006_2023.Rds")[!is.na(postcode)]

# fix postcode-wijk assignment to 2022
pc_wijk <- unique(kwb_pc[year == 2022 & !is.na(postcode), .(postcode, wijkcode, a_inw)], by = c("postcode", "wijkcode"))

# is assignment unique? -  year
pc_wijk[, `:=`(n_pc_perwijk = n_distinct(postcode)), by = wijkcode]
pc_wijk[, `:=`(n_wijk_perpc = n_distinct(wijkcode)), by = postcode]

pc_wijk[, .(n_distinct(postcode)), keyby = n_wijk_perpc]
pc_wijk[, .(n_distinct(wijkcode)), keyby = n_pc_perwijk]

# assign a postcode to a wijk where it has the most population. 
setorder(pc_wijk, postcode, a_inw)
unique_pc_wijk <- pc_wijk[, .(wijkcode_unique = wijkcode[1]), by = postcode]

# aggregate data by postcode, then assign wijk to it, then aggregate by wijkcode 
# aggregate from buurt to wijk - this avoids double weighting issues
kwb_buurt <- kwb[nchar(gwb_code_8) == 8][, postcode := as.numeric(postcode)]
kwb_buurt[, all_woz_na := as.integer(all(is.na(woz))), by = .(year, postcode)]

kwb_buurt <- merge(kwb_buurt, unique_pc_wijk, by = c("postcode"))

# use the same routine as in "1b_kwb_pc4"
sum_cols <- c(grep("^a_", names(kwb_buurt), value = TRUE)) 
avg_woning_cols <- c(grep("^g_", names(kwb_buurt), value = TRUE), "sted", "avg_hh_size", 
                     "p_wont2000", "p_wonv2000")
avg_pop_cols <- c("income_person", "income_worker", "p_laag", "p_hoog")

# all vars that need a simple sum 
kwb_wijk <- kwb_buurt[!is.na(postcode), lapply(.SD, sum), 
                    by = .(year, wijkcode_unique), .SDcols = sum_cols]

# vars weighted by number of houses
kwb_wijk <- merge(kwb_wijk, kwb_buurt[, lapply(.SD, function(var) weighted.mean(x = as.numeric(var), w = woningen)), 
                                  by = .(year, wijkcode_unique), .SDcols = c(avg_woning_cols, "woningen")], 
                by = c("year", "wijkcode_unique"), all.x = TRUE, all.y = FALSE)

# vars weighted by population
kwb_wijk <- merge(kwb_wijk, kwb_buurt[, lapply(.SD, function(var) weighted.mean(x = as.numeric(var), w = a_inw)), 
                                  by = .(year, wijkcode_unique), .SDcols = c(avg_pop_cols)], 
                by = c("year", "wijkcode_unique"), all.x = TRUE, all.y = FALSE)


kwb_wijk <- merge(kwb_wijk, kwb_buurt[!is.na(postcode), .(mean_woz = mean(woz, na.rm = T),
                                                      all_woz_na = all_woz_na[1],
                                                      mean_woz_weight_won = weighted.mean(x = woz, w = woningen, na.rm = T), 
                                                      gemcode = gemcode[1], 
                                                      wijkcode = wijkcode[1], 
                                                      n_postcode = n_distinct(postcode)), 
                                  by = .(year, wijkcode_unique)], by = c("year", "wijkcode_unique"), all.x = TRUE, all.y = FALSE)



# calculate shares for relevant vars 
kwb_wijk[, `:=`(share_rent = a_rent / woningen,
              share_koop = a_koop / woningen,
              share_corp = a_corp / woningen,
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
saveRDS(kwb_wijk, "~/Dropbox/Immigration & Housing Markets/scripts/datasets/kwb_wijk_2006_2023.Rds")





