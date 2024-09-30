## KWB data 
# wijken en burten change every year. Take the postcode-wijken allocation from 2022 and then aggregate via postcodes. 

# Clean KWB data: assign names, adjust dataformat. 

## INPUT: 
# .xlsx sheets in the folder "NeighboorhoodStats"

## OUTPUT:
# panel with a selection of variables, 1995-2023, all levels (buurt, wijk, gemeente)
# saved as "kwb_buurt_1995_2023.Rds" in /datasets

# set up
getwd()
setwd("~/Dropbox/Immigration & Housing Markets")

# packages & paths 
source("scripts/0_setup.R")
setwd("~/Dropbox/Immigration & Housing Markets")


#### Load KWB #####
datalist = list.files(path = "~/Dropbox/Immigration & Housing Markets/NeighborhoodStats", pattern = ".xls")
#for (i in 1:length(datalist))assign(datalist[i], read_excel(datalist[i]))

for (i in 1:length(datalist))assign(datalist[i], setDT(read_excel(paste0("NeighborhoodStats/", datalist[i]))))

kwb1995 <- `kwb-1995.xls`
kwb1997 <- `kwb-1997.xls`
kwb1999 <- `kwb-1999.xls`
kwb2001 <- `kwb-2001.xls`
kwb2003 <- `kwb-2003-versie-2011-08-16.xls`
kwb2004 <- `kwb-2004-versie-2011-08-16.xls`
kwb2005 <- `kwb-2005-versie-2011-08-16.xls`
kwb2006 <- `kwb-2006-versie-2011-08-16.xls`
kwb2007 <- `kwb-2007-versie-2011-08-16.xls`
kwb2008 <- `kwb-2008-versie-2011-11-23.xls`
kwb2009 <- `kwb-2009-versie-2014-06-01.xls`
kwb2010 <- `kwb-2010-versie-2013-12.xls`
kwb2011 <- `kwb-2011.xls`
kwb2012 <- `kwb-2012.xls`
kwb2013 <- `kwb-2013.xls`
kwb2014 <- `kerncijfers-wijken-en-buurten-2014.xls`
kwb2015 <- `kwb-2015.xls`
kwb2016 <- `kwb-2016.xls`
kwb2017 <- `kwb-2017.xls`
kwb2018 <- `kwb-2018.xls`
kwb2019 <- `kwb-2019.xlsx`
kwb2020 <- `kwb-2020.xlsx`
kwb2021 <- `kwb-2021.xlsx`
kwb2022 <- `kwb-2022.xlsx`
kwb2023 <- `kwb-2023.xlsx`

rm(list=ls(pattern = "xls"))

# for 1997 - 1999, the format is different 
colnames(kwb1995) <- as.matrix(kwb1995[3,])
kwb1995 <- kwb1995[-c(1:4),]
colnames(kwb1995)[42] <- "Overig"

colnames(kwb1997) <- as.matrix(kwb1997[3,])
kwb1997 <- kwb1997[-c(1:4),]

colnames(kwb1999) <- as.matrix(kwb1999[3,])
kwb1999 <- kwb1999[-c(1:4),]

colnames(kwb2001) <- as.matrix(kwb2001[3,])
kwb2001 <- kwb2001[-c(1:4),]

colnames(kwb1995)[42] <- "Overig"

colnames(kwb2013)[123] <- "ste_mvs"
colnames(kwb2014)[123] <- "ste_mvs"

# put in list 
kwblist <- Filter(function(x) is(x, "data.frame"), mget(ls()))

# remove data tables that are floating around 
rm(kwb1995, kwb1997, kwb1999, kwb2001, kwb2003, kwb2004, kwb2005, kwb2006, 
   kwb2007, kwb2008, kwb2009, kwb2010, kwb2011, kwb2012, kwb2013, kwb2014, 
   kwb2015,  kwb2016, kwb2017, kwb2018, kwb2019, kwb2020, kwb2021, kwb2022, kwb2023)
gc()

##### clean column names ####
# set to lower case 
kwblist <- lapply(kwblist, function(x){
  setnames(x, old = names(x), new = tolower(names(x)))
})

# remove spaces, %,  (%), comma and dots.
kwblist <- lapply(kwblist, function(x){
  setnames(x, old = names(x), new = gsub("\\s*\\(%)\\s*", "", names(x)))
  setnames(x, old = names(x), new = gsub("\\.", "", names(x)))
  setnames(x, old = names(x), new = gsub(" ", "", names(x)))
})

# adjust the variations of names across kwblist
kwblist[c(1:25)] <- lapply(1:25, function(x){
  if(x <= 4){
    colnames(kwblist[[x]])[c(1,2)] <- c("gwb_code_8", "regio")
  }
  if(x>4 & x <= 14){
    colnames(kwblist[[x]])[c(2,3)] <- c("gwb_code_8", "regio")
  }
  return(kwblist[[names(kwblist)[x]]])
})

##### var names ####
# adjust that names that are common across datasets 
kwblist <- lapply(kwblist, function(x){
  setnames(x, old = "regioaanduiding", new = "recs", skip_absent = TRUE)
  setnames(x, old = c("meestvoorkomendenumeriekepostcode", "pst_mvp"), 
           new = rep("postcode", 2), skip_absent = TRUE)
  
  setnames(x, old = c("omgevingsadressendichtheid", "omgevingsadressendichtheidperkm2", "oad", "ste_oad"),
           new = rep("add_dicht", 4), skip_absent = TRUE)
  
  setnames(x, old = c("stedelijkheidgemeente,wijkofbuurt", "stedelijkheid", "ste_mvs", "2", "pst_dekp"),
           new = c(rep("sted", 4), "dek_perc"), skip_absent = TRUE)
  
  setnames(x, old = c("aantalinwoners", "aant_inw", "inwoners"), 
           new = rep("a_inw", 3), skip_absent = TRUE)
  
  setnames(x, old = c("mannen", "aant_man"),
           new = rep("a_man", 2), skip_absent = TRUE)
  setnames(x, old = c("vrouwen", "vouwen", "aant_vrouw"), 
           new = rep("a_vrouw", 3), skip_absent = TRUE)
  
  setnames(x, old = c("leeftijd0-14jaar", "personen0-14jaar", "personen0-14jr", "p_00_14_jr"), 
           new = rep("p_00_14", 4), skip_absent = TRUE)
  
  setnames(x, old = c("leeftijd15-24jaar", "personen15-24jaar", "personen15-24jr", "p_15_24_jr"), 
           new = rep("p_15_24", 4), skip_absent = TRUE)
  
  setnames(x, old = c("leeftijd25-44jaar","personen25-44jaar", "personen25-44jr", "p_25_44_jr"), 
           new = rep("p_25_44", 4), skip_absent = TRUE)
  
  setnames(x, old = c("leeftijd45-64jaar", "personen45-64jaar", "personen45-64jr", "p_45_64_jr"),
           new = rep("p_45_64", 4), skip_absent = TRUE)
  
  setnames(x, old = c("leeftijd65jaarenouder", "personen65jaarenouder", "personen65jrenouder", "p_65_eo_jr"),
           new = rep("p_65_oo", 4), skip_absent = TRUE)
  
  setnames(x, old = c("aantal_gezinnen", "aantalgezinnen", "gezinnen", "aantalhuishoudens", "aantal_hh"), 
           new = rep("a_hh", 5), skip_absent = TRUE)
  
  setnames(x, old = c("gemiddeldegezinsgrootte", "gezinsgrootte(gemiddelde)", "gemiddeldehuishoudensgrootte", "gem_hh_gr", "g_hhgro"),
           new = rep("avg_hh_size", 5), skip_absent = TRUE)
  
  setnames(x, old = c("bevolkingsdichtheid", "bevolkingsdichtheidperkm²", "bev_dichth", "bev_dich"),
           new = rep("bev_dicht", 4), skip_absent = TRUE)
  
  setnames(x, old = c("allochtonen()", "allochtonen"), 
           new = rep("p_migrant", 2), skip_absent = TRUE)
  
  setnames(x, old = c("p_w_al", "p_w_all", "p_west_all", "p_west_al", 
                      "a_w_all"), 
           new = c(rep("p_west", 4),
                   "a_west"), skip_absent = TRUE)
  
  setnames(x, old = c("p_n_w_al", "p_nw_all", 
                      "a_nw_all"), 
           new = c(rep("p_nonwest", 2), 
                   "a_nonwest"), skip_absent = TRUE)
  
  setnames(x, old = c("allochtonen"), 
           new = c(rep("p_migrant", 1)), skip_absent = TRUE)
  
  setnames(x, old = c("gemiddeldbesteedbaarinkomenperinw", "gemiddeldinkomenperinwoner", "ink_inw", "g_ink_pi", "ink_inw2"),
           new = rep("income_person", 5), skip_absent = TRUE)
  
  setnames(x, old = c("gemiddeldinkomenperinkomensontvanger", "ink_ontv", "g_ink_po", "ink_ontv2"),
           new = rep("income_worker", 4), skip_absent = TRUE)
  
  setnames(x, old = c("inkomensontvangersmetlaaginkomen", "lageinkomens", "p_laaginkp", "p_ink_li", "p_laag_ink"),
           new = rep("p_laag", 5), skip_absent = TRUE)
  
  setnames(x, old = c("inkomensontvangersmethooginkomen", "hogeinkomens", "p_hooginkp", "p_ink_hi", "p_hoog_ink"),
           new = rep("p_hoog", 5), skip_absent = TRUE)
  
  setnames(x, old = c("niet-actieveinkomensontvangers", "nietactieven", "p_niet_act"),
           new = rep("p_nonworking", 3), skip_absent = TRUE)
  
  # houses and house prices
  setnames(x, old = c("woningvoorraad", "a_woning"),
           new = rep("woningen", 2), skip_absent = TRUE)
  setnames(x, old = c("p_huurw", 
                      "p_koopw", 
                      "p_wcorpw"),
           new = c(rep("p_huurwon", 1), 
                   rep("p_koopwon", 1), 
                   rep("p_huurcorp")), skip_absent = TRUE)
  
  setnames(x, old = c("gemiddeldewoz-waardevanwoningen", "woz-waarde", "woz_waarde", "woningwaarde", "g_woz", "g_wozbag"), 
           new = rep("woz", 6), skip_absent = TRUE)
  
  setnames(x, old = c("p_bjj2k", "p_bjo2k"), 
           new = c("p_wont2000", "p_wonv2000"), skip_absent = T)
  
  # space
  setnames(x, old = c("totaleoppervlakteinkm2", "oppervlaktetotaal", "a_opp_ha"),
           new = rep("opp_tot", 3), skip_absent = TRUE)
  
  setnames(x, old = c("oppervlaktelandinkm2", "oppervlakteland", "a_lan_ha"),
           new = rep("opp_land", 3), skip_absent = TRUE)
  
  # gas/electricity by woningtype 
  # as of 2013, KWB also contains categories for huur en koopwoning. 
  setnames(x, 
           old = c("p_gas_tot", "p_gas_app", "p_gas_tus", "p_gas_hoek", "p_gas_21k", "p_gas_vry"), 
           new = c("g_gas", "g_gas_ap", "g_gas_tw", "g_gas_hw", "g_gas_2w", "g_gas_vw"), skip_absent = TRUE)
  setnames(x, 
           old = c("p_elek_tot", "p_elek_app", "p_elek_hoe", "p_elek_21k", "p_elek_tus", "p_elek_vry"), 
           new = c("g_ele", "g_ele_ap", "g_ele_tw", "g_ele_hw", "g_ele_2w", "g_ele_vw"), skip_absent = TRUE)
  
  # social assistance
  setnames(x, 
           old = c("ww_uit_tot", "a_soz_ww", "wwb_uit_tot", "wwb_uittot", "wwb_uit", "a_soz_wb"), 
           new = c(rep("a_ww", 2), rep("a_wwb", 4)), skip_absent = TRUE)
  
  # firms 
  setnames(x, old = c("vestigingen", "aantalvestigingenvanbedrijven", "bedr_tot", "a_bedv", "totaal(excloverheid,onderwijsenzorg)"), 
           new = rep("a_firms", 5), skip_absent = TRUE)
  
  # amenities
  setnames(x, 
           old = c("g_3km_sc"), 
           new = c("av3_ondbas"), skip_absent = T)
  
  setnames(x, 
           old = c("af_artspo", "af_superm", "af_kdv", "af_ondbas"), 
           new = c("g_afs_hp", "g_afs_gs", "g_afs_kv", "g_afs_sc"), skip_absent = T)
})

# as of number 5, there are many more vars, including energy usage 
# as of 2004, there are variables on electricity available, not a full set in all years though. 
lapply(kwblist, function(x){names(x)[grep("laag", names(x))]})


#### bind ####
# first bind all data.tables together, then fill the missing values (for example p_west vs a_west)

# find common names
# 6 - 14 have > 80 names in common 
# 16- 15 also have >80 names in common. 
# in 2023, the concept of migrants was changes, and there is no distinction between west and non-west anymore
common_names <- Reduce(intersect, lapply(kwblist, names))
print(common_names)

# make a sub-list with common variables + names for energy vars. These will be filled with 0 if missing in earlier datasets. 
final_vars <- c(common_names, 
                "bev_dicht", "add_dicht", "income_person", "income_worker", "p_wont2000", "p_wonv2000",
                "a_ww", "a_wwb", "a_firms", "dek_perc", "woz", "woningen",
                "p_laag", "p_hoog", "p_west", "p_nonwest", 
                "a_west", "a_nonwest", 
                "a_00_14", "a_15_24", "a_25_44", "a_45_64", "a_65_oo",
                "p_00_14", "p_15_24", "p_25_44", "p_45_64", "p_65_oo",
                "p_huurwon", "p_huurcorp", "p_koopwon",
                "g_afs_hp", "g_afs_gs", "g_afs_kv", "g_afs_sc", 
                "g_gas", "g_gas_ap", "g_gas_tw", "g_gas_hw", "g_gas_2w", "g_gas_vw", 
                "g_ele", "g_ele_ap", "g_ele_tw", "g_ele_hw", "g_ele_2w", "g_ele_vw", 
                "id")

# make a panel- this is on the buurt level - this binds and interpolates all vars. 
# using fill = TRUE will fill vars that are not available in all years with NA. 
# in the next step, there is a selection of variables 
kwb <- rbindlist(kwblist, use.names = TRUE, fill = TRUE, idcol = "id")

kwb <- kwb[, ..final_vars]

kwb[, year := substr(id, start = nchar(id) - 3, stop = nchar(id))]
kwb[, wijkcode := substr(gwb_code_8, start = 1, stop = 6)]
kwb[, gemcode := substr(gwb_code_8, start = 1, stop = 4)]

##### make variables numeric ####
# in some versions, x is coded as NA. 
vars_num <- c("year", names(kwb)[grepl("a_", names(kwb))], names(kwb)[grepl("g_", names(kwb))],
              names(kwb)[grepl("p_", names(kwb))],
              "income_person", "income_worker", "bev_dicht", "add_dicht", "woz", "woningen")

# replace "," for "."
kwb <- kwb[, (vars_num) := lapply(.SD, function(x){str_replace(x, ",", ".")}), .SDcols = vars_num]
kwb <- kwb[, (vars_num) := lapply(.SD, as.numeric), .SDcols = vars_num]

# adjust values: "." for NA  
# for all other variables, this will be forced when converting to numeric. 
# this only concern would be is "0" is coded for "NA" as for stedelijkheid in 2013. 
kwb[sted == "-" | sted == "." | sted == "0", sted := NA]
kwb[dek_perc == ".", dek_perc := NA]


##### impute ####
#for "p_vars", compute the total, for "a_vars", compute the share. 

var <- c("west", "nonwest", "00_14", "15_24", "25_44", "45_64", "65_oo")

lapply(seq_along(var), function(i) {
  p_var <- paste0("p_", var[i])  # Percentage variable
  a_var <- paste0("a_", var[i])  # Total variable
  
  # Calculate the total if it's missing and percentage + total population are available
  kwb[, (a_var) := ifelse(is.na(get(a_var)) & !is.na(get(p_var)) & !is.na(a_inw),
                         (get(p_var) / 100) * a_inw, get(a_var))]
  
  # Calculate the percentage if it's missing and total + total population are available
  kwb[, (p_var) := ifelse(is.na(get(p_var)) & !is.na(get(a_var)) & !is.na(a_inw),
                         round(get(a_var) / a_inw, 0), get(p_var))]
})

# calculate totals for houses
kwb[, `:=`(a_rent = (p_huurwon/100) * woningen, 
           a_koop = (p_koopwon/100) * woningen, 
           a_corp = (p_huurcorp/100) * woningen)]

# check for missing values and moments. 
# some vars are only available on the buurt level and or for buurts with a minimum number of inhabitants, 
# and not for wijk or gemeente, hence resulting in many missing vars. 
for (i in 35:40){
  print(vars_num[[i]])
  # unique values 
  #print(unique(kwb[, .(get(vars_num[[i]]))]))
  # dimension of NA values
  print(dim(kwb[recs == "B" & is.na(get(vars_num[[i]]))]))
  print(kwb[is.na(get(vars_num[[i]])) & recs == "B", .(length(gwb_code_8)), by = year])
  # quantiles 
  print(kwb[, .(quantile(get(vars_num[[i]]), na.rm = TRUE))])
}

# a_inw, a_man and a_vrouw are rounded to 5. Sometimes it can be that a_man + a_vrouw > a_inw, but never a higher difference than 10.
kwb[as.numeric(a_man) + as.numeric(a_vrouw) > as.numeric(a_inw) & nchar(gwb_code_8) == 8, .(n_distinct(gwb_code_8)), by = year]
kwb[abs(as.numeric(a_man) + as.numeric(a_vrouw) - as.numeric(a_inw)) > 10 & nchar(gwb_code_8) == 8, .(n_distinct(gwb_code_8)), by = year]

#### save ####
saveRDS(kwb, "~/Dropbox/Immigration & Housing Markets/scripts/datasets/kwb_buurt_1995_2023.Rds")





#### old, but useful ? ####
# loop through datasets in kwblist, make some vars missing values and convert to numeric

for (i in 1:length(kwblist)){  # as of 2017, the number of people by age categories + migrants is reported, instead of the percentage 
  # calculate the number of people/ migrants by percentage * total population
  print(i)  
  kwblist[[i]] <- kwblist[[i]][, (vars_num[vars_num %in% names(kwblist[[i]])]) := 
                                 lapply(.SD, function(x){str_replace(x, ",", ".")}), 
                               .SDcols = vars_num[vars_num %in% names(kwblist[[i]])]]
  
  kwblist[[i]] <- kwblist[[i]][, (vars_num[vars_num %in% names(kwblist[[i]])]) := 
                                 lapply(.SD, as.numeric), 
                               .SDcols = vars_num[vars_num %in% names(kwblist[[i]])]]
  
  if("p_west" )  
    if("p_00_14" %in% names(kwblist[[i]]) & "p_west" %in% names(kwblist[[i]])){
      kwblist[[i]] <- kwblist[[i]][, `:=`(a_00_14 = (p_00_14/100) * a_inw, 
                                          a_15_24 = (p_15_24/100) * a_inw, 
                                          a_25_44 = (p_25_44/100) * a_inw, 
                                          a_45_64 = (p_45_64/100) * a_inw, 
                                          a_65_oo = (p_65_oo/100) * a_inw, 
                                          a_west = (p_west/100) * a_inw, 
                                          a_nonwest = (p_nonwest / 100) * a_inw)]
    }
  if("p_huurwon" %in% names(kwblist[[i]])){
    # compute the number of rented, owned, corp houses per buurt and then per pc4
    kwblist[[i]] <- kwblist[[i]][, `:=`(a_rent = (p_huurwon/100) * woningen, 
                                        a_koop = (p_koopwon/100) * woningen, 
                                        a_corp = (p_huurcorp/100) * woningen)]
  }
  if(i == 25){
    kwblist[[i]] <- kwblist[[i]][, `:=`(a_west = NA, 
                                        a_nonwest = NA)]
  }
}





