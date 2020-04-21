source("covid_functions.R")
loop2 <- loop
loop2$a65 <- ifelse(loop2$age >= 65, 1, 0)
loop2$a65chronic <- ifelse(loop2$a65 == 1 & loop2$health_issue.chronic == 1, 1, 0)
loop2$a65andorchronic <- ifelse(loop2$a65 == 1 | loop2$health_issue.chronic == 1, 1, 0)

districtuuid <- response[, c("district", "X_uuid", "strata", "cluster_id", "population_group")]
names(districtuuid)[names(districtuuid) == "X_uuid"] <- "X_submission__uuid"
loop2 <- merge(districtuuid, loop2, by = "X_submission__uuid")

#WEIGHTING
strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe_strata,
                                      sampling.frame.population.column = "population",
                                      sampling.frame.stratum.column = "stratum",
                                      data.stratum.column = "strata",
                                      data = loop2)

# weight_fun <- combine_weighting_functions(strata_weight_fun, clusters_weight_fun)
weight_fun<-strata_weight_fun


loop2$weights<-weight_fun(loop2)



dap_name <- "covid19"
analysisplan <- read.csv(sprintf("input/dap_%s.csv",dap_name), stringsAsFactors = F)
#analysisplan <- analysisplan_nationwide(analysisplan)
#analysisplan <- analysisplan_pop_group_aggregated(analysisplan)

#CREATE LIST OF RESULTS
list_of_results <-  from_analysisplan_map_to_output(loop2, analysisplan = analysisplan,
                                                    weighting = weight_fun,
                                                    cluster_variable_name = "cluster_id",
                                                    questionnaire = NULL, confidence_level = 0.9)


name <- "covid19_popgroup_district"
saveRDS(list_of_results,paste(sprintf("output/result_%s.RDS", name)))
#summary[which(summary$dependent.var == "g51a"),]

lookup_in_camp<-load_samplingframe("./input/sampling_frame_in_camp.csv")
names(lookup_in_camp)[which(names(lookup_in_camp) == "camp")] <- "name"
names(lookup_in_camp)[which(names(lookup_in_camp) == "camp.long.name")] <- "english"
names(lookup_in_camp)[which(names(lookup_in_camp) == "governorate")] <- "filter"

summary <- bind_rows(lapply(list_of_results[[1]], function(x){x$summary.statistic}))
write.csv(summary, sprintf("output/covid19/raw_results_%s.csv", name), row.names=F)
summary <- read.csv(sprintf("output/covid19/raw_results_%s.csv", name), stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA,1))
write.csv(summary, sprintf("output/covid19/raw_results_%s_filtered.csv", name), row.names=F)
if(all(is.na(summary$independent.var.value))){summary$independent.var.value <- "all"}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]
for (i in 1:length(groups)) {
  df <- pretty.output(summary, groups[i], analysisplan, cluster_lookup_table, lookup_table, severity = name == "severity", camp = F)
  write.csv(df, sprintf("output/covid19/summary_sorted_%s_%s.csv", name, groups[i]), row.names = F)
  if(i == 1){
    write.xlsx(df, file=sprintf("output/covid19/summary_sorted_%s.xlsx", name), sheetName=groups[i], row.names=FALSE)
  } else {
    write.xlsx(df, file=sprintf("output/covid19/summary_sorted_%s.xlsx", name), sheetName=groups[i], append=TRUE, row.names=FALSE)
  }
}  


#CALCULATE ABSOLUTE VALUES
#IMPORT DTM RETURNEE AND IDP MASTER LISTS
idp_feb20 <- read.csv("input/covid19/IDP_Feb20.csv", skip = 2)
idp_feb20 <- idp_feb20[,c("Governorate", "District", "Individuals", "Households", "Camp")]
idp_feb20$incamp_ind <- ifelse(idp_feb20$Camp != 0, idp_feb20$Individuals, 0)
idp_feb20$outcamp_ind <- ifelse(idp_feb20$Camp == 0, idp_feb20$Individuals, 0)
idp_feb20 <- idp_feb20[,c("Governorate", "District", "incamp_ind", "outcamp_ind", "Households")]

incamp_feb20 <- read.csv("input/covid19/cccm_camp_dataFeb.csv")
incamp_feb20 <- incamp_feb20[,c("Governorate", "District", "Total.no.of.individuals")]


returnee_feb20 <- read.csv("input/covid19/returnee_Feb20.csv", skip = 2)
returnee_feb20 <- returnee_feb20[,c("Governorate", "District", "Individuals", "Households")]
names(returnee_feb20)[names(returnee_feb20) == "Individuals"] <- "returnee_ind"


agg_returnee = aggregate(returnee_feb20$returnee_ind,
                by = list(returnee_feb20$District),
                FUN = sum)

agg_idp = aggregate(list(idp_feb20$outcamp_ind),
                         by = list(idp_feb20$District),
                         FUN = sum)

agg_incamp = aggregate(list(incamp_feb20$Total.no.of.individuals),
                    by = list(incamp_feb20$District),
                    FUN = sum)


write.csv(agg_returnee, "output/Covid19/agg_returnee.csv")
write.csv(agg_idp, "output/Covid19/agg_idp_outcamp.csv")
write.csv(agg_incamp, "output/Covid19/agg_idp_incamp.csv")


names(agg_idp) <- c("district", "outcamp_IDP")
names(agg_returnee) <- c("district", "returnee")
names(agg_incamp) <- c("district", "incamp_IDP")


lookup_district <- subset(lookup_table, lookup_table$list_name == "district_mcna")
write.csv(lookup_district, "output/Covid19/lookup_district.csv")

incamp_covid <- read.csv("output/Covid19/summary_sorted_covid19_popgroup_district_idp_in_camp.csv")
incamp_covid <- incamp_covid[-c(2,3,4),] 
incamp_covid <- incamp_covid[,-c(4,5,7,8,10,11,13,14)]

outcamp_covid <- read.csv("output/Covid19/summary_sorted_covid19_popgroup_district_idp_out_camp.csv")
outcamp_covid <- outcamp_covid[-c(2,3,4),] 
outcamp_covid <- outcamp_covid[,-c(4,5,7,8,10,11,13,14)]

returnee_covid <- read.csv("output/Covid19/summary_sorted_covid19_popgroup_district_returnee.csv")
returnee_covid <- returnee_covid[-c(2,3,4),] 
returnee_covid <- returnee_covid[,-c(4,5,7,8,10,11,13,14)]

#CHANGE TO DELIVERABLE NAMES: LOAD FILE FORMATTED THE SAME AS THE EXAMPLE EXCEL FILE (KOBO HEADER AND DESIRED NAME FOR OUTPUT)
old_new_names <- read.csv("input/covid19/name_comparison_mcna_iom.csv")
names(old_new_names)[names(old_new_names) == "ï..Name_mcna"] <- "Name_mcna"
#LOOP THROUGH THE ENTIRE NAME CHANGE SHEET AND APPLY TO THE WHOLE RANKED OUTPUT
names(old_new_names)
old_new_names$Name_mcna <- as.character(as.factor(old_new_names$Name_mcna))
old_new_names$Name_iom <- as.character(as.factor(old_new_names$Name_iom))
agg_idp$district <- as.character(as.factor(agg_idp$district))
agg_returnee$district <- as.character(as.factor(agg_returnee$district))

for(i in 1:nrow(old_new_names)){
  agg_idp <- as.data.frame(lapply(agg_idp, function(x) replace(x, grep(as.character(old_new_names[i,2]), x), as.character(old_new_names[i,1]))),stringsAsFactors = FALSE)
}
for(i in 1:nrow(old_new_names)){
  agg_returnee <- as.data.frame(lapply(agg_returnee, function(x) replace(x, grep(as.character(old_new_names[i,2]), x), as.character(old_new_names[i,1]))),stringsAsFactors = FALSE)
}

#MERGE DATASETS
outcamp_covid2 <- merge(outcamp_covid, agg_idp, by="district")
returnee_covid2 <- merge(returnee_covid, agg_returnee, by="district")
incamp_covid2 <- merge(incamp_covid, agg_incamp, by="district")

factorToNumeric <- function(f) as.numeric(levels(f))[as.integer(f)] 
cols <- c(3:6)

incamp_covid2[cols] <- lapply(incamp_covid2[cols], factorToNumeric)
incamp_covid2$incamp_IDP <- as.numeric(incamp_covid2$incamp_IDP)
incamp_covid2$a65 <- round(incamp_covid2$a65 * incamp_covid2$incamp_IDP, digits = 0)
incamp_covid2$a65chronic <- round(incamp_covid2$a65chronic * incamp_covid2$incamp_IDP, digits = 0)
incamp_covid2$health_issue.chronic <- round(incamp_covid2$health_issue.chronic * incamp_covid2$incamp_IDP, digits = 0)
incamp_covid2$a65andorchronic <- round(incamp_covid2$a65andorchronic * incamp_covid2$incamp_IDP, digits = 0)
#incamp_covid2[,c(7,8)] <- NULL

outcamp_covid2[cols] <- lapply(outcamp_covid2[cols], factorToNumeric)
outcamp_covid2$outcamp_IDP <- as.numeric(as.character(outcamp_covid2$outcamp_IDP))
outcamp_covid2$a65 <- round(outcamp_covid2$a65 * outcamp_covid2$outcamp_IDP, digits = 0)
outcamp_covid2$a65chronic <- round(outcamp_covid2$a65chronic * outcamp_covid2$outcamp_IDP, digits = 0)
outcamp_covid2$health_issue.chronic <- round(outcamp_covid2$health_issue.chronic * outcamp_covid2$outcamp_IDP, digits = 0)
outcamp_covid2$a65andorchronic <- round(outcamp_covid2$a65andorchronic * outcamp_covid2$outcamp_IDP, digits = 0)
outcamp_covid2[,c(7,8)] <- NULL

returnee_covid2[cols] <- lapply(returnee_covid2[cols], factorToNumeric)
returnee_covid2$returnee <- as.numeric(as.character(returnee_covid2$returnee))
returnee_covid2$a65 <- round(returnee_covid2$a65 * returnee_covid2$returnee, digits = 0)
returnee_covid2$a65chronic <- round(returnee_covid2$a65chronic * returnee_covid2$returnee, digits = 0)
returnee_covid2$health_issue.chronic <- round(returnee_covid2$health_issue.chronic * returnee_covid2$returnee, digits = 0)
returnee_covid2$a65andorchronic <- round(returnee_covid2$a65andorchronic * returnee_covid2$returnee, digits = 0)
returnee_covid2[,c(7,8)] <- NULL

#EXPORT DATASETS
write.csv(incamp_covid2, "output/Covid19/incamp_covid2.csv")
write.csv(outcamp_covid2, "output/Covid19/outcamp_covid2.csv")
write.csv(returnee_covid2, "output/Covid19/returnee_covid2.csv")



###########################################################################
#ANALYSIS REFUGEES KRI MSNA
###########################################################################
msna <- read.csv("input/covid19/UNHCR_KRI_MSNA_IV_dataset_clean.csv")
msna <- subset(msna, msna$district_KRI != "penjwin" & msna$district_KRI != "dokan")
msna[,c("HH_male_over_60", "HH_female_over_60")] <- lapply(msna[,c("HH_male_over_60", "HH_female_over_60")], as.numeric)
msna$HH_over_60 <- msna$HH_male_over_60 + msna$HH_female_over_60
msna$tot_people <- msna$calc_HH_females + msna$calc_HH_males
msna$HH_over_60_weighted <- msna$HH_over_60 * msna$weight
msna$tot_people_weighted <- msna$tot_people * msna$weight


#CALCULATE WEIGHTED NUMBER OF PEOPLE AND NUMBER OF PEOPLE ABOVE THE AGE OF 60 BY DISTRICT
msna_demographics = aggregate(list(msna$HH_over_60_weighted, msna$tot_people_weighted, msna$HH_over_60, msna$tot_people),
                    by = list(msna$district_KRI),
                    FUN = sum)
names(msna_demographics) <- c("District", "HH_over_60_weighted", "tot_people_weighted", "HH_over_60", "tot_people")
msna_demographics$HH_over_60_weighted_perc <- msna_demographics$HH_over_60_weighted / msna_demographics$tot_people_weighted

dap_name <- "covid_msna"
analysisplan <- read.csv(sprintf("input/covid19/dap_%s.csv",dap_name), stringsAsFactors = F)
#analysisplan <- analysisplan_nationwide(analysisplan)
#analysisplan <- analysisplan_pop_group_aggregated(analysisplan)
weight_fun<-function(df){
  df$weights
}

#CREATE LIST OF RESULTS
list_of_results <-  from_analysisplan_map_to_output(msna, analysisplan = analysisplan,
                                                    weighting = weight_fun,
                                                    cluster_variable_name = NULL,
                                                    questionnaire = NULL, confidence_level = 0.9)


name <- "covid19_msna_district"
saveRDS(list_of_results,paste(sprintf("output/result_%s.RDS", name)))
#summary[which(summary$dependent.var == "g51a"),]

summary <- bind_rows(lapply(list_of_results[[1]], function(x){x$summary.statistic}))
write.csv(summary, sprintf("output/covid19/raw_results_%s.csv", name), row.names=F)
summary <- read.csv(sprintf("output/covid19/raw_results_%s.csv", name), stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA,1))
write.csv(summary, sprintf("output/covid19/raw_results_%s_filtered.csv", name), row.names=F)

if(all(is.na(summary$independent.var.value))){summary$independent.var.value <- "all"}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]
for (i in 1:length(groups)) {
  df <- pretty.output(summary, groups[i], analysisplan, cluster_lookup_table, lookup_table, severity = name == "severity", camp = F)
  write.csv(df, sprintf("output/covid19/summary_sorted_%s_%s.csv", name, groups[i]), row.names = F)
  if(i == 1){
    write.xlsx(df, file=sprintf("output/covid19/summary_sorted_%s.xlsx", name), sheetName=groups[i], row.names=FALSE)
  } else {
    write.xlsx(df, file=sprintf("output/covid19/summary_sorted_%s.xlsx", name), sheetName=groups[i], append=TRUE, row.names=FALSE)
  }
}  
