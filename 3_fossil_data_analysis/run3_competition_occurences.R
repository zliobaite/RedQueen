# 2016 10 10 I.Zliobaite

# occurence matrixes
# competition indices

do_EU <- FALSE
do_NAM <- TRUE

fet_taxa <- 'GENUS'
fet_loc <- 'LOCALITY'
fet_time <- 'TIME_UNIT'

if (do_EU){
  print('doing Europe')
  input_file_localities <- 'data_processed/localities_EU.csv'
  input_file_environment <- 'data_processed/data_environment_EU.csv'
  input_file_taxa <- 'data_processed/taxa_EU.csv'
  input_file <- 'data_processed/data_NOW_filtered_EU_herb.csv'
  out_file_occ_counts <- 'data_processed/occurence_EU_herb.csv'
  out_file_occ_normed <- 'data_processed/occurence_normed_EU_herb.csv'
  out_file_sites <- 'data_processed/data_sites_EU.csv'
  out_file_dataset <- 'data_processed/data_dataset_EU.csv'
}else{
  if (do_NAM){
    print('doing America')
    input_file_localities <- 'data_processed/localities_NAm.csv'
    input_file_environment <- 'data_processed/data_environment_NAm.csv'
    input_file_taxa <- 'data_processed/taxa_NAm.csv'
    input_file <- 'data_processed/data_NOW_filtered_NAm_herb.csv'
    out_file_occ_counts <- 'data_processed/occurence_NAm_herb.csv'
    out_file_occ_normed <- 'data_processed/occurence_normed_NAm_herb.csv'
    out_file_sites <- 'data_processed/data_sites_NAm.csv'
    out_file_dataset <- 'data_processed/data_dataset_NAm.csv'
  }else{
    print('doing Africa')
    input_file_localities <- 'data_processed/localities_TUR.csv'
    input_file_environment <- 'data_processed/data_environment_TUR.csv'
    input_file_taxa <- 'data_processed/taxa_TUR.csv'
    input_file <- 'data_processed/data_filtered_TUR_herb.csv'
    out_file_occ_counts <- 'data_processed/occurence_TUR_herb.csv'
    out_file_occ_normed <- 'data_processed/occurence_normed_TUR_herb.csv'
    out_file_sites <- 'data_processed/data_sites_TUR.csv'
    out_file_dataset <- 'data_processed/data_dataset_TUR.csv'
  }
}

data_localities <- read.csv(input_file_localities, header = TRUE,sep = '\t')
data_environment <- read.csv(input_file_environment, header = TRUE,sep = '\t')
data_taxa <- read.csv(input_file_taxa, header = TRUE,sep = '\t')
data_all <- read.csv(input_file, header = TRUE,sep = '\t')

taxa_all <- as.vector(data_taxa[,'TAXON'])
times_all <- as.vector(data_environment[,'TIME_UNIT'])
occ_counts_all <- matrix(0,dim(data_taxa)[1],dim(data_environment)[1])
#occ_counts_all <- cbind(occ_counts_all,taxa_all)
rownames(occ_counts_all) <- taxa_all
colnames(occ_counts_all) <- times_all

no_locs <- c()
for (sk in 1:length(times_all)){
  time_now <- times_all[sk]
  ind <- which(data_all[,fet_time]==time_now)
  no_locs <- rbind(no_locs,cbind(time_now,length(unique(data_all[ind,fet_loc]))))
}

occ_normed_all <- occ_counts_all
for (sk in 1:length(taxa_all)){
  taxa_now <- taxa_all[sk]
  ind_tx <- which(data_all[,fet_taxa]==taxa_now)
  for (sk2 in 1:length(times_all)){
    time_now <- times_all[sk2]
    ind_tm <- which(data_all[ind_tx,fet_time]==time_now)
    if (length(ind_tm)>0){
      no_locations_now <- length(unique(data_all[ind_tx[ind_tm],fet_loc]))
      occ_counts_all[sk,sk2] <- no_locations_now  
      occ_normed_all[sk,sk2] <- round((no_locations_now/as.numeric(no_locs[sk2,2])),digits = 3)
    }
  }
}

#occ_counts_only_numbers <- occ_counts_all[,c(1:length(times_all))]
occ_counts_only_numbers <- occ_counts_all
occ_normed_only_numbers <- occ_normed_all

occ_extinction <- occ_counts_only_numbers*0
occ_origination <- occ_counts_only_numbers*0
occ_alive <- occ_counts_only_numbers*0
occ_taxa  <- occ_counts_only_numbers*0
occ_peak <- occ_counts_only_numbers*0
for (sk in 1:length(taxa_all)){
  ind <- which(occ_counts_all[sk,c(1:length(times_all))]>0)
  occ_taxa[sk,ind] <- 1
  if (ind[1]>1){
    occ_origination[sk,(ind[1]-1)] <- 1
  }
  #occ_origination[sk,ind[1]] <- 1
  occ_extinction[sk,ind[length(ind)]] <- 1
  occ_alive[sk,c(ind[1]:ind[length(ind)])] <- 1
  ind <- which(occ_normed_only_numbers[sk,]==max(occ_normed_only_numbers[sk,]))
  occ_peak[sk,ind[length(ind)]] <- 1
}

no_union <- apply(occ_counts_only_numbers,2,sum)
no_localities <- as.numeric(no_locs[,2])
no_last <- apply(occ_extinction,2,sum)
no_first <- apply(occ_origination,2,sum)
no_peak <- apply(occ_peak,2,sum)
no_alive <- apply(occ_alive,2,sum)
no_taxa <- apply(occ_taxa,2,sum)
no_observed <- apply(occ_counts_only_numbers,2,sum)

#p_ext <- round(no_last/no_taxa,digits = 3)
#p_orig <- round(no_first/no_taxa,digits = 3)
#p_peak <- round(no_peak/no_taxa,digits = 3)
p_ext <- round(no_last/no_alive,digits = 3)
p_orig <- round(no_first/(no_alive - no_last + no_first),digits = 3)
p_peak <- round(no_peak/no_alive,digits = 3)

#competition <- round(1 - no_localities/no_union,digits = 3)
competition <- round(no_observed/no_localities,digits = 3)

competition_perTIME <- competition/data_environment[,'diffTIME']
p_orig_perTIME <- no_first/data_environment[,'diffTIME']
p_peak_perTIME <- no_peak/data_environment[,'diffTIME']
p_ext_perTIME <- no_last/data_environment[,'diffTIME']

data_environment <- cbind(data_environment,competition,p_ext,p_orig,p_peak,no_alive,no_localities,competition_perTIME,p_orig_perTIME,p_peak_perTIME,p_ext_perTIME)
data_sites <- cbind(data_environment[,c('TIME_UNIT','TIME_AGE','meanHYP','no_localities')],no_observed,no_taxa,no_alive,no_first,no_peak,no_last)
write.table(data_sites,file = out_file_sites,quote = FALSE,row.names = FALSE,sep='\t')

rrange <- c(3:(dim(data_environment)[1]-2))
data_environment <- data_environment[rrange,]

write.table(data_environment,file = out_file_dataset,quote = FALSE,row.names = FALSE,sep='\t')

occ_counts_all <- cbind(occ_counts_all,rownames(occ_counts_all))
occ_normed_all <- cbind(occ_normed_all,rownames(occ_normed_all))

write.table(occ_counts_all,file = out_file_occ_counts,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')
write.table(occ_normed_all,file = out_file_occ_normed,quote = FALSE,row.names = FALSE,col.names = TRUE,sep='\t')

print('origination')
print(cor(data_environment[,'competition'],data_environment[,'p_orig']))
print(cor(data_environment[,'diffHYP'],data_environment[,'p_orig']))
print('peak')
print(cor(data_environment[,'competition'],data_environment[,'p_peak']))
print(cor(data_environment[,'diffHYP'],data_environment[,'p_peak']))
print('extinction')
print(cor(data_environment[,'competition'],data_environment[,'p_ext']))
print(cor(data_environment[,'diffHYP'],data_environment[,'p_ext']))
print('com-env')
print(cor(data_environment[,'competition'],data_environment[,'diffHYP']))
print('orig-ext')
print(cor(data_environment[,'p_orig'],data_environment[,'p_ext']))

#print('--- normalized')
#print('origination')
#print(cor(data_environment[,'competition_perTIME'],data_environment[,'p_orig_perTIME']))
#print(cor(data_environment[,'diffHYP_perTIME'],data_environment[,'p_orig_perTIME']))
#print('peak')
#print(cor(data_environment[,'competition_perTIME'],data_environment[,'p_peak_perTIME']))
#print(cor(data_environment[,'diffHYP_perTIME'],data_environment[,'p_peak_perTIME']))
#print('extinction')
#print(cor(data_environment[,'competition_perTIME'],data_environment[,'p_ext_perTIME']))
#print(cor(data_environment[,'diffHYP_perTIME'],data_environment[,'p_ext_perTIME']))

print(dim(data_environment))