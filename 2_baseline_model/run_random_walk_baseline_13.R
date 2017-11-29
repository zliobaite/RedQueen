# 2016 10 09 I.Zliobaite
# updated 2016 10 23, 2016 11 28

out_file <- 'generated_data_last_run/data_random_13.csv'

n_loops <- 10000
n_times <- 47
warm_up_times <- 30
n_localities <- 20
m_taxa_origin <- 7
sd_origin <- m_taxa_origin/3
m_localities_step <- 3
sd_step <- m_localities_step/3
p_up <- 0.5
param_min_times_occurence <- 1

erange <- c((warm_up_times+1):n_times)

set.seed(1981)

results <- c()

for (cik in 1:n_loops){
  #print(cik)
  
  competition <- c()
  environment <- c()
  
  data_abundance <- matrix(0, (m_taxa_origin*n_times*3), n_times)
  hypsodonty <- rep(0,(m_taxa_origin*n_times*3))
    
  #how many will originate? 
  n_orig <- round(rnorm(1,m_taxa_origin,sd_origin))
  if (n_orig>0){
    for (sk in 1:n_orig){
      data_abundance[sk,1] <- round(rnorm(1,m_localities_step,sd_step))
      # hypsodonty
      hypsodonty[sk] <- ceiling(runif(1, 0, 3))
    }
  }
  last_originated <- n_orig
  
  
  for (time_now in 2:n_times){
    if (last_originated>0){
      for (taxa_now in 1:last_originated){
        #print(paste('tm',time_now,'tx',taxa_now))
        # if is alive then see what's next
        no_loc_before <- data_abundance[taxa_now,(time_now-1)]
        if (no_loc_before>0){
          #up or down
          updown <- runif(1,0,1)<=p_up
          if (updown){#up
            data_abundance[taxa_now,time_now] <- no_loc_before + round(rnorm(1,m_localities_step,sd_step))
          }else{#decline
            data_abundance[taxa_now,time_now] <- max((no_loc_before - round(rnorm(1,m_localities_step,sd_step))),0)
          }
        }
      }  
    }
    #origination
    n_orig <- round(rnorm(1,m_taxa_origin,sd_origin))
    if (n_orig>0){
      for (sk in (last_originated+1):(last_originated+n_orig)){
        data_abundance[sk,time_now] <- round(rnorm(1,m_localities_step,sd_step))
        # hypsodonty
        hypsodonty[sk] <- ceiling(runif(1, 0, 3))
      }
    }
    last_originated <- last_originated + n_orig
  }

  data_abundance <- data_abundance[,erange]
  
  occ_extinction <- data_abundance*0
  occ_origination <- data_abundance*0
  occ_peak <- data_abundance*0
  occ_alive <- data_abundance*0
  occ_hyp <- data_abundance*0
  
  for (sk in 1:last_originated){
    ind <- which(data_abundance[sk,]>0)
    if (length(ind)>=param_min_times_occurence){
      occ_extinction[sk,ind[length(ind)]] <- 1
      if (ind[1]>1){
        occ_origination[sk,(ind[1]-1)] <- 1    #will originate
      }
      occ_alive[sk,ind] <- 1
      occ_hyp[sk,ind] <- hypsodonty[sk]
      abu_normed <- data_abundance[sk,]/n_localities
      ind <- which(abu_normed==max(abu_normed))
      occ_peak[sk,ind[length(ind)]] <- 1
    }else{
      data_abundance[sk,] <- 0
    }
  }
  
  no_alive <- apply(occ_alive,2,sum)
  no_last <- apply(occ_extinction,2,sum)
  no_first <- apply(occ_origination,2,sum)
  no_peak <- apply(occ_peak,2,sum)
  sum_hyp <- apply(occ_hyp,2,sum)
  sum_occur <- apply(data_abundance,2,sum)
  competition <- sum_occur/n_localities
  
  #remove first two and last two because of min occurence requirement
  rrange <- c(3:(length(no_alive)-2))
  
  p_ext <- no_last[rrange]/no_alive[rrange]
  p_orig <- no_first[rrange]/(no_alive[rrange] - no_last[rrange] + no_first[rrange])
  #print((no_alive[rrange] - no_last[rrange] + no_first[rrange]))
  #print(no_alive[rrange])
  p_peak <- no_peak[rrange]/no_alive[rrange]
  competition <- competition[rrange]
  environment_all <- sum_hyp/no_alive
  environment <- abs(environment_all[rrange+1] - environment_all[rrange])
    
  results <- rbind(results,cbind(cor(competition,p_orig),cor(environment,p_orig),cor(competition,p_peak),cor(environment,p_peak),cor(competition,p_ext),cor(environment,p_ext),cor(competition,environment),cor(p_orig,p_ext)))
}

print('no time units')
print(length(rrange))

colnames(results) <- c('comp-orig','env-orig','comp-peak','env-peak','comp-ext','env-ext','comp-env','orig-ext')

mn_results <- round(apply(results,2,mean),digits = 2)
sd_results <- round(apply(results,2,sd),digits= 2)

print(rbind(mn_results,sd_results))

data_abundance <- cbind(data_abundance,hypsodonty)
write.table(data_abundance,file = out_file,quote = FALSE,row.names = FALSE,col.names = FALSE,sep='\t')
