# 2016 10 31 I.Zliobaite

all_plots <- c('random','memory','competition','environment','memory_competition')

for (which_plot in all_plots){
  
  if (which_plot=='random'){
    input_file <- 'generated_data/data_random.csv'
    plot_name <- 'plots/fig_random.pdf'
    ttl <- "Plain random walk"
    aa <- '(a)'
  }else{
    if (which_plot=='memory'){
      input_file <- 'generated_data/data_random_memory.csv'
      plot_name <- 'plots/fig_random_memory.pdf'
      ttl <- "Random walk with memory (correlated random walk)"
      aa <- '(b)'
    }else{
      if (which_plot=='competition'){
        input_file <- 'generated_data/data_random_competition.csv'
        plot_name <- 'plots/fig_random_competition.pdf'
        ttl <- "Random walk no memory with competition"
        aa <- 'na'
      }else{
        if (which_plot=='memory_competition'){
          input_file <- 'generated_data/data_random_memory_competition.csv'
          plot_name <- 'plots/fig_random_memory_competition.pdf'
          ttl <- "Random walk with memory and competition"
          aa <- '(c)'
        }else{
          input_file <- 'generated_data/data_random_environment.csv'
          plot_name <- 'plots/fig_random_environment.pdf'
          ttl <- "Random walk with environmental change"
          aa <- '(d)'
        }
      }
    }
  }
  
  n_plots <- 8
  
  min_abu <- 80
  
  plot_y_max <- 40
  
  plot_x_max <- 100
  
  cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                  "#CC79A7", "#F0E442")
  
  data_now <- read.csv(input_file, header = FALSE,sep = '\t')
  p <- dim(data_now)[2]
  ind <- which(data_now[,p]>0)
  data_now <- data_now[ind,c(1:(p-1))]
  
  ind_finished <- which(data_now[,(p-1)]==0)
  data_now <- data_now[ind_finished,]
  
  sm <- apply(data_now,1,sum)
  ind_large <- which(sm>=min_abu)
  data_now <- data_now[ind_large,]
  
  set.seed(1981)
  
  perm <- sample(length(ind_large))
  ind_select <- perm[1:n_plots]
  
  data_now <- data_now[ind_select,]
  
  pdf(plot_name, height = 3, width = 7)
  plot(NA,NA,main=ttl,xlab="time units", ylab="population size",xlim=c(1, plot_x_max),ylim=c(0, plot_y_max))
  legend("topleft", aa, bty = 'n')
  for (sk in 1:n_plots){
    ind <- which(data_now[sk,]>0)
    points(ind,data_now[sk,ind],type='l', lw =5, col = cbbPalette[sk])
  }
  dev.off()
  
}
