# Red Queen's experiments #

This is to accompany a research paper "Reconciling taxon senescence with the Red Queen’s hypothesis" by I. Zliobaite, M. Fortelius and N. Chr. Stenseth, published in Nature in November 2017.	doi:10.1038/nature24656
	The code contains random seeds to reproduce random walk examples given in the paper, as well computations of proxies and correlations over the fossil data.
I. Zliobaite is responsible for this code. The code as is contains more analyses than reported in the final published version, we think that for reflecting the process of this research it is better to make everything available.The code comes as is with MIT license. 

## Random walk trajectories ##

This code reproduces examples given in Figure 2. The main purpose of this experiment is to provide ilustrative examples to accompany the theorey. The parameters for producing these examples have been chosen arbitrarily. 

The scripts are contained in the folder 

	1_random_walk_trajectories,
	
and the plots can be plotted by 
	
	plot_random_walk.R

and are saved in the folder 

	1_random_walk_trajectories/plots.	

There is a separate script for each panel in the figure. The scripts include random seeds to reproduce the exact plots used in the paper. Some scripts generate more datasets in total than others, there is no particular content reason for that but historical reason how we proceeded in time with the analysis. 

## Baseline model ##

This code reproduces the statistics reported in Table 1. The scripts are contained in the folder 

	2_baseline_model.
	
There is a separate script for each number of time units to compare with. The initial number is larger because we drop 30 first points as a warm up phase and then the first two and the last two time units to match what we do with fossil data. The random seeds allow reproducing the exact runs that we report. The resulting correlations and standard deviations are printed out. The scripts are:

	run_random_walk_baseline_50.R
	run_random_walk_baseline_18.R
	run_random_walk_baseline_13.R
	run_random_walk_baseline_6.R

## Fossil data analysis ##

For America set

	do_EU <- FALSE
	do_NAm <- TRUE,

for Europe set

	do_EU <- TRUE
	do_NAm <- FALSE,
	
for Africa set

	do_EU <- FALSE
	do_NAm <- FALSE.
	
For correlations run

	run3_competition_occurences.R
	
	
	
