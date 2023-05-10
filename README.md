# Specification-Curve-Analysis-ANES2016
This is a code repository to conduct Specification Curve Analysis in R based on the ANES 2016 dataset. 

Original Specification Curve Analysis paper preprint: Specification Curve: Descriptive and Inferential Statistics on All Reasonable Specifications(https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2694998) 
Original Specification Curve Analysis paper published: Simonsohn, U., Simmons, J. P., & Nelson, L. D. (2020). Specification curve analysis. Nature Human Behaviour, 4(11), 1208-1214.
A tutorial: https://dcosme.github.io/specification-curves/SCA_tutorial_inferential_presentation#3

# This Repo: 
  To-be updated.  Summary: For this project, I helped another graduate student conduct a Specification Curve Analysis (SCA) to predict votes in the 2016 presdidential election, support or endorsement of authoritarian attitudes/ideas, support for democracy, and endorsement of different childrearing choices from social and news media variables while accounting for personality variables and several demographics. In total there were more than 57,000 models executed.  Additionally, all the models in this implementation of SCA were cross-validated using a 10-folds with 10 repeats, bringing the total to over 5.7 million models. 

Note: While the script will run succesffully, I would not recommend this approach. I wrote it quickly and early in my graduate studies.  The script is very 'fragile' in the sense that it is designed in a way that it must run to completion without error to provide results. 

#Included files: 
1. SCA fitting procedures.R 
	- This is a script that will execute all of the model fitting and results storage.
2. ggplots for SCA.R
	- This is the 'master' script for plotting results. It depends on 'Graphy.function' and 'plot.mcplot.function' 

3. Graphy.function.R
	- This function does some data wrangling/transformation for the plots.  It operates on our results in "Phase_1_results_7200_Specifications_anes2016_data".

4. Ploty.mcplot.function.R
	- This function builds the plots.  

5. Anes_phase_1_results_final_no_supdem_mean and Anes_phase_1_results_final_supdem_meanonly
	- These are the results of 7200 planned model fits from our first phase. At some point there was an additional DV that may have been missed but was in the plan.  I just left it in from a previous draft. So the total is actually 7920 in this file.  
	- Following an update where supdem variables were averaged, there are now 9504 models for each of the 6 subsamples. This new number was made by splitting the political orientation variable off of the demographics variable.  So from 5 to 6 covariate sets.  And by including null models for comparison where we have no IV in the equation but we do have the various covariate sets. 

In total, the phase 1 executed 9504x10x10x6+9504 = 5,711,904 models 
