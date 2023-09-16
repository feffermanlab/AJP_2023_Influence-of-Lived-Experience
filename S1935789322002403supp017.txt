Before running any code, first decide where to put the files. Right now, the code should run with minimal edits if you place everything in: “C:\Data\Sensitization”. Within this directory, the zip should unpack structured subdirectories: “C:\Data\Sensitization\params\networks2” and “C:\Data\Sensitization\output\results4\COV_Starters”.  Please be aware, if you choose to extract the zip to a different directory, then all of the directories in each of the files that are asterisked below will need to be updated. For more details on the base model, including network generation, please consult its GitHub (https://github.com/matthewsilk/CoupledDynamicsNetworkPaper) and the associated papers:

Silk M. J., Carrignon S., Bentley R. A. and Fefferman N. H. 2021. Improving pandemic mitigation policies across communities through coupled dynamics of risk perception and infection. Proc. R. Soc. B.2882021083420210834. http://doi.org/10.1098/rspb.2021.0834

Silk, M.J., Carrignon, S., Bentley, R.A. et al. 2022. Observations and conversations: how communities learn about infection risk can impact the success of non-pharmaceutical interventions against epidemics. BMC Public Health 22, 13. https://doi.org/10.1186/s12889-021-12353-9

Due to space constraints, the zipped code does not contain the results files we used for our figure generation. If you would like to use our exact results for step 4, then either contact us for the results files (> 1 gb) or precisely follow step 3. 

Important code files for execution:
----------------
Step 1. Run several outbreaks with COVID-19 parameters

Open and execute: Ready_COVScripts.R*. This will call upon Ready_COVFunctions.R and run 6 iterations of a COVID-19 epidemic outbreak. Three replicates, each, with the reassurance dynamics reversed between two subgroups (see COV_mod.csv for parameters details or to modify our defaults).
----------------
Step 2. Extract the subset of the population that underwent a hospitalization during the COVID-19 outbreak from the previous step.

Open and execute: Extract_Sensitives.R*. This will analyze the results from the prior steps and save the node IDs of hospitalized entries for each of the 6 runs in the previous step. These become prior identification of (de)sensitization for the next step. 
----------------
Step 3. Simulate an outbreak of measles in the same populations as in step 1.

Open and execute: Measles_Sens_Scripts.R*. This will called upon Measles_Sens_Functions.R to run a measles outbreak, calling up the priors to identify which individuals are (de)sensitized to concern-generating processes (you can change the parameter values by altering Meas_Params_new.csv). Note that, if you want to reproduce the precise values from our results, then you should divide the runs into 6 independent events. We parallelized execution, by running 1/6th of the iterations in 6 separate instances of R (so, we changed line 35 to “md in” 1:126, 127:252, 253:378, 379:504, 505:630, 631:756). If you decide to run all the runs in a single window (md in 1:756), then this will result in a qualitatively similar, but not quantitatively identical, set of results. 
----------------
Step 4. Visualize

Open and execute: Quick_Analysis.R*. This should read the results files generated from the prior step and create several figures, including what we presented in the manuscript. 


