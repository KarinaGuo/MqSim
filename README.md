Simulating the population of Melaleuca quinquenervia in light of Myrtle rust.

Files:


├── data_sim_2.R - stable version of the simulation, does not have intervention \
├── data_sim_maladaptation.R - altered data_sim_2.R to include maladaptation (two populations running in parallel with different MR impacts) \
├── lifestage_MR_res.R - reads in populations grabbed from simulation runs to plot distribution of phenotypes at timepoints \
├── data_sim_3.R - stable version of the simulation with intervention \
├── intervention_plots.R - plots results after different intervention runs, uses output saved from data_sim_3.R \
├── empirical_recruitment.R - plotting distribution of recruited indivs MR from WGS \
├── pop_init_Cattai.R - creating an initial population using Cattai planted individuals \
 \
├── Age imp val.docx - Results after runs \
├── Mquin_samples_pheno.csv - Input for empirical_recruitment.R \
├── configurations - input for data_sim_2.R  \
 \
├── Intervention \
   └── configurations_int - input for data_sim_3.R \
   └── *.csv - output from simulations used for plotting in intervention_plots.R \
├── Functions \
   └── disturbance_functions.R \
   └── mortality_functions.R - initial mortality function when using base MR status \
   └── mortality_functions_MRintro.R - mortality function invoked when timepoint > MR introduction timepoint \
   └── recruitment_functions_2.R \
   └── recruitment_functions_maladaptation.R \
├── Maladaptation \
   └── configurations_maladaptation - input for data_sim_maladaptaiton.R \
