# Applied Labor Economics

This is the repository for the ENSAE Applied Labor Economics course, year 2023-2024. The course was taught by Pr. Roland RATHELOT and the project is a collaboration between My-Linh NGUYEN and Mael LAOUFI, using data from the CASD.

## Files
R files are adapted from the Matlab codes from Sorkin's 2017 paper (linked [here](https://drive.google.com/file/d/1nbreNXXKAA1cjs70-fWYGMMtZkRiJsSe/view)). The files and functions have the same names, however, our model_master is a script that runs the estimation we carried out, while Sorkin's is a function called in another script. More details on each individual code and on the relevant variables can be found in "memento.txt".
Three R files were added to Sorkin's code:
- "exploration.R" explores the dataset at hand.
- "dirty_AKM.R" prepares the dataset on which we performed the AKM regression in Stata.
- "sauvegarde_akm.R" gives proper layout to the output of that estimation, and saves it.
- "final_computations.R" carries out the decomposition of the gender gap in firm premium and estimates a few key statistics included in our report.

Do files (STATA):
- "package.do" installs packages needed.
- "data_clean.do" cleans data, applies sample restrictions, identify dominant firms and transitions. 
- "akm.do" runs AKM models and obtains firm premiums for each gender.
- "figures.do" generates figures attached in the report. 
