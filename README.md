# LGBTI
This repository hosts the work of a thesis project using FRA data on the LGBTI population(s) of European countries.
The repository contains the following:
1) Under the folder "scripts" there are 3 folders, "data_manipulation","data_analysis" and "generate_ml_obects". 
2) In the folder "data_manipulation" you find all scripts for preprocessing the dataset before running models and analysis.
3) In the folder "generate_ml_objects" you find the scripts for generating the output of running the ML models. This includes the model object itself and the train/test datasets.
4) In the folder "data_analysis" you will find the scripts for generating the results of the 2-way pdp:s and the Shapper value analysis. In this folder there is also a script for an analysis of independence for all combinations of pairs of categorical predictors using  chi-square tests. Lastly you will also find scripts for variable importance analysis using the IML package.
5) In the folder "markdown_templates" you find the rmarkdown scripts which contain all the 1-way pdp and ice plots.You also find the rmarkdown scripts which contain all the 2-way pdp plots and Ale plots which were not included in the thesis.
