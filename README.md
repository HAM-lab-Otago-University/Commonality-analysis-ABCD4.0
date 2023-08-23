# Commonality-analysis-ABCD4.0

 It is assumed that the readers of the codes have ABCD data tables from the internet. So all of the R codes start by processing the raw tables downloaded from ABCD webpage




1. process the five cognitive tasks and compute gfactor. The code can be found in computing gfactor.

2. Modelling the brain scan data. Starts with cleaning the raw data tables with quality control (codes in the folder called cleaning raw table brain scan).  Then all the scan features are processed in the script called scan_feature_processing for the elastic net fitting. Fitting the elastic net is done on super computer called Nesi and the codes are in brain_scan_modelling_nesi (gfactor_DTI_modelling is a separate one because there is some problems when fitting the DTI in Nest).

The predictive performance and feature importance of elastic net can be found in individual_brain_modalities_scatterplot and feature_importance_enet_brain_continuous_palettes respectively.

The folder with the name stacking_brain_scan_models has three files. random_forest_stacking_data_prep is the script to prepare the elastic net predictions to fit the stacking random forest model. random_forest_loaded_shap_plots is the feature importance plots of all the sets of brain features. stacking_random_forest_performance is the performance metrics of the random forest stacking models.

The model fitting and feature importance computation (Shapley values) are in the Nesi folder as those computation is done by Nesi.



3. modelling the mental health. All the codes are in the folder called psychopathology

4. modelling the social demographic lifestyle developmental. All the codes are in the folder called ses_developmental.

5. modelling the genetics that related to the cognition. All the codes are in the folder called genetics.

6. commonality analysis. The analyses used site as a random effects are in the file common_analysis_cross_sites_models_with_genetics_2.0, common_mixed_psy_brain, and common_mixed_psy_ses. The analysis uses all the fixed models and average over sites is in common_analysis_two_level_models.

scatterplot_predicted_real is a file with the scatterplots of all the brain, mental health, social demographic lifestyle developmental and genes. Also, this file has the table of metrics and three different sets of mental health features.

All the functions needed for the analysis are in the R_functions.
