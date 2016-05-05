# Capstone Project - Exploring Task based fMRI Data for Analysis with Limited Data

## Synopsis of Project

This project uses R to explore an fMRI data set and analyzes the potential of using simple logistic models over different subsets of the temporal data to predict regions of relative activation to a given task. The Exploratory Data Analysis (EDA) portion analyzes relationships between a group activation map and functions of the temporal data for one subject. The modeling portion compares a predictive logistic model over all the time series points, a model over select runs of the time series, and a model over PCA components, to asses the robustness of the model to different subsets of the data. 

## Instructions for Running Analysis and Generating HTML Report

The two sections of the analysis (EDA, and modeling portions) are divided into two Markdown files (EDA_portion.Rmd, modeling_portion.Rmd), and each seperate report can be generated by using knit in R.

knit(EDA_portion.Rmd)
knit(modeling_portion.Rmd)

The necessary R libraries are: caTools, ROCR, glmnet, caret, and oro.nifti.

You may need to change the file and directory names for the preprocessed nifti images in the code, such as the raw fMRI 4D data, the group activation cluster map, and the brain mask.
 
