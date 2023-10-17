# 3DInteractiveTSNE

## Background/Objective
The 3D TSNE application helps users in identifying clusters that exist in their data(eg single cell RNASeq). Previously a version of this application was hosted in NIDAP using Slate but was only able to display 10k data points. 
For many single cell analysis, the studies contains much larger number of cell ~100K and increasing. Rare cell populations might not be detected if the data is downsampled. The goal of this Shiny application is to be able to display over 100K data points.

## Deliverable(s)
- Users are able to plot 100K+ points
- Funtional R- shiny App to plot points and enable lasso capability
- Import and Export data to NIDAP

## Problems Encounter
- To project a 3D view onto 2D we required 3 matrices(model, projection, view). Using plotly.js allows us to get those parameters, while using plotly library in R does not. As a work around, the plotly.js script was read into the shiny app and used to create the 3D scatter plot. JS code was then run was get the parameters. After all the points are transformed, the plotly library in R was then used to plot 2D scatter plot and to lasso points.
- Since the 3D plot is using plotly.js and the 2d is back to using plotly library in R, there are color mismatch between the 2 plots
- To get the projection from 3D view onto 2D view, the user would have to click 2 seperate button instead of 1. the first click is to get the parameters, while the second click is to start projecting onto 2D.

## Code Walkthrough
matrix_functions.R File contains 2 functions that this app uses to project 3D onto 2D, they are:
- xformMatrix
- projectVertex
 
Together they project each x,y,z datapoint onto x, y plane, point by point. So depending on how many datapoints are being ploted, this transformation from 3D to 2D could take a while. A progress bar has since been added to track how far along the transformation is.<br>

js_code.js file contains the js script that is used to plot the 3D scatter plot using plotly.js with shinyjs.

## Test Datasets Used
These were the datasets used during the testing phase, they are all currently hosted in NIDAP.<br>
- [3DtSNE_Coords](https://nidap.nih.gov/workspace/data-integration/dataset/preview/ri.foundry.main.dataset.2db34086-b037-4c8f-8d81-aaf5fa53e2ac/master)
- [tSNE3d_v01_test_data_140K](https://nidap.nih.gov/workspace/data-integration/dataset/preview/ri.foundry.main.dataset.85416a76-46aa-4260-bdc7-3cd611ca3c8a/master)
- [tSNE3d_v01_test_data_9K](https://nidap.nih.gov/workspace/data-integration/dataset/preview/ri.foundry.main.dataset.cc20947e-23ea-4e0e-a3eb-e6badeb94221/master)

## Appendix
1) Move App away from Slate and into R Shiny
    - create 3D scatter plot using Plotly with test datasets
2) Add ability to customize marker size and shape as well as select annotation column and factors to includes
3) Use Plotly.js to create 3d plot
    - create simple 3d plot
    - create plot using sample dataset
    - update axis variables
    - update marker size and shape
    - update color
4) Project 3d view onto 2d plane
    - change xformMatrix and projectVertex function code from javascript into R
    - get model, view, and projection matrix
    - generate 2d plot with transformed points
5) Figure out lasso in 2d plot and export points
6) Optimize for 100k+ points for lasso
7) Add progress bar to track the current progress of projecting to 2d
    - testing phase

noop
