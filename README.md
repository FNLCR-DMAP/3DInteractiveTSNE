# 3DInteractiveTSNE

## Background/Objective
The 3D TSNE application helps users in identifying clusters that exist in their data(eg single cell RNASeq). Previously a version of this application was hosted in NIDAP using Slate but was only able to display 10k data points. 
For many single cell analysis, the studies contains much larger number of cell ~100K and increasing. Rare cell populations might not be detected if the data is downsampled. The goal of this Shiny application is to be able to display over 100K data points.

## Problems Encounter
To project a 3D view onto 2D we required 3 matrices(model, projection, view). Using plotly.js allows us to get those parameters, while using plotly library in R does not. As a work around, the plotly.js script was read into the shiny app and used to create the 3D scatter plot. JS code was then run was get the parameters. After all the points are transformed, the plotly library in R was then used to plot 2D scatter plot and to lasso points.

## Code Walkthrough
- There are 2 functions in this app that are used to project 3D onto 2D, they are:
  - xformMatrix
  - projectVertex
 
Together they project each x,y,z datapoint onto x, y plane point by point. So depending on how many datapoints are being plotting, this transformation from 3D to 2D could take a while.
