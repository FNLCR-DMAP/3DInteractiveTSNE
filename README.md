# 3DInteractiveTSNE

## Background/Objective
The 3D TSNE application helps users in identifying clusters that exist in their data(eg single cell RNASeq). Previously a version of this application was hosted in NIDAP using Slate but was only able to display 10k data points. 
For many single cell analysis, the studies contains much larger number of cell ~100K and increasing. Rare cell populations might not be detected if the data is downsampled. The goal of this Shiny application is to be able to display over 100K data points.
