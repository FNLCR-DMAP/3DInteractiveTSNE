library(shiny)
library(shinyjs)
library(plotly)
library(DT)
library(readxl)

source("./matrix_functions.R") # projectVertex, xformMatrix, generate_random_sample_data
source("./UI_functions.R") # get_fluid_page, get_server

df = generate_random_sample_data(300) # takes total number of points as an argument

ui <- get_fluid_page()

shinyApp(ui, get_server)
