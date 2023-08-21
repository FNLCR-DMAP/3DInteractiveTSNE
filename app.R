library(shiny)
library(shinyjs)
library(plotly)
library(DT)
library(auth0)
library(httr)

source("./UI_functions.R") # get_fluid_page, get_server
source("./matrix_functions.R") # projectVertex, xformMatrix, generate_random_sample_data

js_code <- paste(readLines("./js_code.js"), collapse="\n")
markerShape = c('circle', 'circle-open', 'square', 'square-open', 'diamond', 'diamond-open', 'cross', 'x')
ui <-  fluidPage(
  useShinyjs(),
  extendShinyjs(text = js_code, functions = c('plot3d')),
  tags$head(
    tags$script(src = "https://cdn.plot.ly/plotly-latest.min.js")
  ),
  titlePanel("T-SNE 3D Scatterplot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_col", label = "X-Axis", choices = NULL),
      selectInput("y_col", label = "Y-Axis", choices = NULL),
      selectInput("z_col", label = "Z-Axis", choices = NULL),
      selectInput("indicator_col", "Indicator", choices = NULL),
      actionButton('show', 'Generate Plot'),
      br(),
      br(),
      checkboxGroupInput("indicator_values_filter", label = "Features", choices = NULL)
    ),
    mainPanel(
      fluidRow(
        column (4, sliderInput("marker", label = 'Marker Size', min = 1, max = 10, value = 3)),
        column (4, selectInput("shape", label = "Marker Shape", choices = markerShape))
      ),
      tabsetPanel(
        tabPanel("3D Plot",
                 br(),
                 htmlOutput("text"),
                 actionButton('getParam', 'Save View to Project'),
                 actionButton('project2D', "Project to 2D"),
                 tags$body(
                   tags$div(id='mydiv', class = 'myplot')
                 )),
        tabPanel("2D Lasso",
                 br(),
                 textInput("points_names", label = "Name of Selected Points"),
                 actionButton("add_to_list", label = "Add Points to Export List"),
                 plotlyOutput("plot2d")),
        tabPanel("View Export Dataset",
                 br(),
                 fluidRow(
                   column (4, actionButton("clear", label = 'Clear Export List')),
                   column (4, actionButton("exportNidap", label = "Export to NIDAP"))
                 ),
                 br(),
                 br(),
                 DTOutput("Export_Dataset"))
      )
    )
  )
)

server <- function (input, output, session) {
  auth_token <- session$userData$auth0_credentials$access_token
  # rid = "ri.foundry.main.dataset.85416a76-46aa-4260-bdc7-3cd611ca3c8a"
  # fileName = "tSNE3d_v01_test_data_140K.csv"
  # url2 <- paste0("https://nidap.nih.gov/api/v1/datasets/",rid,"/files/",fileName,"/content")
  # response <- GET(url2, httr::add_headers(Authorization = paste("Bearer", auth_token)))
  # raw = content(response, as="text")
  # df = read.csv(text = raw)
  # df = data.frame(df)
  # df = df %>% filter(!is.na(pk))
  
  auth_token <- session$userData$auth0_credentials$access_token
  rid = "ri.foundry.main.dataset.5c075c3b-8195-48ca-aac1-a556f4f96403"
  fileName = "spark_part-00000-e7447c17-60bc-442d-ba6d-8c2126c12be4-c000.snappy.parquet"
  url2 <- paste0("https://nidap.nih.gov/api/v1/datasets/",rid,"/files/",fileName,"/content")
  response <- GET(url2, httr::add_headers(Authorization = paste("Bearer", auth_token)))
  print(status_code(response))
  
  df = generate_random_sample_data(50000) # takes total number of points as an argument
  
  shinyjs::disable("add_to_list")
  shinyjs::disable("getParam")
  shinyjs::disable("project2D")
  print("hello from GIT")
  print("Hello from rstudio")
  mydata <- reactive({
    return(df)
  })
  
  columnType <- reactive({
    sapply(mydata(),class)
  })
  
  pkDataset <- reactiveValues(
    data = data.frame(),
  )
  
  exportDataset <- reactiveValues(
    data = data.frame()
  )
  
  projectedData <- reactiveValues(
    data = data.frame(),
  )
  
  factor_value <- reactiveVal(TRUE)
  
  output$text <- renderText({
    paste("<b>Please save View First before Projecting to 2D<br>", "<br>", "</b>")
  })
  
  #Update Feature Selection and Check if indicator column contains factors.
  #Factors includes columns that have type character or factor
  observeEvent(input$indicator_col, {
    if (!is.null(columnType()) && input$indicator_col %in% names(columnType())) {
      df = mydata()
      unique_values = unique(df[[input$indicator_col]] %>% sort())
      if (columnType()[input$indicator_col] == "character" | columnType()[input$indicator_col] == "factor"){
        factor_value(TRUE)
        updateCheckboxGroupInput(session, inputId = "indicator_values_filter",
                                 choices = unique_values,
                                 selected = unique_values)
      }
      else {
        factor_value(FALSE)
        updateCheckboxGroupInput(session, inputId = "indicator_values_filter",
                                 choices = "Not a Factor",
                                 selected = "Not a Factor")
      }      
    }
  })
  
  #Disable Generate button if no features are chosen
  observe({
    if (length(input$indicator_values_filter) > 0) {
      shinyjs::enable("show")
    }
    else {
      shinyjs::disable("show")
    }
  })
  
  filterData <- eventReactive(input$indicator_values_filter,{
    df = mydata()
    if (factor_value()) {
      df <- df %>% filter(get(input$indicator_col) %in% input$indicator_values_filter)
    }
    return(df)
  })
  
  observe({
    exportDataset$data = data.frame()
    df = mydata()
    updateSelectInput(session, "x_col", choices = colnames(df), selected = colnames(df[1]))
    updateSelectInput(session, "y_col", choices = colnames(df), selected = colnames(df[2]))
    updateSelectInput(session, "z_col", choices = colnames(df), selected = colnames(df[3]))
    updateSelectInput(session, "indicator_col", choices = colnames(df), selected = colnames(df[5]))
  })
  
  observeEvent(input$show,{
    showModal(modalDialog(
      paste0("Plotting..."),
      easyClose = TRUE,
      footer = NULL
    ))
    # print(nrow(exportDataset$data))
    # print(nrow(filterData()))
    ind = filterData()[[input$indicator_col]]
    x = filterData()[[input$x_col]]
    y = filterData()[[input$y_col]]
    z = filterData()[[input$z_col]]
    js$plot3d('mydiv', x, y, z, ind, factor_value(), input$marker, input$shape)
    removeModal()
    shinyjs::enable("getParam")
  })
  
  observeEvent(input$getParam, {
    runjs("
      var plotElement = document.getElementById('mydiv');
      var camera = plotElement._fullLayout.scene._scene.glplot.cameraParams
      var scene = plotElement._fullLayout.scene._scene
      console.log(plotElement._fullLayout.scene._scene.glplot)
      console.log(camera)
      console.log(scene)
      Shiny.onInputChange('view',camera.view);
      Shiny.onInputChange('model',camera.model);
      Shiny.onInputChange('projection',camera.projection);
      Shiny.onInputChange('dataScale',scene.dataScale)
    ")
    showModal(modalDialog(
      paste0("Projection View Saved"),
      easyClose = TRUE,
      footer = NULL
    ))
    shinyjs::enable("project2D")
  })
  
  observeEvent(input$project2D, {
    
    showModal(modalDialog(
      paste0("Please wait, projecting to 2D"),
      easyClose = TRUE,
      footer = NULL
    ))
    
    x2d = c()
    y2d = c()
    pk = c()
    indicator = c()
    
    projectedData$data = data.frame()
    # print(nrow(filterData()))
    ind = filterData()[[input$indicator_col]]
    x = filterData()[[input$x_col]]
    y = filterData()[[input$y_col]]
    z = filterData()[[input$z_col]]
    pkCol = filterData()[['pk']]
    
    #progress bar
    withProgress(message = 'Transforming Points',
                 value = 0,{
      for (ai in 1: length(pkCol)) {
        vp = c(x[ai]*input$dataScale[1],y[ai]*input$dataScale[2], z[ai]*input$dataScale[3])
        transformed = projectVertex(vp, input$model, input$view, input$projection, c(1,1))
                     
        x2d[ai] = transformed[1]
        y2d[ai] = transformed[2]
        indicator[ai] = ind[ai]
        pk[ai] = pkCol[ai]
        
        incProgress(amount = 1/length(pkCol), detail = paste(ai, "of", length(pkCol)))
      } 
    })
 
    transformed1 = data.frame(x = x2d,y = y2d,indicator,pk)
    projectedData$data <- rbind(projectedData$data, transformed1)
    
    projectedData$data <- projectedData$data[projectedData$data['x'] >= 0 & projectedData$data['x'] <= 1,]
    projectedData$data <- projectedData$data[projectedData$data['y'] >= 0 & projectedData$data['y'] <= 1,]
    
    output$plot2d <- renderPlotly({
      fig2d <- plot_ly(source = "2dplot",
                       width = 800,
                       height = 600) %>%
        add_markers(data = projectedData$data,
                    x = as.formula(paste0("~","x")), 
                    y = as.formula(paste0("~","y")),
                    type = "scatter",
                    mode = "markers",
                    color = as.formula(paste0("~",'indicator')),
                    text = paste(projectedData$data[['indicator']]),
                    hoverinfo = "text")
      
      fig2d <- fig2d %>% layout(
        xaxis = list(range=c(0,1)),
        yaxis = list(range=c(0,1))
      )
      
      fig2d <- fig2d %>% layout(
        dragmode = "lasso"
      )
    })
    showModal(modalDialog(
      paste0("Projecting to 2D has been completed"),
      easyClose = TRUE,
      footer = NULL
    ))
    
    shinyjs::disable("project2D")
  })
  
  observeEvent({event_data("plotly_selected", source = "2dplot"); input$points_names}, {
    if (length(event_data("plotly_selected", source = "2dplot")) & input$points_names != "") {
      shinyjs::enable("add_to_list")
    }
    else{
      shinyjs::disable("add_to_list")
    }
  })
  
  observeEvent(input$add_to_list, {
    
    df = mydata()
    
    pkDataset$data <- data.frame()
    selected_points <- event_data("plotly_selected", source = "2dplot")
    
    indicator_col_values = unique(projectedData$data[['indicator']]) %>% sort
    # print(indicator_col_values)
    
    num_selected_points <- nrow(selected_points)
    
    if (factor_value()) {
      for (i in 1:num_selected_points) {
        curveNum = selected_points[i,]$curveNumber
        pointNum = selected_points[i,]$pointNumber
        filtered_data = filter(projectedData$data, get('indicator') == indicator_col_values[curveNum+1])
        filtered_data = filtered_data[pointNum+1,]
        pkDataset$data = rbind(pkDataset$data, filtered_data['pk'])
      }
    }
    else {
      filtered_data = projectedData$data[selected_points$pointNumber+1,]
      pkDataset$data = rbind(pkDataset$data, filtered_data['pk'])
    }
    # print(pkDataset$data)
    pk = pkDataset$data
    exportData = df[df$pk %in% pk$pk,]
    exportData$InterestPoint = input$points_names
    exportDataset$data = rbind(exportDataset$data, exportData)
    # print(exportDataset$data)
  })
  
  observeEvent(input$clear, {
    exportDataset$data = data.frame()
  })
  
  output$Export_Dataset <- renderDT(server = FALSE,{
    DT::datatable(exportDataset$data,
                  rownames = FALSE,
                  extensions = 'Buttons',
                  options = list(pageLength = 10,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel')))
  })
  observeEvent(input$exportNidap, {
    rid = "ri.foundry.main.dataset.1ef74b91-6660-4be5-9080-1267b1f80f50"
    csv_content = capture.output(write.csv(exportDataset$data, row.names = FALSE))
    url = paste0("https://nidap.nih.gov/api/v1/datasets/",rid,"/files:upload")
    response <- POST(url, 
                     httr::add_headers(Authorization = paste("Bearer", auth_token)), 
                     content_type("application/octet-stream"), 
                     body = csv_content)
  })
}

shinyAppAuth0(ui = ui, server = server)
