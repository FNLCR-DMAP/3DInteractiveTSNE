library(shiny)
library(shinyjs)
library(plotly)
library(DT)
library(auth0)
library(httr)
library(jsonlite)
library(tools)
library(urltools)
#library(arrow)

source("./UI_functions.R") # get_fluid_page, get_server
source("./matrix_functions.R") # projectVertex, xformMatrix, generate_random_sample_data

print(Sys.getenv("redirect_uri"))
print(Sys.getenv("auth0_clientid"))

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
        column (4, selectInput("shape", label = "Marker Shape", choices = markerShape)),
        column (4, verbatimTextOutput("response"))
      ),
      tabsetPanel(
        tabPanel("3D Plot",
                 br(),
                 textOutput("debug_query_message"),
                 textOutput("debug_query_message_2"),
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
  observe({ 
    url_search_params <- parseQueryString(session$clientData$url_search)
    #print("url_search_params")
    #print(url_search_params)
    output$debug_query_message <- renderText(paste(url_search_params, sep = " | "))
  })
  shinyjs::logjs("hello from our server function")
  # trying out file system
  auth_token <- session$userData$auth0_credentials$access_token
  rid = "ri.foundry.main.dataset.556cfc74-1c10-4662-a4ed-04feb1c7b6b6"
  url2 <- paste0("https://nidap.nih.gov/api/v1/datasets/",rid,"/files")
  response <- GET(url2, httr::add_headers(Authorization = paste("Bearer", auth_token)))
  data_content = content(response, as="text")
  parsed_json = fromJSON(data_content)
  files = parsed_json$data$path
  files = files[!file_ext(files) %in% c("log", "")] #filter out log and spark success files
  
  # looping through file name
  if(FALSE){
    print("reading through files")
    df = data.frame()
    for (file in files) {
      #print(file_ext(file))
      file = url_encode(file)
      url3 <- paste0("https://nidap.nih.gov/api/v1/datasets/",rid,"/files/",file,"/content")
      response2 <- GET(url3, httr::add_headers(Authorization = paste("Bearer", auth_token)))
      if (file_ext(file) == "csv") {
        raw = content(response2, as="text")
        dataset = read.csv(text = raw)
        dataset = data.frame(dataset)
        df = rbind(df, dataset)
      }
      else if (file_ext(file) == "parquet") {
        raw = content(response2, as="raw")
        # dataset = read_parquet(raw)
        # dataset = data.frame(dataset)
        #print("reading parquet file")
        #print(file)
        #print(raw[1:100])
        dataset = generate_random_sample_data(10)
        dataset$name = file
        df = rbind(df, dataset)
      }
      else {
        dataset = generate_random_sample_data(100)
        dataset$name = "else"
        df = rbind(df, dataset)
      }
    }
    
    df = df %>% filter(!is.na(pk))
  
    # fileName = files[1]
    # print(fileName)
    # handling / in file name
    # fileName = url_encode(fileName)
    # print(fileName)
    # url3 = paste0("https://nidap.nih.gov/api/v1/datasets/",rid,"/files/",fileName,"/content")
    # response2 <- GET(url3, httr::add_headers(Authorization = paste("Bearer", auth_token)))
    # print(response2)
    # print("reading content here")
    # raw_data = content(response2, as="raw")
  }
  df = generate_random_sample_data(10000) # takes total number of points as an argument
  
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
                       height = 800) %>%
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
    print("exporting to nidap")
    rid = "ri.foundry.main.dataset.1ef74b91-6660-4be5-9080-1267b1f80f50"
    filePath = "tempFile_from_posit.csv"
    data_to_upload = exportDataset$data
    #data_to_upload =  data.frame(replicate(10,sample(0:10,10,rep=TRUE)))

    two_d_csv = capture.output(write.csv(data_to_upload, row.names = FALSE)) #list of lists
    character_list = paste(two_d_csv, collapse="\n")
    raw_char_array = charToRaw(character_list)
    print(class(raw_char_array))
    upload_url = paste0("https://nidap.nih.gov/api/v1/datasets/",rid,"/files:upload?filePath=",filePath)
    
    response <- POST(upload_url, 
                     content_type("application/octet-stream"),
                     httr::add_headers(Authorization = paste("Bearer", auth_token)),
                     body = raw_char_array)
   
    print(status_code(response))
    print(content(response))
  })
}

#taken from auth0.R for scope
has_auth_code <- function(params, state) {
  is.null(params$error) && !is.null(params$code) && params$state == state
}

auth0_server_verify <- function(session, app, api, state) {
  
  u_search <- session[["clientData"]]$url_search
  params <- shiny::parseQueryString(u_search)
  
  if (has_auth_code(params, state)) {
    cred <- httr::oauth2.0_access_token(api, app(redirect_uri), params$code)
    token <- httr::oauth2.0_token(
      app = app(redirect_uri), endpoint = api, cache = FALSE, credentials = cred,
      user_params = list(grant_type = "authorization_code"))
    
    userinfo_url <- sub("authorize", "userinfo", api$authorize)
    resp <- httr::RETRY(
      verb = "GET"
      , url = userinfo_url
      , httr::config(token = token)
      , times = 5
    )
    
    assign("auth0_credentials", token$credentials, envir = session$userData)
    assign("auth0_info", httr::content(resp, "parsed"), envir = session$userData)
  }
  
}
#auth0_state
#url_search_params <- parseQueryString(session$clientData$url_search)
my_auth0_ui <- function(ui, info) {
  print("my auth0 UI called")
  if (missing(info)) info <- auth0_info()
  function(req) {
    
    #shinyjs::useShinyjs()
    #shinyjs::logjs("hello from the myauth0ui function")
    print("shiny query string:")
    q_string <- shiny::parseQueryString(req$QUERY_STRING)
    print(q_string)
    if("inputRID" %in% names(q_string)){
      myGlobalQueryVars <- q_string
    }
    
    verify <- has_auth_code(shiny::parseQueryString(req$QUERY_STRING), info$state)
    print("auth0 ui verify")
    print(verify)
    
    if (!verify) {
      print("verify came back false") 
      if (grepl("error=unauthorized", req$QUERY_STRING)) {
        redirect <- sprintf("location.replace(\"%s\");", logout_url())
        shiny::tags$script(shiny::HTML(redirect))
      } 
      else {
        print("not unauthorized")
        
        params <- shiny::parseQueryString(req$QUERY_STRING)
        params$code <- NULL
        params$state <- NULL
        
        query <- paste0("/?", paste(
          mapply(paste, names(params), params, MoreArgs = list(sep = "=")),
          collapse = "&"))
        if (!is.null(info$remote_url) && info$remote_url != "" && !getOption("auth0_local")) {
          redirect_uri <- info$remote_url
        } else {
          if (grepl("127.0.0.1", req$HTTP_HOST)) {
            redirect_uri <- paste0("http://", gsub("127.0.0.1", "localhost", req$HTTP_HOST, query))
          } else {
            redirect_uri <- paste0("http://", req$HTTP_HOST, query)
          }
        }
        redirect_uri <<- redirect_uri
        
        query_extra <- if(is.null(info$audience)) list() else list(audience=info$audience)
        url <- httr::oauth2.0_authorize_url(
          info$api, info$app(redirect_uri), scope = info$scope, state = info$state,
          query_extra=query_extra
        )
        redirect <- sprintf("location.replace(\"%s\");", url)
        shiny::tags$script(shiny::HTML(redirect))
      }
    } 
    else {
      if (is.function(ui)) {
        ui(req)
      } 
      else {
        ui
      }
    }
  }
}


myGlobalQueryVars <- list()

my_auth0_server <- function(server, info) {
  print("using my auth0 server")
  if (missing(info)) info <- auth0_info()
  function(input, output, session) {
    print("funciton wihin myauth0server")
    shiny::isolate(auth0_server_verify(session, info$app, info$api, info$state))
    
    shiny::observeEvent(input[["._auth0logout_"]], logout())
    if( shiny::isRunning() ) {
      print("my auth0 server, shiny is running!")
    }
    else{
      print("my auth0 server, shiny is NOT running!")      
    }
    #shinyjs::logjs("Hello from shinyjs inmyauth0 function")

    observe({
      if(length(myGlobalQueryVars) > 0) {
        output$debug_query_message_2 <- renderText(paste(myGlobalQueryVars, sep = " | "))
      }
      else {
        output$debug_query_message_2 <- renderText("No Global Vars Found")
      }
    })
    
    server(input, output, session)
  }
}
assignInNamespace("auth0_ui", my_auth0_ui, ns = "auth0")
assignInNamespace("auth0_server", my_auth0_server, ns = "auth0")

#sillyUI <- fluidPage(
#  useShinyjs()
#)

#sillyServer <- function (input, output, session) {
#   logjs("Inside the temporary server")
#  observe({
#    url_search_params <- parseQueryString(session$clientData$url_search)
    #shinyjs::logjs("search params")
    #shinyjs::logjs(url_search_params)
#    myGlobalQueryVars <- url_search_params
#    output$debug_query_message_2 <- renderText(paste(url_search_params, sep = " | "))
#  })
#}

shinyAppAuth0(ui = ui, server = server)


