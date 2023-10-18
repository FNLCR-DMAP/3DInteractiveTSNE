source("./matrix_functions.R") # projectVertex, xformMatrix, generate_random_sample_data

tsne_server <- function (input, output, session, session_info = NULL) {
  print("regular server function: Global nonce data:")
  auth_token <- session$userData$auth0_credentials$access_token
  shinyjs::disable("add_to_list")
  shinyjs::disable("getParam")
  shinyjs::disable("project2D")

  mydata <- reactive({
    cookie <- cookies::get_cookie(session_info$state)
    rid <- NULL
    branch <- NULL
    if (!is.null(cookie)) {
      foundry_rids <- fromJSON(cookie)
      rid <- foundry_rids$inputRID
      branch <- foundry_rids$inputBranch
      output$error_message_box <- renderText(paste("Found cookie with input dataset rid : ", rid, "branch:", branch))
      output$upload_error_message_box <- renderText(paste("Uploading to dataset:", foundry_rids$outputRID))
    } else {
      print(paste("could not find cooke: ", session_info$state))
      output$error_message_box <- renderText(
        paste("ERROR: Could not find cookie with input dataset rid. State: ", session_info$state)
      )

      output$upload_error_message_box <- renderText(
        paste("ERROR: Could not find cookie with input dataset rid. State: ", session_info$state)
      )
      return(NULL)
    }
    withProgress(message="Downloading Data From NIDAP", value = 0, {
      df = downloa_dataset_from_nidap(rid, auth_token, branch)
    })
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
      df <- mydata()
      if (!is.null(df) ){
        unique_values = unique(df[[input$indicator_col]] %>% sort())
        if (columnType()[input$indicator_col] == "character" | columnType()[input$indicator_col] == "factor"){
          factor_value(TRUE)
          updateCheckboxGroupInput(
            session,
            inputId = "indicator_values_filter",
            choices = unique_values,
            selected = unique_values
          )
        } else {
          factor_value(FALSE)
          updateCheckboxGroupInput(
            session,
            inputId = "indicator_values_filter",
            choices = "Not a Factor",
            selected = "Not a Factor"
          )
        }
      }
    }
  })
  #Disable Generate button if no features are chosen
  observe({
    if (length(input$indicator_values_filter) > 0) {
      shinyjs::enable("show")
    } else {
      shinyjs::disable("show")
    }
  })

  filterData <- eventReactive(input$indicator_values_filter, {
    df = mydata()
    if (!is.null(df) ){ 
      if (factor_value()) {
        df <- df %>% filter(get(input$indicator_col) %in% input$indicator_values_filter)
      }
      return(df)
    }
    NULL
  })

  observe({
    exportDataset$data <- data.frame()
    df <- mydata()
    if( !is.null(df) ){
      if("x" %in% colnames(df)){
        x_default_col = "x"
      } else {
        x_default_col = colnames(df[1])
      }
      if("y" %in% colnames(df)){
        y_default_col = "y"
      } else {
        y_default_col = colnames(df[2])
      }
      if("z" %in% colnames(df)){
        z_default_col = "z"
      } else {
        z_default_col = colnames(df[3])
      }

      updateSelectInput(session, "x_col", choices = colnames(df), selected = x_default_col)
      updateSelectInput(session, "y_col", choices = colnames(df), selected = y_default_col)
      updateSelectInput(session, "z_col", choices = colnames(df), selected = z_default_col)
      updateSelectInput(session, "indicator_col", choices = colnames(df), selected = colnames(df[5]))
    }
  })

  observeEvent(input$show,{
    showModal( 
      modalDialog(
        paste0("Plotting..."),
        easyClose = TRUE,
        footer = NULL
      )
    )
    ind <- filterData()[[input$indicator_col]]
    x <- filterData()[[input$x_col]]
    y <- filterData()[[input$y_col]]
    z <- filterData()[[input$z_col]]
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

    showModal(
      modalDialog(
        paste0("Projection View Saved"),
        easyClose = TRUE,
        footer = NULL
      )
    )
    shinyjs::enable("project2D")
  })

  observeEvent(
    input$project2D, 
    {
      showModal(
        modalDialog(
          paste0("Please wait, projecting to 2D"),
          easyClose = TRUE,
          footer = NULL
        )
      )

      x2d <- c()
      y2d <- c()
      pk <- c()
      indicator <- c()

      projectedData$data <- data.frame()

      ind <- filterData()[[input$indicator_col]]
      x <- filterData()[[input$x_col]]
      y <- filterData()[[input$y_col]]
      z <- filterData()[[input$z_col]]
      pkCol <- filterData()[['pk']]

      #progress bar
      withProgress(
        message = 'Transforming Points',
        value = 0,
        {
          for (ai in 1: length(pkCol)) {
            vp <- c(x[ai]*input$dataScale[1],y[ai]*input$dataScale[2], z[ai]*input$dataScale[3])
            transformed <- projectVertex(vp, input$model, input$view, input$projection, c(1,1))
            x2d[ai] <- transformed[1]
            y2d[ai] <- transformed[2]
            indicator[ai] <- ind[ai]
            pk[ai] <- pkCol[ai]
            incProgress(amount = 1/length(pkCol), detail = paste(ai, "of", length(pkCol)))
          }
        }
      )

      transformed1 <- data.frame(x = x2d,y = y2d,indicator,pk)
      projectedData$data <- rbind(projectedData$data, transformed1)
      projectedData$data <- projectedData$data[projectedData$data['x'] >= 0 & projectedData$data['x'] <= 1,]
      projectedData$data <- projectedData$data[projectedData$data['y'] >= 0 & projectedData$data['y'] <= 1,]

      output$plot2d <- renderPlotly(
        {
          fig2d <- plot_ly(
            source = "2dplot",
            width = 800,
            height = 800) %>%
            add_markers(
              data = projectedData$data,
              x = as.formula(paste0("~","x")), 
              y = as.formula(paste0("~","y")),
              type = "scatter",
              mode = "markers",
              color = as.formula(paste0("~",'indicator')),
              text = paste(projectedData$data[['indicator']]),
              hoverinfo = "text"
            )

          fig2d <- fig2d %>% layout(
            xaxis = list(range=c(0,1)),
            yaxis = list(range=c(0,1))
          )

          fig2d <- fig2d %>% layout(
            dragmode = "lasso"
          )
        }
      )

      showModal(
        modalDialog(
          paste0("Projecting to 2D has been completed"),
          easyClose = TRUE,
          footer = NULL
        )
      )
      shinyjs::disable("project2D")
    }
  )
  
  observeEvent(
    {
      event_data("plotly_selected", source = "2dplot"); input$points_names}, {
    if (length(event_data("plotly_selected", source = "2dplot")) & input$points_names != "") {
      shinyjs::enable("add_to_list")
    }
    else{
      shinyjs::disable("add_to_list")
    }
    }
  )
  
  observeEvent(input$add_to_list, {

    df <- mydata()
    if(!is.null(df)){
      pkDataset$data <- data.frame()
      selected_points <- event_data("plotly_selected", source = "2dplot")

      indicator_col_values <- unique(projectedData$data[['indicator']]) %>% sort

      num_selected_points <- nrow(selected_points)

      if (factor_value()) {
        for (i in 1:num_selected_points) {
          curveNum <- selected_points[i,]$curveNumber
          pointNum <- selected_points[i,]$pointNumber
          filtered_data <- filter(projectedData$data, get('indicator') == indicator_col_values[curveNum+1])
          filtered_data <- filtered_data[pointNum+1,]
          pkDataset$data <- rbind(pkDataset$data, filtered_data['pk'])
        }
      } else {
        filtered_data <- projectedData$data[selected_points$pointNumber+1,]
        pkDataset$data <- rbind(pkDataset$data, filtered_data['pk'])
      }

      pk <- pkDataset$data
      exportData <- df[df$pk %in% pk$pk,]
      exportData$InterestPoint <- input$points_names
      exportDataset$data <- rbind(exportDataset$data, exportData)
    }

  })

  observeEvent(
    input$clear, 
    {
      exportDataset$data = data.frame()
    }
  )

  output$Export_Dataset <- renderDT(
    server = FALSE,
    {
      DT::datatable(
        exportDataset$data,
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        )
      )
    }
  )

  observeEvent(
    input$exportNidap, 
    {
      print("exporting to nidap")
      cookie <- cookies::get_cookie(session_info$state)
      if(!is.null(cookie)){
        cookie_json <- fromJSON(cookie)  
        rid <- cookie_json$outputRID      
        branch <- cookie_json$outputBranch
        filePath <- sprintf("tempFile_from_posit-%s.csv", Sys.Date())
        data_to_upload <- exportDataset$data
        two_d_csv <- capture.output(write.csv(data_to_upload, row.names = FALSE)) #list of lists
        character_list <- paste(two_d_csv, collapse="\n")
        raw_char_array <- charToRaw(character_list)
        # /NIH/tSNE3d_v01/datasets/posit_output_test
        # ri.foundry.main.dataset.b2dcb103-6b5f-4411-90eb-dd0f6043b54a
        # https://rstudio-connect-dev.cancer.gov/content/529413aa-fc85-4353-9355-07d249a3f25c/?inputRID=ri.foundry.main.dataset.f0708c74-d5b1-4e73-9fe7-6a086cdf0b95&outputRID=ri.foundry.main.dataset.b2dcb103-6b5f-4411-90eb-dd0f6043b54a # nolint
        upload_url <- paste0("https://nidap.nih.gov/api/v1/datasets/",rid,"/files:upload?filePath=",filePath,"&branchId=",branch)

        response <- POST(
          upload_url, 
          content_type("application/octet-stream"),
          httr::add_headers(Authorization = paste("Bearer", auth_token)),
          body = raw_char_array
        )
        print(status_code(response))
        print(content(response))

      } else{
        output$upload_error_message_box <- renderText("ERROR, could not find upload RID in cookies")
      }
    }
  )
}