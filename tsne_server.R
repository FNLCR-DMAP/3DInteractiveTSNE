source("./matrix_functions.R") # projectVertex, xformMatrix, generate_random_sample_data



tsne_server <- function (input, output, session, session_info = NULL) {
  print("regular server function: Global nonce data:")
  auth_token <- session$userData$auth0_credentials$access_token
  shinyjs::disable("add_to_export_list")
  shinyjs::disable("getParam")
  shinyjs::disable("project2D")

  inputData <- reactiveVal(NULL)

  mydata <- reactive({
    if(!is.null(inputData())){
      return(inputData())
    }
    else {
        
      df <- NULL
      print("session info")
      print(session_info)
      cookie <- cookies::get_cookie(session_info$state)
      
      print("cookie")
      print(cookie)
      if (!is.null(cookie)) {
        cookie_data <- fromJSON(cookie)
        dataset_rid <- cookie_data$inputRID
        branch <- cookie_data$inputBranch
        output$error_message_box <- renderText(paste("Found cookie with input dataset rid : ", dataset_rid, "branch:", branch))
        output$upload_error_message_box <- renderText(paste("Uploading to dataset:", cookie_data$outputRID))
      } else {
        cookie_data <- NULL
        print(paste("could not find cooke: ", session_info$state))
        output$error_message_box <- renderText(
          paste("ERROR: Could not find cookie with input dataset rid. State: ", session_info$state)
        )
        output$upload_error_message_box <- renderText(
          paste("ERROR: Could not find cookie with input dataset rid. State: ", session_info$state)
        )
        return(NULL)
      }
        
      dataset_rid <- cookie_data$inputRID
      branch <- cookie_data$inputBranch
      withProgress(message="Downloading Data From NIDAP", value = 0, {
      
        list_files_url <- paste0("https://nidap.nih.gov/api/v1/datasets/",dataset_rid,"/files?branchId=", branch)
        print(paste("making request to ", list_files_url))

        response <- GET(list_files_url, httr::add_headers(Authorization = paste("Bearer", auth_token)))
        if(status_code(response) != 200){
          print(content(response, as="text"))
          output$error_message_box <- renderText("ERROR: Could not list files in NIDAP")
          stop("Error listing files from NIDAP")
        }

        incProgress(0.10, detail="Listed files from dataset")
        
        data_content <- content(response, as="text")
        parsed_json <- fromJSON(data_content)
        files <- parsed_json$data$path
        files <- files[!file_ext(files) %in% c("log", "")] #filter out log and spark success files
        
        num_files <- length(files)
        if (num_files == 0 ){
            stop("Error, zero files found in dataset")
        }
        print("reading through files")
        df = data.frame()
        index <- 0

        for (file in files) {
          print(paste("getting data from", file))
          file <- url_encode(file)
          get_file_content_url <- paste0("https://nidap.nih.gov/api/v1/datasets/",dataset_rid,"/files/",file,"/content?branchId=", branch)
          response2 <- GET(get_file_content_url, httr::add_headers(Authorization = paste("Bearer", auth_token)))
          if(status_code(response2) != 200){
            output$error_message_box <- renderText("ERROR: Could not get file content from NIDAP")
            stop("Error getting file content from NIDAP")
          }

          if (file_ext(file) == "csv") {
            raw <- content(response2, as="text")
            dataset <- read.csv(text = raw)
            dataset <- data.frame(dataset)
            df <- rbind(df, dataset)
          } else if (file_ext(file) == "parquet") {
            raw = content(response2, as="raw")
            dataset = read_parquet(raw)
            dataset = data.frame(dataset)
            dataset$name <- file
            df <- rbind(df, dataset)
          } else if(file_ext(file) == "rds"){
            raw = content(response2, as="raw")
            dataset = readRDS(raw)
            dataset = data.frame(dataset)
            dataset$name <- file
            df <- rbind(df, dataset)
          }else {
            #TODO raise an error
            print("unsoupported file type")
          }
          index <- index + 1
          incProgress(0.9 / num_files, detail=paste("Downloaded", index + 1, "of", num_files, "files"))
        }
        # df = df %>% filter(!is.na(pk))
        print("successfully read in all data")
        #print(head(df, 5))
      }) #withProgress
      inputData(df)
      return(df)
    }
  }) #reactive mydata
  

  columnType <- reactive({
    sapply(mydata(),class)
  })

  pkDataset <- reactiveValues(
    data = data.frame(),
  )

  exportDataPrimaryKeysLabels <- reactiveValues(
    data = data.frame()
  )

  dataToExport <- reactiveVal(NULL)

  projectedData <- reactiveValues(
    data = data.frame(),
  )
  
  isDiscreteValue <- reactive({
    input$toggle_discrete
  })

  output$text <- renderText({
    paste("<b>Please save View First before Projecting to 2D<br>", "<br>", "</b>")
  })

  selectedColumnValidationListener <- reactive({
    list(input$pk_col, input$x_col, input$y_col, input$z_col, input$indicator_col) 
  })
  selectedColumnValidation <- observeEvent(selectedColumnValidationListener(), {
    df <- mydata()
    if (!is.null(df) ){
      print("selectedColumnValidation")
      if(sum(is.na(df[input$pk_col])) > 0){ #this one needs to go first, superseds the next PK check
        output$selection_error_message_box <- renderText('ERROR: Primary Key column contains null values')
        shinyjs::disable("show")
        return()
      }
      else if(sum(duplicated(df[input$pk_col])) > 0){
        output$selection_error_message_box <- renderText('ERROR: Primary Key column contains duplicate values')
        shinyjs::disable("show")
        return()
      }
      else {
        output$selection_error_message_box <- renderText('')
        shinyjs::enable("show")
      }
      
      colsToCheck <- tail(selectedColumnValidationListener(), -1)
      for(col in colsToCheck){  
        if(sum(is.na(df[col])) > 0){
          output$selection_error_message_box <- renderText(paste('WARNING:', col, 'contains null values'))
        }
      }        
    }
  })

  #function to check if a string is a valid R variable name


  updateDiscreteContinuousListener <- reactive({
    # not actually used, but needed to trigger update
    list(input$indicator_col, input$toggle_discrete) 
  })
  #Update Feature Selection and Check if indicator column contains factors.
  #Factors includes columns that have type character or factor
  observeEvent(updateDiscreteContinuousListener(), {
    #if (!is.null(columnType()) && input$indicator_col %in% names(columnType())) {
      df <- mydata()
      if (!is.null(df) ){
        withProgress(message="Updating Indicator Column", value = 0, {   
          unique_values = unique(df[[input$indicator_col]] %>% sort())
          incProgress(0.5, detail="Updating Indicator Column")

          #if (columnType()[input$indicator_col] == "character" | columnType()[input$indicator_col] == "factor"){
          if (isDiscreteValue()){
            #isDiscreteValue(TRUE)
            updateCheckboxGroupInput(
              session,
              inputId = "indicator_values_filter",
              choices = unique_values,
              selected = unique_values
            )
          } else {
            #isDiscreteValue(FALSE)
            updateCheckboxGroupInput(
              session,
              inputId = "indicator_values_filter",
              choices = "Not a Factor",
              selected = "Not a Factor"
            )
          }
        })    
      }
    #}
  })
  #Disable Generate button if no features are chosen
  # observe({
  #   if (length(input$indicator_values_filter) > 0) {
  #     shinyjs::enable("show")
  #   } else {
  #     shinyjs::disable("show")
  #   }
  # })

  filterData <- eventReactive(input$indicator_values_filter, {
    df = mydata()
    if (!is.null(df) ){ 
      if (isDiscreteValue()) {
        df <- df %>% filter(get(input$indicator_col) %in% input$indicator_values_filter)
      }
      return(df)
    }
    NULL
  })


  observe({ #set default columns
    df <- mydata()
    if( !is.null(df) ){
      print("setting default columns")
      if("x" %in% colnames(df)){ #TODO check if df length > 4
        x_default_col = "x"
      } else {
        x_default_col = colnames(df)[1]
      }
      if("y" %in% colnames(df)){
        y_default_col = "y"
      } else {
        y_default_col = colnames(df)[2]
      }
      if("z" %in% colnames(df)){
        z_default_col = "z"
      } else {
        z_default_col = colnames(df)[3]
      }
      if("pk" %in% colnames(df)){
        pk_default_col = "pk"
      } else {
        pk_default_col = colnames(df)[4]
      }

      pkColumn = df[pk_default_col]
      
      updateSelectInput(session, "pk_col", choices = colnames(df), selected = pk_default_col)
      updateSelectInput(session, "x_col", choices = colnames(df), selected = x_default_col)
      updateSelectInput(session, "y_col", choices = colnames(df), selected = y_default_col)
      updateSelectInput(session, "z_col", choices = colnames(df), selected = z_default_col)
      updateSelectInput(session, "indicator_col", choices = colnames(df), selected = colnames(df)[5] )
      print("done setting default columns")
      
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
    js$plot3d('mydiv', x, y, z, ind, isDiscreteValue(), input$marker, input$shape)
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

  observeEvent(input$project2D, 
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
      pkCol <- filterData()[[input$pk_col]]
      # get null value cout for pkCol
      null_count <- sum(is.na(pkCol))
      print("null count")
      print(null_count)
      #progress bar
      withProgress(
        message = 'Transforming Points',
        value = 0,
        {
          loading_bar_amount <- (1/length(pkCol))*100
          print("Total length")
          print(length(pkCol))
          for (ai in 1:length(pkCol)) {
            vp <- c(x[ai]*input$dataScale[1],
                    y[ai]*input$dataScale[2], 
                    z[ai]*input$dataScale[3])
            transformed <- projectVertex(vp, input$model, input$view, input$projection, c(1,1))
            x2d[ai] <- transformed[1]
            y2d[ai] <- transformed[2]
            indicator[ai] <- ind[ai]
            pk[ai] <- pkCol[ai]
            
            #print(ai)d
            if(ai %% 100 == 0 ){
              incProgress(amount = loading_bar_amount , detail = paste(ai, "of", length(pkCol)))
            }
          }
        }
      )
      print("done transforming points")
      
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
      print("plotly rendered")
      showModal(
        modalDialog(
          paste0("Projecting to 2D has been completed"),
          easyClose = TRUE,
          footer = NULL
        )
      )
      shinyjs::disable("project2D")
      print("successfully projected to 2D") 
  })
  
  selectedPointsLabel <- reactiveVal(NULL)
  observeEvent(input$points_names, {
    trimmed <- str_trim(input$points_names)
    trimmed <- gsub(" ", "_", trimmed)
    # list of all non alphanumeric charcters in trimmed
    invalid_chars <- c()
    for( char in str_split(trimmed, pattern = "")){
      matched <- grep("[a-zA-Z0-9_]+", char, invert = TRUE, value = TRUE)
      invalid_chars <- append(invalid_chars, matched)
    }
    print("invalid chars")
    print(invalid_chars)
    if (length(invalid_chars) > 0) {
      output$name_message_box <- renderText({
        paste('<b style="color:red">Error</b>', trimmed, 'contains invalid characters:', invalid_chars)
      })
      shinyjs::disable("add_to_export_list")
      selectedPointsLabel(NULL)
    }
    else {
      output$name_message_box <- renderText(paste("using column name: ", trimmed))
      selectedPointsLabel(trimmed)
      shinyjs::enable("add_to_export_list")
    }
  })

  # observeEvent(
  #   {
  #     event_data("plotly_selected", source = "2dplot"); input$points_names}, {
  #   if (length(event_data("plotly_selected", source = "2dplot")) > 0 & selectedPointsLabel() != NULL) {
  #     shinyjs::enable("add_to_export_list")
  #   }
  #   else{
  #     shinyjs::disable("add_to_export_list")
  #   }
  #   }
  # )

  
  output$data_format_info <- renderText({
    paste0(
      "The following are the options for the output format of this data<br>",
      "<ul>",
      "<li><b>Indicator_Column</b>: A new column for each label consisting of TRUE / FALSE values deppending if that datapoint was selected or not</li>",
      "<li><b>Subset</b>: A subset of the original data with a column coanining the label value, could have duplicates</li>",
      "</ul>"
    )
  })

  
  observeEvent(input$add_to_export_list, {

    df <- mydata()
    if(!is.null(df)){
      pkDataset$data <- data.frame()
      selected_points <- event_data("plotly_selected", source = "2dplot")
      print("selected points")
      print(selected_points)
      indicator_col_values <- unique(projectedData$data[['indicator']]) %>% sort
      num_selected_points <- nrow(selected_points)

      if(!is.null(selectedPointsLabel()) ) 
      { 
        if(!is.null(num_selected_points) && num_selected_points > 0) { 
          if (isDiscreteValue()) {
            for (i in 1:num_selected_points) {
              curveNum <- selected_points[i,]$curveNumber
              pointNum <- selected_points[i,]$pointNumber
              filtered_data <- filter(projectedData$data, get('indicator') == indicator_col_values[curveNum+1])
              filtered_data <- filtered_data[pointNum+1,]
              pkDataset$data <- rbind(pkDataset$data, filtered_data[input$pk_col])
            }
          } else {
            filtered_data <- projectedData$data[selected_points$pointNumber+1,]
            pkDataset$data <- rbind(pkDataset$data, filtered_data[input$pk_col])
          }

          pk <- pkDataset$data
          print("pk pkDataset$data")
          print(pk) 

          exportData <- df[df$pk %in% pk$pk,] 
          exportData <- exportData[input$pk_col]
          exportData$InterestPoint <- selectedPointsLabel()
          exportDataPrimaryKeysLabels$data <- rbind(exportDataPrimaryKeysLabels$data, exportData)
          showModal(
            modalDialog(
              paste("Added", num_selected_points, "datapoints to export list"),
              easyClose = TRUE,
              footer = NULL
            )
          )
        } else {
          showModal(
            modalDialog(
              paste0("Zero datapoints selected, adding nothing to export list"),
              easyClose = TRUE,
              footer = NULL
            )
          )
          print("no points selected, doing nothing")
        }
      }
    }
  })

  

  observeEvent(
    input$clear, 
    {
      exportDataPrimaryKeysLabels$data = data.frame()
      dataToExport(NULL)
    }
  )

  dataToExportFormatListener <- reactive({
    # not actually used, but needed to trigger update
    list(input$export_data_format, exportDataPrimaryKeysLabels$data)
  })
  observeEvent(dataToExportFormatListener(), {
    dataToExportFormat <- dataToExportFormatListener()
    df <- mydata()
    if(nrow(exportDataPrimaryKeysLabels$data) > 0 && !is.null(df)){
      if(input$export_data_format == "Subset"){
        #df_subset <- df[df[input$pk_col] %in% exportDataPrimaryKeysLabels$data$pk,]
        # inner join df and exportDataPrimaryKeysLabels$data
        df_subset <- merge(
          x = df, 
          y = exportDataPrimaryKeysLabels$data,
          by.x=input$pk_col,
          by.y="pk",
          all = FALSE
        )
    
        dataToExport(df_subset)
      } else if (input$export_data_format == "Indicator_Column"){
        
        unique_interest_points <- unique(exportDataPrimaryKeysLabels$data$InterestPoint)
        df_with_indicator <- df 

        for(intrest_point in unique_interest_points){

          current_intrest_points <- exportDataPrimaryKeysLabels$data[exportDataPrimaryKeysLabels$data$InterestPoint == intrest_point,]        
          df_with_indicator <- merge(
            x = df_with_indicator, 
            y = current_intrest_points,
            by.x=input$pk_col,
            by.y="pk",
            all.x = TRUE
          )
          #new_col_name <- selectedPointsLabel()
          
          df_with_indicator[intrest_point] <- ifelse(is.na(df_with_indicator$InterestPoint), FALSE, TRUE)
          df_with_indicator <- subset(df_with_indicator, select = -InterestPoint)
        }
        
        dataToExport(df_with_indicator)
      } 
    }
    

  })
  output$Export_Dataset <- renderDT(
    server = FALSE,
    {
      render_data <- dataToExport()
      if( nrow(exportDataPrimaryKeysLabels$data) > 0 ){
        column_names <- colnames(render_data)
        if(input$export_data_format == "Subset"){
          column_names <- c("InterestPoint", setdiff(column_names, "InterestPoint"))
        }
        else if (input$export_data_format == "Indicator_Column"){
          label_columns <- unique(exportDataPrimaryKeysLabels$data$InterestPoint)
          column_names <- c(label_columns, setdiff(column_names, label_columns))
        }
        render_data <- render_data[column_names]
      }
      DT::datatable(
        render_data,#
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
        withProgress(message="Uploading Results to NIDAP", value = 0, {
          
          cookie_json <- fromJSON(cookie)  
          rid <- cookie_json$outputRID      
          branch <- cookie_json$outputBranch
          filePath <- sprintf("tempFile_from_posit-%s.csv", Sys.Date())

          incProgress(0.25, detail="Converting data to CSV...")
          
          data_to_upload <- dataToExport()

          two_d_csv <- capture.output(write.csv(data_to_upload, row.names = FALSE)) #list of lists
          character_list <- paste(two_d_csv, collapse="\n")
          raw_char_array <- charToRaw(character_list)
          
          #========Upload to NIDAP===========
          # /NIH/tSNE3d_v01/datasets/posit_output_test
          # ri.foundry.main.dataset.b2dcb103-6b5f-4411-90eb-dd0f6043b54a
          # https://rstudio-connect-dev.cancer.gov/content/529413aa-fc85-4353-9355-07d249a3f25c/?inputRID=ri.foundry.main.dataset.f0708c74-d5b1-4e73-9fe7-6a086cdf0b95&outputRID=ri.foundry.main.dataset.b2dcb103-6b5f-4411-90eb-dd0f6043b54a # nolint
          upload_url <- paste0("https://nidap.nih.gov/api/v1/datasets/",rid,"/files:upload?filePath=",filePath,"&branchId=",branch)
          incProgress(0.25, detail="Uploading Data...")
          response <- POST(
            upload_url, 
            content_type("application/octet-stream"),
            httr::add_headers(Authorization = paste("Bearer", auth_token)),
            body = raw_char_array
          )
          if (status_code(response) == 200) {
            print("Data Upload Success")
          } else {
            error_message <- content(response, "text")
            print(paste("Data Upload Error:", error_message))
            output$upload_error_message_box <- renderText("ERROR: Error uploading data (see logs for more details)")
            stop("Error uploading data")
          }
        
          #========Get Default Schema===========
          incProgress(0.25, detail="Getting Schema...")
          get_default_schema_url <- sprintf("https://nidap.nih.gov/foundry-schema-inference/api/datasets/%s/branches/%s/schema", rid, branch)
          create_schema_response <- POST(
            get_default_schema_url,
            add_headers(
              Authorization = paste0("Bearer ", auth_token),
              "Content-Type" = "application/json"),
            body = '{}',
            encode = "json"
          )
          if (status_code(create_schema_response) == 200) {
            print("Schema Acquisition Success")
          } else {
            error_message <- content(response, "text")
            print(paste("Schema Acquisition Error:", error_message))
            output$upload_error_message_box <- renderText("ERROR: Schema Acquisition Error (see logs for more detail)")
            stop("Error getting default schema")
          }
          response_content <- content(create_schema_response, "text")
          foundrySchema <- fromJSON(response_content)$data$foundrySchema
          
          #========Apply Schema===========
          incProgress(0.25, detail="Applying Schema...")
          schema_set_url <- sprintf("https://nidap.nih.gov/foundry-metadata/api/schemas/datasets/%s/branches/%s", rid, branch)
          update_schema_response <- POST(
            schema_set_url,
            add_headers(
              Authorization = paste0("Bearer ", auth_token),
              "Content-Type" = "application/json"
            ),
            body = foundrySchema,
            encode = "json"
          )

          if (status_code(update_schema_response) == 200) {
            print("Schema Update Success")
          } else {
            error_message <- content(update_schema_response, "text")
            print(paste("Schema Update Error:", error_message))
            output$upload_error_message_box <- renderText("ERROR: Error applying schema (see logs for more detail)")
            stop("Error updating schema")
          }
          output$upload_error_message_box <- renderText("SUCCESS: Successfully uploaded data back to NIDAP, you can close this window now")
        }) #withprogress
      } else{
        output$upload_error_message_box <- renderText("ERROR, could not find upload RID in cookies")
      }
    }
  )
}
