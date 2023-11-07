library(cookies)
library(shiny)
js_code <- paste(readLines("./js_code.js"), collapse="\n")
markerShape <- c("circle", "circle-open", "square", "square-open", "diamond", "diamond-open", "cross", "x")

tsne_ui <-  cookies::add_cookie_handlers(
  fluidPage(
    useShinyjs(),
    extendShinyjs(text = js_code, functions = c('plot3d')),
    tags$head(
      tags$script(src = "https://cdn.plot.ly/plotly-latest.min.js"),
      tags$style(
        HTML(".shiny-notification {
           height: 100px;
           width: 800px;
           position:fixed;
           top: calc(50% - 50px);
           left: calc(50% - 400px);
           font-size: 150%;
           text-align: center;
           }
           #selection_error_message_box {
             color: red;
             font-weight: bold;
             text-decoration: underline;
           }
           .container-fluid {
             background-color: #FBFBFD;
             height: 100%;
             width: 100%;
           }
           html, body {
             height: 100%;
           }
           mydiv {
             height: 100%;
             width: 100%;
           }
           "
        )
      )
    ),
    #titlePanel("T-SNE 3D Scatterplot"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h3("T-SNE 3D Scatterplot"),
        textOutput("selection_error_message_box"),
        selectInput("pk_col", label = "Primary Key", choices = NULL),
        selectInput("x_col", label = "X-Axis", choices = NULL),
        selectInput("y_col", label = "Y-Axis", choices = NULL),
        selectInput("z_col", label = "Z-Axis", choices = NULL),
        selectInput("indicator_col", "Indicator", choices = NULL),
        checkboxInput("toggle_discrete", label="Discrete? (uncheck for continuous)", value = TRUE),
        actionButton('show', 'Generate Plot'),
        br(),
        br(),
        checkboxGroupInput("indicator_values_filter", label = "Features", choices = NULL)
      ),
      mainPanel(
        width = 9, 
        fluidRow(
          column (4, sliderInput("marker", label = 'Marker Size', min = 1, max = 10, value = 3)),
          column (4, selectInput("shape", label = "Marker Shape", choices = markerShape)),
          column (4, verbatimTextOutput("response"))
        ),
        tabsetPanel(
          tabPanel("3D Plot",
                   br(),
                   textOutput("error_message_box"),
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
                   p("Please only use alphanumeric characters and underscores"),
                   htmlOutput("name_message_box"),
                   actionButton("add_to_export_list", label = "Add Points to Export List"),
                   plotlyOutput("plot2d")),
          tabPanel("View Export Dataset",
                   br(),
                   textOutput("upload_error_message_box"),
                   br(),
                   htmlOutput("data_format_info"),
                   br(),
                   selectInput(inputId = "export_data_format", 
                                            label = "Export Format", 
                                            choices = c("Indicator_Column", "Subset"),
                                            selected = "Indicator_Column"
                   ),
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
)
