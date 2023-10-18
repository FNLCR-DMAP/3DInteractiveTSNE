library(cookies)
library(shiny)
js_code <- paste(readLines("./js_code.js"), collapse="\n")
markerShape <- c("circle", "circle-open", "square", "square-open", "diamond", "diamond-open", "cross", "x")

tsne_ui <-  cookies::add_cookie_handlers(
  fluidPage(
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
                   actionButton("add_to_list", label = "Add Points to Export List"),
                   plotlyOutput("plot2d")),
          tabPanel("View Export Dataset",
                   br(),
                   textOutput("upload_error_message_box"),
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