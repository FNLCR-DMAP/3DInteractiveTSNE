library(shiny)
library(shinyjs)
library(plotly)
library(DT)
library(httr)
library(jsonlite)
library(tools)
library(urltools)
library(arrow)
library(cookies)
library(auth0)
library(stringr)

source("./UI_functions.R") # get_fluid_page, get_server
source("./matrix_functions.R") # projectVertex, xformMatrix, generate_random_sample_data
source("./tsne_UI.R") # tsne_ui
source("./tsne_server.R") #tsne_server

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
      user_params = list(grant_type = "authorization_code")
    )

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

my_auth0_server <- function(server, info) {
  print("using my auth0 server")
  if (missing(info)){
    info <- auth0_info()
  }

  function(input, output, session) {
    shiny::isolate(auth0_server_verify(session, info$app, info$api, info$state))
    shiny::observeEvent(input[["._auth0logout_"]], logout())
    #you need to add an optional field to your server funciton signature to pass the session info
    server(input, output, session, session_info = info)
  }
}



redirect_and_serve_UI <- function(ui, info) {
  print("my auth0 UI called")
  if (missing(info)){
    info <- auth0_info()
  }

  function(req) {
    q_string <- shiny::parseQueryString(req$QUERY_STRING)
    verify <- has_auth_code(shiny::parseQueryString(req$QUERY_STRING), info$state)

    if (!verify) {
      #------------original oauth stuff------------
      if (grepl("error=unauthorized", req$QUERY_STRING)) {
        redirect <- sprintf("location.replace(\"%s\");", logout_url())
        shiny::tags$script(shiny::HTML(redirect))
      } else {
        params <- shiny::parseQueryString(req$QUERY_STRING)
        params$code <- NULL
        params$state <- NULL

        query <- paste0("/?", paste(
          mapply(paste, names(params), params, MoreArgs = list(sep = "=")),
          collapse = "&"
        ))
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
        #------------/original oauth stuff------------

        if ("inputRID" %in% names(q_string)) {
          print(paste("setting var with state", info$state, "to", q_string$inputRID))
          nonce <- info$state
          inputRID <- q_string$inputRID
          outputRID <- q_string$outputRID
          inputBranch <- q_string$inputBranch
          outputBranch <- q_string$outputBranch

          set_cookie_and_redirect_script <- sprintf(
            "document.cookie='%s={\"inputRID\":\"%s\",\"outputRID\":\"%s\", \"inputBranch\":\"%s\", \"outputBranch\":\"%s\"}'; %s", #nolint
            nonce, inputRID, outputRID, inputBranch, outputBranch, redirect
          )
          print("cookie redirect script")
          print(set_cookie_and_redirect_script)
          return(fluidPage(
            tags$head(tags$script(HTML(set_cookie_and_redirect_script)))
          ))
        }
        shiny::tags$script(shiny::HTML(redirect))

      }
    } else {
      if (is.function(ui)) {
        ui(req)
      } else {
        ui
      }
    }
  }
}

assignInNamespace("auth0_ui", redirect_and_serve_UI, ns = "auth0")
assignInNamespace("auth0_server", my_auth0_server, ns = "auth0")

shinyAppAuth0(ui = tsne_ui, server = tsne_server)