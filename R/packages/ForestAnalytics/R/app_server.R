#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  
  # -------------------------------
  # Application server logic
  # -------------------------------
  
  # Reactive values to hold the database path and connection pool
  db_path  <- shiny::reactiveVal(NULL)
  pool_obj <- shiny::reactiveVal(NULL)
  
  # --------------------------------------------
  # Initialize database connection from environment
  # --------------------------------------------
  # If an environment variable "DB_PATH" exists and points to a file,
  # use it to create an initial database connection.
  if (base::nzchar(Sys.getenv("DB_PATH")) &&
      base::file.exists(Sys.getenv("DB_PATH"))) {
    
    db_path(Sys.getenv("DB_PATH"))
    
    pool_obj(
      pool::dbPool(
        drv    = RSQLite::SQLite(),
        dbname = Sys.getenv("DB_PATH")
      )
    )
  }
  
  # -------------------------------------------
  # Observe user clicking "Connect" button
  # -------------------------------------------
  shiny::observeEvent(input$connect, {
    
    # Require that a manual path has been entered
    shiny::req(input$manual_path)
    
    # Clean up the entered file path:
    # - remove surrounding quotes
    # - convert Windows backslashes to forward slashes
    # - normalize the path
    clean_path <- input$manual_path |>
      base::gsub('^["\']|["\']$', '', x = _) |>
      base::gsub("\\\\", "/", x = _) |> 
      base::normalizePath(winslash = "/", mustWork = FALSE)
    
    # If the cleaned path points to an existing file
    if (base::file.exists(clean_path)) {
      
      # Store the new path
      db_path(clean_path)
      
      # If a previous connection pool existed, close it
      if (!base::is.null(pool_obj())) {
        pool::poolClose(pool_obj())
      }
      
      # Create a new database connection pool
      pool_obj(
        pool::dbPool(
          drv    = RSQLite::SQLite(),
          dbname = clean_path
        )
      )
      
    } else {
      
      # Show an error notification if the file does not exist
      shiny::showNotification(
        "No DB at this location",
        type = "error"
      )
    }
  })
  
  # -------------------------------------------
  # Render a preview table of the database
  # -------------------------------------------
  output$preview <- shiny::renderTable({
    
    # Require that a connection pool has been established
    shiny::req(pool_obj())
    
    # Run a query to get the first 10 rows of the "fulldata" table
    DBI::dbGetQuery(
      pool_obj(),
      "SELECT * FROM fulldata LIMIT 100;"
    )
  })
  
  # -------------------------------------------
  # Render UI elements for database connection
  # -------------------------------------------
  output$db_ui <- shiny::renderUI({
    
    # If there is no database path yet
    if (base::is.null(db_path())) {
      
      shiny::tagList(
        shiny::h4("No valid database path found."),
        shiny::textInput("manual_path", "Please enter SQLite file path:"),
        shiny::actionButton("connect", "Connect")
      )
      
    } else {
      
      # If a database path exists, show the connected path
      shiny::h4(
        base::paste0('Connected to: "', db_path(), '"')
      )
    }
  })
  
  # -------------------------------------------
  # Clean up when the session ends
  # -------------------------------------------
  session$onSessionEnded(function() {
    
    # Isolate the pool value so it doesn't react
    pool_val <- shiny::isolate(pool_obj())
    
    # If a pool exists, close it
    if (!base::is.null(pool_val)) {
      pool::poolClose(pool_val)
    }
  })
}
