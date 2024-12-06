# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           Program Header Status  
#                   Header: X  Comments: X   Refactored: X         
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# Handles input for text field for a new database name (Builder)
observeEvent(input$sqlNameEntry, {
  # Only continue if text provided
  if (isTruthy(input$sqlNameEntry)){
    # Save input to temp variable
    tempname = input$sqlNameEntry
    # If not .sqlite, add it as extension
    if (!grepl(".sqlite", tempname)){
      tempname = paste0(tempname, ".sqlite")
    }
    # Update reactive variable for name
    db$name = tempname
  } else {
    # If no input is provided, set db$name to default value (Not truthy)
    db$name = "Default_Database.sqlite"
  }
})



openDBConnection <- function(mode){
  # Default name and path of database (DB)
  dbPath = "SQL/"
  
  if (mode == "Build" && isTruthy(db$name)){
    # Use text field entry for DB name
    nameOfDB = db$name
    # Reset the text field once used
    reset("sqlNameEntry")
  } 
  # else if (mode == "Load" && isTruthy(db$Select_SQL_Database)){
  #   # Use dropdown menu selection for DB name
  #   nameOfDB = db$Select_SQL_Database
  # } else if (mode == "Search" && isTruthy(db$Select_SQL_Database_Search)){
  #   nameOfDB = db$Select_SQL_Database_Search
  # } 
  
  # Concatenate path and name
  totalPath = paste0(dbPath, nameOfDB)
  
  # If the DB connection is active
  if (!db$dbEmpty){
    # Close connection
    dbDisconnect(con)
    con <<- NULL
    # reactive variable to show database is empty
    db$dbEmpty = TRUE
  }
  # Proceed with connection if successful disconnect and valid name provided
  if (!isTruthy(con) && isTruthy(nameOfDB)){
    # Params 1,2,3 & 5 are default
    con <<- DBI::dbConnect(RSQLite::SQLite(), user = 'root', password = '',
                           dbname = totalPath, host = 'localhost')
  } else {
    # Display expected error message here (does this ever happen?)
  }
  # Set reactive value to show DB connected 
  db$dbEmpty = FALSE
  
  # Save history to action log
  newMsg = paste0("Connected to SQL database backend<br/>",
                  "Database Name: ", nameOfDB, "<br/>",
                  "Database Path: ", dbPath)
  updateActionLog(newMsg)
  
  
  
}

legendOnly_classes <- function(q, classesToFilter){

  # print(paste0("ClassesToFilter:", classesToFilter))
  for(i in seq_along(q$x$data)){
    # print(paste0("q$x$data$name:", q$x$data[[i]]$name))
    # print(paste0("q$x$data$visible[[", i, "]]:", q$x$data[[i]]$visible))
    currName = q$x$data[[i]]$name
    
    if (currName %in% classesToFilter){
      q$x$data[[i]]$visible = "TRUE"
    } else {
      q$x$data[[i]]$visible = "legendonly"
      
    }
  }
  return(q)
}
