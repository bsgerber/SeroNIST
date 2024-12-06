# To end the application
session$onSessionEnded(function() {
  if (isTruthy(con)){
    # Disconnect from the active database
    dbDisconnect(con)
  }
  
  # Stop the application
  stopApp()
})  
