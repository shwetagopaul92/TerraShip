######## Shiny APP ########
### TerraShip ###

#'to help search, submit, monitor workflows
#'@import shiny
#'@export
runTerraShip<-function(){
  myfile = system.file("shinyApps/TerraShipApp.R",package="TerraShip")
  source(myfile)
  TerraShip()
}