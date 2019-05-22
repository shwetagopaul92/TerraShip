####################################
# Name: TerraShip
# Goal: To help search, submit, monitor workflows.
#
####################################

########################
# load libraries
########################
require(shiny)
require(DT)
require(AnVIL)
########################

########################
# functions + setup
########################

# function to get tool names for the specified project
getMethodConfigNames <- function(billingworkspace_name, project_name){
  ws = terra$listWorkspaceMethodConfigs(workspaceNamespace = billingworkspace_name,
                                   workspaceName = project_name,allRepos = TRUE)
  ws_tool_names = content(ws)
  ws_tool_names
  
}

# function to fetch project names under a billing group
getProjectNames <- function(billingworkspace_name){
  ws = content(terra$listWorkspaces())
  mine = sapply(ws, function(x){x$accessLevel=="PROJECT_OWNER"})
  myws_details = ws[mine]
  for(i in myws_details){
    mybilling = sapply(myws_details, function(x) {x$workspace$namespace==billingworkspace_name})
  }
  myProjectName = myws_details[mybilling]
  myProjectName
}

# function to fetch billing groups a user belongs to
getBillingWorkspace <- function(){
  ws = content(terra$listWorkspaces())
  mine = sapply(ws, function(x){x$accessLevel=="PROJECT_OWNER"})
  myws_details = ws[mine]
  billingworkspace_name = lapply(myws_details, function(x) {x$workspace$namespace})  # options for workspace namespace
  #from this get the names avaiable for the chosen billing(workspace) group
  getProjectNames(billingworkspace_name)
}

list_to_df <- function(listfordf){
  if(!is.list(listfordf)) stop("it should be a list")
  
  df <- list(list.element = listfordf)
  class(df) <- c("tbl_df", "data.frame")
  attr(df, "row.names") <- .set_row_names(length(listfordf))
  
  if (!is.null(names(listfordf))) {
    df$name <- names(listfordf)
  }
  df
}

# Setup:

########################
# Shiny
########################

TerraShip = function() {
  
  curPath=NA
  mytable=NA
  # start shiny app config
  shinyApp(
    ##########
    # start UI Config
    ##########
    ui = fluidPage(
      titlePanel("TerraShip"),
      sidebarLayout(position = "left",
                    sidebarPanel(width=2,
                                 uiOutput("billingwsnamespace_dropdown"),
                                 uiOutput("projectnames_dropdown"),
                                 uiOutput("toolnames_dropdown"),
                                 actionButton("submitButton", "Submit"),
                                 actionButton("resetButton", "Reset")
                    ),
                    mainPanel("",width=10,
                              tabsetPanel(
                                tabPanel("ConfigureWorkflow",h3("Workflow & Configure"),
                                         DT::dataTableOutput("methoddetails")),
                                         actionButton("runOnTerra", "Run"),
                                tabPanel("Monitor",h3("Monitor Job"),
                                         actionButton("monitorSubmission", "Monitor"),
                                         DT::dataTableOutput("submissionDetails")),
                                tabPanel("About", h3("About"), HTML('<br> TerraShip is a shiny interface to help search, submit, monitor workflows. 
                                                                    <br>')
                                )
                                )
                              )
                    )
      ),
      ####################
      # Start Server Config
      ####################
      server = function(input, output, session) {
        
        # dropdown for billing groups a user belongs to
        output$billingwsnamespace_dropdown <- renderUI({
          ws = content(terra$listWorkspaces())
          mine = sapply(ws, function(x){x$accessLevel=="PROJECT_OWNER"})
          myws_details = ws[mine]
          # options for workspace namespace
          workspace_name = lapply(myws_details, function(x) {x$workspace$namespace})  
          # from this get the names avaiable for the chosen billing(workspace) group
          workspacename = as.list(workspace_name)
          selectInput("workspaceNamespace", 
                      "Select Workspace Namespace",
                      choices = workspacename
          )
        })
        
        # dropdown for project names available under a billing group
        output$projectnames_dropdown <- renderUI({
          ws = content(terra$listWorkspaces())
          mine = sapply(ws, function(x){x$accessLevel=="PROJECT_OWNER"})
          myws_details = ws[mine]
          for(i in myws_details){
            mybilling = sapply(myws_details, function(x) {x$workspace$namespace==input$workspaceNamespace})
          }
          myProjectName = myws_details[mybilling]
          project_names = lapply(myProjectName, function(x) {x$workspace$name})
          myProjectNames = as.list(project_names)
          selectInput("wdlnamespace", 
                      "Select Project Name",
                      choices = myProjectNames
          )
        })
        
        # dropdown for tools names available under a billing group
        output$toolnames_dropdown <- renderUI({
          ws = terra$listWorkspaceMethodConfigs(workspaceNamespace = input$workspaceNamespace,
                                                workspaceName = input$wdlnamespace,allRepos = TRUE)
          ws_tool_names = content(ws)
          all_tool_names = lapply(ws_tool_names, function(x) {x$name})
          selectInput("name", 
                      "Select Tool",
                      choices = all_tool_names
          )
        })
        
        # show selected tool's details
        observeEvent(input$submitButton, {
          details = terra$getWorkspaceMethodConfig(input$workspaceNamespace,input$wdlnamespace
                                                   ,input$wdlnamespace, input$name)
          tooldetails = content(details)
          df = list_to_df(tooldetails)
          reordered_df = df[, c(2,1)]
          output$methoddetails = DT::renderDataTable(reordered_df)
        })
      
        
        # create and submit jobs 
        observeEvent(input$runOnTerra, {
          terra$createSubmission(
            workspaceNamespace=input$workspaceNamespace,
            workspaceName=input$wdlnamespace,
            methodConfigurationNamespace=input$wdlnamespace,
            methodConfigurationName=input$wdlname,
            useCallCache=TRUE)
          showNotification("Job created!")
        })
        
        # monitor job submission on 
        observeEvent(input$monitorSubmission, {
        subDetails = content(terra$listSubmissions(input$workspaceNamespace,input$wdlnamespace))
        for(detail in subDetails){
          mydetail = sapply(subDetails, function(x) {x$methodConfigurationName==input$name})
        }
        print(mydetail)
        mytooldetail = as.data.frame(subDetails[mydetail])
        output$submissionDetails = DT::renderDataTable(mytooldetail)
        })
        
        
  }
)}