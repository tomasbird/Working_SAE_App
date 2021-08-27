
### Conditional panel if loading data
dataloadPanel=function(){conditionalPanel(
  condition = "input.loadnew == true",
  # Input: Select a survey file ----
  fileInput("survey.file", "Choose file for survey data",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  
  # Input: Select a census file ----
  fileInput("census.file", "Choose file for census data",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  # Horizontal line ----
  tags$hr(),
  
  # Input: Select number of rows to display ----
  radioButtons("disp", "Display",
               choices = c(Head = "head",
                           All = "all"),
               selected = "head")
)
}