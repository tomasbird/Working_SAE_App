#
# SAE Shiny Application
## load DHS


#library(leaflet)
source("global.R", local=TRUE)
source("R/UI_functions/loaddata_fn.R",local=TRUE)

ui <- fluidPage(
    
    # Define UI for application that draws a histogram
    navbarPage(
        title = 'SAE pages',
        
        ## Data load
        # App title ----
        tabPanel("Choose files",
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
            
            # Sidebar panel for inputs ----
            sidebarPanel(
                
                titlePanel("Choose a data source"),
                
                # Input: Choose a data source
                checkboxInput("loadnew", label = "Check this box to load your own data", value = FALSE),
                
                hr(),
                fluidRow(column(3, verbatimTextOutput("value"))),
           
           #Choose data files 
           dataloadPanel(),
           
           # or use demo files from Nepal
           conditionalPanel(
               condition = "input.loadnew == false",
               p("If no data are loaded, demo data from Nepal will be used")
           )
           ),
           
            # Display Census, survey and spatial Data summaries ----
            mainPanel(
                # Output: Data file ----
                tableOutput("survey_contents")
            ))),
        
        
        ## Tab for Data view
        tabPanel('Data View', sidebarLayout(
            sidebarPanel(
                conditionalPanel(
                    'input.dataset === "Survey"',
                    #checkboxGroupInput("Ind_show_vars", "Columns in DHS to show:",
                    #                   names(DHS_ind), selected = names(DHS_ind)), 
                    uiOutput("choose_survey_vars"),
                    helpText("Click the column header to sort a column.")),
                
                conditionalPanel(
                    'input.dataset === "Census"',
                    checkboxGroupInput("Cen_show_vars", "Columns in census to show:",
                                   names(census_vars), selected = names(census_vars)), 
                    helpText("Click the column header to sort a column."))
                    ),
            
            mainPanel(
                tabsetPanel(
                    id = 'dataset',
                    tabPanel("Survey", DT::dataTableOutput("surveytable")),
                    tabPanel("Census", DT::dataTableOutput("censustable")))
                )
        )),
        
        ## Tab for Map
        tabPanel("Map View",  div(class="outer",
                                  tags$head(
                                      # Include our custom CSS
                                      includeCSS("styles.css"),
                                      includeScript("gomap.js")
                                  ),
                                  
                                  # If not using custom CSS, set height of leafletOutput to a number instead of percent
                                  leafletOutput("map", width="100%", height="100%"),
                                  
                                  # Shiny versions prior to 0.11 should use class = "modal" instead.
                                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                                width = 330, height = "auto",
                                                
                                                h2("Sample mapped output")
                                                
                                  )
        ), tags$div(id="cite",
                    'Data compiled for ', tags$em('SAE analysis for Nepal'))
        )
    ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    ## Objects for Data load Tab
    source("R/server/load_server_objects.R", local=TRUE)
    
    
    # Map outputs
    # Create the map
    output$map <- renderLeaflet({
        leaflet() %>%
            # base layers
            addTiles(group = "OSM (default)") %>%
            addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
            
            # adm areas
            addPolygons(data=adm0, group="National", weight = 2, color = "red",) %>%
            addPolygons(data=districts, group="Districts", weight = 2, color = "blue",) %>%
            addPolygons(data=subreg, group="Sub-Regions", weight = 2, color = "yellow",) %>%
            
            # sample locations
            addCircleMarkers(data=clusters, group="DHS clusters", radius=2,
                             clusterOptions = markerClusterOptions(freezeAtZoom = FALSE)) %>%
            
            # add controls
            addLayersControl(
                baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
                overlayGroups = c("National", "Sub-Regions", "Districts"),
                options = layersControlOptions(collapsed = FALSE),
                position="bottomleft"
            ) %>%
            
            # set viewing area
            setView(lng = 84.61, lat = 28.033, zoom = 7)
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
