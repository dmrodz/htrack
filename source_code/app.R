## HTrack_v0.0.111
## version 0.0.111 
## HTrack is a Shiny app built to run completely offline in the field. It is 
## made so that field team members can track houses and enter data related with 
## the status of each visit attempt, for the COPA project.

library(shiny)
library(leaflet)
library(shinydashboard)
library(leaflet.extras)
library(DT)
library(magrittr)
library(reshape2)
library(ggplot2)


# Setting UI components #
header <- dashboardHeader(
  title = h4(HTML("HTrack: A household tracking app")),
  titleWidth = 400
)

body <- dashboardBody(
  tags$head(
    tags$style(
      HTML(".shiny-notification {
            position:fixed;
            top: calc(50%);;
            left: calc(50%);;
            }
            .skin-blue 
            .main-header .logo {
             background-color: #12480D;
            }
            .skin-blue .main-header .navbar {
            background-color: #12480D;
            } 
           "
      ))),
  
  tags$style(HTML("
                  .box.box-solid.box-primary>.box-header {
                  color:#fff;
                  background:#3f3f3f
                  }
                  
                  .box.box-solid.box-primary{
                  border-bottom-color:#3f3f3f;
                  border-left-color:#3f3f3f;
                  border-right-color:#3f3f3f;
                  border-top-color:#3f3f3f;
                  background:#dcdcdc;
                  }
                  ")),
  tabsetPanel(
    # MAIN TAB
    tabPanel(title = "Main",
             # Map location
             fluidRow(
               column(width = 8, solidHeader = TRUE,
                      box(width = NULL, solidHeader = TRUE, 
                          leafletOutput("map", height = 600)
                      )
               ),
               tags$script("
                           Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                           Shiny.onInputChange(variableName, null);
                           });
                           "),
               
               # Right Column: Cluster selection and Data Entry
               column(width = 4,
                      box(width = NULL, status = "primary", solidHeader = TRUE,
                          strong(div("To save data successfully: (1) Choose Cluster to activate map.", 
                                     style = "color:#B22222; margin-left: 5px;")),
                          selectInput("cluster", label = "Cluster", 
                                           choices = c(" ", "XX"),
                                           selected = " "),
                          strong(div("(2) Choose Structure marker in the map before entering data.", 
                                     style = "color:#B22222; margin-left: 5px;")),
                          selectInput("struc", 
                                     label = "Structure type", 
                                     choices = c(" ",
                                                 "House - one level", 
                                                 "House - two levels",
                                                 "Condo or Apartment", 
                                                 "1st floor apt in house",
                                                 "2nd floor apt in house",
                                                 "Other"),
                                     selected = " "),
                          
                          selectInput("status", 
                                      label = "Status",
                                      choices = c(" ",
                                                  "Needs follow-up",
                                                  "Visit completed"),
                                      selected = " "),
                          
                          uiOutput("details"),
                          
                          dateInput("date", 
                                    label = "Follow-up date",
                                    format = "yyyy-mm-dd",
                                    max = Sys.Date() + 60,
                                    value = NA),

                          textInput("notes", 
                                    label = "Notes",
                                    value = " "),
                          
                          textInput("initials", 
                                    label = "Initials",
                                    value = " "),
                          
                          actionButton("saveData", 
                                       label = HTML("Save <br />data"),
                                       icon = icon("save"), 
                                       style ="color: #fff; background-color: #D55E00; border-color: #D55E00; 
                                       width:45%; margin-left: 15px;"),
                          
                          actionButton("mapRefresh", 
                                       label = HTML("Update <br />map"), icon = icon("refresh"), 
                                       style ="color: #fff; background-color: #D55E00; border-color: #D55E00; 
                                       width:45%; margin-left: 5px; float: left;")
                          
                          )
                      )
               ),
             
             # Data table location
               fluidRow(
                 column(width = 10, solidHeader = TRUE,
                      box(width = NULL,
                          div(dataTableOutput("table"), style = "font-size:100%")
                         ) 
                    ),
                 column(width = 2, solidHeader = TRUE,
                     box(width = NULL,
                          uiOutput("EpiInfo")
                                   )
                          )
                 )
             ),
  
  # EDITIONS TAB
  tabPanel(title = "Edit",
           fluidRow(
             column(width = 8,
                    box(width = NULL,
                        dataTableOutput("hot")
                    )
             ),
             column(width = 4,
                    box(width = NULL, status = "primary", solidHeader = TRUE,
                        strong(div("Press a row to continue", 
                                   style = "color:#D55E00; margin-left: 5px;")),
                        
                         selectInput("ed_struc", 
                                     label = "Structure type", 
                                     choices = c(" ",
                                                 "House - one level", 
                                                 "House - two levels",
                                                 "Condo or Apartment", 
                                                 "1st floor apt in house",
                                                 "2nd floor apt in house",
                                                 "Other"),
                                     selected = " "),
                        
                        selectInput("ed_status", 
                                    label = "Status",
                                    choices = c(" ",
                                                "Needs follow-up",
                                                "Visits completed"),
                                    selected = " "),
                        
                        uiOutput("ed_details"),
                        
                        dateInput("ed_date", 
                                  label = "Follow-up date",
                                  format = "yyyy-mm-dd",
                                  min = Sys.Date(),
                                  max = Sys.Date() + 60,
                                  value = NA),
                        

                        textInput("ed_notes", 
                                  label = "Notes",
                                  value = " "),
                        
                        textInput("ed_initials", 
                                  label = "Initials",
                                  value = " "),

                        actionButton("saveEdits", "Save changes", icon = icon("save"), 
                                     style ="color: #fff; background-color: #D55E00; border-color: #D55E00; 
                                     width:45%; margin-left: 15px; float: left;"),
                        
                        actionButton("deleteRow", "Delete row", icon = icon("edit"), 
                                     style ="color: #fff; background-color: #CC79A7; border-color: #CC79A7; 
                                     width:45%; margin-left: 5px;")
                        )
                        )
             )
  ),
  
  # STATS TAB 
  tabPanel(title = "Statistics",
             fluidRow(
               column(width = 8, solidHeader = TRUE,
                      box(width = NULL, solidHeader = TRUE, 
                          plotOutput("stats", height = 300)
                      )
                     ),
               
               column(width = 4,
                      box(width = NULL, status = "primary", solidHeader = TRUE,
                          strong(div("Press 'See graph' to view all visits per cluster", 
                                     style = "color:#B22222; margin-left: 5px;")),
                          tags$br(),
                          strong(div("For visit status per date, select cluster and press 'See graph' again.", 
                                     style = "color:#B22230; margin-left: 5px;")),
                  
                          selectInput("stats.cluster", label = "Cluster", 
                                           choices = c(" ", "XX"),
                                           selected = " "),
                          actionButton("showStats", "See graph", icon = icon("chart-bar"), 
                                     style ="color: #fff; background-color: #D55E00; border-color: #D55E00; 
                                             width:45%; margin-left: 15px; float: left;"),
                          
                          actionButton("download_graph", "Save graph", icon = icon("save"),
                                       style ="color: #fff; background-color: #D55E00; border-color: #D55E00; 
                                             width:45%; margin-left: 15px; float: left;"),
                          tags$br(),
                          tags$br(),
                          
                          actionButton("showTable", "See table", icon = icon("chart-bar"), 
                                     style ="color: #fff; background-color: #D55E00; border-color: #D55E00; 
                                             width:45%; margin-left: 15px; float: left;"),
           
                          actionButton("download_table", "Show table", icon = icon("save"),
                                       style ="color: #fff; background-color: #D55E00; border-color: #D55E00; 
                                             width:45%; margin-left: 15px; float: left;")))),
           fluidRow(
               column(width = 12,
                      box(width = NULL,
                          dataTableOutput("stable")
                      )))

             ), 
  # DEVELOPER TAB
  
  tabPanel(title = "Dev",
           passwordInput("password", "Password: "),
           actionButton("login", "Login"),
           verbatimTextOutput("copadev"),
           conditionalPanel("input.password == 'copadev' && input.login && !output.hide_panel", 
                            fileInput("sel_file", "Select file containing new selection",
                                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                            strong(div("Choose cluster where new selections will be added", 
                                       style = "color:#D55E00; margin-left: 5px;")),
                            selectInput("current_cluster", 
                                        label = "Cluster", 
                                        choices = c(" ", "XX"),
                                        selected = " "),
                            actionButton("new_selection", 
                                         label = HTML("Add new <br />selection"),
                                         style = "color: #fff; background-color: #D55E00; border-color: #D55E00; width:45%; margin-left: 15px;")
                            )
           )
  )
)


ui <- dashboardPage(header, dashboardSidebar(disable = TRUE), body) 



###### SERVER SETTINGS #####
server <- shinyServer(
  
  function(input, output, session) {
    
    #### MAP ####
    ## Map Tile Location
    start <- eventReactive(input$cluster, {
          
          if(!file.exists("preload.csv")) {
            showNotification(paste("Preload file not found!"),
                           type = "error",
                           duration = 4)
          } else {
          
          selection <- read.csv("preload.csv", header = TRUE, stringsAsFactors = FALSE)
          
          if(!input$cluster %in% c(selection$CLUSTER, "", " ")) {
            showNotification(paste("Data not available, select another cluster"),
                             type = "error",
                             duration = 4)
          } else {
            if(input$cluster %in% selection$CLUSTER) {
             selection <- selection[selection$CLUSTER == input$cluster, ]   
            } else {
              selection <- selection
            }

             hids <- data.frame(
                REF = 1:nrow(selection),
                CLUSTER = as.character(selection$CLUSTER),
                HHID = selection$HHID, 
                LONG = selection$LONG, 
                LAT = selection$LAT)
          
              hids$VISIT <- 0
              hids$VISIT_DATETIME <- NA
              hids$STRUCTURE <- NA
              hids$STATUS <- NA
              hids$DETAILS <- NA
              hids$FU_DATE <- NA
              hids$INITIALS <- NA
              hids$NOTES <- NA
    
              hids <- hids[, c("REF", "HHID", "LONG", "LAT", 
                               "VISIT", "STRUCTURE", "VISIT_DATETIME", 
                               "STATUS", "DETAILS", "FU_DATE", 
                               "NOTES", "INITIALS", "CLUSTER")]
          }
      }
    })
    
    
    map <- eventReactive(input$cluster, {
      if(file.exists(paste0("htrack_single_", input$cluster, ".csv"))) {
        df <- read.csv(paste0("htrack_single_", input$cluster, ".csv"), header=TRUE, stringsAsFactors = FALSE)
      } else {
        hids <- start()
        df <- hids
      }
      
      if(input$cluster %in% c("", " ")) {
        df <- df
      } else {
        df <- df[which(df$CLUSTER == input$cluster), ]
      }
      
      ## Adding color codes to encounter status
      df$cols <- ifelse(is.na(df$STATUS), "#0072B2",
                 ifelse(df$STATUS == "Needs follow-up" & is.na(df$FU_DATE), "#D55E00",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE == "", "#D55E00",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE != as.character(Sys.Date()), "#808080",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE == as.character(Sys.Date()), "#CC79A7",
                 ifelse(df$STATUS == "Visit completed", "#009E73",
                        "#0072B2"))))))
      df$vcol <- ifelse(df$FU_DATE < as.character(Sys.Date()) &                         
                        df$FU_DATE != "", "#ff0318", "#000000")
      
      ############################################################
      #### To use offlnie map tiles, use the code in line 360 ####
      ############################################################

      # addResourcePath("mytiles", "/storage/internal/htrack/mapTiles/ALL/OSM")
                    
      ## Produce leaflet map
      map <- leaflet(df) %>%
        addTiles() %>%
      ##########################################################
      #### To use offlnie map tiles, use the following code ####
      #### and comment out line 369                         #### 
      ##########################################################
        # addTiles(urlTemplate = "mytiles/{z}_{x}_{y}.png") %>%
        setView(lng = mean(df$LONG), lat = mean(df$LAT), zoom = 17) %>%
        addCircleMarkers(
          lat = ~ LAT,
          lng = ~ LONG,
          radius = 8,
          color = ~ as.character(vcol),
          opacity = 1,
          weight = 2,
          fillColor = ~ as.character(cols),
          fillOpacity = 1,
          layerId = ~ REF,
          label = ~ HHID) %>%
        addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                           autoCenter = FALSE, maxZoom = 20, 
                                           setView = FALSE))
      activateGPS(map)
    })
    
    # Produce Map
    output$map <- renderLeaflet({
      map()
    })
    

    # REACTIVE DATA TABLE
    dtable <- eventReactive(
      {input$saveData
        input$deleteRow
         input$saveEdits
          input$cluster}, {
          
          if(file.exists(paste0("htrack_single_", input$cluster, ".csv"))) {
            df <- read.csv(paste0("htrack_single_", input$cluster, ".csv"), 
                           header=TRUE, stringsAsFactors = FALSE)
          } else {
            hids <- start()
            df <- hids
          }
          
          if(input$cluster %in% c("", " ")) {
            df <- df
          } else {
            df <- df[which(df$CLUSTER == input$cluster), ]
          }
          # Ordering the data frame everytime the user saves, helps maintain
          # REF order for table & marker selection  
          df <- as.data.frame(df[order(df$REF), ])
          # Create reactive data table
          # Activate row selection for later map-click row selection
          DT::datatable(df,
                        selection = "single",
                        rownames = FALSE,
                        callback = JS("table.on('click.dt', 'tr', function() {
                                      Shiny.onInputChange('row', table.rows(this).data().toArray());
                                      });"),
              
                    options = list(scrollX = TRUE,
                                   stateSave = TRUE,
                                   dom = "ftp"))
          }, ignoreNULL = FALSE)
    
    # Render table
    output$table <- renderDataTable({
      dtable()
    })

    
    
    # MAP CLICK TRIGGERS ROW SELECTION ON TABLE SUBSET
    observeEvent(input$map_marker_click, {
      # Read in data & filter to most recent visit attempt
      if(file.exists(paste0("htrack_single_", input$cluster, ".csv"))) {
        df <- read.csv(paste0("htrack_single_", input$cluster, ".csv"), header=TRUE, stringsAsFactors = FALSE)
      } else {
        hids <- start()
        df <- hids
      }
      
      if(input$cluster %in% c("", " ")) {
        df <- df
      } else {
        df <- df[which(df$CLUSTER == input$cluster), ]
      }
      
      # Read in cumulative to pop up a Possible Sat visit
      if(file.exists(paste0("htrack_cumulative_", input$cluster, ".csv"))) {
        bf <- read.csv(paste0("htrack_cumulative_", input$cluster, ".csv"), header=TRUE, stringsAsFactors = FALSE)
      }
      
      if(!is.null(input$map_marker_click)) {
        click <- input$map_marker_click  
        clickId <- input$map_marker_click$id
        
        # Call data table proxy & select row and corresponding data table page
        dataTableProxy("table", session) %>%
          selectRows(which(df$REF == clickId)) %>%
          selectPage(which(df$REF == clickId) %/% input$table_state$length + 1)
        
        # Adding color to encounter status
        df$cols <- ifelse(is.na(df$STATUS), "#0072B2",
                   ifelse(df$STATUS == "Needs follow-up" & is.na(df$FU_DATE), "#D55E00",
                   ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE == "", "#D55E00",
                   ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE != as.character(Sys.Date()), "#808080",
                   ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE == as.character(Sys.Date()), "#CC79A7",
                   ifelse(df$STATUS == "Visit completed", "#009E73",
                          "#0072B2"))))))
        df$vcol <- ifelse(df$FU_DATE < as.character(Sys.Date()) &                         
                            df$FU_DATE != "", "#ff0318", "#000000")
        
        df <- df[which(df$REF == clickId), ]

        # Using map proxy to update map icon colors
        map <- leafletProxy("map", data = df, session) %>%
          clearMarkers() %>%
          setView(lng = df$LONG, lat = df$LAT, zoom = 17) %>%
          addCircleMarkers(
            lat = ~ LAT,
            lng = ~ LONG,
            radius = 8,
            color = ~ as.character(vcol),
            opacity = 1,
            weight = 2,
            fillColor = ~ as.character(cols),
            fillOpacity = 1,
            layerId = ~ REF,
            label = ~ HHID,
            labelOptions = labelOptions(noHide = TRUE)) %>%
          addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                             autoCenter = FALSE, maxZoom = 17, 
                                             setView = FALSE))
        activateGPS(map)
        

        updateSelectInput(session, "ed_status", 
                          label = "Status",
                          choices = c(" ",
                                      "Needs follow-up",
                                      "Visit completed"),
                          selected = df$STATUS)
      }
    })
    

    # Row click and house marker selection for navigation purposes only
    observeEvent(input$row, {
      if(file.exists(paste0("htrack_single_", input$cluster, ".csv"))) {
        df <- read.csv(paste0("htrack_single_", input$cluster, ".csv"), header=TRUE, stringsAsFactors = FALSE)
      } else {
        hids <- start()
        df <- hids
      }
      
      if(input$cluster %in% c("", " ")) {
        df <- df
      } else {
        df <- df[which(df$CLUSTER == input$cluster), ]
      }
      
      # Adding color to encounter status
      df$cols <- ifelse(is.na(df$STATUS), "#0072B2",
                 ifelse(df$STATUS == "Needs follow-up" & is.na(df$FU_DATE), "#D55E00",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE == "", "#D55E00",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE != as.character(Sys.Date()), "#808080",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE == as.character(Sys.Date()), "#CC79A7",
                 ifelse(df$STATUS == "Visit completed", "#009E73",
                        "#0072B2"))))))
      df$vcol <- ifelse(df$FU_DATE < as.character(Sys.Date()) &                         
                          df$FU_DATE != "", "#ff0318", "#000000")
        
        rowid <- input$row[1]
        df <- df[which(df$REF == rowid), ]
        
        ## Using map proxy to update map icon colors
        map <- leafletProxy("map", data = df, session) %>%
          clearMarkers() %>%
          setView(lng = df$LONG, lat = df$LAT, zoom = 17) %>%
          addCircleMarkers(
            lat = ~ LAT,
            lng = ~ LONG,
            radius = 8,
            color = ~ as.character(vcol),
            opacity = 1,
            weight = 2,
            fillColor = ~ as.character(cols),
            fillOpacity = 1,
            layerId = ~ REF,
            label = ~ HHID,
            labelOptions = labelOptions(noHide = FALSE)) %>%
          addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                             autoCenter = FALSE, maxZoom = 17, 
                                             setView = FALSE))
        activateGPS(map)
         updateSelectInput(session, "struc", label = "Structure type", 
                           choices = c(" ",
                                       "House - one level", 
                                       "House - two levels",
                                       "Condo or Apartment", 
                                       "1st floor apt in house",
                                       "2nd floor apt in house",
                                       "Other"),
                           selected = df$STRUCTURE)
         updateSelectInput(session, "ed_struc", label = "Structure type", 
                           choices = c(" ",
                                       "House - one level", 
                                       "House - two levels",
                                       "Condo or Apartment", 
                                       "1st floor apt in house",
                                       "2nd floor apt in house",
                                       "Other"),
                           selected = df$STRUCTURE)
        
        updateSelectInput(session, "ed_status", 
                          label = "Status",
                          choices = c(" ",
                                      "Needs follow-up",
                                      "Visit completed"),
                          selected = df$STATUS)
    })
    
    # Conditional visit details for Main tab
    output$details <- renderUI({
      if(input$status == "Needs follow-up") {
        selectInput("details", "Details",
                    choices = c(" ",
                                "No one home - attempt 1",
                                "No one home - attempt 2",
                                "Requested visit at later date",
                                "Recruited - return for more members",
                                "Wrong home"),
                    selected = " ")
      } else {
        selectInput("details", "Details",
                    choices = c(" ",
                                "Recruited",
                                "Reached max visit attempts",
                                "Vacant",
                                "Not a home",
                                "HHID repeated",
                                "Not eligible - over 50",
                                "Not eligible - moving",
                                "Not eligible - infrequent stays",
                                "Not eligible - other",
                                "Refused - will not provide sample",
                                "Refused - does not want to spend time",
                                "Refused - medical contraindication",
                                "Refused - other",
                                "No contact - safety concerns"),
                    selected = " ")
      }
    })
    
    # Conditional visit details for Editions tab
    output$ed_details <- renderUI({
      if(input$ed_status == "Needs follow-up") {
        selectInput("details", "Details",
                    choices = c(" ",
                                "No one home - attempt 1",
                                "No one home - attempt 2",
                                "Requested visit at later date",
                                "Recruited - return for more members",
                                "Wrong home"),
                    selected = " ")
      } else {
        selectInput("details", "Details",
                    choices = c(" ",
                                "Recruited",
                                "Reached max visit attempts",
                                "Vacant",
                                "Not a home",
                                "HHID repeated",
                                "Not eligible - over 50",
                                "Not eligible - moving",
                                "Not eligible - infrequent stays",
                                "Not eligible - other",
                                "Refused - will not provide sample",
                                "Refused - does not want to spend time",
                                "Refused - medical contraindication",
                                "Refused - other",
                                "No contact - safety concerns"),
                    selected = " ")
      }
    })

    
    # DATA SAVING AND VALIDATIONS
    save <- eventReactive(input$map_marker_click, {
      
      if(file.exists(paste0("htrack_single_", input$cluster, ".csv"))) {
        df <- read.csv(paste0("htrack_single_", input$cluster, ".csv"))
      } else {
        hids <- start()
        df <- hids
      }
      
      if(input$cluster %in% c("", " ")) {
        df <- df
      } else {
        df <- df[which(df$CLUSTER == input$cluster), ]
      }
      
      if(file.exists(paste0("htrack_cumulative_", input$cluster, ".csv"))) {
        cumulative_records <- read.csv(paste0("htrack_cumulative_", input$cluster, ".csv"))
      } 

      if(is.null(input$status) | input$status == " ") {
        showNotification(paste("Missed Status. Select home and try again!"),
                         type = "error",
                         duration = 5)  
      } else {

      if(is.null(input$details) | input$details == " ") {
         showNotification(paste("Missing Detials. Select home and try again!"),
                          type = "error",
                          duration = 5)
      } else {
      if(is.null(input$initials) | input$initials == " ") {
         showNotification(paste("You forgot your initials!"),
                          type = "error",
                          duration = 5)
      } else {
      if(input$details == "Not elegible - other" & (input$notes == " " | is.na(input$notes))) {
         showNotification(paste("Add a reason in Notes. Select home and try again!"),
                          type = "error",
                          duration = 5)
      } else {
      if(!is.null(input$map_marker_click)) {
        clickId <- input$map_marker_click$id
        
        newrow <- data.frame(
          REF = clickId,
          HHID = df$HHID[which(df$REF == clickId)],
          LONG = df$LONG[which(df$REF == clickId)],
          LAT = df$LAT[which(df$REF == clickId)],
          VISIT = df$VISIT[which(df$REF == clickId)] + 1,
          STRUCTURE = df$STRUCTURE[which(df$REF == clickId)],
          VISIT_DATETIME = as.character(as.POSIXlt(Sys.time(), "EST")[1]),
          STATUS = input$status,
          DETAILS = input$details,
          FU_DATE = ifelse(grepl("follow-up", input$status) & isTruthy(input$date),
                             as.character(input$date), ""),
          NOTES = input$notes,
          INITIALS = input$initials,
          CLUSTER = df$CLUSTER[which(df$REF == clickId)]
          )
      
       # Save ALL parcel encounters
       if(exists("cumulative_records")) {
          DF <- as.data.frame(rbind(cumulative_records, newrow))
          DF <- DF[order(as.integer(DF$REF), abs(as.integer(DF$VISIT))), ]
       } else {
          DF <- as.data.frame(rbind(df, newrow))
          DF <- DF[order(as.integer(DF$REF), abs(as.integer(DF$VISIT))), ]
       }
       # Save big file
       write.csv(DF, paste0("htrack_cumulative_", input$cluster, ".csv"), row.names = FALSE)
       # Save daily with dateStamp
       write.csv(DF, paste0("htrack_archive/htrack_cumulative_", input$cluster, sep = "_", as.character(Sys.Date()), sep=".csv"), row.names = FALSE)
        
       # Save most recent encounter -- this will be shown on main app 
       subDF <- DF[order(as.integer(DF$REF), -abs(as.integer(DF$VISIT))), ]
       subDF <- subDF[!duplicated(as.integer(subDF$REF)), ]
       write.csv(subDF, paste0("htrack_single_", input$cluster, ".csv"), row.names = FALSE)
      
       # Small pop up stating if data has been saved.
       showNotification(paste("Data saved successfully!"),
                        type = "message",
                        duration = 4)
           }
          }
         }
        }
       }
      })
    
    
    # Activate data saving
    observeEvent(input$saveData, {
      save()
      if(file.exists(paste0("htrack_single_", input$cluster, ".csv"))) {
        df <- read.csv(paste0("htrack_single_", input$cluster, ".csv"), header=TRUE, stringsAsFactors = FALSE)
      } else {
        hids <- start()
        df <- hids
      }
      
      if(input$cluster %in% c("", " ")) {
        df <- df
      } else {
        df <- df[which(df$CLUSTER == input$cluster), ]
      }
      
      df$cols <- ifelse(is.na(df$STATUS), "#0072B2",
                 ifelse(df$STATUS == "Needs follow-up" & is.na(df$FU_DATE), "#D55E00",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE == "", "#D55E00",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE != as.character(Sys.Date()), "#808080",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE == as.character(Sys.Date()), "#CC79A7",
                 ifelse(df$STATUS == "Visit completed", "#009E73",
                        "#0072B2"))))))
      df$vcol <- ifelse(df$FU_DATE < as.character(Sys.Date()) &                         
                          df$FU_DATE != "", "#ff0318", "#000000")
      
      ## Using map proxy to update map icon colors
      map <- leafletProxy("map", data = df, session) %>%
        clearMarkers() %>%
        setView(lng = mean(df$LONG), lat = mean(df$LAT), zoom = 17) %>%
        addCircleMarkers(
          lat = ~ LAT, 
          lng = ~ LONG, 
          radius = 8,
          color = ~ as.character(vcol),
          opacity = 1,
          weight = 2,
          fillColor = ~ as.character(cols),
          fillOpacity = 1,
          layerId = ~ REF,
          label = ~ HHID) %>%
        addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                           autoCenter = FALSE, maxZoom = 20, 
                                           setView = FALSE))
      activateGPS(map)
      
      ## Reset all inputs
      session$sendCustomMessage(type = "resetValue", message = "details")
      
       updateSelectInput(session, "struc", label = "Structure", 
                   choices = c(" ",
                               "House - one level", 
                               "House - two levels",
                               "Condo or Apartment", 
                               "1st floor apt in house",
                               "2nd floor apt in house",
                               "Other"),
                   selected = " ")
      
      updateSelectInput(session, "status", 
                  label = "Status",
                  choices = c(" ",
                              "Needs follow-up",
                              "Visit completed"),
                  selected = " ")
      
      updateDateInput(session, "date", 
                label = "Follow-up date",
                min = Sys.Date(),
                max = Sys.Date() + 60,
                value = NA)

      updateTextInput(session, "notes", 
                label = "NOTES",
                value = " ")
      
    })
    
   
    # Statistics tab
    stats.plot <- eventReactive(input$showStats, {
      files <- list.files(path = "htrack_archive/", pattern = "cumulative", full.names = TRUE)
      list <- lapply(files, function(x) read.csv(x, header = TRUE, stringsAsFactors = FALSE))
      reads <- Reduce(function(...) merge(..., all = TRUE), list)
      reads <- reads[!duplicated(reads), ]
      sin <- reads[order((reads$HHID), -abs(as.integer(reads$VISIT))), ]
      sin <- sin[!duplicated((sin$HHID)), ]
      sin$STATUS = ifelse(is.na(sin$STATUS), "Needs initial visit", sin$STATUS)
      sin$DETAILS = ifelse(is.na(sin$DETAILS), "Needs initial visit", sin$DETAILS)
      sin$VISIT_DATETIME = as.Date(sin$VISIT_DATETIME, "%Y-%m-%d")
      sin2 <- sin[sin$STATUS != "Needs initial visit", ]
      
      if(is.null(input$stats.cluster) | input$stats.cluster == " ") {
        ggplot(sin, aes(x = DETAILS, fill = CLUSTER)) +
          geom_histogram(stat = "count") +
          theme_classic() +
          scale_fill_discrete() +
          theme(legend.position="bottom") +
          xlab("") +
          coord_flip()
      } else {
        sin2 <- sin2[sin2$CLUSTER == input$stats.cluster, ]
        ggplot(sin2, aes(x = VISIT_DATETIME, fill = DETAILS)) +
          geom_histogram() +
          theme_classic() +
          scale_fill_discrete() +
          theme(legend.position="bottom") +
          xlab("") + 
          coord_flip()
      }
    }, ignoreNULL = FALSE)
    
    output$stats <- renderPlot({
      stats.plot()
    })
    
    # Render table
    s.table <- eventReactive(input$showTable, {
          
       if(is.null(input$stats.cluster) | input$stats.cluster == " ") {
         files <- list.files(path = "htrack_archive/", pattern = "cumulative", full.names = TRUE)
         list <- lapply(files, function(x) read.csv(x, header = TRUE, stringsAsFactors = FALSE))
         reads <- Reduce(function(...) merge(..., all = TRUE), list)
         reads <- reads[!duplicated(reads), ]
         sin <- reads[order((reads$HHID), -abs(as.integer(reads$VISIT))), ]
         sin <- sin[!duplicated((sin$HHID)), ]
         sin$STATUS = ifelse(is.na(sin$STATUS), "Needs initial visit", sin$STATUS)
         sin$DETAILS = ifelse(is.na(sin$DETAILS), "Needs initial visit", sin$DETAILS)
         sin$VISIT_DATETIME = as.Date(sin$VISIT_DATETIME, "%Y-%m-%d")  
         stats <- NULL
           for(i in unique(sin$CLUSTER)) {
             stat = as.data.frame(table(DETAILS = sin$DETAILS, Status = sin$STATUS))
             stat.df <- as.data.frame(stat)
             stat.dfw <- dcast(stat.df, DETAILS ~ Status, value.var = "Freq")
             
             stat.dfw$Cluster = i
             stats[[i]] <- stat.dfw
           }
         wide.stat <- Reduce(function(...) merge(..., all = TRUE), stats)
         wide.stat <- as.data.frame(wide.stat)
         
         DT::datatable(wide.stat,
                       options = list(scrollX = TRUE,
                                 stateSave = TRUE,
                                 dom = "ftp"))

        } else {
          files <- list.files(path = "htrack_archive/", pattern = "cumulative", full.names = TRUE)
          list <- lapply(files, function(x) read.csv(x, header = TRUE, stringsAsFactors = FALSE))
          reads <- Reduce(function(...) merge(..., all = TRUE), list)
          reads <- reads[!duplicated(reads), ]
          sin <- reads[order((reads$HHID), -abs(as.integer(reads$VISIT))), ]
          sin <- sin[!duplicated((sin$HHID)), ]
          sin$STATUS = ifelse(is.na(sin$STATUS), "Needs initial visit", sin$STATUS)
          sin$DETAILS = ifelse(is.na(sin$DETAILS), "Needs initial visit", sin$DETAILS)
          sin$VISIT_DATETIME = as.Date(sin$VISIT_DATETIME, "%Y-%m-%d")
          sin2 <- sin[sin$STATUS != "Needs initial visit", ]
          stats <- as.data.frame.matrix(table(sin2$VISIT_DATETIME, sin2$DETAILS))
          stats$`Fecha de VISIT` <- rownames(stats)
          rownames(stats) <- c()
          stats$Cluster = input$stats.cluster
          stats <- stats[, c(ncol(stats), ncol(stats)-1, 1:(ncol(stats)-2))]
          stats$Total <- rowSums(stats[-c(1, 2)])
          wide.stat <- as.data.frame(stats)
          
          DT::datatable(wide.stat,
                        options = list(scrollX = TRUE,
                                  stateSave = TRUE,
                                  dom = "ftp"))
         }
     }, ignoreNULL = FALSE)
    
 
   output$stable <- renderDataTable({
      s.table()
    })
   
   
   stat.graph <- eventReactive(input$download_graph, {
      files <- list.files(path = "htrack_archive/", pattern = "cumulative", full.names = TRUE)
      list <- lapply(files, function(x) read.csv(x, header = TRUE, stringsAsFactors = FALSE))
      reads <- Reduce(function(...) merge(..., all = TRUE), list)
      reads <- reads[!duplicated(reads), ]
      sin <- reads[order((reads$HHID), -abs(as.integer(reads$VISIT))), ]
      sin <- sin[!duplicated((sin$HHID)), ]
      sin$STATUS = ifelse(is.na(sin$STATUS), "Needs initial visit", sin$STATUS)
      sin$DETAILS = ifelse(is.na(sin$DETAILS), "Needs initial visit", sin$DETAILS)
      sin$VISIT_DATETIME = as.Date(sin$VISIT_DATETIME, "%Y-%m-%d")
      sin2 <- sin[sin$STATUS != "Needs initial visit", ]
      
      if(is.null(input$stats.cluster) | input$stats.cluster == " ") {
        ggplot(sin, aes(x = DETAILS, fill = CLUSTER)) +
          geom_histogram(stat = "count") +
          theme_classic() +
          scale_fill_discrete() +
          theme(legend.position="bottom") +
          xlab("") +
          coord_flip()
        ggsave(paste0("Status per cluster - ", Sys.Date(), ".png"))
      } else {
        sin2 <- sin2[sin2$CLUSTER == input$stats.cluster, ]
        ggplot(sin2, aes(x = VISIT_DATETIME, fill = DETAILS)) +
          geom_histogram() +
          theme_classic() +
          scale_fill_discrete() +
          theme(legend.position="bottom") +
          xlab("") + 
          coord_flip()
        ggsave(paste0("Status by visit date - ", input$stats.cluster, " - ", Sys.Date(), ".png"))
      }
      showNotification("Graph saved successfully!")

    })

   
    stat.table <- eventReactive(input$download_table, {
       files <- list.files(path = "htrack_archive/", pattern = "cumulative", full.names = TRUE)
       list <- lapply(files, function(x) read.csv(x, header = TRUE, stringsAsFactors = FALSE))
       reads <- Reduce(function(...) merge(..., all = TRUE), list)
       reads <- reads[!duplicated(reads), ]
       sin <- reads[order((reads$HHID), -abs(as.integer(reads$VISIT))), ]
       sin <- sin[!duplicated((sin$HHID)), ]
       sin$STATUS = ifelse(is.na(sin$STATUS), "Needs initial visit", sin$STATUS)
       sin$DETAILS = ifelse(is.na(sin$DETAILS), "Needs initial visit", sin$DETAILS)
       sin$VISIT_DATETIME = as.Date(sin$VISIT_DATETIME, "%Y-%m-%d")
       sin2 <- sin[sin$STATUS != "Needs initial visit", ]
       
       if(input$stats.cluster == " " | is.null(input$stats.cluster)) {
         stats <- NULL
         for(i in unique(sin$CLUSTER)) {
           stat = as.data.frame(table(DETAILS = sin$DETAILS, Status = sin$STATUS))
           stat.df <- as.data.frame(stat)
           stat.dfw <- dcast(stat.df, DETAILS ~ Status, value.var = "Freq")
           
           stat.dfw$Cluster = i
           stats[[i]] <- stat.dfw
         }
         wide.stat <- Reduce(function(...) merge(..., all = TRUE), stats)
         write.csv(wide.stat, paste0("Visit details per cluster - ",
                                      Sys.Date(), ".csv"), na = "", row.names=F)
         
       } else {
         stats <- as.data.frame.matrix(table(sin2$VISIT_DATETIME, sin2$DETAILS))
         stats$`Fecha de VISIT` <- rownames(stats)
         rownames(stats) <- c()
         stats$Cluster = input$stats.cluster
         stats <- stats[, c(ncol(stats), ncol(stats)-1, 1:(ncol(stats)-2))]
         stats$Total <- rowSums(stats[-c(1, 2)])
         write.csv(stats, paste0("Visit details per cluster - ", input$stats.cluster, " - ",
                                 Sys.Date(), ".csv"), na = "", row.names=F)
         
       }
       showNotification("Table saved succesfully!")
     })
    
    observeEvent(input$download_graph, {
      stat.graph()
    })

    observeEvent(input$download_table, {
      stat.table()
    })
    
        
    # Link to EpiInfo
    output$EpiInfo <- renderUI({
      
      if(!is.null(input$map_marker_click)) {
          if(file.exists(paste0("htrack_single_", input$cluster, ".csv"))) {
              df <- read.csv(paste0("htrack_single_", input$cluster, ".csv"), header=TRUE, stringsAsFactors = FALSE)
          } else {
              hids <- start()
              df <- hids
          }
        
        if(input$cluster %in% c("", " ")) {
          df <- df
        } else {
          df <- df[which(df$CLUSTER == input$cluster), ]
        }
        
        df <- df[which(df$REF == input$map_marker_click$id), ]
        
        ################################################################
        #### This will vary according to how EpiInfo is programmed. ####
        ################################################################
        
        tags$div(class = "header", checked = NA,
                 tags$a(href = paste0("epi://info/?form=COPABY2&HHID='", df$HHID, "'"), "OPEN IN EPI INFO")
            )
      } else {
        p("Select structure marker in map to activate.")
      }
    })
    
    
    # ON ACTION BUTTON, AFTER LOCATION SELECTION, POP UP ALL ATTEMPTS OF PARCEL ####
    pop <- eventReactive(
      {input$map_marker_click
        input$deleteRow
         input$saveEdits
          input$cluster}, {
          if(file.exists(paste0("htrack_cumulative_", input$cluster, ".csv"))) {
            df <- read.csv(paste0("htrack_cumulative_", input$cluster, ".csv"), header = TRUE, stringsAsFactors = FALSE)
          } else {
            hids <- start()
            df <- hids
          }
            
          if(input$cluster %in% c("", " ")) {
            df <- df
          } else {
            df <- df[which(df$CLUSTER == input$cluster), ]
          }
            
          df <- df[which(df$REF == input$map_marker_click$id), ]
          
          ## Ordering the data frame everytime the user saves, helps in maintaining 
          ## REF order for table & marker selection  
          df <- as.data.frame(df[order(df$VISIT), ])
          DT::datatable(df,
                        selection = "single",
                        rownames = FALSE,
                        callback = JS("table.on('click.dt', 'tr', function() {
                                      Shiny.onInputChange('rows', table.rows(this).data().toArray());
                                      });"),
                        options = list(scrollX = TRUE,
                                       stateSave = TRUE,
                                       bPaginate = FALSE))
          }, ignoreNULL = FALSE)
    
    output$hot <- renderDataTable({
      pop()
    })
    
    
    # DATA EDITS
    saveds <- eventReactive(
      {input$rows
        input$cluster}, {
      df <- read.csv(paste0("htrack_cumulative_", input$cluster, ".csv"), 
                     header = TRUE, stringsAsFactors = FALSE)
      
      ## Editions to single_records
       df$STRUCTURE <- ifelse(df$REF == input$rows[1] & 
                            df$VISIT == input$rows[5] & 
                              input$ed_struc != " ", 
                          input$ed_struc,
                   ifelse(df$REF == input$rows[1] & 
                            df$VISIT == input$rows[5] & 
                            input$ed_struc == " ", 
                          df$STRUCTURE[which(df$REF == input$rows[1] & 
                                           df$VISIT == input$rows[5])],
                          df$STRUCTURE))
      
      
      df$STATUS <- ifelse(df$REF == input$rows[1] & 
                          df$VISIT == input$rows[5] & 
                          input$ed_status != " ",
                           input$ed_status,
                   ifelse(df$REF == input$rows[1] & 
                          df$VISIT == input$rows[5] & 
                          input$ed_status == " ",
                           df$STATUS[which(df$REF == input$rows[1] & df$VISIT == input$rows[5])],
                   df$STATUS))
      
      df$DETAILS <- ifelse(df$REF == input$rows[1] & 
                            df$VISIT == input$rows[5] & 
                            input$ed_details != " ", 
                             input$ed_details,
                     ifelse(df$REF == input$rows[1] & 
                            df$VISIT == input$rows[5] & 
                            input$ed_details == " ", 
                             df$DETAILS[which(df$REF == input$rows[1] & df$VISIT == input$rows[5])],
                     df$DETAILS))
      
      df$FU_DATE <- ifelse(df$REF == input$rows[1] & 
                                  df$VISIT == input$rows[5] & 
                                  !grepl("later", input$ed_details) &
                                  isTruthy(input$ed_date),
                                  " ",
                           ifelse(df$REF == input$rows[1] & 
                                  df$VISIT == input$rows[5] & 
                                  grepl("later", input$ed_details) &
                                  !isTruthy(input$ed_date),
                                  as.character(df$FU_DATE[which(df$REF == input$rows[1] & df$VISIT == input$rows[5])]),
                           ifelse(df$REF == input$rows[1] & 
                                  df$VISIT == input$rows[5] & 
                                  !grepl("later", input$ed_details) &
                                  !isTruthy(input$ed_date), 
                                  " ",
                           ifelse(df$REF == input$rows[1] & 
                                  df$VISIT == input$rows[5] & 
                                  grepl("later", input$ed_details) &
                                  isTruthy(input$ed_date), 
                                   as.character(input$ed_date),
                           df$FU_DATE))))
      

      
      df$NOTES <- ifelse(df$REF == input$rows[1] & 
                         df$VISIT == input$rows[5] &
                         input$ed_notes != " ", 
                         input$ed_notes,
                  ifelse(df$REF == input$rows[1] & 
                         df$VISIT == input$rows[5] & 
                         input$ed_notes == " ", 
                         df$NOTES[which(df$REF == input$rows[1] & df$VISIT == input$rows[5])],
                  df$NOTES))
      
      df$INITIALS <- ifelse(df$REF == input$rows[1] & 
                             df$VISIT == input$rows[5] & 
                             input$ed_initials != " ", 
                             input$ed_initials,
                      ifelse(df$REF == input$rows[1] & 
                             df$VISIT == input$rows[5] & 
                             input$ed_initials == " ", 
                             df$INITIALS[which(df$REF == input$rows[1] & df$VISIT == input$rows[5])],
                      df$INITIALS))
      

      DF <- df[order(as.integer(df$REF), abs(as.integer(df$VISIT))), ]
      
      write.csv(DF, paste0("htrack_cumulative_", input$cluster, ".csv"), row.names = FALSE)
      write.csv(DF, paste0("htrack_archive/htrack_cumulative_", input$cluster, sep = "_", as.character(Sys.Date()), sep=".csv"), row.names = FALSE)
      
      ## Save most recent encounter -- this will be shown on main app 
      subDF <- DF[order(as.integer(DF$REF), -abs(as.integer(DF$VISIT))), ]
      subDF <- subDF[!duplicated(as.integer(subDF$REF)), ]
      write.csv(subDF, paste0("htrack_single_", input$cluster, ".csv"), row.names = FALSE)
      
      ## Small pop up stating if data has been saved.
      showNotification(paste("Data edits saved successfully!"),
                       type = "message",
                       duration = 3)
    })
    
    observeEvent(input$saveEdits, {
      saveds()      
      if(file.exists(paste0("htrack_single_", input$cluster, ".csv"))) {
        df <- read.csv(paste0("htrack_single_", input$cluster, ".csv"), header = TRUE, stringsAsFactors = FALSE)
      } else {
        hids <- start()
        df <- hids
      }
      
      if(input$cluster %in% c("", " ")) {
        df <- df
      } else {
        df <- df[which(df$CLUSTER == input$cluster), ]
      }
      
      df$cu_day <- as.character(as.Date(df$VISIT_DATETIME, "%Y-%m-%d"))
      df$fu_day <- as.character(weekdays(as.Date(df$FU_DATE, "%Y-%m-%d")))
      df$cols <- ifelse(is.na(df$STATUS), "#0072B2",
                 ifelse(df$STATUS == "Needs follow-up" & is.na(df$FU_DATE), "#D55E00",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE == "", "#D55E00",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE != as.character(Sys.Date()), "#808080",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE == as.character(Sys.Date()), "#CC79A7",
                 ifelse(df$STATUS == "Visit completed", "#009E73",
                        "#0072B2"))))))
      df$vcol <- ifelse(df$FU_DATE < as.character(Sys.Date()) &                         
                          df$FU_DATE != "", "#ff0318", "#000000")
      ## Using map proxy to update map icon colors
      map <- leafletProxy("map", data = df, session) %>%
        clearMarkers() %>%
        setView(lng = mean(df$LONG), lat = mean(df$LAT), zoom = 17) %>%
        addCircleMarkers(
          lat = ~ LAT,
          lng = ~ LONG,
          radius = 8,
          color = ~ as.character(vcol),
          opacity = 1,
          weight = 2,
          fillColor = ~ as.character(cols),
          fillOpacity = 1,
          layerId = ~ REF,
          label = ~ HHID) %>%
        addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                           autoCenter = FALSE, maxZoom = 20, 
                                           setView = FALSE))
      activateGPS(map)
      
      ## Reset all inputs
      session$sendCustomMessage(type = "resetValue", message = "ed_details")
      
       updateSelectInput(session, "ed_struc", label = "Structure type", 
                         choices = c(" ",
                                     "House - one level", 
                                     "House - two levels",
                                     "Condo or Apartment", 
                                     "1st floor apt in house",
                                     "2nd floor apt in house",
                                     "Other"),
                         selected = " ")
      
      updateSelectInput(session, "ed_status", 
                        label = "Status",
                        choices = c(" ",
                                    "Needs follow-up",
                                    "Visit completed"),
                        selected = " ")
      
      updateDateInput(session, "ed_date", 
                      label = "Follow-up date",
                      min = Sys.Date(),
                      max = Sys.Date() + 60,
                      value = NA)

      updateTextInput(session, "ed_notes", 
                      label = "Notes",
                      value = " ")
      
          })
    
    
    # Row deletion from data edits parcel info tab
    del <- eventReactive(
      {input$rows
        input$cluster}, {
      df <- read.csv(paste0("htrack_cumulative_", input$cluster, ".csv"), header = TRUE, stringsAsFactors = FALSE)
      if(input$rows[5] == max(df$VISIT[which(df$REF == input$rows[1])]) & input$rows[5] != 0) {
        df <- df[-which(df$REF == input$rows[1] & df$VISIT == input$rows[5]), ]
        DF <- df[order(as.integer(df$REF), abs(as.integer(df$VISIT))), ]
        
        write.csv(DF, paste0("htrack_cumulative_", input$cluster, ".csv"), row.names = FALSE)
        write.csv(DF, paste0("htrack_archive/htrack_cumulative_", input$cluster, sep = "_", as.character(Sys.Date()), sep=".csv"), row.names = FALSE)
        
        ## Save most recent encounter -- this will be shown on main app 
        subDF <- DF[order(as.integer(DF$REF), -abs(as.integer(DF$VISIT))), ]
        subDF <- subDF[!duplicated(as.integer(subDF$REF)), ]
        write.csv(subDF, paste0("htrack_single_", input$cluster, ".csv"), row.names = FALSE)
        
        showNotification(paste("Row deleted successfully!"),
                         type = "message")
      } else {
        showNotification(paste("This row cannot be deleted, please edit instead."),
                         type = "error",
                         closeButton = TRUE)
      }
    })
    
    
    observeEvent(input$deleteRow, {
      del()
      if(file.exists(paste0("htrack_single_", input$cluster, ".csv"))) {
        df <- read.csv(paste0("htrack_single_", input$cluster, ".csv"), header = TRUE, stringsAsFactors = FALSE)
      } else {
        hids <- start()
        df <- hids
      }
      
      if(input$cluster %in% c("", " ")) {
        df <- df
      } else {
        df <- df[which(df$CLUSTER == input$cluster), ]
      }
      
      df$cols <- ifelse(is.na(df$STATUS), "#0072B2",
                 ifelse(df$STATUS == "Needs follow-up" & is.na(df$FU_DATE), "#D55E00",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE == "", "#D55E00",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE != as.character(Sys.Date()), "#808080",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE == as.character(Sys.Date()), "#CC79A7",
                 ifelse(df$STATUS == "Visit completed", "#009E73",
                        "#0072B2"))))))
      df$vcol <- ifelse(df$FU_DATE < as.character(Sys.Date()) &                         
                          df$FU_DATE != "", "#ff0318", "#000000")
      
      
      ## Using map proxy to update map icon colors
      map <- leafletProxy("map", data = df, session) %>%
        clearMarkers() %>%
        setView(lng = mean(df$LONG), lat = mean(df$LAT), zoom = 17) %>%
        addCircleMarkers(
          lat = ~ LAT,
          lng = ~ LONG,
          radius = 8,
          color = ~ as.character(vcol),
          opacity = 1,
          weight = 2,
          fillColor = ~ as.character(cols),
          fillOpacity = 1,
          layerId = ~ REF,
          label = ~ HHID) %>%
        addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                           autoCenter = FALSE, maxZoom = 20, 
                                           setView = FALSE))
      activateGPS(map)
    })
    
    
     Map refresh
    observeEvent(input$mapRefresh, {
      if(file.exists(paste0("htrack_single_", input$cluster, ".csv"))) {
        df <- read.csv(paste0("htrack_single_", input$cluster, ".csv"), header = TRUE, stringsAsFactors = FALSE)
      } else {
        hids <- start()
        df <- hids
      }
      
      if(input$cluster %in% c("", " ")) {
        df <- df
      } else {
        df <- df[which(df$CLUSTER == input$cluster), ]
      }
      
       df$cols <- ifelse(is.na(df$STATUS), "#0072B2",
                 ifelse(df$STATUS == "Needs follow-up" & is.na(df$FU_DATE), "#D55E00",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE == "", "#D55E00",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE != as.character(Sys.Date()), "#808080",
                 ifelse(df$STATUS == "Needs follow-up" & df$FU_DATE == as.character(Sys.Date()), "#CC79A7",
                 ifelse(df$STATUS == "Visit completed", "#009E73",
                        "#0072B2"))))))
      df$vcol <- ifelse(df$FU_DATE < as.character(Sys.Date()) &                         
                          df$FU_DATE != "", "#ff0318", "#000000")
      
      ## Using map proxy to update map icon colors
      map <- leafletProxy("map", data = df, session) %>%
        clearMarkers() %>%
        setView(lng = mean(df$LONG), lat = mean(df$LAT), zoom = 17) %>%
        addCircleMarkers(
          lat = ~ LAT,
          lng = ~ LONG,
          radius = 8,
          color = ~ as.character(vcol),
          opacity = 1,
          weight = 2,
          fillColor = ~ as.character(cols),
          fillOpacity = 1,
          layerId = ~ REF,
          label = ~ HHID) %>%
        addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                           autoCenter = FALSE, maxZoom = 20, 
                                           setView = FALSE))
      activateGPS(map)
      
      session$sendCustomMessage(type = "resetValue", message = "details")
      
       updateSelectInput(session, "struc", label = "Structure type", 
                         choices = c(" ",
                                     "House - one level", 
                                     "House - two levels",
                                     "Condo or Apartment", 
                                     "1st floor apt in house",
                                     "2nd floor apt in house",
                                     "Other"),
                         selected = " ")
      
      updateSelectInput(session, "status", 
                        label = "Status",
                        choices = c(" ",
                                    "Needs follow-up",
                                    "Visit completed"),
                        selected = " ")
      
      updateDateInput(session, "date", 
                      label = "Follow-up date",
                      min = Sys.Date(),
                      max = Sys.Date() + 60,
                      value = NA)

      updateTextInput(session, "notes", 
                      label = "Notes",
                      value = " ")
      
    })
    
    
    # Preparing developers tab
    output$hide_panel <- eventReactive(input$password, TRUE, ignoreInit = TRUE)
    outputOptions(output, "hide_panel", suspendWhenHidden = TRUE)
    
    sel <- eventReactive(input$current_cluster, {
      inFile <- input$sel_file 
      if (is.null(inFile)) {
        return(NULL) 
        } else {
    
        if(!paste0("htrack_cumulative_", input$current_cluster, ".csv") %in% 
           list.files("/storage/internal/htrack/")) {
           
          new_files <- read.csv(inFile$datapath, header=T, stringsAsFactors=F)
          new_hids <- data.frame(
              REF = 1:nrow(new_files),
              CLUSTER = new_files$CLUSTER,
              HHID = new_files$HHID, 
              LONG = new_files$LONG, 
              LAT = new_files$LAT)
        
            new_hids$VISIT <- 0
            new_hids$VISIT_DATETIME <- NA
            new_hids$STATUS <- NA
            new_hids$DETAILS <- NA
            new_hids$FU_DATE <- NA
            new_hids$STRUCTURE <- NA
            new_hids$INITIALS <- NA
            new_hids$NOTES <- NA
  
            new_hids <- new_hids[, c("REF", "HHID", "LONG", "LAT", 
                                     "VISIT", "STRUCTURE", "VISIT_DATETIME", 
                                     "STATUS", "DETAILS", "FU_DATE", 
                                     "NOTES", "INITIALS", "CLUSTER")]
            
           write.csv(new_hids, paste0("/storage/internal/htrack/HTrack_single_", input$current_cluster, ".csv"), 
                     row.names=F)
          
           showNotification(paste("A new HTrack_single file was created for the selected cluster."),
                         type = "message",
                         duration = 4)
        } else {
          
          cum <- read.csv(paste0("/storage/internal/htrack/HTrack_cumulative_", input$current_cluster, ".csv"),
                 header = TRUE, stringsAsFactors = FALSE)
          sin <- cum[order(as.integer(cum$REF), -abs(as.integer(cum$VISIT))), ]
          sin <- sin[!duplicated(as.integer(sin$REF)), ]
          
          ## Load new selections per cluster
          if(exists(c("sin"))) {
            new_sel1 <- read.csv(inFile$datapath, header=T, stringsAsFactors=F)
            
            new_sel <- new_sel1[, c("HHID", "LONG", "LAT", "CLUSTER")]
            colnames(new_sel) <- c("HHID", "LONG", "LAT", "CLUSTER")
            
            new_sel$REF = seq(from = max(sin$REF, na.rm = T) + 1, to = max(sin$REF, na.rm = T) + nrow(new_sel1))
            new_sel$VISIT = 0
            new_sel$VISIT_DATETIME = NA
            new_sel$STATUS = NA
            new_sel$DETAILS = NA
            new_sel$FU_DATE = NA
            new_sel$STRUCTURE = NA
            new_sel$INITIALS = NA
            new_sel$NOTES = NA
          
            new_sel <- new_sel[, c("REF", "HHID", "LONG", "LAT",
                                   "VISIT", "STRUCTURE", "VISIT_DATETIME", 
                                   "STATUS", "DETAILS", "FU_DATE", 
                                   "NOTES", "INITIALS", "CLUSTER")]
            ## this filters out repeated HHIDs that may be in the new selection
            new_sel <- subset(new_sel, !HHID %in% c(cum$HHID))
          } 
          
          ## Add new selection to each zone for cumulative file
          new_cum <- rbind(cum, new_sel)
          new_sin <- rbind(sin, new_sel)
          
          # write HTrack single file for each zone/tablet **NO DATESTAMP
          write.csv(new_sin, paste0("/storage/internal/htrack/HTrack_single_", input$current_cluster, ".csv"), row.names=F) 
          write.csv(new_cum, paste0("/storage/internal/htrack/HTrack_cumulative_", input$current_cluster, ".csv"), row.names=F)
          
          showNotification(paste("New selection added successfully"),
                             type = "message",
                             duration = 4)
      }
    
    }
  })
    
  
  observeEvent(input$new_selection, {
      sel()
  })

})

shinyApp(ui = ui, server = server)
