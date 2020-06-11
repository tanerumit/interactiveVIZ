
source("./global.R")

#### UI-SIDE -------------------------------------------------------------------


Tab1 <- tabPanel("About", icon = icon("home"),
                 jumbotron(header  = "Multidimensional Data Analysis", 
                           content = "Visualize data",
                           button  = FALSE)
)


Tab2 <- tabPanel("Analyze", icon = icon("cog"),
                 fluidRow(
                   column(3,
                          br(),
                          br(),
                          uiOutput('prdsDataUI'),
                          uiOutput('selectAxesUI'),
                          uiOutput('color.varUI'),
                          uiOutput('pthresholdUI'),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br()
                          
                   ),
                   column(9,
                          parcoordsOutput("pc")#,
                         # uiOutput('snapBtnUI')
                   )
                 )
)
                          
                          
ui <- navbarPage(
  title = "Climate Explore",  
  theme = shinytheme("cerulean"),
  selected = "Analyze",
  Tab1,
  Tab2
) 

#### SERVER-SIDE ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # Input data UI element
  output$prdsDataUI = renderUI({
    fileInput("prdsData", label = NULL, multiple = F, accept = ".csv", width = '100%')
  })
  
  # Input data server element
  parcoordsData <- reactive({
    req(input$prdsData)
    read.csv(input$prdsData$datapath, header = T, sep = ",", stringsAsFactors = T, row.names = NULL)
  })
  
  # Buttons/sliders
  output$selectAxesUI = renderUI({

    req(input$prdsData)

    selectizeInput(
      inputId = "columns",
      label = "Columns to Hide",
      choices = c(colnames(parcoordsData())),
      selected = "names",
      multiple = TRUE
    )
  })
  output$color.varUI  = renderUI({

    req(input$prdsData)

    selectizeInput(
      inputId = "color.var",
      label = "Coloring variable",
      choices = c(colnames(parcoordsData())),
      selected = colnames(parcoordsData())[3],
      multiple = FALSE
    )
    
  })

  output$pthresholdUI = renderUI({
    
    req(parcoordsData())
    
    sliderInput(inputId = "pthreshold",
                label = "Performance Threshold",
                ticks = FALSE,
                step  = NULL, #this needs to be fixed
                min   = parcoordsData() %>% pull(input$color.var) %>% min()  %>% round(),
                max   = parcoordsData() %>% pull(input$color.var) %>% max()  %>% round(),
                value = parcoordsData() %>% pull(input$color.var) %>% mean() %>% round(),
                round = 0,
    )
  })
  
  
  
  output$snapBtnUI    = renderUI({
    actionButton(inputId = "snapBtn", label = "snapshot")
  })
  
  parcoordsDf <- reactive({
    
    req(parcoordsData())
    
    df <- parcoordsData() 
    df$bin_color <- ifelse(df %>% pull(input$color.var) > input$pthreshold, 0, 1)
    df
  })
    

  output$pc <- renderParcoords({
  
    parcoords(
        data     = parcoordsDf(),
        rownames = FALSE,
        color = list(
          colorBy = "bin_color",
          colorScheme = c("blue", "red")
        ),
        tasks = list(htmlwidgets::JS("
            function() {
              HTMLWidgets.parcoordsWidget.methods.hide.call(this.parcoords, ['names','bin_color'])
            }
        ")),
        brushMode = "1d",
        brushPredicate = "and",
        alphaOnBrushed = 0.3,
        reorderable = TRUE,
        axisDots = TRUE,
        margin = NULL,
        composite = NULL,
        alpha = NULL,
        queue = FALSE,
        mode = FALSE,
        rate = NULL,
        dimensions = NULL,
        bundleDimension = NULL,
        bundlingStrength = 0,
        smoothness = 0,
        autoresize = FALSE,
        withD3 = TRUE,
        width = NULL,
        height = 400
    )
  })
  
  pcp <- parcoordsProxy("pc")
  
  observeEvent(input$columns, {

    # create a proxy with which we will communicate between
    #   Shiny and the parallel coordinates without a re-render
    pcp <- parcoordsProxy("pc")

    pcHide(pcp, c(input$columns, 'names','bin_color'))

  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  
  observeEvent(input$snapBtn, {
    # create a proxy with which we will communicate between
    #   Shiny and the parallel coordinates without a re-render
    pcp <- parcoordsProxy("pc")
    pcSnapshot(pcp)
  })
  
}



shinyApp(ui = ui, server = server)
