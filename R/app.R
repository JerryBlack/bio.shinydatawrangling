#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(bio.datawrangling)
require(bio.plotting)

plot.by = NULL
plot.value = NULL
crs.out = '+init=epsg:2220'
x.limits = c(-72, -56)
y.limits = c(40, 47.5)
lat.field = "LATITUDE"
lon.field = "LONGITUDE"
save.plot = FALSE
bin.style = 'pretty' #'fixed'
fixed.breaks.bins = c(1, 4, 32, 256, 2045, 16336, 130496, 1042432, 8327186)
trim.fixed.breaks = TRUE

#' save the data as it was in a new environment; and
#' get a df of all of the available fields, the tables they come from, and how many unique values 
#' are in each field
virgin = new.env()
cols <-  data.frame(FIELD = NA, CLAS = NA, TAB = NA)
db = 'rv'
tab=table_renamer(get_tables(db), dest = 'clean')

#CLAS is not working because times are returning 2 classes! 
for (k in 1:length(tab)){
  assign(tab[k], get(tab[k]), envir = virgin)
  colrows = data.frame(FIELD = colnames(get(tab[k])), 
                       #the magic below is kludgy since dates return multiple classes
                       CLAS = unname(rapply(lapply(get(tab[k]),class), function(x) x[1], how = "unlist")),
                       TAB = rep(tab[k], length(colnames(get(tab[k])))))
  m = lapply(get(tab[k]),class)
  cols = rbind(cols, colrows)
}
cols = cols[complete.cases(cols),]
cols = cols[order(cols$FIELD, cols$CLAS, cols$TAB),]
#' keep just one row per FIELD
cols = cols[!duplicated(cols$FIELD), ]


sel.field = "SPEC"
sel.value = "TOTWGT"
plot.by = gsub('\\s\\([^)]*\\)', '\\1', sel.field)

fields = cols$FIELD
fields[length(fields)] = "NULL"


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("bio.shinydatawrangling"),

   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("selected.db", "Data Source:",
                    c('Groundfish/RV/Ecosystem Surveys' = 'rv',
                      'Industry Surveys Database' = 'isdb',
                      'Cape Chidley Surveys' = 'chid',
                      'Redfish Surveys' = 'redfish',
                      'Pre-1970s Research Surveys' = 'rvp70')),
        selectInput("crs.out", "Map Projection:",
                    c('UTM Zone 20 N' = '+init=epsg:2220',
                      'Lat/Lon WGS84' = '+init=epsg:4326')),
        selectInput("colour.ramp", "Symbol Colour:",
                    c('Colours' = ' c("#edf8b1", "#7fcdbb", "#2c7fb8")',
                      'Black' = 'NULL')),
        
        selectInput("bin.style", "Bin Style:",
                    c('Fixed' = 'fixed',
                      'SD' = 'sd',
                      'Equal' = 'equal',
                      'Pretty' = 'pretty',
                      'Quantile' = 'quantile',
                      'K Means' = 'kmeans',
                      'H Cluster' = 'hclust',
                      'B Cluster' = 'bclust',
                      'Fisher' = 'fisher',
                      'Jenks' = 'jenks')),
        
         selectInput("plot.by", "Partition Variable:",
                     fields,selected=sel.field),
         
         selectInput("plot.value", "Display Variable:",
           cols[(cols$CLAS %in% c('numeric','integer','double')) & (!cols$FIELD %in% sel.field),]$FIELD,
           selected=sel.value
         )
           
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         uiOutput("plots")
      )
   )
)

get_plot_output_list <- function(max_plots, input_n, input) {
  # Insert plot output objects the list
  plot.by = input$plot.by
  if (plot.by=="<NONE>") plot.by = NULL
  eval(parse(text = paste("colour.ramp =",input$colour.ramp))) 
  
  plot.value = input$plot.value
  print(paste("input$plot.value",input$plot.value))
  
  chosen = cols
  if (!is.null(plot.by)) chosen = cols[cols$FIELD == plot.by,]
  chosen.table = get(chosen[[3]])
  entries=sort(unique(chosen.table[chosen[[1]]][,1]))

  plot_output_list <- lapply(1:length(entries), function(i) {
    plotname <- paste("plot", i, sep="")
    plot_output_object <- plotOutput(plotname, height = 280, width = 250)
    plot_output_object <- renderPlot({
    #  for (j in 1:length(entries)){
        cat(paste0("\nWorking on ",plot.by," = '", entries[i],"'"))
        
        assign(chosen[[3]],chosen.table[chosen.table[[plot.by]]==entries[i],], envir = .GlobalEnv)
        self_filter(db)
        df.plot = summarize_catches(db=db,  lat.field = lat.field,
                                    lon.field = lon.field, quiet=TRUE)
        #added a try block in case no data falls in the plot area
        #browser()
        bm=bio.plotting::make_basemap(x.limits=x.limits, y.limits=y.limits, crs.out = input$crs.out)
        try(
          
          bio.plotting::add_points(df.plot, basemap = bm,
                                   bin.style = input$bin.style, fixed.breaks.bins=fixed.breaks.bins,
                                   trim.fixed.breaks = trim.fixed.breaks, plot.field = input$plot.value, lat.field = lat.field,
                                   lon.field = lon.field,colour.ramp = colour.ramp, 
                                   pnt.fill = 'red', 
                                   plot.field.pretty = entries[i])
          ,silent = TRUE
        )
        
        for (m in 1:length(tab)){
          assign(tab[m], virgin[[tab[m]]], envir = .GlobalEnv)
        }
        
    #  }
    })
    
  })
  
  do.call(tagList, plot_output_list) # needed to display properly.
  
  return(plot_output_list)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # output$distPlot <- renderPlot({
  #     plot.by = input$plot.by
  #     if (plot.by=="<NONE>") plot.by = NULL
  #     eval(parse(text = paste("colour.ramp =",input$colour.ramp))) 
  #     
  #     plot.value = input$plot.value
  #     print(paste("input$plot.value",input$plot.value))
  #     
  #     chosen = cols[cols$FIELD ==plot.by,]
  #     chosen.table = get(chosen[[3]])
  #     entries=sort(unique(chosen.table[chosen[[1]]][,1]))
  #     for (j in 1:length(entries)){
  #       cat(paste0("\nWorking on ",plot.by," = '", entries[j],"'"))
  # 
  #       assign(chosen[[3]],chosen.table[chosen.table[[plot.by]]==entries[j],], envir = .GlobalEnv)
  #       self_filter(db)
  #       df.plot = summarize_catches(db=db,  lat.field = lat.field,
  #                                   lon.field = lon.field, quiet=TRUE)
  #       #added a try block in case no data falls in the plot area
  #       #browser()
  #       bm=bio.plotting::make_basemap(x.limits=x.limits, y.limits=y.limits, crs.out = input$crs.out)
  #       try(
  #         
  #         bio.plotting::add_points(df.plot, basemap = bm,
  #                                  bin.style = input$bin.style, fixed.breaks.bins=fixed.breaks.bins,
  #                                  trim.fixed.breaks = trim.fixed.breaks, plot.field = input$plot.value, lat.field = lat.field,
  #                                  lon.field = lon.field,colour.ramp = colour.ramp,
  #                                  plot.field.pretty = entries[j])
  #         ,silent = TRUE
  #       )
  #       
  #       for (m in 1:length(tab)){
  #         assign(tab[m], virgin[[tab[m]]], envir = .GlobalEnv)
  #       }
  #     
  #     }
  # })
  
  
   
    observe({
      output$plots <- renderUI({ get_plot_output_list(5, 5,input) })
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

