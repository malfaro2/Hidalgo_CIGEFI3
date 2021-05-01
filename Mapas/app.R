library(shiny)

ui <- pageWithSidebar(
    headerPanel("Central American Trends"),
    sidebarPanel(
        radioButtons(inputId = "variable",
               label = 'Variable:', choices = c("temp" = "temp", 
                                                "prec"="prec"), 
               selected = c("prec"="prec"),inline=FALSE),
selectInput('index', 'Choose an index:',1:10),
selectInput('time', 'Choose a time domain:',c("Yearly"="year", 
                                              "Bimonthly: Dec - Jan - Feb" = "DJF",
                                              "Bimonthly: Mar - Apr" = "MA",
                                              "Bimonthly: May - Jun" = "MJ",
                                              "Bimonthly: Jul - Aug" = "JA",
                                              "Bimonthly: Sept - Oct" = "SO"))),
mainPanel(
# Use imageOutput to place the image on the page
imageOutput("myImage")
)
)

server <- function(input, output, session) {
    output$myImage <- renderImage({
        if(input$time=="year"){filename=paste0("imgs2/",input$variable,"_year_index",input$index,".png")}
        else{filename=paste0("imgs2/",input$variable,"_bimonth_index",
                    input$index,"_",input$time,".png")}
        
        filename <- normalizePath(filename)
        
        # Return a list containing the filename
        list(src = filename,
             width = "100%")
}, deleteFile = FALSE)}

shinyApp(ui, server)