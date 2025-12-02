library(shiny)
library(bslib)

ui <- page_sidebar(
  theme = bs_theme(version = 5, "darkly"),

  sidebar = sidebar(
    sliderInput("size", "Width and height (pixels)", min = 100, max = 1000, value = 288),
    sliderInput("margin", "Margin (lines)", min = 1, max = 20, value = 10),
  ),
    uiOutput("details"),
    plotOutput("plot") 
)

server <- function(input, output, session){

  output$details <- renderUI({
    line_height <- 0.2
    dpi <- 72
    margin_width <- line_height * dpi * input$margin * 2
    plot_width <- input$size - margin_width
    
    tagList(
      p("Margin width: ", margin_width, " pixels"),
      p("Plot area width: ", plot_width, " pixels")
    )
    
  })
  
  output$plot <- renderPlot({
    par(mar = rep(input$margin, 4))
    plot(1, 1, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    for (side in 1:4) mtext(1:input$margin, side, 0:(input$margin - 1))
  }, 
    width = \() input$size, 
    height = \() input$size
  )
  
}

shinyApp(ui, server)
