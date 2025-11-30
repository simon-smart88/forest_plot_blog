library(shiny)
library(bslib)
library(meta)
library(svglite)
library(xml2)
library(shinyjs)

crop_svg <- function(svg, margin = 10){

  pixel_data <- paste(svg, collapse = "\n") |>
    magick::image_read_svg() |>
    magick::image_data()

  # Create a matrix of pixels containing content
  is_content <- !(
    # white (all RGB channels > 250)
    (pixel_data[1,,] > 250 &
       pixel_data[2,,] > 250 &
       pixel_data[3,,] > 250)
  )

  # bodge to get around grey border pixels
  is_content[, 1] <- FALSE
  is_content[1, ] <- FALSE
  is_content[nrow(is_content), ] <- FALSE
  is_content[, ncol(is_content)] <- FALSE

  content_pixels <- which(is_content, arr.ind = TRUE)

  x_coords <- content_pixels[, 1]
  y_coords <- content_pixels[, 2]

  x_min <- min(x_coords)
  x_max <- max(x_coords)
  y_min <- min(y_coords)
  y_max <- max(y_coords)

  width <- x_max - x_min
  height <- y_max - y_min

  bbox <- list(
    x = x_min - margin,
    y = y_min - margin,
    width = width + (margin * 2),
    height = height + (margin * 2)
  )

  # update viewBox
  svg_node <- xml2::xml_find_first(svg, "//svg:svg", ns = c(svg = "http://www.w3.org/2000/svg"))
  xml2::xml_attr(svg_node, "viewBox") <- paste(bbox$x, bbox$y, bbox$width, bbox$height)

  # fix the background element
  total_width <- gsub("pt", "", xml2::xml_attr(svg_node, "width"))
  total_height <- gsub("pt", "", xml2::xml_attr(svg_node, "height"))
  rect_node <- xml2::xml_find_first(svg, "//svg:rect[@width='100%']", ns = c(svg = "http://www.w3.org/2000/svg"))
  xml2::xml_attr(rect_node, "width") <- total_width
  xml2::xml_attr(rect_node, "height") <- total_height

  # set the width and height
  xml2::xml_attr(svg_node, "width") <- paste0(bbox$width, "pt")
  xml2::xml_attr(svg_node, "height") <- paste0(bbox$height, "pt")

  list(
    svg = paste(svg, collapse = "\n"),
    width = bbox$width,
    height = bbox$height)
}

plotting_function <- function(dataset){

  if (dataset == "Olkin1995"){
    data(Olkin1995)
    m <- metabin(ev.exp, n.exp, ev.cont, n.cont,
                 data = Olkin1995, subset = c(41, 47, 51, 59),
                 sm = "RR", method = "I",
                 studlab = paste(author, year))
    plot_height <- 10
    plot_width <- 15
  }

  if (dataset == "woodyplants"){
    data(woodyplants)
    m <- metacont(n.elev, mean.elev, sd.elev, n.amb, mean.amb, sd.amb,
                  data = woodyplants, sm = "ROM")
    plot_height <- 30
    plot_width <- 15
  }

  svglite::xmlSVG({
    meta::forest(m)
  },
  standalone = TRUE,
  height = plot_height,
  width = plot_width,
  web_fonts = list("https://fonts.googleapis.com/css2?family=Arimo:wght@400;700&display=swap")
  ) |> crop_svg()

}

write_svg_plot <- function(file, type, svg) {
  if (type == "pdf") {
    rsvg::rsvg_pdf(charToRaw(svg$svg), file, svg$width, svg$height)
  }
  if (type == "png") {
    rsvg::rsvg_png(charToRaw(svg$svg), file, svg$width * 3, svg$height * 3)
  }
  if (type == "svg") {
    writeLines(svg$svg, file)
  }
}

ui <- page_sidebar(
  shinyjs::useShinyjs(),
  theme = bs_theme(version = 5, "darkly"),
  tags$head(tags$style("
    .svg_container {
      width: 100%;
      height: auto;
      display: block;
      overflow: hidden;
    }

    .svg_container svg {
      width: 100%;
      height: auto;
      display: block;
    }
                    ")),

  sidebar = sidebar(
    selectInput("dataset", "Dataset", choices =  c("Olkin1995", "woodyplants")),
    sliderInput("width", "Plot width (pixels)", min = 100, max = 1000, value = 500),
    selectInput("format", "Download format", choices = c("svg", "pdf", "png")),
    downloadButton("download", "Download")
  ),

  div(id = "plot_container",
      uiOutput("plot")
      )
)

server <- function(input, output) {

  svg <- reactive(plotting_function(input$dataset))

  output$plot <- renderUI({
    div(class = "svg_container",
        HTML(svg()$svg))
  })

  observe({
    runjs(sprintf("
      $('#plot_container').css({
        'width': '%spx'
      });
    ", input$width))
  })

  output$download <- downloadHandler(
    filename = function() {
      paste("plot.", input$format, sep="")
    },
    content = function(file) {
      write_svg_plot(file, input$format, svg()$svg, svg()$height, svg()$width)
    }
  )

}

shinyApp(ui = ui, server = server)

