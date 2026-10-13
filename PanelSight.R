# =================================================================
# 1. AUTOMATIC DEPENDENCY CHECK & INSTALLATION
# =================================================================
required_packages <- c("shiny", "ggplot2", "patchwork", "magick", "cowplot")

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  message("Installing missing dependencies: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))

options(shiny.maxRequestSize = 50 * 1024^2)

# =================================================================
# 2. USER INTERFACE (UI)
# =================================================================
ui <- fluidPage(
  titlePanel(
    div(
      h1("PanelSight", style = "font-weight: bold; color: #2c3e50; margin-bottom: 5px;"),
      p("Multi-Panel Figure Generator for Academic Journals", style = "font-size: 15px; color: #7f8c8d; margin-bottom: 2px;"),
      p(tags$b("Prepared by Dr. Himel Mondal"), style = "font-size: 13px; color: #34495e; font-style: italic;")
    )
  ),
  
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      h4("1. Image Selection & Layout"),
      numericInput("num_images", "Number of Images:", value = 3, min = 1, max = 16, step = 1),
      numericInput("imgs_per_row", "Images per Row:", value = 2, min = 1, max = 6, step = 1),
      sliderInput("panel_gap", "Inter-panel Gap (pt):", min = 0, max = 30, value = 4, step = 1),
      
      wellPanel(
        style = "max-height: 200px; overflow-y: auto; background: #f9f9f9;",
        uiOutput("upload_slots")
      ),
      
      hr(),
      
      h4("2. Panel Labeling"),
      selectInput("tag_style", "Label Style:", 
                  choices = c("Uppercase (A, B, C)" = "A", "Lowercase (a, b, c)" = "a")),
      
      radioButtons("tag_location", "Label Placement:", 
                   choices = c("Inside Image Panel" = "inside", "Outside Image Panel" = "outside"),
                   selected = "inside"),
      
      fluidRow(
        column(6, selectInput("tag_vpos", "Vertical Position:", choices = c("Top" = "top", "Bottom" = "bottom"))),
        column(6, selectInput("tag_align", "Horizontal Position:", choices = c("Left-most" = "left", "Middle" = "middle", "Right-most" = "right")))
      ),
      
      fluidRow(
        column(6, numericInput("font_size", "Label Font Size (pt):", value = 22, min = 8, max = 72, step = 1)),
        column(6, selectInput("font_color", "Font Color:", choices = c("Black" = "black", "White" = "white"), selected = "black"))
      ),
      
      conditionalPanel(
        condition = "input.tag_location == 'inside'",
        wellPanel(
          selectInput("tag_box", "Background Box Style:", 
                      choices = c("Without background box (Plain)" = "none",
                                  "White box with black text" = "white_box",
                                  "Black box with white text" = "black_box")),
          sliderInput("box_pad_pt", "Box Margin / Padding:", min = 0, max = 1.0, value = 0.35, step = 0.05)
        )
      ),
      
      hr(),
      
      h4("3. Publication Export"),
      checkboxInput("auto_adapt", "Auto-adapt export size to Panel 1 aspect ratio", value = TRUE),
      fluidRow(
        column(6, numericInput("fig_width", "Width (in):", value = 8, min = 2, max = 25)),
        column(6, numericInput("fig_height", "Height (in):", value = 6, min = 2, max = 25))
      ),
      fluidRow(
        column(6, numericInput("fig_dpi", "DPI:", value = 300, step = 50)),
        column(6, selectInput("fig_format", "Format:", 
                              choices = c("jpg", "png", "pdf", "tiff", "svg"),
                              selected = "jpg"))
      ),
      downloadButton("download_fig", "Export High-Res Figure", class = "btn-primary", style = "width:100%;")
    ),
    
    mainPanel(
      width = 8,
      h4("Figure Preview (Synchronized Aspect Ratio)"),
      div(
        style = "overflow-x: auto; text-align: center; background: #eaeded; padding: 15px; border-radius: 5px;",
        uiOutput("preview_container")
      ),
      br(),
      div(
        style = "text-align: center; color: #7f8c8d; font-size: 12px; margin-top: 15px;",
        "PanelSight | Prepared by Dr. Himel Mondal"
      )
    )
  )
)

# =================================================================
# 3. SERVER LOGIC
# =================================================================
server <- function(input, output, session) {
  
  output$upload_slots <- renderUI({
    num <- req(input$num_images)
    lapply(1:num, function(i) {
      fileInput(paste0("img_file_", i), paste0("Upload Panel ", i, ":"), 
                accept = c("image/png", "image/jpeg", "image/tiff"))
    })
  })
  
  observeEvent(list(input$img_file_1, input$imgs_per_row, input$num_images, input$auto_adapt, input$fig_width), {
    req(input$auto_adapt, input$img_file_1, input$imgs_per_row, input$num_images, input$fig_width)
    tryCatch({
      img_info <- magick::image_info(magick::image_read(input$img_file_1$datapath))
      cols <- input$imgs_per_row
      rows <- ceiling(input$num_images / cols)
      grid_aspect <- (img_info$width * cols) / (img_info$height * rows)
      new_height <- round(input$fig_width / grid_aspect, 2)
      new_height <- max(2, min(25, new_height))
      updateNumericInput(session, "fig_height", value = new_height)
    }, error = function(e) {})
  })
  
  combined_plot <- reactive({
    num <- req(input$num_images)
    gap <- req(input$panel_gap)
    f_size <- req(input$font_size)
    f_col <- req(input$font_color)
    box_style <- req(input$tag_box)
    box_pad <- if (!is.null(input$box_pad_pt)) input$box_pad_pt else 0.35
    
    plot_list <- list()
    
    for (i in 1:num) {
      file_input <- input[[paste0("img_file_", i)]]
      tag_char <- if (input$tag_style == "A") LETTERS[i] else letters[i]
      
      if (!is.null(file_input)) {
        raw_img <- magick::image_read(file_input$datapath)
        p <- cowplot::ggdraw() + cowplot::draw_image(raw_img)
      } else {
        p <- ggplot() + 
          theme_void() + 
          theme(panel.background = element_rect(fill = "#f2f2f2", color = "#cccccc", linewidth = 1)) +
          annotate("text", x = 0.5, y = 0.5, label = paste("Panel", i, "\n(Upload Image)"), 
                   color = "#888888", fontface = "bold", size = f_size / 2.845)
      }
      
      if (input$tag_location == "inside") {
        x_pos <- switch(input$tag_align, "left" = 0.02, "middle" = 0.50, "right" = 0.98)
        h_just <- switch(input$tag_align, "left" = 0, "middle" = 0.5, "right" = 1)
        y_pos <- if (input$tag_vpos == "top") 0.98 else 0.02
        v_just <- if (input$tag_vpos == "top") 1 else 0
        
        txt_c <- f_col
        fill_c <- NA
        
        if (box_style == "white_box") {
          txt_c <- "black"
          fill_c <- "white"
        } else if (box_style == "black_box") {
          txt_c <- "white"
          fill_c <- "black"
        }
        
        if (box_style == "none") {
          p <- p + cowplot::draw_label(
            tag_char,
            x = x_pos, y = y_pos,
            hjust = h_just, vjust = v_just,
            size = f_size, fontface = "bold",
            color = txt_c
          )
        } else {
          p <- p + annotate(
            "label",
            x = x_pos, y = y_pos,
            label = tag_char,
            hjust = h_just, vjust = v_just,
            size = f_size / 2.845,
            fontface = "bold",
            color = txt_c,
            fill = fill_c,
            label.size = 0,
            label.padding = unit(box_pad, "lines"),
            label.r = unit(0, "lines")
          )
        }
      }
      
      if (input$tag_location == "outside") {
        tag_pos <- paste0(
          if (input$tag_vpos == "top") "top" else "bottom",
          switch(input$tag_align, "left" = "left", "middle" = "", "right" = "right")
        )
        p <- p + theme(
          plot.tag = element_text(size = f_size, face = "bold", color = f_col),
          plot.tag.position = tag_pos
        )
      }
      
      p <- p + theme(plot.margin = margin(t = gap, r = gap, b = gap, l = gap, unit = "pt"))
      plot_list[[i]] <- p
    }
    
    base_fig <- patchwork::wrap_plots(plot_list, ncol = input$imgs_per_row)
    
    if (input$tag_location == "outside") {
      base_fig <- base_fig + patchwork::plot_annotation(
        tag_levels = input$tag_style,
        theme = theme(plot.tag = element_text(size = f_size, face = "bold", color = f_col))
      )
    }
    
    return(base_fig)
  })
  
  output$preview_container <- renderUI({
    req(input$fig_width, input$fig_height)
    aspect_ratio <- input$fig_width / input$fig_height
    preview_h <- 580
    preview_w <- round(preview_h * aspect_ratio)
    plotOutput("figure_preview", width = paste0(preview_w, "px"), height = paste0(preview_h, "px"))
  })
  
  output$figure_preview <- renderPlot({
    combined_plot()
  })
  
  # Export High-Res File
  output$download_fig <- downloadHandler(
    filename = function() {
      paste0("PanelSight_Figure.", input$fig_format)
    },
    content = function(file) {
      # ggsave expects device = "jpeg" when writing a JPG file
      export_dev <- if (input$fig_format == "jpg") "jpeg" else input$fig_format
      
      ggsave(file,
             plot = combined_plot(), 
             width = input$fig_width, 
             height = input$fig_height, 
             dpi = input$fig_dpi, 
             device = export_dev)
    }
  )
}

shinyApp(ui = ui, server = server)
