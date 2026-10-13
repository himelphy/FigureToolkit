# ================================================================
# 1. AUTOMATIC DEPENDENCY CHECK & INSTALLATION
# ================================================================
required_packages <- c("shiny", "ggplot2", "magick", "cowplot")

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  message("Installing missing dependencies: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))

# Increase upload limit to 30 MB for high-resolution images
options(shiny.maxRequestSize = 30 * 1024^2)

# ================================================================
# 2. USER INTERFACE (UI)
# ================================================================
ui <- fluidPage(
  titlePanel(
    div(
      h1("AnnotateSight", style = "font-weight: bold; color: #2c3e50; margin-bottom: 2px;"),
      p("Precision Scientific Figure Annotator & Free-Form Cropper", style = "color: #7f8c8d; font-size: 14px; margin-bottom: 2px;"),
      p(tags$b("Prepared by Dr. Himel Mondal"), style = "font-size: 13px; color: #34495e; font-style: italic;")
    )
  ),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # Step 1: Upload Image
      h4("1. Upload Image"),
      fileInput("user_img", "Choose Image File:", accept = c("image/png", "image/jpeg", "image/tiff")),
      
      hr(),
      
      # Step 2: Free-Form Crop Controls
      h4("2. Free-Form Crop"),
      helpText("Drag a box over any target area on the preview canvas, then click 'Apply Crop'."),
      fluidRow(
        column(6, actionButton("btn_apply_crop", "Apply Crop", class = "btn-warning", style = "width: 100%;")),
        column(6, actionButton("btn_reset_img", "Reset Original", class = "btn-secondary", style = "width: 100%;"))
      ),
      
      hr(),
      
      # Step 3: Add Markings & Precision Arrow Settings
      h4("3. Add Precision Markings"),
      selectInput("shape_type", "Select Marking Tool:", 
                  choices = c("Precision Arrow (2-Point Click)" = "arrow",
                              "Circle / Oval (Drag Box)" = "circle", 
                              "Rectangle / Box (Drag Box)" = "rect", 
                              "Text Label (Single Click)" = "text")),
      
      conditionalPanel(
        condition = "input.shape_type == 'text'",
        textInput("text_label", "Text Content:", value = "Key Feature")
      ),
      
      fluidRow(
        column(6, selectInput("shape_color", "Color:", 
                              choices = c("Yellow"="yellow", "Red"="red", "White"="white", 
                                          "Black"="black", "Blue"="blue", "Green"="green", 
                                          "Cyan"="cyan", "Magenta"="magenta"))),
        column(6, numericInput("shape_thick", "Line Thickness:", value = 0.8, min = 0.2, max = 3.0, step = 0.1))
      ),
      
      # Precision Arrowhead Parameters
      conditionalPanel(
        condition = "input.shape_type == 'arrow'",
        wellPanel(
          style = "background: #f8f9fa; padding: 10px; margin-bottom: 10px;",
          fluidRow(
            column(6, numericInput("arrow_head_size", "Head Size (mm):", value = 3.5, min = 1.0, max = 10.0, step = 0.5)),
            column(6, selectInput("arrow_style", "Pointer Style:", 
                                  choices = c("Closed Sleek (18°)" = "18", 
                                              "Standard Journal (22°)" = "22", 
                                              "Needle Pointer (14°)" = "14")))
          )
        )
      ),
      
      # Tool action buttons
      conditionalPanel(
        condition = "input.shape_type == 'circle' || input.shape_type == 'rect'",
        actionButton("btn_add_drag_shape", "Add Circle/Box from Dragged Area", class = "btn-info", style = "width: 100%;"),
        helpText("Drag a box over target area on preview, then click button above.")
      ),
      
      conditionalPanel(
        condition = "input.shape_type == 'arrow' || input.shape_type == 'text'",
        actionButton("btn_start_click_mode", "Activate Click Placement", class = "btn-info", style = "width: 100%;"),
        br(), br(),
        verbatimTextOutput("click_mode_status")
      ),
      
      br(),
      fluidRow(
        column(6, actionButton("btn_undo", "Undo Last Marking", class = "btn-secondary", style = "width: 100%;")),
        column(6, actionButton("btn_clear", "Clear All Markings", class = "btn-danger", style = "width: 100%;"))
      ),
      
      hr(),
      
      # Step 4: Publication Export Options
      h4("4. Publication Export"),
      fluidRow(
        column(6, numericInput("fig_width", "Figure Width (in):", value = 7, min = 3, max = 15, step = 0.5)),
        column(6, numericInput("export_dpi", "Target DPI:", value = 300, step = 50))
      ),
      selectInput("export_format", "Format:", choices = c("png", "tiff", "jpeg", "pdf")),
      downloadButton("download_fig", "Export High-Res Figure", class = "btn-primary", style = "width: 100%;")
    ),
    
    mainPanel(
      width = 8,
      h4("Interactive Canvas"),
      p("Drag box for Crop/Circle/Box OR click 2 points on image for Arrow Tip & Tail:"),
      div(
        style = "overflow-x: auto; text-align: center; background: #eaeded; padding: 15px; border-radius: 5px;",
        uiOutput("preview_container")
      ),
      br(),
      div(
        style = "text-align: center; color: #7f8c8d; font-size: 12px; margin-top: 15px;",
        "AnnotateSight | Prepared by Dr. Himel Mondal"
      )
    )
  )
)

# ================================================================
# 3. SERVER LOGIC
# ================================================================
server <- function(input, output, session) {
  
  # Reactive state store
  rv <- reactiveValues(
    orig_img = NULL,
    current_img = NULL,
    markings = data.frame(
      type = character(), x1 = numeric(), y1 = numeric(), x2 = numeric(), y2 = numeric(),
      color = character(), thick = numeric(), text = character(),
      head_size = numeric(), head_angle = numeric(), stringsAsFactors = FALSE
    ),
    click_active = FALSE,
    temp_pt = NULL
  )
  
  # 1. Load Uploaded Image
  observeEvent(input$user_img, {
    req(input$user_img)
    raw <- magick::image_read(input$user_img$datapath)
    rv$orig_img <- raw
    rv$current_img <- raw
    rv$markings <- rv$markings[0, ]
  })
  
  # Reset Image
  observeEvent(input$btn_reset_img, {
    req(rv$orig_img)
    rv$current_img <- rv$orig_img
    rv$markings <- rv$markings[0, ]
    rv$click_active <- FALSE
    rv$temp_pt <- NULL
    session$resetBrush("plot_brush")
  })
  
  # 2. Pure Free-Form Crop Handler
  observeEvent(input$btn_apply_crop, {
    req(rv$current_img, input$plot_brush)
    b <- input$plot_brush
    info <- magick::image_info(rv$current_img)
    img_h <- info$height
    
    # Calculate crop dimensions directly from brush bounding box
    crop_w <- round(abs(b$xmax - b$xmin))
    crop_h <- round(abs(b$ymax - b$ymin))
    crop_x <- round(b$xmin)
    crop_y <- round(img_h - b$ymax) # Translate ggplot y-origin to Magick top-left
    
    # Execute crop
    geo <- sprintf("%dx%d+%d+%d", crop_w, crop_h, crop_x, crop_y)
    rv$current_img <- magick::image_crop(rv$current_img, geo)
    rv$markings <- rv$markings[0, ] # Clear markings after crop to prevent offset shift
    session$resetBrush("plot_brush")
  })
  
  # 3A. Add Circle / Rectangle via Dragged Area
  observeEvent(input$btn_add_drag_shape, {
    req(rv$current_img, input$plot_brush)
    b <- input$plot_brush
    
    new_mark <- data.frame(
      type = input$shape_type,
      x1 = round(b$xmin, 1),
      y1 = round(b$ymin, 1),
      x2 = round(b$xmax, 1),
      y2 = round(b$ymax, 1),
      color = input$shape_color,
      thick = input$shape_thick,
      text = input$text_label,
      head_size = 3.5,
      head_angle = 18,
      stringsAsFactors = FALSE
    )
    
    rv$markings <- rbind(rv$markings, new_mark)
    session$resetBrush("plot_brush")
  })
  
  # 3B. Arrow Placement via 2-Click System
  observeEvent(input$btn_start_click_mode, {
    rv$click_active <- TRUE
    rv$temp_pt <- NULL
  })
  
  output$click_mode_status <- renderText({
    if (!rv$click_active) return("Status: Idle")
    if (input$shape_type == "text") {
      return("Status: Click preview image to place text label.")
    } else {
      if (is.null(rv$temp_pt)) {
        return("Status: [Click 1/2] Click preview to place ARROW TIP (Head).")
      } else {
        return("Status: [Click 2/2] Click preview to place ARROW TAIL.")
      }
    }
  })
  
  observeEvent(input$plot_click, {
    if (!rv$click_active) return()
    
    click_x <- round(input$plot_click$x, 1)
    click_y <- round(input$plot_click$y, 1)
    
    # Text Placement
    if (input$shape_type == "text") {
      new_mark <- data.frame(
        type = "text",
        x1 = click_x, y1 = click_y, x2 = click_x, y2 = click_y,
        color = input$shape_color, thick = input$shape_thick, text = input$text_label,
        head_size = 3.5, head_angle = 18,
        stringsAsFactors = FALSE
      )
      rv$markings <- rbind(rv$markings, new_mark)
      rv$click_active <- FALSE
    } 
    # Precision Arrow Placement
    else if (input$shape_type == "arrow") {
      if (is.null(rv$temp_pt)) {
        rv$temp_pt <- c(x = click_x, y = click_y) # Store Tip
      } else {
        new_mark <- data.frame(
          type = "arrow",
          x1 = rv$temp_pt["x"], y1 = rv$temp_pt["y"], # Tip
          x2 = click_x, y2 = click_y,                 # Tail
          color = input$shape_color, 
          thick = input$shape_thick, 
          text = input$text_label,
          head_size = input$arrow_head_size,
          head_angle = as.numeric(input$arrow_style),
          stringsAsFactors = FALSE
        )
        rv$markings <- rbind(rv$markings, new_mark)
        rv$click_active <- FALSE
        rv$temp_pt <- NULL
      }
    }
  })
  
  # Undo & Clear Handlers
  observeEvent(input$btn_undo, {
    if (nrow(rv$markings) > 0) {
      rv$markings <- rv$markings[-nrow(rv$markings), ]
    }
  })
  
  observeEvent(input$btn_clear, {
    rv$markings <- rv$markings[0, ]
  })
  
  # 4. Canvas Plot Generator
  canvas_plot <- reactive({
    if (is.null(rv$current_img)) {
      p <- ggplot() + 
        theme_void() + 
        xlim(0, 100) + ylim(0, 100) +
        annotate("text", x = 50, y = 50, label = "Upload an image to start in AnnotateSight", color = "#888888", size = 6)
      return(p)
    }
    
    info <- magick::image_info(rv$current_img)
    w <- info$width
    h <- info$height
    
    raster_img <- as.raster(rv$current_img)
    
    p <- ggplot() + 
      annotation_raster(raster_img, xmin = 0, xmax = w, ymin = 0, ymax = h) +
      scale_x_continuous(limits = c(0, w), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, h), expand = c(0, 0)) +
      theme_void() +
      coord_fixed()
    
    # Render all markings with physical millimeter dimensions
    if (nrow(rv$markings) > 0) {
      for (k in 1:nrow(rv$markings)) {
        m <- rv$markings[k, ]
        
        # CIRCLE / OVAL
        if (m$type == "circle") {
          cx <- (m$x1 + m$x2) / 2
          cy <- (m$y1 + m$y2) / 2
          rx <- abs(m$x2 - m$x1) / 2
          ry <- abs(m$y2 - m$y1) / 2
          t <- seq(0, 2 * pi, length.out = 100)
          ellipse_df <- data.frame(x = cx + rx * cos(t), y = cy + ry * sin(t))
          
          p <- p + annotate("path", x = ellipse_df$x, y = ellipse_df$y, 
                            color = m$color, linewidth = m$thick)
        }
        # RECTANGLE BOX
        else if (m$type == "rect") {
          p <- p + annotate("rect", xmin = min(m$x1, m$x2), xmax = max(m$x1, m$x2),
                            ymin = min(m$y1, m$y2), ymax = max(m$y1, m$y2),
                            color = m$color, fill = NA, linewidth = m$thick)
        }
        # HIGH-PRECISION MEDICAL ARROW
        else if (m$type == "arrow") {
          p <- p + annotate("segment", 
                           x = m$x2, y = m$y2,       # Tail
                           xend = m$x1, yend = m$y1, # Tip
                           arrow = arrow(length = unit(m$head_size, "mm"), 
                                         angle = m$head_angle, 
                                         type = "closed"), 
                           color = m$color, 
                           linewidth = m$thick)
        }
        # TEXT LABEL
        else if (m$type == "text") {
          p <- p + annotate("text", x = m$x1, y = m$y1, label = m$text,
                            color = m$color, fontface = "bold", size = m$thick * 4, hjust = 0)
        }
      }
    }
    
    # Temporary indicator for Arrow Tip click
    if (!is.null(rv$temp_pt)) {
      p <- p + annotate("point", x = rv$temp_pt["x"], y = rv$temp_pt["y"], 
                        color = "cyan", size = 4, shape = 19)
    }
    
    return(p)
  })
  
  # Synchronized preview container
  output$preview_container <- renderUI({
    req(rv$current_img, input$fig_width)
    info <- magick::image_info(rv$current_img)
    aspect_ratio <- info$width / info$height
    
    preview_w <- 600
    preview_h <- round(preview_w / aspect_ratio)
    
    plotOutput("image_preview", 
               click = "plot_click",
               brush = brushOpts(id = "plot_brush", resetOnNew = FALSE),
               width = paste0(preview_w, "px"), 
               height = paste0(preview_h, "px"))
  })
  
  output$image_preview <- renderPlot({
    canvas_plot()
  })
  
  # 5. Synchronized Export Handler
  output$download_fig <- downloadHandler(
    filename = function() {
      paste0("AnnotateSight_Figure.", input$export_format)
    },
    content = function(file) {
      req(rv$current_img)
      info <- magick::image_info(rv$current_img)
      
      fig_w_in <- input$fig_width
      fig_h_in <- fig_w_in * (info$height / info$width)
      target_dpi <- as.numeric(input$export_dpi)
      
      ggsave(
        filename = file, 
        plot = canvas_plot(), 
        width = fig_w_in, 
        height = fig_h_in, 
        dpi = target_dpi, 
        device = input$export_format
      )
    }
  )
}

shinyApp(ui = ui, server = server)
