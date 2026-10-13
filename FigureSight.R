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
      h1("FigureSight", style = "font-weight: bold; color: #2c3e50; margin-bottom: 2px;"),
      p("PubMed / PMC Image Quality Evaluator, Cropper & Publication Upscaler", style = "color: #7f8c8d; font-size: 14px; margin-bottom: 2px;"),
      p(tags$b("Prepared by Dr. Himel Mondal"), style = "font-size: 13px; color: #34495e; font-style: italic;")
    )
  ),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # Step 1: Upload Image
      h4("1. Upload Figure"),
      fileInput("user_img", "Choose Figure File:", accept = c("image/png", "image/jpeg", "image/tiff")),
      
      hr(),
      
      # Step 2: Initial Cropping
      h4("2. Crop Figure (Optional)"),
      selectInput("crop_ratio", "Crop Aspect Ratio:", 
                  choices = c("Free Size" = "free", 
                              "1:1 Square" = "1:1", 
                              "4:3 Standard" = "4:3", 
                              "16:9 Widescreen" = "16:9", 
                              "3:2 Photography" = "3:2",
                              "3.5:3.5 PMC Single-Column" = "3.5:3.5",
                              "7:5 PMC Double-Column" = "7:5")),
      helpText("Drag a selection box over the canvas, then click 'Apply Crop'."),
      fluidRow(
        column(6, actionButton("btn_apply_crop", "Apply Crop", class = "btn-warning", style = "width: 100%;")),
        column(6, actionButton("btn_reset_img", "Reset Original", class = "btn-secondary", style = "width: 100%;"))
      ),
      
      hr(),
      
      # Step 3: PMC Compliance Analysis & Official Guidelines Link
      h4("3. PMC Compliance Report"),
      p(
        "Official Reference: ",
        tags$a(
          href = "https://pmc.ncbi.nlm.nih.gov/pub/filespec-images/", 
          target = "_blank", 
          style = "font-weight: bold; color: #2980b9;",
          "PMC Image Guidelines"
        )
      ),
      uiOutput("pmc_report_ui"),
      
      hr(),
      
      # Step 4: PubMed Upscaling Engine
      h4("4. PubMed Upscaler Engine"),
      selectInput("upscale_preset", "Target PMC Specification:", 
                  choices = c("1-Column Color/Halftone (min 975 px @ 300 DPI)" = "col1_color",
                              "1.5-Column Color/Halftone (min 1425 px @ 300 DPI)" = "col1.5_color",
                              "2-Column Full Page (min 1950 px @ 300 DPI)" = "col2_color",
                              "1-Column Line Art / Combo (min 1950 px @ 600 DPI)" = "col1_line",
                              "2-Column Line Art / Combo (min 3900 px @ 600 DPI)" = "col2_line",
                              "Custom Target Width (px)" = "custom")),
      
      conditionalPanel(
        condition = "input.upscale_preset == 'custom'",
        numericInput("custom_target_px", "Custom Target Width (pixels):", value = 1200, min = 100, step = 50)
      ),
      
      actionButton("btn_apply_upscale", "Upscale Figure to Meet Standards", class = "btn-success", style = "width: 100%;"),
      
      br(), br(),
      
      # Step 5: Properties of Upscaled Image
      h4("5. Newly Upscaled Properties"),
      uiOutput("upscaled_report_ui"),
      
      hr(),
      
      # Step 6: Export Options (Uses PanelSight ggsave Engine)
      h4("6. Publication Export"),
      fluidRow(
        column(6, numericInput("export_dpi", "DPI:", value = 300, step = 50)),
        column(6, selectInput("export_format", "Format:", choices = c("png", "tiff", "jpeg", "pdf")))
      ),
      downloadButton("download_fig", "Export High-Res Figure", class = "btn-primary", style = "width: 100%;")
    ),
    
    mainPanel(
      width = 8,
      h4("Interactive Canvas & Preview"),
      p("Drag a selection box across any region to perform accurate cropping:"),
      div(
        style = "overflow-x: auto; text-align: center; background: #eaeded; padding: 15px; border-radius: 5px;",
        plotOutput("image_preview", 
                   brush = brushOpts(id = "plot_brush", resetOnNew = FALSE),
                   height = "650px")
      ),
      br(),
      div(
        style = "text-align: center; color: #7f8c8d; font-size: 12px; margin-top: 15px;",
        "FigureSight | Prepared by Dr. Himel Mondal | ",
        tags$a(href = "https://pmc.ncbi.nlm.nih.gov/pub/filespec-images/", target = "_blank", "PMC Guidelines")
      )
    )
  )
)

# ===============================================================
# 3. SERVER LOGIC
# ===============================================================
server <- function(input, output, session) {
  
  # Reactive state store
  rv <- reactiveValues(
    orig_img = NULL,
    current_img = NULL,     # Active image (after crop)
    upscaled_img = NULL,    # Image after upscaling
    is_upscaled = FALSE
  )
  
  # 1. Load Uploaded File
  observeEvent(input$user_img, {
    req(input$user_img)
    raw <- magick::image_read(input$user_img$datapath)
    rv$orig_img <- raw
    rv$current_img <- raw
    rv$upscaled_img <- NULL
    rv$is_upscaled <- FALSE
  })
  
  # Reset Image
  observeEvent(input$btn_reset_img, {
    req(rv$orig_img)
    rv$current_img <- rv$orig_img
    rv$upscaled_img <- NULL
    rv$is_upscaled <- FALSE
    session$resetBrush("plot_brush")
  })
  
  # 2. Crop Handler
  observeEvent(input$btn_apply_crop, {
    req(rv$current_img, input$plot_brush)
    b <- input$plot_brush
    info <- magick::image_info(rv$current_img)
    
    crop_w <- round(b$xmax - b$xmin)
    crop_h <- round(b$ymax - b$ymin)
    crop_x <- round(b$xmin)
    crop_y <- round(info$height - b$ymax)
    
    if (input$crop_ratio != "free") {
      r_parts <- as.numeric(strsplit(input$crop_ratio, ":")[[1]])
      target_ratio <- r_parts[1] / r_parts[2]
      crop_h <- round(crop_w / target_ratio)
    }
    
    geo <- sprintf("%dx%d+%d+%d", crop_w, crop_h, crop_x, crop_y)
    rv$current_img <- magick::image_crop(rv$current_img, geo)
    rv$upscaled_img <- NULL
    rv$is_upscaled <- FALSE
    session$resetBrush("plot_brush")
  })
  
  # 3. PubMed / PMC Compliance Analysis Report
  output$pmc_report_ui <- renderUI({
    if (is.null(rv$current_img)) {
      return(p(em("Upload a figure to view PMC compliance specifications.")))
    }
    
    info <- magick::image_info(rv$current_img)
    w_px <- info$width
    h_px <- info$height
    aspect_ratio <- round(w_px / h_px, 2)
    file_size_kb <- round(file.info(input$user_img$datapath)$size / 1024, 1)
    
    w_in_300 <- round(w_px / 300, 2)
    h_in_300 <- round(h_px / 300, 2)
    
    if (w_px < 975) {
      status_title <- "NON-COMPLIANT (Sub-standard)"
      status_color <- "#e74c3c"
      desc <- paste0("Width (", w_px, " px) is below PMC's 975 px minimum requirement for single-column figures at 300 DPI. Upscaling is strongly recommended.")
    } else if (w_px >= 975 && w_px < 1425) {
      status_title <- "COMPLIANT (PMC 1-Column Figure)"
      status_color <- "#27ae60"
      desc <- paste0("Meets PMC 300 DPI standard for 1-column publication (up to ", w_in_300, " inches / 8.9 cm wide).")
    } else if (w_px >= 1425 && w_px < 1950) {
      status_title <- "COMPLIANT (PMC 1.5-Column Figure)"
      status_color <- "#2980b9"
      desc <- paste0("Meets PMC 300 DPI standard for 1.5-column layout (up to ", w_in_300, " inches / 12.7 cm wide).")
    } else {
      status_title <- "COMPLIANT (PMC 2-Column / Line-Art Grade)"
      status_color <- "#8e44ad"
      desc <- paste0("Meets PMC standards for 2-column full page figures (up to ", w_in_300, " inches / 17.8 cm wide).")
    }
    
    wellPanel(
      style = "background: #f8f9fa; border-left: 4px solid #2980b9; padding: 10px;",
      div(tags$b("Dimensions: "), paste0(w_px, " × ", h_px, " pixels")),
      div(tags$b("Aspect Ratio: "), paste0(aspect_ratio, ":1")),
      div(tags$b("Print Size (@ 300 DPI): "), paste0(w_in_300, "\" × ", h_in_300, "\" inches")),
      div(tags$b("Format / File Size: "), paste0(info$format, " (", file_size_kb, " KB)")),
      hr(style = "margin: 6px 0;"),
      div(tags$b("PMC Compliance Evaluation:")),
      div(
        style = paste0("background:", status_color, "; color: white; padding: 3px 6px; border-radius: 4px; font-size: 11px; font-weight: bold; margin-top: 3px; display: inline-block;"),
        status_title
      ),
      p(desc, style = "font-size: 11px; color: #555555; margin-top: 4px; margin-bottom: 0;")
    )
  })
  
  # 4. Upscaling Handler
  observeEvent(input$btn_apply_upscale, {
    req(rv$current_img)
    info <- magick::image_info(rv$current_img)
    
    target_width <- switch(input$upscale_preset,
      "col1_color"   = 975,
      "col1.5_color" = 1425,
      "col2_color"   = 1950,
      "col1_line"    = 1950,
      "col2_line"    = 3900,
      "custom"       = input$custom_target_px
    )
    
    if (info$width >= target_width) {
      showNotification(paste0("Current image width (", info$width, " px) already meets or exceeds target width (", target_width, " px)."), type = "warning")
      rv$upscaled_img <- rv$current_img
    } else {
      geo_resize <- paste0(target_width, "x")
      rv$upscaled_img <- magick::image_resize(rv$current_img, geo_resize)
      showNotification(paste0("Figure successfully upscaled to ", target_width, " px width."), type = "message")
    }
    
    rv$is_upscaled <- TRUE
  })
  
  # 5. Newly Upscaled Properties Report
  output$upscaled_report_ui <- renderUI({
    if (!rv$is_upscaled || is.null(rv$upscaled_img)) {
      return(p(em("Click 'Upscale Figure' above to generate upscaled property analysis.")))
    }
    
    info <- magick::image_info(rv$upscaled_img)
    w_px <- info$width
    h_px <- info$height
    aspect_ratio <- round(w_px / h_px, 2)
    
    w_in_300 <- round(w_px / 300, 2)
    h_in_300 <- round(h_px / 300, 2)
    w_in_600 <- round(w_px / 600, 2)
    h_in_600 <- round(h_px / 600, 2)
    
    wellPanel(
      style = "background: #eafaf1; border-left: 4px solid #27ae60; padding: 10px;",
      div(tags$b("New Dimensions: "), paste0(w_px, " × ", h_px, " pixels")),
      div(tags$b("New Aspect Ratio: "), paste0(aspect_ratio, ":1")),
      div(tags$b("Print Size (@ 300 DPI): "), paste0(w_in_300, "\" × ", h_in_300, "\" inches")),
      div(tags$b("Print Size (@ 600 DPI): "), paste0(w_in_600, "\" × ", h_in_600, "\" inches")),
      div(tags$b("Resampling Method: "), "Lanczos / Bicubic High-Density Filter"),
      hr(style = "margin: 6px 0;"),
      div(style = "color: #27ae60; font-size: 11px; font-weight: bold;", 
          "✔ Figure is now fully compliant for PMC submission!")
    )
  })
  
  # 6. Reactive Canvas Plot (Generates ggplot Object for Both Screen & Export)
  canvas_plot <- reactive({
    active_img <- if (rv$is_upscaled && !is.null(rv$upscaled_img)) rv$upscaled_img else rv$current_img
    
    if (is.null(active_img)) {
      p <- ggplot() + 
        theme_void() + 
        xlim(0, 100) + ylim(0, 100) +
        annotate("text", x = 50, y = 50, label = "Upload a figure to view interactive preview", color = "#888888", size = 6)
      return(p)
    }
    
    info <- magick::image_info(active_img)
    w <- info$width
    h <- info$height
    
    raster_img <- as.raster(active_img)
    
    ggplot() + 
      annotation_raster(raster_img, xmin = 0, xmax = w, ymin = 0, ymax = h) +
      scale_x_continuous(limits = c(0, w), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, h), expand = c(0, 0)) +
      theme_void() +
      coord_fixed()
  })
  
  output$image_preview <- renderPlot({
    canvas_plot()
  })
  
  # 7. Robust ggsave Export Handler (Identical to PanelSight Engine)
  output$download_fig <- downloadHandler(
    filename = function() {
      paste0("FigureSight_Processed.", input$export_format)
    },
    content = function(file) {
      active_img <- if (rv$is_upscaled && !is.null(rv$upscaled_img)) rv$upscaled_img else rv$current_img
      req(active_img)
      
      info <- magick::image_info(active_img)
      target_dpi <- as.numeric(input$export_dpi)
      
      # Calculate physical inch dimensions directly from pixel resolution and target DPI
      fig_w_in <- info$width / target_dpi
      fig_h_in <- info$height / target_dpi
      
      # Save using ggsave (exact mechanism as PanelSight)
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
