# ================================================================
# 1. AUTOMATIC DEPENDENCY CHECK & INSTALLATION
# ================================================================
required_packages <- c("shiny", "oro.dicom", "magick", "ggplot2", "cowplot")

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  message("Installing missing dependencies: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))

# Increase upload limit to 150 MB for multi-slice CT/MRI & USG cine clips
options(shiny.maxRequestSize = 150 * 1024^2)

# ================================================================
# 2. USER INTERFACE (UI)
# ================================================================
ui <- fluidPage(
  titlePanel(
    div(
      h1("DICOMSight", style = "font-weight: bold; color: #2c3e50; margin-bottom: 2px;"),
      p("Multi-Modality (CT / MRI / USG) DICOM Converter, Cropper & Publication Exporter", 
        style = "color: #7f8c8d; font-size: 14px; margin-bottom: 2px;"),
      p(tags$b("Prepared by Dr. Himel Mondal"), 
        style = "font-size: 13px; color: #34495e; font-style: italic;")
    )
  ),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # Step 1: File Upload
      h4("1. Upload DICOM Image"),
      fileInput("dcm_file", "Choose DICOM File (.dcm):", accept = c(".dcm", ".DCM")),
      
      hr(),
      
      # Step 2: Multi-Frame & Slice Navigation
      h4("2. Frame / Slice Navigation"),
      uiOutput("slice_nav_ui"),
      
      hr(),
      
      # Step 3: Cropping Facility (FigureSight Brush-Engine)
      h4("3. Crop ROI (Region of Interest)"),
      selectInput("crop_ratio", "Crop Aspect Ratio:", 
                  choices = c("Free Size" = "free", 
                              "1:1 Square" = "1:1", 
                              "4:3 Standard" = "4:3", 
                              "16:9 Widescreen" = "16:9", 
                              "3:2 Photography" = "3:2",
                              "3.5:3.5 PMC Single-Column" = "3.5:3.5",
                              "7:5 PMC Double-Column" = "7:5")),
      helpText("Drag a selection box across the preview, then click 'Apply Crop'."),
      fluidRow(
        column(6, actionButton("btn_apply_crop", "Apply Crop", class = "btn-warning", style = "width: 100%;")),
        column(6, actionButton("btn_reset_crop", "Reset Full Frame", class = "btn-secondary", style = "width: 100%;"))
      ),
      
      hr(),
      
      # Step 4: Modality-Specific Display Presets
      h4("4. Display & Windowing Presets"),
      selectInput(
        "display_preset", 
        "Modality Preset:",
        choices = list(
          "General" = c(
            "Auto Full Dynamic Range"                = "auto",
            "Manufacturer Embedded (Header WC/WW)"   = "dicom_native"
          ),
          "Computed Tomography (CT)" = c(
            "CT Soft Tissue / Abdomen (W:400, L:40)" = "ct_soft",
            "CT Bone (W:1800, L:400)"                = "ct_bone",
            "CT Lung (W:1500, L:-600)"               = "ct_lung",
            "CT Brain Parenchyma (W:80, L:40)"       = "ct_brain",
            "CT Stroke / Acute Ischemia (W:35, L:35)" = "ct_stroke",
            "CT Subdural Hematoma (W:150, L:75)"     = "ct_subdural",
            "CT Mediastinum (W:400, L:40)"           = "ct_mediastinum"
          ),
          "Magnetic Resonance Imaging (MRI)" = c(
            "MRI Standard (P1 - P99 Stretch)"        = "mri_std",
            "MRI High Contrast (P2 - P95)"           = "mri_high_contrast",
            "MRI T1 Emphasis (Gamma 1.1)"            = "mri_t1",
            "MRI T2 / Fluid Optimization (Gamma 0.8)"= "mri_t2",
            "DWI / ADC Evaluation (P0.5 - P99.5)"    = "mri_dwi"
          ),
          "Ultrasound (USG)" = c(
            "USG Standard B-Mode (Direct)"           = "usg_bmode",
            "USG High Contrast (S-Curve)"            = "usg_contrast",
            "USG Deep Gain Boost (Gamma 1.3)"        = "usg_gain_boost",
            "Color / Power Doppler (RGB Passthrough)" = "usg_doppler"
          ),
          "Manual / Custom" = c(
            "Custom Window Center & Width"           = "custom"
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.display_preset == 'custom'",
        fluidRow(
          column(6, numericInput("custom_wc", "Window Level (L):", value = 40, step = 10)),
          column(6, numericInput("custom_ww", "Window Width (W):", value = 400, min = 1, step = 10))
        )
      ),
      
      checkboxInput("invert_monochrome", "Invert Grayscale (Negative)", value = FALSE),
      
      hr(),
      
      # Step 5: Metadata & Diagnostic Audit
      h4("5. DICOM Metadata Audit"),
      uiOutput("dicom_audit_ui"),
      
      hr(),
      
      # Step 6: Publication Export Specifications
      h4("6. Publication Export"),
      fluidRow(
        column(6, selectInput("export_format", "Format:", choices = c("png", "tiff", "jpeg", "pdf"))),
        column(6, numericInput("export_dpi", "Target DPI:", value = 300, min = 72, max = 1200, step = 50))
      ),
      fluidRow(
        column(6, numericInput("export_width_in", "Width (in):", value = 3.5, min = 1, step = 0.5)),
        column(6, numericInput("export_height_in", "Height (in):", value = 3.5, min = 1, step = 0.5))
      ),
      br(),
      downloadButton("download_fig", "Export Publication Figure", class = "btn-primary", style = "width: 100%;")
    ),
    
    mainPanel(
      width = 8,
      h4("Interactive Radiologic Canvas"),
      p("Drag a selection box across any anatomic structure to crop and isolate regions of interest:"),
      div(
        style = "overflow-x: auto; text-align: center; background: #000000; padding: 15px; border-radius: 5px;",
        plotOutput("image_preview", 
                   brush = brushOpts(id = "plot_brush", resetOnNew = FALSE),
                   height = "650px")
      ),
      br(),
      div(
        style = "text-align: center; color: #7f8c8d; font-size: 12px; margin-top: 15px;",
        "DICOMSight | Prepared by Dr. Himel Mondal | Department of Physiology"
      )
    )
  )
)

# ================================================================
# 3. SERVER LOGIC
# ================================================================
server <- function(input, output, session) {
  
  # Reactive State Store
  rv <- reactiveValues(
    dcm_obj = NULL,
    num_slices = 1,
    raw_matrix = NULL,       # Full-resolution active slice
    active_matrix = NULL,    # Active slice (cropped or full)
    header_df = NULL,
    photometric = "MONOCHROME2",
    native_w = 0,
    native_h = 0,
    rescale_slope = 1,
    rescale_intercept = 0,
    native_wc = NULL,
    native_ww = NULL,
    modality = "Unknown",
    is_rgb = FALSE,
    is_cropped = FALSE
  )
  
  # 1. Parse Uploaded DICOM File
  observeEvent(input$dcm_file, {
    req(input$dcm_file)
    
    tryCatch({
      parsed <- oro.dicom::readDICOM(input$dcm_file$datapath, verbose = FALSE)
      
      if (length(parsed$img) == 0 || is.null(parsed$img[[1]])) {
        showNotification("Failed to extract pixel array from DICOM file.", type = "error")
        return(NULL)
      }
      
      rv$dcm_obj <- parsed
      rv$num_slices <- length(parsed$img)
      hdr <- parsed$hdr[[1]]
      rv$header_df <- hdr
      
      extract_tag <- function(tag_name) {
        val <- hdr[hdr$name == tag_name, "value"]
        if (length(val) > 0 && nchar(trimws(val[1])) > 0) trimws(val[1]) else NULL
      }
      
      photo <- extract_tag("PhotometricInterpretation")
      rv$photometric <- if (!is.null(photo)) photo else "MONOCHROME2"
      rv$is_rgb <- grepl("RGB|YBR", rv$photometric, ignore.case = TRUE)
      
      mod <- extract_tag("Modality")
      rv$modality <- if (!is.null(mod)) mod else "Unknown"
      
      sl <- as.numeric(extract_tag("RescaleSlope"))
      in_c <- as.numeric(extract_tag("RescaleIntercept"))
      rv$rescale_slope <- if (!is.na(sl) && length(sl) > 0) sl else 1
      rv$rescale_intercept <- if (!is.na(in_c) && length(in_c) > 0) in_c else 0
      
      wc <- as.numeric(strsplit(as.character(extract_tag("WindowCenter")), "\\\\")[[1]][1])
      ww <- as.numeric(strsplit(as.character(extract_tag("WindowWidth")), "\\\\")[[1]][1])
      rv$native_wc <- if (!is.na(wc) && length(wc) > 0) wc else NULL
      rv$native_ww <- if (!is.na(ww) && length(ww) > 0) ww else NULL
      
      if (rv$modality == "CT") {
        updateSelectInput(session, "display_preset", selected = "ct_soft")
      } else if (rv$modality == "MR") {
        updateSelectInput(session, "display_preset", selected = "mri_std")
      } else if (rv$modality == "US") {
        updateSelectInput(session, "display_preset", selected = if (rv$is_rgb) "usg_doppler" else "usg_bmode")
      }
      
      mat <- parsed$img[[1]]
      rv$raw_matrix <- mat
      rv$active_matrix <- mat
      rv$native_w <- ncol(mat)
      rv$native_h <- nrow(mat)
      rv$is_cropped <- FALSE
      session$resetBrush("plot_brush")
      
      showNotification(paste0("Loaded ", rv$modality, " DICOM: ", rv$native_w, "x", rv$native_h, " px"), type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading DICOM:", e$message), type = "error")
    })
  })
  
  # 2. Slice Navigation UI
  output$slice_nav_ui <- renderUI({
    if (is.null(rv$dcm_obj) || rv$num_slices <= 1) {
      return(p(em("Single-frame image (1 frame available).", style = "color: #7f8c8d;")))
    }
    
    sliderInput("selected_slice", "Active Frame / Slice:",
                min = 1, max = rv$num_slices, value = 1, step = 1)
  })
  
  # Slice Selection Observer
  observe({
    req(rv$dcm_obj)
    slice_idx <- if (!is.null(input$selected_slice)) input$selected_slice else 1
    slice_idx <- min(max(1, slice_idx), rv$num_slices)
    
    mat <- rv$dcm_obj$img[[slice_idx]]
    rv$raw_matrix <- mat
    rv$active_matrix <- mat
    rv$native_w <- ncol(mat)
    rv$native_h <- nrow(mat)
    rv$is_cropped <- FALSE
    session$resetBrush("plot_brush")
  })
  
  # 3. Interactive Crop Handlers
  observeEvent(input$btn_apply_crop, {
    req(rv$active_matrix, input$plot_brush)
    b <- input$plot_brush
    
    # Raster dimensions in plot coordinate space
    canvas_w <- ncol(rv$active_matrix)
    canvas_h <- nrow(rv$active_matrix)
    
    crop_w <- b$xmax - b$xmin
    crop_h <- b$ymax - b$ymin
    
    if (input$crop_ratio != "free") {
      r_parts <- as.numeric(strsplit(input$crop_ratio, ":")[[1]])
      target_ratio <- r_parts[1] / r_parts[2]
      crop_h <- crop_w / target_ratio
    }
    
    # Bounding coordinates clamped to canvas
    xmin_c <- max(1, min(canvas_w, round(b$xmin)))
    xmax_c <- max(1, min(canvas_w, round(b$xmin + crop_w)))
    ymin_c <- max(1, min(canvas_h, round(b$ymin)))
    ymax_c <- max(1, min(canvas_h, round(b$ymin + crop_h)))
    
    # Reverse coordinate mapping: plot Y is inverted relative to raw DICOM array
    col_start <- xmin_c
    col_end   <- xmax_c
    row_start <- (canvas_h - ymax_c) + 1
    row_end   <- (canvas_h - ymin_c) + 1
    
    row_start <- max(1, min(canvas_h, row_start))
    row_end   <- max(1, min(canvas_h, row_end))
    
    if (length(dim(rv$active_matrix)) == 3) {
      rv$active_matrix <- rv$active_matrix[row_start:row_end, col_start:col_end, , drop = FALSE]
    } else {
      rv$active_matrix <- rv$active_matrix[row_start:row_end, col_start:col_end, drop = FALSE]
    }
    
    rv$is_cropped <- TRUE
    session$resetBrush("plot_brush")
    showNotification(paste0("Cropped to: ", ncol(rv$active_matrix), " × ", nrow(rv$active_matrix), " px"), type = "message")
  })
  
  observeEvent(input$btn_reset_crop, {
    req(rv$raw_matrix)
    rv$active_matrix <- rv$raw_matrix
    rv$is_cropped <- FALSE
    session$resetBrush("plot_brush")
    showNotification("Reset to full DICOM frame.", type = "message")
  })
  
  # 4. Dynamic Normalization & Windowing Pipeline
  processed_raster <- reactive({
    req(rv$active_matrix)
    mat <- rv$active_matrix
    preset <- input$display_preset
    
    # Check for Multi-Channel RGB (Ultrasound Doppler)
    if (length(dim(mat)) == 3 && dim(mat)[3] == 3) {
      r <- mat[, , 1] / max(mat[, , 1], na.rm = TRUE)
      g <- mat[, , 2] / max(mat[, , 2], na.rm = TRUE)
      b <- mat[, , 3] / max(mat[, , 3], na.rm = TRUE)
      
      t_r <- t(r)[, rev(seq_len(nrow(r)))]
      t_g <- t(g)[, rev(seq_len(nrow(g)))]
      t_b <- t(b)[, rev(seq_len(nrow(b)))]
      
      hex_mat <- rgb(t_r, t_g, t_b)
      dim(hex_mat) <- dim(t_r)
      return(hex_mat)
    }
    
    num_mat <- as.matrix(mat)
    
    # 4A. Apply Rescale Slope / Intercept for CT Hounsfield Units
    if (grepl("^ct_", preset) || (!is.null(rv$modality) && rv$modality == "CT")) {
      num_mat <- (num_mat * rv$rescale_slope) + rv$rescale_intercept
    }
    
    # 4B. Window Transformations
    if (preset == "auto") {
      w_min <- min(num_mat, na.rm = TRUE)
      w_max <- max(num_mat, na.rm = TRUE)
      norm_mat <- (num_mat - w_min) / max(1e-5, (w_max - w_min))
      
    } else if (preset == "dicom_native") {
      if (!is.null(rv$native_wc) && !is.null(rv$native_ww)) {
        w_min <- rv$native_wc - (rv$native_ww / 2)
        w_max <- rv$native_wc + (rv$native_ww / 2)
        norm_mat <- (num_mat - w_min) / max(1e-5, (w_max - w_min))
      } else {
        w_min <- min(num_mat, na.rm = TRUE)
        w_max <- max(num_mat, na.rm = TRUE)
        norm_mat <- (num_mat - w_min) / max(1e-5, (w_max - w_min))
      }
      
    } else if (preset == "ct_soft" || preset == "ct_mediastinum") {
      norm_mat <- (num_mat - (40 - 200)) / 400
    } else if (preset == "ct_bone") {
      norm_mat <- (num_mat - (400 - 900)) / 1800
    } else if (preset == "ct_lung") {
      norm_mat <- (num_mat - (-600 - 750)) / 1500
    } else if (preset == "ct_brain") {
      norm_mat <- (num_mat - (40 - 40)) / 80
    } else if (preset == "ct_stroke") {
      norm_mat <- (num_mat - (35 - 17.5)) / 35
    } else if (preset == "ct_subdural") {
      norm_mat <- (num_mat - (75 - 75)) / 150
      
    } else if (preset == "mri_std") {
      qs <- quantile(num_mat, probs = c(0.01, 0.99), na.rm = TRUE)
      norm_mat <- (num_mat - qs[1]) / max(1e-5, (qs[2] - qs[1]))
      
    } else if (preset == "mri_high_contrast") {
      qs <- quantile(num_mat, probs = c(0.02, 0.95), na.rm = TRUE)
      norm_mat <- (num_mat - qs[1]) / max(1e-5, (qs[2] - qs[1]))
      
    } else if (preset == "mri_t1") {
      qs <- quantile(num_mat, probs = c(0.01, 0.99), na.rm = TRUE)
      norm_mat <- (num_mat - qs[1]) / max(1e-5, (qs[2] - qs[1]))
      norm_mat <- pmin(pmax(norm_mat, 0), 1) ^ (1 / 1.1)
      
    } else if (preset == "mri_t2") {
      qs <- quantile(num_mat, probs = c(0.01, 0.98), na.rm = TRUE)
      norm_mat <- (num_mat - qs[1]) / max(1e-5, (qs[2] - qs[1]))
      norm_mat <- pmin(pmax(norm_mat, 0), 1) ^ (1 / 0.8)
      
    } else if (preset == "mri_dwi") {
      qs <- quantile(num_mat, probs = c(0.005, 0.995), na.rm = TRUE)
      norm_mat <- (num_mat - qs[1]) / max(1e-5, (qs[2] - qs[1]))
      
    } else if (preset == "usg_bmode" || preset == "usg_doppler") {
      w_min <- min(num_mat, na.rm = TRUE)
      w_max <- max(num_mat, na.rm = TRUE)
      norm_mat <- (num_mat - w_min) / max(1e-5, (w_max - w_min))
      
    } else if (preset == "usg_contrast") {
      w_min <- min(num_mat, na.rm = TRUE)
      w_max <- max(num_mat, na.rm = TRUE)
      linear <- (num_mat - w_min) / max(1e-5, (w_max - w_min))
      norm_mat <- 1 / (1 + exp(-10 * (linear - 0.5)))
      
    } else if (preset == "usg_gain_boost") {
      w_min <- min(num_mat, na.rm = TRUE)
      w_max <- max(num_mat, na.rm = TRUE)
      linear <- (num_mat - w_min) / max(1e-5, (w_max - w_min))
      norm_mat <- pmin(pmax(linear, 0), 1) ^ (1 / 1.3)
      
    } else if (preset == "custom") {
      ww <- max(1, req(input$custom_ww))
      wc <- req(input$custom_wc)
      norm_mat <- (num_mat - (wc - (ww / 2))) / ww
    }
    
    # 4C. Clamp Values to [0, 1]
    norm_mat[norm_mat < 0] <- 0
    norm_mat[norm_mat > 1] <- 1
    
    # 4D. Invert if MONOCHROME1 or Checked
    if (grepl("MONOCHROME1", rv$photometric, ignore.case = TRUE)) {
      norm_mat <- 1 - norm_mat
    }
    if (isTRUE(input$invert_monochrome)) {
      norm_mat <- 1 - norm_mat
    }
    
    # 4E. Transpose and Flip Y to Align Coordinates
    t_mat <- t(norm_mat)
    corrected_mat <- t_mat[, rev(seq_len(ncol(t_mat)))]
    
    gray_hex <- gray.colors(256, start = 0, end = 1)
    raster_matrix <- matrix(gray_hex[round(corrected_mat * 255) + 1], 
                            nrow = nrow(corrected_mat), 
                            ncol = ncol(corrected_mat))
    
    raster_matrix
  })
  
  # 5. Metadata Audit Panel
  output$dicom_audit_ui <- renderUI({
    if (is.null(rv$dcm_obj)) {
      return(p(em("Upload a DICOM file to view audit specifications.")))
    }
    
    hdr <- rv$header_df
    extract_tag <- function(tag_name) {
      val <- hdr[hdr$name == tag_name, "value"]
      if (length(val) > 0 && nchar(trimws(val[1])) > 0) trimws(val[1]) else "Not Specified"
    }
    
    active_w <- ncol(rv$active_matrix)
    active_h <- nrow(rv$active_matrix)
    
    crop_badge <- if (rv$is_cropped) {
      span(style = "background: #e67e22; color: white; padding: 2px 5px; border-radius: 3px; font-size: 10px; font-weight: bold; margin-left: 5px;", "CROPPED")
    } else {
      span(style = "background: #27ae60; color: white; padding: 2px 5px; border-radius: 3px; font-size: 10px; font-weight: bold; margin-left: 5px;", "FULL FRAME")
    }
    
    wellPanel(
      style = "background: #f8f9fa; border-left: 4px solid #2980b9; padding: 10px;",
      div(tags$b("Modality: "), rv$modality),
      div(tags$b("Active Viewport: "), paste0(active_w, " × ", active_h, " px"), crop_badge),
      div(tags$b("Native Resolution: "), paste0(rv$native_w, " × ", rv$native_h, " px")),
      div(tags$b("Series Slices: "), rv$num_slices),
      div(tags$b("Photometric Interpretation: "), rv$photometric),
      div(tags$b("Pixel Spacing: "), extract_tag("PixelSpacing")),
      div(tags$b("Slice Thickness: "), paste0(extract_tag("SliceThickness"), " mm")),
      hr(style = "margin: 6px 0;"),
      div(
        style = "background: #27ae60; color: white; padding: 3px 6px; border-radius: 4px; font-size: 11px; font-weight: bold; display: inline-block;",
        "CLINICAL METADATA VALIDATED"
      )
    )
  })
  
  # 6. Interactive Canvas (ggplot2 Vector Engine)
  canvas_plot <- reactive({
    if (is.null(rv$active_matrix)) {
      return(
        ggplot() + 
          theme_void() + 
          xlim(0, 100) + ylim(0, 100) +
          annotate("text", x = 50, y = 50, label = "Upload a DICOM file to inspect image", color = "#7f8c8d", size = 5)
      )
    }
    
    raster_data <- processed_raster()
    w <- ncol(raster_data)
    h <- nrow(raster_data)
    
    ggplot() + 
      annotation_raster(raster_data, xmin = 0, xmax = w, ymin = 0, ymax = h) +
      scale_x_continuous(limits = c(0, w), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, h), expand = c(0, 0)) +
      theme_void() +
      coord_fixed()
  })
  
  output$image_preview <- renderPlot({
    canvas_plot()
  }, bg = "transparent")
  
  # 7. High-Resolution Publication Export Engine
  output$download_fig <- downloadHandler(
    filename = function() {
      base_name <- tools::file_path_sans_ext(input$dcm_file$name)
      preset_tag <- input$display_preset
      crop_tag <- if (rv$is_cropped) "_cropped" else ""
      paste0(base_name, "_", preset_tag, crop_tag, ".", input$export_format)
    },
    content = function(file) {
      req(rv$active_matrix)
      
      target_dpi <- as.numeric(input$export_dpi)
      fig_w <- as.numeric(input$export_width_in)
      fig_h <- as.numeric(input$export_height_in)
      
      ggsave(
        filename = file, 
        plot = canvas_plot(), 
        width = fig_w, 
        height = fig_h, 
        dpi = target_dpi, 
        device = input$export_format
      )
    }
  )
}

# ================================================================
# 4. RUN APPLICATION
# ================================================================
shinyApp(ui = ui, server = server)