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
      
      # Step 3: Cropping Facility
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
    current_img = NULL,
    is_cropped = FALSE
  )
  
  # 1. Parse Uploaded DICOM File
  observeEvent(input$dcm_file, {
    req(input$dcm_file)
    
    tryCatch({
      parsed <- tryCatch({
        oro.dicom::readDICOMFile(input$dcm_file$datapath)
      }, error = function(e) {
        oro.dicom::readDICOM(input$dcm_file$datapath, verbose = FALSE)
      })
      
      if (!is.null(parsed$img) && is.list(parsed$img)) {
        img_list <- parsed$img
        hdr_df   <- parsed$hdr[[1]]
      } else if (!is.null(parsed$img)) {
        if (length(dim(parsed$img)) == 3 && dim(parsed$img)[3] > 3) {
          img_list <- lapply(seq_len(dim(parsed$img)[3]), function(i) parsed$img[, , i])
        } else {
          img_list <- list(parsed$img)
        }
        hdr_df <- parsed$hdr
      } else {
        showNotification("No image matrix found in this DICOM file.", type = "error")
        return(NULL)
      }
      
      rv$dcm_obj    <- list(hdr = list(hdr_df), img = img_list)
      rv$num_slices <- length(img_list)
      rv$header_df  <- hdr_df
      
      extract_tag <- function(tag_name) {
        rows <- which(hdr_df$name == tag_name)
        if (length(rows) > 0) {
          first_val <- trimws(as.character(hdr_df$value[rows[1]]))
          if (!is.na(first_val) && nchar(first_val) > 0) return(first_val)
        }
        return(NULL)
      }
      
      photo <- extract_tag("PhotometricInterpretation")
      rv$photometric <- if (!is.null(photo) && length(photo) == 1) photo else "MONOCHROME2"
      rv$is_rgb <- isTRUE(grepl("RGB|YBR", rv$photometric, ignore.case = TRUE))
      
      mod <- extract_tag("Modality")
      rv$modality <- if (!is.null(mod) && length(mod) == 1) mod else "Unknown"
      
      sl_str <- extract_tag("RescaleSlope")
      in_str <- extract_tag("RescaleIntercept")
      sl_num <- if (!is.null(sl_str)) suppressWarnings(as.numeric(sl_str)) else NA_real_
      in_num <- if (!is.null(in_str)) suppressWarnings(as.numeric(in_str)) else NA_real_
      
      rv$rescale_slope     <- if (length(sl_num) == 1 && !is.na(sl_num)) sl_num else 1
      rv$rescale_intercept <- if (length(in_num) == 1 && !is.na(in_num)) in_num else 0
      
      wc_str <- extract_tag("WindowCenter")
      ww_str <- extract_tag("WindowWidth")
      wc_num <- if (!is.null(wc_str)) suppressWarnings(as.numeric(strsplit(wc_str, "\\\\")[[1]][1])) else NA_real_
      ww_num <- if (!is.null(ww_str)) suppressWarnings(as.numeric(strsplit(ww_str, "\\\\")[[1]][1])) else NA_real_
      
      rv$native_wc <- if (length(wc_num) == 1 && !is.na(wc_num)) wc_num else NULL
      rv$native_ww <- if (length(ww_num) == 1 && !is.na(ww_num)) ww_num else NULL
      
      if (identical(rv$modality, "CT")) {
        updateSelectInput(session, "display_preset", selected = "ct_soft")
      } else if (identical(rv$modality, "MR")) {
        updateSelectInput(session, "display_preset", selected = "mri_std")
      } else if (identical(rv$modality, "US")) {
        updateSelectInput(session, "display_preset", selected = if (rv$is_rgb) "usg_doppler" else "usg_bmode")
      }
      
      mat <- img_list[[1]]
      rv$native_w   <- ncol(mat)
      rv$native_h   <- nrow(mat)
      rv$is_cropped <- FALSE
      session$resetBrush("plot_brush")
      
      showNotification(paste0("Loaded ", rv$modality, " DICOM: ", rv$native_w, " × ", rv$native_h, " px"), type = "message")
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
  
  # 3. Base Frame Processing with 3D Array Enforcement
  base_frame_magick <- reactive({
    req(rv$dcm_obj)
    
    slice_idx <- if (!is.null(input$selected_slice)) input$selected_slice else 1
    slice_idx <- min(max(1, slice_idx), rv$num_slices)
    mat <- rv$dcm_obj$img[[slice_idx]]
    
    preset <- input$display_preset
    
    # Handle RGB Multi-Channel DICOMs (e.g., Color Doppler)
    if (length(dim(mat)) == 3 && dim(mat)[3] == 3) {
      r <- t(mat[, , 1]) / max(mat[, , 1], na.rm = TRUE)
      g <- t(mat[, , 2]) / max(mat[, , 2], na.rm = TRUE)
      b <- t(mat[, , 3]) / max(mat[, , 3], na.rm = TRUE)
      
      arr <- array(0, dim = c(nrow(r), ncol(r), 3))
      arr[, , 1] <- pmin(pmax(r, 0), 1)
      arr[, , 2] <- pmin(pmax(g, 0), 1)
      arr[, , 3] <- pmin(pmax(b, 0), 1)
      
      return(magick::image_read(arr))
    }
    
    # Single-Channel Grayscale Processing
    num_mat <- as.matrix(mat)
    
    if (grepl("^ct_", preset) || identical(rv$modality, "CT")) {
      num_mat <- (num_mat * rv$rescale_slope) + rv$rescale_intercept
    }
    
    if (preset == "auto" || is.null(preset)) {
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
    
    norm_mat[norm_mat < 0] <- 0
    norm_mat[norm_mat > 1] <- 1
    
    if (grepl("MONOCHROME1", rv$photometric, ignore.case = TRUE)) {
      norm_mat <- 1 - norm_mat
    }
    if (isTRUE(input$invert_monochrome)) {
      norm_mat <- 1 - norm_mat
    }
    
    # Transpose so rows correspond to physical image height
    t_mat <- t(norm_mat)
    
    # Structure as a 3D array (H x W x 1) to satisfy image_readbitmap requirement
    dim(t_mat) <- c(nrow(t_mat), ncol(t_mat), 1)
    
    magick::image_read(t_mat)
  })
  
  # Synchronize current viewport
  observe({
    req(base_frame_magick())
    if (!rv$is_cropped) {
      rv$current_img <- base_frame_magick()
    }
  })
  
  observeEvent(input$selected_slice, {
    rv$is_cropped <- FALSE
    session$resetBrush("plot_brush")
  })
  
  # 4. Pixel-Accurate Cropping
  observeEvent(input$btn_apply_crop, {
    req(rv$current_img, input$plot_brush)
    b <- input$plot_brush
    
    info <- magick::image_info(rv$current_img)
    w_px <- info$width
    h_px <- info$height
    
    crop_w <- b$xmax - b$xmin
    crop_h <- b$ymax - b$ymin
    
    if (input$crop_ratio != "free") {
      r_parts <- as.numeric(strsplit(input$crop_ratio, ":")[[1]])
      target_ratio <- r_parts[1] / r_parts[2]
      crop_h <- crop_w / target_ratio
    }
    
    crop_w <- round(crop_w)
    crop_h <- round(crop_h)
    crop_x <- round(max(0, min(w_px - 1, b$xmin)))
    # Invert Y: ggplot 0 is bottom, Magick 0 is top
    crop_y <- round(max(0, min(h_px - 1, h_px - b$ymax)))
    
    crop_w <- min(crop_w, w_px - crop_x)
    crop_h <- min(crop_h, h_px - crop_y)
    
    if (crop_w > 5 && crop_h > 5) {
      geo <- sprintf("%dx%d+%d+%d", crop_w, crop_h, crop_x, crop_y)
      rv$current_img <- magick::image_crop(rv$current_img, geo)
      rv$is_cropped <- TRUE
      session$resetBrush("plot_brush")
      showNotification(paste0("Cropped successfully to: ", crop_w, " × ", crop_h, " px"), type = "message")
    } else {
      showNotification("Selected crop area is too small.", type = "warning")
    }
  })
  
  observeEvent(input$btn_reset_crop, {
    req(base_frame_magick())
    rv$current_img <- base_frame_magick()
    rv$is_cropped <- FALSE
    session$resetBrush("plot_brush")
    showNotification("Reset to full frame view.", type = "message")
  })
  
  # 5. Metadata Audit Panel
  output$dicom_audit_ui <- renderUI({
    if (is.null(rv$dcm_obj)) {
      return(p(em("Upload a DICOM file to view audit specifications.")))
    }
    
    hdr <- rv$header_df
    extract_tag <- function(tag_name) {
      rows <- which(hdr$name == tag_name)
      if (length(rows) > 0) {
        val <- trimws(as.character(hdr$value[rows[1]]))
        if (!is.na(val) && nchar(val) > 0) return(val)
      }
      return("Not Specified")
    }
    
    cur_info <- if (!is.null(rv$current_img)) magick::image_info(rv$current_img) else NULL
    active_w <- if (!is.null(cur_info)) cur_info$width else rv$native_w
    active_h <- if (!is.null(cur_info)) cur_info$height else rv$native_h
    
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
  
  # 6. Interactive Canvas
  canvas_plot <- reactive({
    if (is.null(rv$current_img)) {
      return(
        ggplot() + 
          theme_void() + 
          xlim(0, 100) + ylim(0, 100) +
          annotate("text", x = 50, y = 50, label = "Upload a DICOM file to inspect image", color = "#7f8c8d", size = 5)
      )
    }
    
    info <- magick::image_info(rv$current_img)
    w <- info$width
    h <- info$height
    
    raster_g <- as.raster(rv$current_img)
    
    ggplot() + 
      annotation_raster(raster_g, xmin = 0, xmax = w, ymin = 0, ymax = h) +
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
      req(rv$current_img)
      
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