FigureToolkit is a collection of four open-source R Shiny applications developed to support scientific figure preparation, panel arrangement, image annotation, and medical imaging conversion. The toolkit aims to make figure-related workflows more accessible to researchers, students, and academicians, including those working in resource-limited settings.

The applications use the R ecosystem and provide a graphical interface to support relevant tasks without requiring users to write R code during routine use.

Applications
1. FigureSight: An R Shiny application that checks the compliance of figures with PubMed Central’s suggested quality standards for figures. It helps to upscale and crop images according to necessity. It also helps to save images in TIFF, PNG, JPG, and PDF formats.

2. AnnotateSight: An R Shiny application designed to help in the annotation of images with arrows, circles, boxes, or text. Users need to select the components, activate click placement, and click on the image to place the element.

3. PanelSight: An R Shiny application that automates the assembly of multi-part scientific figures, enabling researchers to combine individual charts, clinical photographs, and diagnostic scans into cohesive, journal-ready layouts. Users can add panel labels in various formats.

4. DICOMSight: An R Shiny application for converting, windowing, and cropping medical imaging files (.dcm) across Computed Tomography (CT), Magnetic Resonance Imaging (MRI), and Ultrasound (USG). It provides modality-specific contrast presets (e.g., bone, soft tissue, lung, brain, MRI fluid optimization, and Doppler RGB passthrough), slice navigation for multi-frame series, interactive region-of-interest (ROI) cropping, and direct export to high-resolution TIFF, PNG, JPG, or PDF formats at publication-grade DPI settings.

Requirements
* R
* RStudio Desktop (recommended)
* An internet connection for initial installation of R packages, if required

Installation and Use:

* Step 1: Install R
Download and install R from:
https://cran.r-project.org/

* Step 2: Install RStudio
Download RStudio Desktop from:
https://posit.co/download/rstudio-desktop/

* Step 3: Download the repository
Clone this repository using Git:
git clone https://github.com/himelphy/FigureToolkit
Alternatively, click Code → Download ZIP on the GitHub repository page and extract the downloaded folder.

* Step 4: Run an application
1. Open the downloaded FigureToolkit folder.
2. Open the folder of the application you want to use.
3. Open its app.R file in RStudio.
4. Click Run App.
Run each application separately. Install any missing R packages if prompted.

Intended Use
FigureToolkit is intended to support research-related figure preparation and scientific communication. Users should verify the quality, accuracy, resolution, dimensions, and publication requirements of the final figures before submission.
The applications do not replace the scientific interpretation of data or the responsibility of authors to ensure that figures accurately represent their research findings.

Data Privacy
Review the source code and the individual application's documentation to understand how files and data are processed. Do not upload confidential, identifiable, or otherwise sensitive information to any online service unless its data-handling arrangements have been appropriately assessed.

Open-Source Development
Contributions, suggestions, bug reports, and feature requests are welcome. Users are encouraged to report reproducible errors and propose improvements that may benefit the research community.

License
This project is intended to be released under the MIT License. Add a LICENSE file containing the full MIT License text before describing the repository as officially MIT-licensed.

Author and Acknowledgement
Developed as an open-source research software initiative to support accessible scientific figure preparation and research communication.

Author: Dr Himel Mondal 
Institution: All India Institute of Medical Sciences (AIIMS), Deoghar, Jharkhand, India
If you use or adapt these applications in research, please acknowledge the relevant application and cite any associated publication, where available.
