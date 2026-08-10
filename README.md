# AlleloBin: A Shiny Application for Allele Binning in Microsatellite Markers

**AlleloBin** is an R package designed to automate the conversion of imprecise fragment sizes from automated DNA sequencers into representative, discrete allele sizes. It implements and extends the least-squares minimization algorithm developed by Idury & Cardon (1997) to eliminate time-consuming manual intervention in high-throughput genotyping workflows.

## 👥 Authors & Citation
* **VP Prasanth, S Chandra, DA Hoisington, B Jayashree**
* International Crops Research Institute for the Semi-Arid Tropics (ICRISAT), Patancheru, India.
* Contact/E-mail: prasanth.stat@gmail.com

If you use this software, please cite:
> Idury, R. M., & Cardon, L. R. (1997). A least-squares algorithm for a semi-automated allele sizing system. *Genome Research*, 7(11), 1104-1109.

---

## ✨ Key Features
* **Automated Binning:** Rapidly processes large-scale genotyping datasets using an objective least-squares algorithm.
* **Ploidy Flexibility:** Extends the original algorithm to support multi-ploidy levels (not just diploids).
* **Quality Control:** Generates useful summary statistics, calculates a distinct quality index for each marker, and checks for allelic drift.
* **Interactive Visualization:** Produces clear histograms overlaying observed data with inferred allele classes for easy visual inspection.
* **User-Friendly Interface:** Includes an interactive R Shiny application for researchers who prefer a graphical interface over command-line code.

---

## 🛠️ Installation

You can install the development version of `AlleloBin` directly from GitHub using the `devtools` package:

```R
# Install devtools if you haven't already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install AlleloBin
devtools::install_github("vpprasanth/AlleloBin")
```

## 🚀 Quick Start

### Command Line Usage
```R
library(AlleloBin)

# 1. Load your raw fragment size genotyping data
my_data <- read.table(system.file("extdata", "data.txt", package = "AlleloBin"), header = FALSE)

# 2. Load the corresponding marker configuration data (repeat lengths, etc.)
marker_info <- read.table(system.file("extdata", "repeat_length.txt", package = "AlleloBin"), header = FALSE)

# 3. Extract the repeat lengths vector from the repeat_length file
repeat_lens <- marker_info$V1 

# 4. Run the binning process
results <- allele_binning(data = my_data, repeat_lengths = repeat_lens, ploidy = 2)

# Save results to Excel
save_results(results, "AlleloBin_Output.xlsx")
```

### Launch the Shiny Web Interface
If you prefer a graphical layout to upload your files and look at histograms, simply launch the built-in app:
```R
library(AlleloBin)
AlleloBin()
```
