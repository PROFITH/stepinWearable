[![Codecov test coverage](https://codecov.io/gh/youruser/stepinWearable/branch/main/graph/badge.svg)](https://app.codecov.io/gh/youruser/stepinWearable)

# stepinWearable: Personalized Wearable Platform for Research

A specialized **Shiny application** designed to facilitate the end-to-end process of **personalized intervention generation** and data management for minute-level physical activity data, from **Fitbit**, **Matrix**, and **StepWatch** devices.

This tool is built for researchers managing the wearable data collected under the umbrella of the **STEP-IN** project, at the University of Granada.

------------------------------------------------------------------------

## Project Status

The application is structured into two main modules:

1.  **Intervention (Active):** Fully functional and used for processing data, visualization, and challenge generation.
2.  **Assessment (Under Development):** Reserved for future integration of analysis methods for the wearable data assessments within the project.

------------------------------------------------------------------------

## Key Features

### Intervention Module (Active)

This module handles the core workflow for generating personalized steps and cadence challenges:

-   **Data Ingestion & Preprocessing:** Processes minute-level steps data collected with **Fitbit** monitors.
-   **Intervention State Management:** Loads and saves essential historical data (*targets, achievements, and fails*) by unique participant and measurement cycle (known as the $t$-index).
-   **Data Visualization:** Provides **interactive plots** of steps and cadence minutes over the selected 14-day analysis window.
-   **Challenge Generation:** Automates the creation of **customized motivational messages** and sets new, personalized targets based on predefined rules.

> **What is the** $t$-index? The $t$-index indicates the time index for the measurement cycle being processed. For example, $t=0$ contains the 2 weeks of the baseline assessment, and $t=1$ contains the first 2 weeks of the intervention.

### Assessment Module (Future Development)

The **Assessment** section is under active development. This module will expand the platform by integrating analysis methods for data collected using **Matrix (Parmay Tech)** and **StepWatch** devices, as well as providing visualizations for data quality checks.

------------------------------------------------------------------------

## Getting Started

You can install the development version of `stepinWearable` directly from GitHub using the `remotes` package:

``` r
# Install remotes if you don't have it
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}

remotes::install_github("PROFITH/stepinWearable", build_vignettes = TRUE)
```

Once installed, **to launch** the Shiny application, simply call the main function:

``` r
stepinWearable::run_app()
```

------------------------------------------------------------------------

## Connectivity & Setup (Crucial)

This application is designed to operate within a specific secure environment for data handling.

**The app requires that:**

-   You are connected to the VPN of the University of Granada.

-   The application requires specific, pre-configured directory structures for the stable loading and saving of participant state and outputs (.RData and state files).

------------------------------------------------------------------------

## Configuration (paths & server)

`stepinWearable` reads a few **options** at load time so it works both on the UGR network and on laptops without the VPN. You can set them once (in `~/.Rprofile`) or per session (before `run_app()`).

### Quick start (per session)

Put this at the top of your R session **before** running the app:

``` r
# Optional: ONLY set what you actually use.
options(
  # Network/VPN check (optional). Replace with your own host or leave NULL.
  stepin.server_host     = NULL,                # e.g., "vpn.example.org"

  # Network share roots (optional). Leave NULL if you don't use them.
  stepin.path_windows    = NULL,                # e.g., "\\\\server\\share\\Project"
  stepin.path_unix       = NULL,                # e.g., "/Volumes/share/Project"

  # Local output & input defaults (recommended to set)
  stepin.output_dir      = "~/stepinWearable-data",
  stepin.fitbit_download = "~/stepinWearable-data/download/fitbit"
)

stepinWearable::run_app()
```

-   `stepin.server_host` – host used to check VPN connectivity.

-   `stepin.path_windows` / `stepin.path_unix` – shared drive roots (used when VPN is up).

-   `stepin.output_dir` – local fallback/output directory (used when shares aren’t reachable).

-   `stepin.fitbit_download` – where Fitbit CSV/XLSX are by default.\
    If **not** set, it defaults to `<stepin.output_dir>/download/fitbit`.

### Make it permanent (recommended)

Create or edit `~/.Rprofile` (user-level) and add the same `options(...)` block.\
These settings will be picked up automatically every time you start R.

### Environment variables (alternative)

Set in `~/.Renviron` (or your system env). Use placeholders and keep them out of version control:

```         
# Optional network bits (leave unset if not needed)
# STEPIN_SERVER_HOST=vpn.example.org
# STEPIN_PATH_WINDOWS=\\server\share\Project
# STEPIN_PATH_UNIX=/Volumes/share/Project

# Recommended locals
STEPIN_OUTPUT_DIR=~/stepinWearable-data
STEPIN_FITBIT_DOWNLOAD=~/stepinWearable-data/download/fitbit
```

### Defaults if you set nothing

-   `stepin.output_dir` defaults to a safe user folder: `tools::R_user_dir("stepinWearable", "data")`

-   `stepin.fitbit_download` defaults to `<stepin.output_dir>/download/fitbit`

-   Network paths are only used if they **exist**; otherwise the app falls back to the local `stepin.output_dir`.

### Verify your configuration

``` r
str(list(
  server_host     = getOption("stepin.server_host"),   
  path_windows    = getOption("stepin.path_windows"),   
  path_unix       = getOption("stepin.path_unix"),   
  output_dir      = getOption("stepin.output_dir"),   
  fitbit_download = getOption("stepin.fitbit_download") 
)) 
```

If the VPN or shared drive isn’t available, the app will use `output_dir` automatically.

------------------------------------------------------------------------

## License

This project is licensed under the **GNU General Public License v3.0 (GPL-3.0)**. The GPL-3.0 ensures that all modifications and derivative works of this software remain open source. See the [LICENSE](LICENSE) file for the full text.

------------------------------------------------------------------------

## Contact

Developed for the Step-IN project.\
For support or technical questions, contact: [[jairo\@jhmigueles.com](mailto:jairo@jhmigueles.com){.email}]
