#' Show a vignette (HTML) in a modal inside Shiny.
#' Works for installed packages and dev repos; will render the Rmd if needed.
#' @param session Shiny session
#' @param pkg     Package name (default: "stepinWearable")
#' @param name    Vignette base name (no extension), e.g. "intervention-decision-logic"
#' @param title   Modal title
show_vignette_modal <- function(session,
                                pkg   = "stepinWearable",
                                name  = "intervention-decision-logic",
                                title = "Intervention Decision Logic") {
  
  # 1) installed path: inst/doc/<name>.html
  html_path <- system.file("doc", paste0(name, ".html"), package = pkg)
  
  # 2) dev path: vignettes/<name>.html (when running from the repo)
  if (!nzchar(html_path)) {
    dev_pkg_root <- tryCatch(getwd(), error = function(e) "")
    cand <- file.path(dev_pkg_root, "vignettes", paste0(name, ".html"))
    if (file.exists(cand)) html_path <- cand
  }
  
  # 3) render on-the-fly from the Rmd if no HTML is found
  if (!nzchar(html_path) || !file.exists(html_path)) {
    rmd_path <- file.path(getwd(), "vignettes", paste0(name, ".Rmd"))
    if (file.exists(rmd_path) && requireNamespace("rmarkdown", quietly = TRUE)) {
      out <- file.path(tempdir(), paste0(name, ".html"))
      rmarkdown::render(rmd_path, output_file = out, quiet = TRUE, encoding = "UTF-8")
      html_path <- out
    }
  }
  
  if (!nzchar(html_path) || !file.exists(html_path)) {
    shiny::showNotification("Vignette HTML not found. If installed from GitHub, install with build_vignettes=TRUE.", type = "error")
    return(invisible())
  }
  
  # Serve the directory containing the HTML so relative assets (css/img) work
  dir_path  <- normalizePath(dirname(html_path), winslash = "/", mustWork = TRUE)
  file_name <- basename(html_path)
  alias     <- paste0("vhelp_", as.integer(stats::runif(1, 1, 1e9)))  # unique alias
  shiny::addResourcePath(alias, dir_path)
  
  url <- paste0("/", alias, "/", file_name)
  
  shiny::showModal(
    shiny::modalDialog(
      title = title, size = "l", easyClose = TRUE, footer = NULL,
      shiny::tags$iframe(src = url, style = "width:100%;height:70vh;border:0;")
    )
  )
}
