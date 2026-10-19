# Deployment entry point (shinyapps.io, Posit Connect, Shiny Server).
# Deploy from this directory; singlehit must be installed from GitHub so the
# deployment manifest records its remote source.
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options(golem.app.prod = TRUE)
run_app()
