# Run the app against the in-repo singlehit source, so edits to the statistics
# package are picked up on every run without reinstalling. export_all = FALSE
# matches an installed package: only exported functions are visible, which keeps
# the app honest about using singlehit's public API.
options(golem.app.prod = FALSE)
options(shiny.port = httpuv::randomPort())
# Detach first: detach_all_attached() unloads everything attached, which would
# otherwise discard the in-repo singlehit loaded below.
golem::detach_all_attached()
pkgload::load_all("..", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
golem::document_and_reload()
# run_app() only builds the app object. Calling runApp() on it launches the app
# however this file is run: source() does not print the returned object, so a
# bare run_app() would silently do nothing.
shiny::runApp(run_app())
