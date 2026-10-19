# Run the app tests against the in-repo singlehit source (exports only). Run
# from the app/ directory.
pkgload::load_all("..", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)
testthat::test_local(".", load_package = "source")
