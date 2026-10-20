test_that("the mirai worker does not capture the frame that creates it", {
  worker <- bootstrap_worker()

  # A closure built inside start_mirai_bootstrap() would carry that frame, which
  # holds every task, and mirai would serialize it with each batch.
  expect_identical(environment(worker), globalenv())
  expect_lt(length(serialize(worker, NULL)), 10000L)
})

test_that("the mirai worker refits a batch exactly like the sequential path", {
  fit <- fit_dose_response(ward_fixture(), "beta_poisson")
  tasks <- generate_bootstrap_tasks(fit, fit$data$response, 3L, 42L)

  batched <- bootstrap_worker()(tasks, fit, NULL)
  sequential <- run_sequential_bootstrap(tasks, fit)

  expect_equal(tibble::as_tibble(batched), tibble::as_tibble(sequential))
})

test_that("start_mirai_bootstrap sends mirai the detached worker", {
  skip_if_not_installed("mirai")
  fit <- fit_dose_response(ward_fixture(), "beta_poisson")
  inputs <- prepare_bootstrap(fit, 3L, "observed", 42L, "mirai", NULL, NULL)
  captured <- NULL
  local_mocked_bindings(
    daemons_set = function(...) TRUE,
    mirai_map = function(.x, .f, ...) {
      captured <<- .f
      list()
    },
    .package = "mirai"
  )

  job <- start_mirai_bootstrap(inputs, fit)

  expect_s3_class(job, "qdr_bootstrap_job")
  expect_identical(environment(captured), globalenv())
  expect_lt(length(serialize(captured, NULL)), 10000L)
})
