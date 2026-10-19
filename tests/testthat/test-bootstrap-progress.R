test_that("bootstrap_progress rejects objects that are not bootstrap jobs", {
  expect_error(bootstrap_progress(list(map = NULL)), "qdr_bootstrap_job")
})

test_that("bootstrap_progress polls a job without blocking and reports completion", {
  skip_if_not_installed("mirai")
  mirai::daemons(1, dispatcher = FALSE)
  on.exit(mirai::daemons(0), add = TRUE)
  map <- mirai::mirai_map(1:3, function(x) {
    Sys.sleep(0.3)
    x
  })
  job <- structure(
    list(map = map, fit = NULL, resample = "observed", compute = NULL, owns_daemons = FALSE),
    class = "qdr_bootstrap_job"
  )

  early <- bootstrap_progress(job)
  expect_identical(early$total, 3L)
  expect_false(early$finished)
  expect_lt(early$done, early$total)

  mirai::collect_mirai(map)
  late <- bootstrap_progress(job)
  expect_identical(late, list(done = 3L, total = 3L, finished = TRUE))
  # polling is idempotent and leaves the job collectable
  expect_identical(bootstrap_progress(job), late)
})
