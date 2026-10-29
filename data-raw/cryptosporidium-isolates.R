# Build inst/extdata/cryptosporidium-isolates.csv, the Cryptosporidium human
# challenge experiments used in the pooling article
# (vignettes/articles/pooling.Rmd). They come from the QMRA
# wiki compilation (https://qmrawiki.org), whose source workbook is not in this
# repository; the counts are those reported in the cited studies.
#
# Needs readxl and the QMRA wiki workbook. Run from the package root:
# Rscript data-raw/cryptosporidium-isolates.R "QMRA wiki data_v4.xlsx"
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Supply the path to the QMRA wiki workbook.")
}

raw <- readxl::read_excel(args[[1L]], sheet = "raw data", .name_repair = "minimal")
raw <- raw[grepl("Cryptosporidium", raw$pathogen) & raw$experiment_id %in% c("108", "139", "140", "141", "181", "183"), ]

# "DuPont, H. L., Chappell, ... (1995). The infectivity ..." -> "DuPont et al. (1995)"
short_citation <- function(reference) {
  author <- sub(",.*", "", trimws(reference))
  year <- sub(".*?\\((\\d{4})\\).*", "\\1", reference, perl = TRUE)
  paste0(author, " et al. (", year, ")")
}

isolates <- data.frame(
  study_id = raw$experiment_id,
  isolate = trimws(raw[["Agent Strain"]]),
  host = trimws(raw[["Host type"]]),
  route = trimws(raw[["Exposure route"]]),
  dose_unit = trimws(raw[["Dose Units"]]),
  endpoint = trimws(raw[["Response type"]]),
  dose = as.numeric(raw$Dose),
  positive = as.integer(raw$Positive),
  negative = as.integer(raw$Negative),
  source = short_citation(raw$Reference)
)
stopifnot(nrow(isolates) == 32L, !anyNA(isolates))
utils::write.csv(isolates, "inst/extdata/cryptosporidium-isolates.csv", row.names = FALSE)
cat(sprintf("Wrote %d rows from %d experiments.\n", nrow(isolates), length(unique(isolates$study_id))))
