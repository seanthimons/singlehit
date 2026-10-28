# Build the small QMRA wiki sample workbook used by the Library tab's "Use
# sample workbook" button and by the tests. The full workbook stays out of the
# repository; this keeps only the "raw data" sheet, only count and metadata
# columns, and a handful of experiments chosen to cover each pathogen group,
# several dose units, both preferred models, a wiki-pooled entry, a
# parameter-only entry, one with too few doses to fit, and one with an
# unsupported model.
#
# Run from the app/ directory:
# Rscript dev/make_qmra_sample.R "../QMRA wiki data_v4.xlsx"
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Supply the path to the full QMRA wiki workbook.")
}

keys <- c(
  "18|Burkholderia mallei|infection|exponential",
  "21|Burkholderia mallei|death|beta-Poisson",
  "23|Burkholderia mallei|death|beta-Poisson",
  "21,23|Burkholderia mallei|death|beta-Poisson",
  "1|Yersinia pestis|death|exponential",
  "Ojielo2003|Pseudomonas aeruginosa|death|exponential",
  "71|Rotavirus|infection|beta-Poisson",
  "31|Adenovirus|infection|exponential",
  "63|Enterovirus|infection|exponential",
  "1|Variola major|death|NR",
  "108|Cryptosporidium hominis/ Cryptosporidium parvum|infection|exponential",
  "46|Giardia|infection|exponential",
  "48|Giardia|infection|exponential",
  "252|scrapie|infection|exponential"
)
columns <- c(
  "pathogen_category", "pathogen", "experiment_id", "Exposure route", "# of Doses",
  "Agent Strain", "Dose Units", "Host type", "Preferred model", "LD50/ID50",
  "Optimized parameters", "Response type", "Reference", "Dose", "Positive",
  "Negative", "Total", "ref_doi", "wiki_link"
)

raw <- readxl::read_excel(args[[1L]], sheet = "raw data", .name_repair = "minimal")
raw <- as.data.frame(raw[columns], check.names = FALSE)
key <- paste(
  trimws(raw$experiment_id), trimws(raw$pathogen), trimws(raw[["Response type"]]),
  trimws(raw[["Preferred model"]]),
  sep = "|"
)
missing <- setdiff(keys, key)
if (length(missing) > 0L) {
  stop("Not in the workbook: ", paste(missing, collapse = "; "))
}
sample <- raw[key %in% keys, , drop = FALSE]
writexl::write_xlsx(list(`raw data` = sample), "inst/extdata/qmra-wiki-sample.xlsx")
cat(sprintf("Wrote %d rows from %d experiments.\n", nrow(sample), length(keys)))
