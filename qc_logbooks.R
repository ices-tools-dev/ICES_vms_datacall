
# to run
# Rscript qc_logbooks.R ICES_LE_BEL.csv test.html

require(rmarkdown)

logbook_filename <- commandArgs(trailingOnly = TRUE)[1]
output_file <- commandArgs(trailingOnly = TRUE)[2]

# make sure file name is html
output_file <- paste(tools::file_path_sans_ext(output_file), ".html")

render(
  output_file = output_file,
  "QC_logbooks.Rmd",
  params = list(
    logbook_filename = logbook_filename
  )
)
