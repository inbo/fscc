
save_excel <- function(x,
                       file) {

  wb <- createWorkbook()
  addWorksheet(wb, "Data")
  writeData(wb, 1, x)
  addFilter(wb, 1, row = 1, cols = seq_len(ncol(x)))
  freezePane(wb, 1, firstRow = TRUE)
  addCreator(wb, "ICP Forests - FSCC")

  openxlsx::saveWorkbook(wb,
                         file = file,
                         overwrite = TRUE)

  cat(paste0(" \nSaved data locally as '", file, "'.\n"))
}
