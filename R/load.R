#' Import GEM files, ICD9, ICD10 code descriptions
#'
#' Reads in fixed with ICD file, keeping all columns as character variables, trimming white space off ends of each column
#' @param path The file path to the file to be loaded.
#' @param width A numeric vector specifying widths for each of the columns.
#' @param colNames A character vector specifying names of the columns.
#' @param fileEncoding A character string specifying the file encoding of the fixed width file.
#'
#' @return A dataframe of the ICD file.
#'
#' @export
icd_read_fwf <- function(path, width, colNames, fileEncoding = ""){
  icdData <- utils::read.fwf(path, width = width, col.names = colNames, colClasses = "character", fileEncoding = fileEncoding)
  icdData <- as.data.frame(apply(icdData, 2, function(x) trimws(x, which = 'both')))

  # Split up the map_code column if it exists
  if("map_code" %in% colNames){
    out <- strsplit(as.character(icdData$map_code), '')
    map_code_cols <- do.call(rbind, out)
    colnames(map_code_cols) <- c('approximate','no_map','combination','scenario','choice_lists')
    icdData <- cbind(icdData, map_code_cols)
  }

  return(icdData)
}
