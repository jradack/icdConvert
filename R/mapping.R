#' Map Code
#'
#' Map ICD diagnosis or procedure codes to and from either ICD9 or ICD10.
#' @param codes A vector of ICD diagnosis or procedure codes.
#' @param icdVer_dest A number, either 9 or 10, indicating the destination ICD version.
#' @param code_type A string, either "dg" or "pc," indicating the codes are diagnosis or procedure, respectively.
#'
#' @return A dataframe with the source code, the matching destination code, and additional columns for cases where multiple codes represent a single source code.
#'
#' @export
map_code <- function(codes, icdVer_dest, code_type){
  # Load appropriate GEM file
  gem <- switch(paste0(icdVer_dest, code_type), "9dg" = dg_10_9_gem, "10dg" = dg_9_10_gem, "9pc" = pc_10_9_gem, "10pc" = pc_9_10_gem)
  if(is.null(gem)){
    stop("Error: improper ICD version or code type, or GEM file currently unavailable")
  }

  # Subset GEM file based on codes
  matches <- subset(gem, src %in% codes)
  colnames(matches)[1:2] <- c('src_code', 'dest_code')

  # Clean up for cases where no matching code was found
  matches <- merge(data.frame(src_code = codes), matches, by.x = "src_code", by.y = "src_code", all.x = TRUE)
  matches$dest_code <- ifelse(is.na(matches$dest_code), "No matches found", matches$dest_code)

  # Handling cases of a one to many map of the source code
  matches <- matches[order(matches$src_code,matches$combination,matches$scenario,matches$choice_lists),]

  return(matches)
}



#' Get Description
#'
#' Function to get description of a code.
#' @param codes A vector of ICD diagnosis or procedure codes.
#' @param icdVer A number, either 9 or 10, indicating the ICD version.
#' @param code_type A string, either "dg" or "pc," indicating the codes are diagnosis or procedure, respectively.
#'
#' @return A dataframe with the ICD code and its description.
#'
#' @export
get_description <- function(codes, icdVer, code_type){
  # Load description file
  descriptions <- switch(paste0(icdVer, code_type), "9dg" = icd9_dg, "10dg" = icd10_dg, "9pc" = icd9_pc, "10pc" = icd10_pc)
  if(is.null(descriptions)){
    stop("Error: improper ICD version or code type")
  }

  # Subset description file based on list of codes
  # codes <- as.character(codes)
  matches <- subset(descriptions, code %in% codes)

  # Join descriptions with the codes
  result <- merge(as.data.frame(codes), matches, by.x = "codes", by.y = "code", all.x = TRUE)
  result <- result[!duplicated(result),]
  return(result)

}


#' Map Code and Get Description
#'
#' Function that maps code and gets description of the mapped codes
#' @param codes A vector of ICD diagnosis or procedure codes.
#' @param icdVer_dest A number, either 9 or 10, indicating the destination ICD version.
#' @param code_type A string, either "dg" or "pc," indicating the codes are diagnosis or procedure, respectively.
#' @param keepMapCode Boolean - if true, returned data frame will keep the map code and associated columns, otherwise they are dropped.
#'
#' @return A dataframe with the original ICD code, the matching code, descriptions for both, and potentially columns for the map codes.
#'
#' @export
map_describe <- function(codes, icdVer_dest, code_type, keepMapCode = FALSE) {
  srcICDVer <- ifelse(icdVer_dest == 9, 10, 9)

  mapped <- map_code(codes, icdVer_dest, code_type)
  mapped$ord <- 1:nrow(mapped)
  src_desc <- get_description(mapped$src_code, srcICDVer, code_type)
  colnames(src_desc)[2] <- "src_desc"
  dest_desc <- get_description(mapped$dest_code, icdVer_dest, code_type)
  colnames(dest_desc)[2] <- "dest_desc"

  result <- merge(mapped, src_desc, by.x = "src_code", by.y = "codes", all.x = TRUE)
  result <- merge(result, dest_desc, by.x = "dest_code", by.y = "codes", all.x = TRUE)

  result <- result[order(result$ord),]

  if(keepMapCode){
    cols <- c("src_code","src_desc","dest_code","desc_dest","map_code","approximate","no_map","combination","scenario","choice_lists")
  } else {
    cols <- c("src_code","src_desc","dest_code","dest_desc")
  }
  result <- result[,cols]

  return(result)
}
