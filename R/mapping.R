#' Map Stage
#'
#' Performs a single stage of ICD code mapping, either the forward map or the
#' backward map. The forward map takes the source codes and maps to the destination
#' codes using the GEM, while the backward map finds all the codes mapped to the
#' source codes in the GEM of the opposite direction.
#'
#' @param codes A vector of ICD diagnosis or procedure codes.
#' @param icdVer_dest A number, either 9 or 10, indicating the destination ICD version.
#' @param code_type A string, either "dg" or "pc," indicating the codes are diagnosis or procedure, respectively.
#' @param direction A string, either "forward" or "backward", to indicate the direction of the mapping.
#'
#' @return A dataframe with the source code, the matching destination code, and additional columns for cases where multiple codes represent a single source code.
#'
#' @export
map_stage <- function(codes, icdVer_dest, code_type = c("dg", "pc"), direction = c("forward", "backward")){
  code_type <- match.arg(code_type)
  direction <- match.arg(direction)

  if(direction == "forward"){
    # Load appropriate GEM file
    gem <- switch(paste0(icdVer_dest, code_type), "9dg" = dg_10_9_gem, "10dg" = dg_9_10_gem, "9pc" = pc_10_9_gem, "10pc" = pc_9_10_gem)
    # Subset GEM file based on codes
    matches <- subset(gem, src %in% codes)
  } else if(direction == "backward"){
    # Load appropriate GEM file
    gem <- switch(paste0(icdVer_dest, code_type), "9dg" = dg_9_10_gem, "10dg" = dg_10_9_gem, "9pc" = pc_9_10_gem, "10pc" = pc_10_9_gem)
    # Subset GEM file based on codes
    matches <- subset(gem, dest %in% codes)
    matches <- matches[,c('dest','src',colnames(matches)[3:8])]
  }

  colnames(matches)[1:2] <- c('src_code', 'dest_code')

  # Clean up for cases where no matching code was found
  matches <- merge(data.frame(src_code = codes), matches, by.x = "src_code", by.y = "src_code", all.x = TRUE)
  # matches$dest_code <- ifelse(is.na(matches$dest_code), "No matches found", matches$dest_code)

  # Handling cases of a one to many map of the source code
  matches <- matches[order(matches$src_code,matches$combination,matches$scenario,matches$choice_lists),]

  return(matches)
}

#' Map Code
#'
#' Map ICD diagnosis or procedure codes to and from either ICD9 or ICD10.
#' @param codes A vector of ICD diagnosis or procedure codes.
#' @param icdVer_dest A number, either 9 or 10, indicating the destination ICD version.
#' @param code_type A string, either "dg" or "pc," indicating the codes are diagnosis or procedure, respectively.
#' @param method A string specifying the method for mapping the codes. The same methods as implemented by the \code{icd_convert} function in the touch \code{package}. \itemize{
#'     \item \code{"gem"} performs a single forward mapping.
#'     \item \code{"reverse-gem"} performs a single backward mapping.
#'     \item \code{"both"} perfroms a single forward and backward mapping, combining the results.
#'     \item \code{"multi-stage"} performs the multiple-stage mapping, as described in the \code{touch} package.
#' }
#'
#' @return A dataframe with the source code, the matching destination code, and additional columns for cases where multiple codes represent a single source code.
#'
#' @export
map_code <- function(codes, icdVer_dest, code_type = c("dg", "pc"), method = c("gem", "reverse-gem", "both", "multi-stage")){
  code_type <- match.arg(code_type)
  method <- match.arg(method)

  if(method == "gem"){
    matches <- map_stage(codes, icdVer_dest, code_type, "forward")
  }else if(method == "reverse-gem"){
    matches <- map_stage(codes, icdVer_dest, code_type, "backward")
  }else if(method == "both"){
    matches <- unique(rbind(map_stage(codes, icdVer_dest, code_type, "forward"),
                            map_stage(codes, icdVer_dest, code_type, "backward")))
    matches <- matches[!is.na(matches$dest_code),]
  }else if(method == "multi-stage"){
    srcICDVer <- ifelse(icdVer_dest == 9, 10, 9)

    # Perform multi-stage matching
    stage1 <- unique(rbind(map_stage(codes, icdVer_dest, code_type, "forward"),
                           map_stage(codes, icdVer_dest, code_type, "backward")))
    stage1_codes <- unique(stage1$dest_code[!is.na(stage1$dest_code)])
    stage2 <- unique(rbind(map_stage(stage1_codes, srcICDVer, code_type, "forward"),
                           map_stage(stage1_codes, srcICDVer, code_type, "backward")))
    stage2_codes <- unique(stage2$dest_code[!is.na(stage2$dest_code)])
    stage3 <- unique(rbind(map_stage(stage2_codes, icdVer_dest, code_type, "forward"),
                           map_stage(stage2_codes, icdVer_dest, code_type, "backward")))

    # Clean up results
    matches <- merge(merge(stage1[,c("src_code", "dest_code")], stage2[,c("src_code", "dest_code")],
                     by.x = "dest_code", by.y = "src_code"),
               stage3[,c("src_code", "dest_code")],
               by.x = "dest_code.y", by.y = "src_code")
    matches <- matches[,c("src_code", "dest_code.y.y")]
    colnames(matches) <- c("src_code", "dest_code")
    matches <- unique(matches[!is.na(matches$dest_code),])
    matches <- matches[order( matches[,"src_code"], matches[,"dest_code"] ),]
  }

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
#' @param method A string, either "gem", "reverse-gem", "both", or "multi-stage". See documentation on \code{map_code} for more details.
#' @param keepMapCode Boolean - if true, returned data frame will keep the map code and associated columns, otherwise they are dropped.
#'
#' @return A dataframe with the original ICD code, the matching code, descriptions for both, and potentially columns for the map codes.
#'
#' @export
map_describe <- function(codes, icdVer_dest, code_type = c("dg", "pc"), method = c("gem", "reverse-gem", "both", "multi-stage"), keepMapCode = FALSE) {
  code_type <- match.arg(code_type)
  method <- match.arg(method)

  srcICDVer <- ifelse(icdVer_dest == 9, 10, 9)

  mapped <- map_code(codes, icdVer_dest, code_type, method)
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
