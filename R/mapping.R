#' Match Prefixes
#'
#' Checks whether a string matches any of a vector of prefixes.
#'
#' @param strings A vector of character string to test
#' @param prefixes A vector of character strings representing the prefixes
#'
#' @returns A boolean indicating whether the string matches any of the prefixes.
#'
#' @keywords Internal
match_prefixes <- function(strings, prefixes) {
  purrr::map_lgl(
    strings,
    \(s) startsWith(s, prefixes) |>
      any()
  )
}

#' Exact or partial match
#'
#' Wrapper for whether to search for an exact match or a partial match
#' of a code in a vector of codes.
#'
#' @param codes Vector of character string of code to search for
#' @param code_vec Vector of character strings to compare against
#' @param exact_match Boolean for whether an exact or prefix match should be used.
#'
#' @returns Boolean for whether a match was found.
#'
#' @keywords Internal
match_code <- function(codes, code_vec, match_method = c("exact", "prefix")) {
  match_method = match.arg(match_method)

  switch(
    match_method,
    exact = {
      codes %in% code_vec
    },
    prefix = {
      match_prefixes(codes, code_vec)
    }
  )
}

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
#' @importFrom rlang .data
#'
#' @export
map_stage <- function(codes, icdVer_dest, code_type = c("dg", "pc"), direction = c("forward", "backward")){
  code_type <- match.arg(code_type)
  direction <- match.arg(direction)

  if(direction == "forward"){
    # Load appropriate GEM file
    gem <- switch(paste0(icdVer_dest, code_type), "9dg" = dg_10_9_gem, "10dg" = dg_9_10_gem, "9pc" = pc_10_9_gem, "10pc" = pc_9_10_gem)
    # Subset GEM file based on codes
    matches <- dplyr::filter(gem, .data$src %in% codes)
  } else if(direction == "backward"){
    # Load appropriate GEM file
    gem <- switch(paste0(icdVer_dest, code_type), "9dg" = dg_9_10_gem, "10dg" = dg_10_9_gem, "9pc" = pc_9_10_gem, "10pc" = pc_10_9_gem)
    # Subset GEM file based on codes
    matches <- dplyr::filter(gem, .data$dest %in% codes) |>
      dplyr::select('dest', 'src', 3:8)
  }

  colnames(matches)[1:2] <- c('src_code', 'dest_code')

  # Clean up for cases where no matching code was found
  matches <- data.frame(src_code = codes) |>
    dplyr::left_join(
      matches,
      by = "src_code"
    )

  # Handling cases of a one to many map of the source code
  matches <- matches |>
    dplyr::arrange(
      .data$src_code, .data$combination, .data$scenario, .data$choice_lists
    )

  return(matches)
}

#' Forward-Backward Mapping
#'
#' Performs the bidirectional mapping of a code.
#' @param codes A vector of ICD diagnosis or procedure codes.
#' @param icdVer_dest A number, either 9 or 10, indicating the destination ICD version.
#' @param code_type A string, either "dg" or "pc," indicating the codes are diagnosis or procedure, respectively.
#'
#' @return A dataframe with the bidirectionally mapped codes.
#'
#' @importFrom rlang .data
#'
#' @keywords Internal
forward_backward <- function(codes, icdVer_dest, code_type) {
  map_stage(codes, icdVer_dest, code_type, "forward") |>
    dplyr::bind_rows(
      map_stage(codes, icdVer_dest, code_type, "backward")
    ) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(.data$dest_code))
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
#' @importFrom rlang .data
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
    matches <- forward_backward(codes, icdVer_dest, code_type)
  }else if(method == "multi-stage"){
    srcICDVer <- ifelse(icdVer_dest == 9, 10, 9)

    # Perform multi-stage matching
    stage1 <- forward_backward(codes, icdVer_dest, code_type)
    stage1_codes <- stage1 |>
      dplyr::distinct() |>
      dplyr::pull(.data$dest_code)
    stage2 <- forward_backward(stage1_codes, srcICDVer, code_type)
    stage2_codes <- stage2 |>
      dplyr::distinct() |>
      dplyr::pull(.data$dest_code)
    stage3 <- forward_backward(stage2_codes, icdVer_dest, code_type)

    # Clean up results
    matches <- stage1 |>
      dplyr::select("src_code", "dest_code") |>
      dplyr::inner_join(
        stage2 |>
          dplyr::select("src_code", "dest_code") |>
          dplyr::filter(!is.na(.data$dest_code)),
        by = c("dest_code" = "src_code"),
        relationship = "many-to-many"
      ) |>
      dplyr::inner_join(
        stage3 |>
          dplyr::select("src_code", "dest_code") |>
          dplyr::filter(!is.na(.data$dest_code)),
        by = c("dest_code.y" = "src_code"),
        relationship = "many-to-many"
      )

    matches <- matches |>
      dplyr::select("src_code", "dest_code.y.y") |>
      dplyr::rename(
        "dest_code" = "dest_code.y.y"
      ) |>
      dplyr::filter(!is.na(.data$dest_code)) |>
      dplyr::distinct() |>
      dplyr::arrange(.data$src_code, .data$dest_code)
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
#' @importFrom rlang .data
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
  matches <- descriptions |>
    dplyr::filter(.data$code %in% codes)

  # Join descriptions with the codes
  result <- as.data.frame(codes) |>
    dplyr::left_join(
      matches,
      by = c("codes" = "code")
    ) |>
    dplyr::distinct()
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
#' @importFrom rlang .data
#'
#' @export
map_describe <- function(codes, icdVer_dest, code_type = c("dg", "pc"), method = c("gem", "reverse-gem", "both", "multi-stage"), keepMapCode = FALSE) {
  code_type <- match.arg(code_type)
  method <- match.arg(method)

  srcICDVer <- ifelse(icdVer_dest == 9, 10, 9)

  # Map codes and get descriptions
  mapped <- map_code(codes, icdVer_dest, code_type, method)
  src_desc <- get_description(mapped$src_code, srcICDVer, code_type)
  colnames(src_desc)[2] <- "src_desc"
  dest_desc <- get_description(mapped$dest_code, icdVer_dest, code_type)
  colnames(dest_desc)[2] <- "dest_desc"

  # Create column for ordering the rows
  if(nrow(mapped) > 0) {
    mapped$ord <- 1:nrow(mapped)
  }

  # Join mapped codes with descriptions, filter columns based on keepMapCode option
  result <- mapped |>
    dplyr::left_join(
      src_desc,
      by = c("src_code" = "codes")
    ) |>
    dplyr::left_join(
      dest_desc,
      by = c("dest_code" = "codes")
    ) |> (
      \(.)
      if(nrow(mapped) > 0) {
        dplyr::arrange(., .data$ord)
      }
      else {
        .
      }
    )() |>
    (
      \(.){
        if(keepMapCode) {
          dplyr::select(., "src_code", "src_desc", "dest_code", "dest_desc", "map_code", "approximate", "no_map", "combination", "scenario", "choice_lists")
        } else {
          dplyr::select(., "src_code", "src_desc", "dest_code", "dest_desc")
        }
      }
    )()

  return(result)
}
