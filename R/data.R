################################################################################
# These datasets now live internally to the package to be used by the package
# functions. They cannot be directly accessed by the package user. This
# documentation is left here for reference.
################################################################################

#' ICD-9 Procedure Code Example Data
#'
#' An example dataset of ICD-9 procedure codes.
#'
#' @format ## `example_data_icd9_pc`
#' A data frame with 116 rows and 1 column:
#' \describe{
#'   \item{icd9_pc}{ICD-9 procedure codes}
#' }
"example_data_icd9_pc"


#' 2018 ICD-9 to ICD-10 Diagnosis Code GEMs
#'
#' A crosswalk from ICD-9 to ICD-10 for diagnosis (dg) codes using General
#' equivalence mappings.
#'
#' @format ## `dg_9_10_gem`
#' A data frame with 24,860 rows and 8 columns:
#' \describe{
#'   \item{src}{The source ICD-9 dg code}
#'   \item{dest}{The destination ICD-10 dg code}
#'   \item{map_code}{}
#' }
#' @source <https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2018-ICD-10-CM-General-Equivalence-Mappings.zip>
# "dg_9_10_gem"


#' 2018 ICD-9 to ICD-10 Procedure Code GEMs
#'
#' A crosswalk from ICD-9 to ICD-10 for procedure (pc) codes using General
#' equivalence mappings.
#'
#' @format ## `pc_9_10_gem`
#' A data frame with 73,593 rows and 8 columns:
#' \describe{
#'   \item{src}{The source ICD-9 pc code}
#'   \item{dest}{The destination ICD-10 pc code}
#'   \item{map_code}{}
#' }
#' @source <https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2018-ICD-10-PCS-General-Equivalence-Mappings.zip>
# "pc_9_10_gem"


#' ICD-9 Diagnosis Code Descriptions
#'
#' Descriptions of ICD-9 diagnosis (dg) codes.
#'
#' @format ## `icd9_dg`
#' A data frame with 14,567 rows and 2 columns:
#' \describe{
#'   \item{code}{ICD-9 dg code}
#'   \item{desc}{Code description}
#' }
#' @source <https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/ICD-9-CM-v32-master-descriptions.zip>
# "icd9_dg"

#' ICD-9 Procedure Code Descriptions
#'
#' Descriptions of ICD-9 procedure (pc) codes.
#'
#' @format ## `icd9_pc`
#' A data frame with 3,882 rows and 2 columns:
#' \describe{
#'   \item{code}{ICD-9 pc code}
#'   \item{desc}{Code description}
#' }
#' @source <https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads/ICD-9-CM-v32-master-descriptions.zip>
# "icd9_pc"

#' ICD-10 Diagnosis Code Descriptions
#'
#' Descriptions of ICD-10 diagnosis (dg) codes.
#'
#' @format ## `icd10_dg`
#' A data frame with 71,704 rows and 2 columns:
#' \describe{
#'   \item{code}{ICD-10 dg code}
#'   \item{desc}{Code description}
#' }
#' @source <https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2018-ICD-10-Code-Descriptions.zip>
# "icd10_dg"

#' ICD-10 Procedure Code Descriptions
#'
#' Descriptions of ICD-10 procedure (pc) codes.
#'
#' @format ## `icd10_pc`
#' A data frame with 78,705 rows and 2 columns:
#' \describe{
#'   \item{code}{ICD-10 pc code}
#'   \item{desc}{Code description}
#' }
#' @source <https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2018-ICD-10-PCS-Codes-File.zip>
# "icd10_pc"


