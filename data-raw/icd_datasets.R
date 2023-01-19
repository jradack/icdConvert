################################################################################
# This file builds the datasets used by the package for converting ICD diagnosis
# and procedure codes. The General Equivalence Mapping (GEM) files are used for
# mapping the codes between ICD 9 and 10 and the description files are used for
# getting the description of the codes.
################################################################################

load_all()

#-----------------
# Test Dataset
#-----------------
example_data_icd9_pc <-
  data.frame(icd9_pc = c("456","4561","4562","4563","457","4571","4572","4573",
                         "4574","4575","4576","4579","458","459","4590","4591",
                         "4592","4593","4594","4595","460","4601","4602","4603",
                         "4604","461","4610","4611","4612","4613","4614","462",
                         "4620","4621","4622","4623","4624","463","4631","4632",
                         "4639","464","4640","4641","4642","4643","465","4650",
                         "4651","4652","468","4680","4681","4682","4685","541",
                         "5411","5412","5419","24","241","242","243","3834",
                         "3835","3844","3845","3864","3865","3880","3884",
                         "3885","142","1421","1422","1423","1424","1425","1426",
                         "1427","1429","1434","144","1441","1449","145","1451",
                         "1452","1453","1454","1455","1459","303","304","311",
                         "312","3121","3129","3174","9390","9670","9671","9672",
                         "23","231","232","233","234","235","239","3404","9601",
                         "9602","9603","9604","9605"))
usethis::use_data(example_data_icd9_pc, overwrite = TRUE)

#-----------------
# GEM Files
#-----------------
# Diagnosis Codes (ICD 9 -> 10)
dg_9_10_gem <- icd_read_fwf("data-raw/gems_dg_2018/2018_I9gem.txt",
                            width = c(5,-1,7,-1,5),
                            colNames = c('src','dest','map_code'))
# usethis::use_data(dg_9_10_gem, overwrite = TRUE)

# Procedure Codes (ICD 9 -> 10)
pc_9_10_gem <- icd_read_fwf("data-raw/gems_pc_2018/gem_i9pcs.txt",
                            width = c(4,-2,7,-1,5),
                            colNames = c('src','dest','map_code'))
# usethis::use_data(pc_9_10_gem, overwrite = TRUE)

#--------------------
# Description Files
#--------------------
# ICD 9 Diagnosis Codes
icd9_dg <- icd_read_fwf("data-raw/descriptions/CMS32_DESC_LONG_DX.txt",
                        width = c(6,400),
                        colNames = c('code', 'desc'),
                        fileEncoding = "ANSI_X3.4-1986")
# usethis::use_data(icd9_dg, overwrite = TRUE)

# ICD 10 Diagnosis Codes
icd10_dg <- icd_read_fwf("data-raw/descriptions/icd10cm_codes_2018.txt",
                         width = c(7,-1,400),
                         colNames = c('code', 'desc'))
# usethis::use_data(icd10_dg, overwrite = TRUE)

# ICD 9 Procedure Codes
icd9_pc <- icd_read_fwf("data-raw/descriptions/CMS32_DESC_LONG_SG.txt",
                        width = c(4,-1,400),
                        colNames = c('code', 'desc'))
# usethis::use_data(icd9_pc, overwrite = TRUE)

# ICD 9 Procedure Codes
icd10_pc <- icd_read_fwf("data-raw/descriptions/icd10pcs_codes_2018.txt",
                         width = c(7,-1,400),
                         colNames = c('code', 'desc'))
# usethis::use_data(icd10_pc, overwrite = TRUE)

usethis::use_data(dg_9_10_gem, pc_9_10_gem, icd9_dg, icd10_dg, icd9_pc, icd10_pc, internal = TRUE)
