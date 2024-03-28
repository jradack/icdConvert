# Setup global variables
dg_test_codes1 <- c("65640","65641")
dg_test_codes2 <- c("64601","65131","65133","65141","65161","65571",
                "65643","66320","66331","66501")
pc_test_codes1 <- c("1421", "1422")

#--------------------
# Testing map_stage
#--------------------
test_that("map_stage: forward mapping works",{
  m1 <- map_stage(dg_test_codes2, icdVer_dest = 10, code_type = "dg", direction = "forward")
  expect_equal(m1$dest_code, c("O3101X0","O3102X0","O3103X0","O3111X0","O3111X0","O3111X0","O3111X0","O368120","O368130","O364XX0","O692XX0","O6981X0","O6982X0","O6989X0","O7102","O7103"))
})


test_that("map_stage: backward mapping works",{
  m1 <- map_stage(dg_test_codes1, icdVer_dest = 10, code_type = "dg", direction = "backward")
  expect_equal(m1$dest_code, c(NA,"O364XX0","O364XX1","O364XX2","O364XX3","O364XX4","O364XX5","O364XX9"))
})


#----------------
# Test map_code
#----------------
test_that("map_code: gem method works",{
  m1 <- map_code(dg_test_codes2, icdVer_dest = 10, code_type = "dg", method = "gem")
  expect_equal(m1$dest_code, c("O3101X0","O3102X0","O3103X0","O3111X0","O3111X0","O3111X0","O3111X0","O368120","O368130","O364XX0","O692XX0","O6981X0","O6982X0","O6989X0","O7102","O7103"))
})

test_that("map_code: reverse-gem method works",{
  m1 <- map_code(dg_test_codes1, icdVer_dest = 10, code_type = "dg", method = "reverse-gem")
  expect_equal(m1$dest_code, c(NA,"O364XX0","O364XX1","O364XX2","O364XX3","O364XX4","O364XX5","O364XX9"))
})

test_that("map_code: both method works",{
  m1 <- map_code(dg_test_codes1, icdVer_dest = 10, code_type = "dg", method = "both")
  expect_equal(m1$dest_code, c("O364XX0","O364XX0","O364XX1","O364XX2","O364XX3","O364XX4","O364XX5","O364XX9"))
})

test_that("map_code: multi-stage works",{
  m1 <- map_code(dg_test_codes1, icdVer_dest = 10, code_type = "dg", method = "multi-stage")
  expect_equal(m1$dest_code, c("O364XX0","O364XX1","O364XX2","O364XX3","O364XX4","O364XX5","O364XX9","O364XX0","O364XX1","O364XX2","O364XX3","O364XX4","O364XX5","O364XX9"))
})

#---------------------
# Test get_description
#---------------------
test_that("get_description: check that descriptions are pulled correctly",{
 d1 <- get_description(dg_test_codes1, icdVer = 9, code_type = "dg")
 expect_equal(d1,
   data.frame(
     codes = c("65640", "65641"),
     desc = c("Intrauterine death, affecting management of mother, unspecified as to episode of care or not applicable", "Intrauterine death, affecting management of mother, delivered, with or without mention of antepartum condition")
   )
 )
})

#-------------------
# Test map_describe
#-------------------
test_that("map_describe: check that codes are mapped and description provided",{
  m1 <- map_describe(dg_test_codes1, icdVer_dest = 10, code_type = "dg", method = "gem")
  expect_equal(m1,
               data.frame(src_code = c("65640", "65641"),
                          src_desc = c("Intrauterine death, affecting management of mother, unspecified as to episode of care or not applicable", "Intrauterine death, affecting management of mother, delivered, with or without mention of antepartum condition"),
                          dest_code = c("O364XX0","O364XX0"),
                          dest_desc = c("Maternal care for intrauterine death, not applicable or unspecified", "Maternal care for intrauterine death, not applicable or unspecified")
                          )
               )
})

test_that("map_describe: check that keepMapCode works properly",{
  m1 <- map_describe(dg_test_codes1, icdVer_dest = 10, code_type = "dg", method = "gem", keepMapCode = TRUE)
  expect_equal(m1,
               data.frame(src_code = c("65640", "65641"),
                          src_desc = c("Intrauterine death, affecting management of mother, unspecified as to episode of care or not applicable", "Intrauterine death, affecting management of mother, delivered, with or without mention of antepartum condition"),
                          dest_code = c("O364XX0","O364XX0"),
                          dest_desc = c("Maternal care for intrauterine death, not applicable or unspecified", "Maternal care for intrauterine death, not applicable or unspecified"),
                          map_code = c("10000", "10000"),
                          approximate = c("1", "1"),
                          no_map = c("0", "0"),
                          combination = c("0", "0"),
                          scenario = c("0", "0"),
                          choice_lists = c("0", "0")
               )
)
})

test_that("map_describe: multi-stage works when there are no matches",{
  m1 <- map_describe(c("1234567", "098765"), icdVer_dest = 10, code_type = "dg", method = "multi-stage")
  e1 <- data.frame(src_code = character(), src_desc = character(), dest_code = character(), dest_desc = character())
  expect_equal(m1, e1)
})

