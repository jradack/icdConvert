
test_that("forward mapping works",{
  test_codes <- c("64601","65131","65133","65141","65161","65571",
                  "65643","66320","66331","66501")
  m1 <- map_code(test_codes, icdVer_dest = 10, code_type = "dg", direction = "forward")
  t1 <- touch::icd_map(test_codes, method = "gem", decimal = FALSE, nomatch = NA, output = "tidy-data")
  expect_equal(m1$dest_code, t1$`ICD-10`)
})


test_that("backward mapping works",{
  test_codes <- c("65640","65641")
  m1 <- map_code(test_codes, icdVer_dest = 10, code_type = "dg", direction = "backward")
  t1 <- touch::icd_map(test_codes, method = "reverse-gem", decimal = FALSE, nomatch = NA, output = "tidy-data")
  expect_equal(m1$dest_code, t1$`ICD-10`)
})
