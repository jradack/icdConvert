# Setup global variables
dg_test_codes1 <- c("65640","65641")
dg_test_codes2 <- c("64601","65131","65133","65141","65161","65571",
                "65643","66320","66331","66501")
dg_test_codes3 <- c("7641", "7642", "7649")
dg_test_codes4 <- c("7690")
dg_test_codes5 <- c("7690", "65640")
pc_test_codes1 <- c("1421", "1422")

#------------------------
# Testing match_prefixes
#------------------------
test_that("match_prefixes: No matching prefixes", {
  expect_equal(match_prefixes("124", dg_test_codes3), FALSE)
})

test_that("match_prefixes: Does not fully match any prefix", {
  expect_equal(match_prefixes("764", dg_test_codes3), FALSE)
})

test_that("match_prefixes: Exactly matches a prefix", {
  expect_equal(match_prefixes("7642", dg_test_codes3), TRUE)
})

test_that("match_prefixes: Matches a prefix with extra characters", {
  expect_equal(match_prefixes("76410", dg_test_codes3), TRUE)
})

test_that("match_prefixes: Match multiple strings", {
  expect_equal(
    match_prefixes(c("124", "764", "7642", "76410"), dg_test_codes3),
    c(FALSE, FALSE, TRUE, TRUE)
  )
})

#--------------------
# Testing match_code
#--------------------
test_that("match_code: No match existing using exact matching", {
  expect_equal(match_code("76410", dg_test_codes3, "exact"), FALSE)
})

test_that("match_code: Match existing using exact matching", {
  expect_equal(match_code("7641", dg_test_codes3, "exact"), TRUE)
})

test_that("match_code: No match existing using prefix matching", {
  expect_equal(match_code("123", dg_test_codes3, "prefix"), FALSE)
})

test_that("match_code: Match existing using prefix matching", {
  expect_equal(match_code("76410", dg_test_codes3, "prefix"), TRUE)
})

#--------------------
# Testing map_stage
#--------------------
test_that("map_stage: forward mapping works for exact matching",{
  m1 <- map_stage(dg_test_codes2, icdVer_dest = 10, code_type = "dg", direction = "forward")
  expect_equal(
    m1[, c("origin_code", "src_code", "dest_code")],
    data.frame(
      origin_code = c("64601", "64601", "64601", "65131", "65133", "65141", "65161", "65571", "65571", "65643", "66320", "66331", "66331", "66331", "66501", "66501"),
      src_code = c("64601", "64601", "64601", "65131", "65133", "65141", "65161", "65571", "65571", "65643", "66320", "66331", "66331", "66331", "66501", "66501"),
      dest_code = c("O3101X0", "O3102X0", "O3103X0", "O3111X0", "O3111X0", "O3111X0", "O3111X0", "O368120",
                    "O368130", "O364XX0", "O692XX0", "O6981X0", "O6982X0", "O6989X0", "O7102", "O7103")
    )
  )
})


test_that("map_stage: backward mapping works for exact matching",{
  m1 <- map_stage(dg_test_codes1, icdVer_dest = 10, code_type = "dg", direction = "backward")
  expect_equal(
    m1[, c("origin_code", "src_code", "dest_code")],
    data.frame(
      origin_code = c("65640", "65641", "65641", "65641", "65641", "65641", "65641", "65641"),
      src_code = c(NA, "65641", "65641", "65641", "65641", "65641", "65641", "65641"),
      dest_code = c(NA, "O364XX0", "O364XX1", "O364XX2", "O364XX3", "O364XX4", "O364XX5", "O364XX9")
    )
  )
})

test_that("map_stage: forward mapping works for prefix matching", {
  m1 <- map_stage(dg_test_codes3, icdVer_dest = 10, code_type = "dg", direction = "forward", match_method = "prefix")
  e1 <- data.frame(
    origin_code = c("7641", "7641", "7641", "7641", "7641", "7641", "7641", "7641", "7641", "7641", "7641",
                    "7641", "7641", "7641", "7641", "7641", "7641", "7641", "7641", "7642", "7642", "7642",
                    "7642", "7642", "7642", "7642", "7642", "7642", "7642", "7649", "7649", "7649", "7649",
                    "7649", "7649", "7649", "7649", "7649", "7649"),
    src_code = c("76410", "76410", "76411", "76411", "76412", "76412", "76413", "76413", "76414", "76414",
                 "76415", "76415", "76416", "76416", "76417", "76417", "76418", "76418", "76419", "76420",
                 "76421", "76422", "76423", "76424", "76425", "76426", "76427", "76428", "76429", "76490",
                 "76491", "76492", "76493", "76494", "76495", "76496", "76497", "76498", "76499"),
    dest_code = c("P0500", "P0510", "P0501", "P0511", "P0502", "P0512", "P0503", "P0513", "P0504", "P0514",
                  "P0505", "P0515", "P0506", "P0516", "P0507", "P0517", "P0508", "P0518", "P0509", "P052",
                  "P052", "P052", "P052", "P052", "P052", "P052", "P052", "P052", "P052", "P059",
                  "P059", "P059", "P059", "P059", "P059", "P059", "P059", "P059", "P059")
  )
  expect_equal(m1[, c("origin_code", "src_code", "dest_code")], e1)
})

test_that("map_stage: backward mapping works for prefix matching", {
  m1 <- map_stage(dg_test_codes3, icdVer_dest = 10, code_type = "dg", direction = "backward", match_method = "prefix")
  e1 <- data.frame(
    origin_code = c("7641", "7641", "7641", "7641", "7641", "7641", "7641", "7641", "7641", "7641", "7642", "7649"),
    src_code = c("76410", "76411", "76412", "76413", "76414", "76415", "76416", "76417", "76418", "76419", "76420", "76490"),
    dest_code = c("P0510", "P0511", "P0512", "P0513", "P0514", "P0515", "P0516", "P0517", "P0518", "P0509", "P052", "P059")
  )
  expect_equal(m1[, c("origin_code", "src_code", "dest_code")], e1)
})

test_that("map_stage: works when no matches are found", {
  m1 <- map_stage(dg_test_codes4, icdVer_dest = 10, code_type = "dg", direction = "forward", match_method = "prefix")
  expect_equal(
    m1[, c("origin_code", "src_code", "dest_code")],
    data.frame(
      origin_code = "7690",
      src_code = NA,
      dest_code = NA
    )
  )
})

test_that("map_stage: works when some match, some don't", {
  m1 <- map_stage(dg_test_codes5, icdVer_dest = 10, code_type = "dg", direction = "forward", match_method = "prefix")
  e1 <- data.frame(
    origin_code = c("65640", "7690"),
    src_code = c("65640", NA),
    dest_code = c("O364XX0", NA)
  )
  expect_equal(m1[, c("origin_code", "src_code", "dest_code")], e1)
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

test_that("map_code: gem method works - prefix match", {
  m1 <- map_code(dg_test_codes3, icdVer_dest = 10, code_type = "dg", method = "gem", match_method = "prefix")
  expect_equal(
    m1$dest_code,
    c("P0500", "P0510", "P0501", "P0511", "P0502", "P0512", "P0503", "P0513",
      "P0504", "P0514", "P0505", "P0515", "P0506", "P0516", "P0507", "P0517",
      "P0508", "P0518", "P0509", "P052", "P052", "P052", "P052", "P052", "P052",
      "P052", "P052", "P052", "P052", "P059", "P059", "P059", "P059", "P059",
      "P059", "P059", "P059", "P059", "P059")
  )
})

test_that("map_code: reverse-gem method works - prefix match", {
  m1 <- map_code(dg_test_codes3, icdVer_dest = 10, code_type = "dg", method = "reverse-gem", match_method = "prefix")
  expect_equal(
    m1$dest_code,
    c("P0510", "P0511", "P0512", "P0513", "P0514", "P0515", "P0516", "P0517",
      "P0518", "P0509", "P052", "P059")
  )
})

test_that("map_code: both method works - prefix match", {
  m1 <- map_code(dg_test_codes3, icdVer_dest = 10, code_type = "dg", method = "both", match_method = "prefix")
  expect_equal(
    m1$dest_code,
    c("P0500", "P0510", "P0501", "P0511", "P0502", "P0512", "P0503", "P0513",
      "P0504", "P0514", "P0505", "P0515", "P0506", "P0516", "P0507", "P0517",
      "P0508", "P0518", "P0509", "P052", "P052", "P052", "P052", "P052", "P052",
      "P052", "P052", "P052", "P052", "P059", "P059", "P059", "P059", "P059",
      "P059", "P059", "P059", "P059", "P059")
  )
})

test_that("map_code: multi-stage method works - prefix match", {
  m1 <- map_code(dg_test_codes3, icdVer_dest = 10, code_type = "dg", method = "multi-stage", match_method = "prefix")
  expect_equal(
    m1$dest_code,
    c("P0500", "P0509", "P0510", "P0519", "P0501", "P0511", "P0502", "P0512",
      "P0503", "P0513", "P0504", "P0514", "P0505", "P0515", "P0506", "P0516",
      "P0507", "P0517", "P0508", "P0518", "P0500", "P0509", "P0510", "P0519",
      "P052", "P052", "P052", "P052", "P052", "P052", "P052", "P052", "P052",
      "P052", "P059", "P059", "P059", "P059", "P059", "P059", "P059", "P059",
      "P059", "P059")
  )
})

test_that("map_code: gem method when no matches found", {
  m1 <- map_code(dg_test_codes4, icdVer_dest = 10, code_type = "dg", method = "gem")
  expect_equal(
    m1[, c("origin_code", "src_code", "dest_code")],
    data.frame(
      origin_code = "7690",
      src_code = NA,
      dest_code = NA
    )
  )
})

test_that("map_code: reverse-gem method when no matches found", {
  m1 <- map_code(dg_test_codes4, icdVer_dest = 10, code_type = "dg", method = "reverse-gem")
  expect_equal(
    m1[, c("origin_code", "src_code", "dest_code")],
    data.frame(
      origin_code = "7690",
      src_code = NA,
      dest_code = NA
    )
  )
})

test_that("map_code: both method when no matches found", {
  m1 <- map_code(dg_test_codes4, icdVer_dest = 10, code_type = "dg", method = "both")
  expect_equal(
    m1[, c("origin_code", "src_code", "dest_code")],
    data.frame(
      origin_code = "7690",
      src_code = NA,
      dest_code = NA
    )
  )
})

test_that("map_code: multi-stage method when no matches found", {
  m1 <- map_code(dg_test_codes4, icdVer_dest = 10, code_type = "dg", method = "multi-stage")
  expect_equal(
    m1[, c("origin_code", "src_code", "dest_code")],
    data.frame(
      origin_code = "7690",
      src_code = NA,
      dest_code = NA
    )
  )
})

test_that("map_code: gem method when some matches found", {
  m1 <- map_code(dg_test_codes5, icdVer_dest = 10, code_type = "dg", method = "gem")
  expect_equal(
    m1[, c("origin_code", "src_code", "dest_code")],
    data.frame(
      origin_code = c("65640", "7690"),
      src_code = c("65640", NA),
      dest_code = c("O364XX0", NA)
    )
  )
})

test_that("map_code: reverse-gem method when some matches found", {
  m1 <- map_code(dg_test_codes5, icdVer_dest = 10, code_type = "dg", method = "reverse-gem")
  expect_equal(
    m1[, c("origin_code", "src_code", "dest_code")],
    data.frame(
      origin_code = c("65640", "7690"),
      src_code = c(NA, NA),
      dest_code = c(NA, NA)
    )
  )
})

test_that("map_code: both method when some matches found", {
  m1 <- map_code(dg_test_codes5, icdVer_dest = 10, code_type = "dg", method = "both")
  expect_equal(
    m1[, c("origin_code", "src_code", "dest_code")],
    data.frame(
      origin_code = c("65640", "7690"),
      src_code = c("65640", NA),
      dest_code = c("O364XX0", NA)
    )
  )
})

test_that("map_code: multi-stage method when some matches found", {
  m1 <- map_code(dg_test_codes5, icdVer_dest = 10, code_type = "dg", method = "multi-stage")
  expect_equal(
    m1[, c("origin_code", "src_code", "dest_code")],
    data.frame(
      origin_code = c("65640", "65640", "65640", "65640", "65640", "65640", "65640", "7690"),
      src_code = c("65640", "65640", "65640", "65640", "65640", "65640", "65640", NA),
      dest_code = c("O364XX0", "O364XX1", "O364XX2", "O364XX3", "O364XX4", "O364XX5", "O364XX9", NA)
    )
  )
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

test_that("get_description: when no matching codes are found",{
  d1 <- get_description(dg_test_codes4, icdVer = 9, code_type = "dg")
  expect_equal(
    d1,
    data.frame(
     codes = c("7690"),
     desc = NA |> as.character()
    )
  )
})

test_that("get_description: when some codes match and others don't",{
  d1 <- get_description(dg_test_codes5, icdVer = 9, code_type = "dg")
  expect_equal(
    d1,
    data.frame(
      codes = c("65640", "7690"),
      desc = c("Intrauterine death, affecting management of mother, unspecified as to episode of care or not applicable", NA)
    )
  )
})


#-------------------
# Test map_describe
#-------------------
test_that("map_describe: check that codes are mapped and description provided",{
  m1 <- map_describe(dg_test_codes1, icdVer_dest = 10, code_type = "dg", method = "gem")
  expect_equal(
    m1,
    data.frame(
      origin_code = c("65640", "65641"),
      src_code = c("65640", "65641"),
      src_desc = c("Intrauterine death, affecting management of mother, unspecified as to episode of care or not applicable", "Intrauterine death, affecting management of mother, delivered, with or without mention of antepartum condition"),
      dest_code = c("O364XX0","O364XX0"),
      dest_desc = c("Maternal care for intrauterine death, not applicable or unspecified", "Maternal care for intrauterine death, not applicable or unspecified")
    )
  )
})

test_that("map_describe: check that keepMapCode works properly",{
  m1 <- map_describe(dg_test_codes1, icdVer_dest = 10, code_type = "dg", method = "gem", keepMapCode = TRUE)
  expect_equal(
    m1,
    data.frame(
      origin_code = c("65640", "65641"),
      src_code = c("65640", "65641"),
      src_desc = c("Intrauterine death, affecting management of mother, unspecified as to episode of care or not applicable", "Intrauterine death, affecting management of mother, delivered, with or without mention of antepartum condition"),
      dest_code = c("O364XX0","O364XX0"),
      dest_desc = c("Maternal care for intrauterine death, not applicable or unspecified", "Maternal care for intrauterine death, not applicable or unspecified"),
      map_code = c("10000", "10000")
    )
)
})

test_that("map_describe: check that keepMapCode works when using multi-stage method",{
  m1 <- map_describe(dg_test_codes1, icdVer_dest = 10, code_type = "dg", method = "multi-stage")
  expect_equal(
    m1,
    data.frame(
      origin_code = c("65640", "65640", "65640", "65640", "65640", "65640", "65640", "65641", "65641", "65641", "65641", "65641", "65641", "65641"),
      src_code = c("65640", "65640", "65640", "65640", "65640", "65640", "65640", "65641", "65641", "65641", "65641", "65641", "65641", "65641"),
      src_desc = c(
        "Intrauterine death, affecting management of mother, unspecified as to episode of care or not applicable",
        "Intrauterine death, affecting management of mother, unspecified as to episode of care or not applicable",
        "Intrauterine death, affecting management of mother, unspecified as to episode of care or not applicable",
        "Intrauterine death, affecting management of mother, unspecified as to episode of care or not applicable",
        "Intrauterine death, affecting management of mother, unspecified as to episode of care or not applicable",
        "Intrauterine death, affecting management of mother, unspecified as to episode of care or not applicable",
        "Intrauterine death, affecting management of mother, unspecified as to episode of care or not applicable",
        "Intrauterine death, affecting management of mother, delivered, with or without mention of antepartum condition",
        "Intrauterine death, affecting management of mother, delivered, with or without mention of antepartum condition",
        "Intrauterine death, affecting management of mother, delivered, with or without mention of antepartum condition",
        "Intrauterine death, affecting management of mother, delivered, with or without mention of antepartum condition",
        "Intrauterine death, affecting management of mother, delivered, with or without mention of antepartum condition",
        "Intrauterine death, affecting management of mother, delivered, with or without mention of antepartum condition",
        "Intrauterine death, affecting management of mother, delivered, with or without mention of antepartum condition"
      ),
      dest_code = c("O364XX0", "O364XX1", "O364XX2", "O364XX3", "O364XX4", "O364XX5", "O364XX9", "O364XX0", "O364XX1", "O364XX2", "O364XX3", "O364XX4", "O364XX5", "O364XX9"),
      dest_desc = c(
        "Maternal care for intrauterine death, not applicable or unspecified",
        "Maternal care for intrauterine death, fetus 1",
        "Maternal care for intrauterine death, fetus 2",
        "Maternal care for intrauterine death, fetus 3",
        "Maternal care for intrauterine death, fetus 4",
        "Maternal care for intrauterine death, fetus 5",
        "Maternal care for intrauterine death, other fetus",
        "Maternal care for intrauterine death, not applicable or unspecified",
        "Maternal care for intrauterine death, fetus 1",
        "Maternal care for intrauterine death, fetus 2",
        "Maternal care for intrauterine death, fetus 3",
        "Maternal care for intrauterine death, fetus 4",
        "Maternal care for intrauterine death, fetus 5",
        "Maternal care for intrauterine death, other fetus"
      )
    )
  )
})

test_that("map_describe: multi-stage works when there are no matches",{
  m1 <- map_describe(c("1234567", "098765"), icdVer_dest = 10, code_type = "dg", method = "multi-stage")
  e1 <- data.frame(
    origin_code = c("098765", "1234567"),
    src_code = NA |> as.character(),
    src_desc = NA |> as.character(),
    dest_code = NA |> as.character(),
    dest_desc = NA |> as.character()
  )
  expect_equal(m1, e1)
})

test_that("map_describe: check prefix matching works",{
  m1 <- map_describe(dg_test_codes3, icdVer_dest = 10, code_type = "dg", method = "gem", match_method = "prefix")
  expect_equal(
    m1,
    data.frame(
      origin_code = c(
        "7641", "7641", "7641", "7641", "7641", "7641", "7641", "7641", "7641", "7641", "7641", "7641",
        "7641", "7641", "7641", "7641", "7641", "7641", "7641", "7642", "7642", "7642", "7642", "7642",
        "7642", "7642", "7642", "7642", "7642", "7649", "7649", "7649", "7649", "7649", "7649", "7649",
        "7649", "7649", "7649"
        ),
      src_code = c(
        "76410", "76410", "76411", "76411", "76412", "76412", "76413", "76413", "76414", "76414",
        "76415", "76415", "76416", "76416", "76417", "76417", "76418", "76418", "76419", "76420",
        "76421", "76422", "76423", "76424", "76425", "76426", "76427", "76428", "76429", "76490",
        "76491", "76492", "76493", "76494", "76495", "76496", "76497", "76498", "76499"
        ),
      src_desc = c(
        "\"Light-for-dates\" with signs of fetal malnutrition, unspecified [weight]",
        "\"Light-for-dates\" with signs of fetal malnutrition, unspecified [weight]",
        "\"Light-for-dates\" with signs of fetal malnutrition, less than 500 grams",
        "\"Light-for-dates\" with signs of fetal malnutrition, less than 500 grams",
        "\"Light-for-dates\"with signs of fetal malnutrition, 500-749 grams",
        "\"Light-for-dates\"with signs of fetal malnutrition, 500-749 grams",
        "\"Light-for-dates\" with signs of fetal malnutrition, 750-999 grams",
        "\"Light-for-dates\" with signs of fetal malnutrition, 750-999 grams",
        "\"Light-for-dates\" with signs of fetal malnutrition, 1,000-1,249 grams",
        "\"Light-for-dates\" with signs of fetal malnutrition, 1,000-1,249 grams",
        "\"Light-for-dates\" with signs of fetal malnutrition, 1,250-1,499 grams",
        "\"Light-for-dates\" with signs of fetal malnutrition, 1,250-1,499 grams",
        "\"Light-for-dates\" with signs of fetal malnutrition, 1,500-1,749 grams",
        "\"Light-for-dates\" with signs of fetal malnutrition, 1,500-1,749 grams",
        "\"Light-for-dates\" with signs of fetal malnutrition, 1,750-1,999 grams",
        "\"Light-for-dates\" with signs of fetal malnutrition, 1,750-1,999 grams",
        "\"Light-for-dates\"with signs of fetal malnutrition, 2,000-2,499 grams",
        "\"Light-for-dates\"with signs of fetal malnutrition, 2,000-2,499 grams",
        "\"Light-for-dates\"with signs of fetal malnutrition, 2,500 grams and over",
        "Fetal malnutrition without mention of \"light-for-dates\", unspecified [weight]",
        "Fetal malnutrition without mention of \"light-for-dates\", less than 500 grams",
        "Fetal malnutrition without mention of \"light-for-dates\", 500-749 grams",
        "Fetal malnutrition without mention of \"light-for-dates\", 750-999 grams",
        "Fetal malnutrition without mention of \"light-for-dates\", 1,000-1,249 grams",
        "Fetal malnutrition without mention of \"light-for-dates\", 1,250-1,499 grams",
        "Fetal malnutrition without mention of \"light-for-dates\", 1,500-1,749 grams",
        "Fetal malnutrition without mention of \"light-for-dates\", 1,750-1,999 grams",
        "Fetal malnutrition without mention of \"light-for-dates\", 2,000-2,499 grams",
        "Fetal malnutrition without mention of \"light-for-dates\", 2,500 grams and over",
        "Fetal growth retardation, unspecified, unspecified [weight]",
        "Fetal growth retardation, unspecified, less than 500 grams",
        "Fetal growth retardation, unspecified, 500-749 grams",
        "Fetal growth retardation, unspecified, 750-999 grams",
        "Fetal growth retardation, unspecified, 1,000-1,249 grams",
        "Fetal growth retardation, unspecified, 1,250-1,499 grams",
        "Fetal growth retardation, unspecified, 1,500-1,749 grams",
        "Fetal growth retardation, unspecified, 1,750-1,999 grams",
        "Fetal growth retardation, unspecified, 2,000-2,499 grams",
        "Fetal growth retardation, unspecified, 2,500 grams and over"
      ),
      dest_code = c(
        "P0500", "P0510", "P0501", "P0511", "P0502", "P0512", "P0503", "P0513", "P0504", "P0514",
        "P0505", "P0515", "P0506", "P0516", "P0507", "P0517", "P0508", "P0518", "P0509", "P052",
        "P052", "P052", "P052", "P052", "P052", "P052", "P052", "P052", "P052", "P059",
        "P059", "P059", "P059", "P059", "P059", "P059", "P059", "P059", "P059"
      ),
      dest_desc = c(
        "Newborn light for gestational age, unspecified weight",
        "Newborn small for gestational age, unspecified weight",
        "Newborn light for gestational age, less than 500 grams",
        "Newborn small for gestational age, less than 500 grams",
        "Newborn light for gestational age, 500-749 grams",
        "Newborn small for gestational age, 500-749 grams",
        "Newborn light for gestational age, 750-999 grams",
        "Newborn small for gestational age, 750-999 grams",
        "Newborn light for gestational age, 1000-1249 grams",
        "Newborn small for gestational age, 1000-1249 grams",
        "Newborn light for gestational age, 1250-1499 grams",
        "Newborn small for gestational age, 1250-1499 grams",
        "Newborn light for gestational age, 1500-1749 grams",
        "Newborn small for gestational age, 1500-1749 grams",
        "Newborn light for gestational age, 1750-1999 grams",
        "Newborn small for gestational age, 1750-1999 grams",
        "Newborn light for gestational age, 2000-2499 grams",
        "Newborn small for gestational age, 2000-2499 grams",
        "Newborn light for gestational age, 2500 grams and over",
        "Newborn affected by fetal (intrauterine) malnutrition not light or small for gestational age",
        "Newborn affected by fetal (intrauterine) malnutrition not light or small for gestational age",
        "Newborn affected by fetal (intrauterine) malnutrition not light or small for gestational age",
        "Newborn affected by fetal (intrauterine) malnutrition not light or small for gestational age",
        "Newborn affected by fetal (intrauterine) malnutrition not light or small for gestational age",
        "Newborn affected by fetal (intrauterine) malnutrition not light or small for gestational age",
        "Newborn affected by fetal (intrauterine) malnutrition not light or small for gestational age",
        "Newborn affected by fetal (intrauterine) malnutrition not light or small for gestational age",
        "Newborn affected by fetal (intrauterine) malnutrition not light or small for gestational age",
        "Newborn affected by fetal (intrauterine) malnutrition not light or small for gestational age",
        "Newborn affected by slow intrauterine growth, unspecified",
        "Newborn affected by slow intrauterine growth, unspecified",
        "Newborn affected by slow intrauterine growth, unspecified",
        "Newborn affected by slow intrauterine growth, unspecified",
        "Newborn affected by slow intrauterine growth, unspecified",
        "Newborn affected by slow intrauterine growth, unspecified",
        "Newborn affected by slow intrauterine growth, unspecified",
        "Newborn affected by slow intrauterine growth, unspecified",
        "Newborn affected by slow intrauterine growth, unspecified",
        "Newborn affected by slow intrauterine growth, unspecified"
      )
    )
  )
})
