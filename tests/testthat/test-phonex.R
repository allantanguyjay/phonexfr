# Regles de Brouard

test_that("PH se lit F et les lettres doublees comptent pour une", {
  expect_equal(phonex("PHILIPPE"), "filit")
  expect_equal(phonex("PHILIPPE"), phonex("FILIPE"))
})

test_that("le H initial est muet", {
  expect_equal(phonex("HUGO"), phonex("UGO"))
})

test_that("les espaces et la casse ne comptent pas", {
  expect_equal(phonex("Le Brun"), phonex("LEBRUN"))
  expect_equal(phonex("Martin"), phonex("MARTIN"))
})

test_that("les finales T, X, S, Z sont muettes", {
  expect_equal(phonex("DUBOIS"), phonex("DUBOI"))
})

test_that("C doux et double S se lisent pareil", {
  expect_equal(phonex("MAURICE"), phonex("MAURISSE"))
})

test_that("le son 'on' n'est pas confondu avec 'an'", {
  expect_false(phonex("BLONDEL") == phonex("BLANDEL"))
})

test_that("les accents sont traites comme chez Brouard", {
  expect_equal(phonex("H\u00e9l\u00e8ne"), "ylyn")
})

# Modifications

test_that("EIM et AIM se lisent 'in'", {
  expect_equal(phonex("DAIM"), phonex("DIN"))
})

test_that("pas de son nasal devant un N ou M double", {
  expect_equal(phonex("ANNE"), phonex("ANE"))
})

test_that("SC se lit 'sk' devant A, O, U", {
  expect_equal(phonex("PASCAL"), "toskol")
  expect_equal(phonex("PASCAL"), phonex("PASKAL"))
})

test_that("QU se lit K", {
  expect_equal(phonex("JACQUES"), phonex("JACQ"))
})

test_that("GU se lit K devant un son e ou i, KU ailleurs", {
  expect_equal(phonex("GUERIN"), phonex("QUERIN"))
  expect_equal(phonex("GUINCHARD"), phonex("QUINCHARD"))
  expect_equal(phonex("GUSTAVE"), "kustof")
})

test_that("-QUE final se lit comme -C", {
  expect_equal(phonex("LEVEQUE"), phonex("LEVEC"))
})

test_that("le E final est muet", {
  expect_equal(phonex("FAURE"), phonex("FORT"))
  expect_equal(phonex("MAURICE"), phonex("MAURISSE"))
})

test_that("les chiffres sont retires", {
  expect_equal(phonex("MARTIN 2"), phonex("MARTIN"))
})

test_that("les lettres accentuees hors table sont simplifiees", {
  expect_equal(phonex("Mu\u00f1oz"), phonex("MUNOZ"))
})

# Comportement general

test_that("les NA sont conserves, ainsi que la longueur et l'ordre", {
  resultat <- phonex(c("MARTIN", NA, "DUBOIS", "MARTIN"))
  expect_length(resultat, 4)
  expect_true(is.na(resultat[2]))
  expect_equal(resultat[c(1, 3)], c("nort4", "tuf2"))
  expect_equal(resultat[1], resultat[4])
})
