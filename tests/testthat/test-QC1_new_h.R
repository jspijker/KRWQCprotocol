test_that("QC1_new_h T1 standaard waarden", {

  data(metingen)
  d <- metingen
  
  x <- QC1_new_h(d_metingen = d)
  
  # test if attributes exist
  expect_true(qcout_attrexists(x))
  x_attr <- attr(x, "qcout")
  expect_false(is.null(x_attr[["QC1_new_h"]]))
  expect_true(is.list(x_attr[["QC1_new_h"]][["resultaat"]]))


  ids <- x_attr[["QC1_new_h"]][["oordeel"]][["verdacht"]]
  qcids <- metingen$qcid
  v1 <- intersect(ids, qcids)
  expect_true(length(v1) == 0)

  expect_true(nrow(metingen) == nrow(x))


})


test_that("QC1_new_h T2 negatieve waarden", {

              data(metingen)
              d <- metingen
              d$waarde[1] <- -1

              x <- QC1_new_h(d_metingen = d)

              # test if attributes exist
              x_attr <- attr(x, "qcout")
              
              ids <- x_attr[["QC1_new_h"]][["oordeel"]][["verdacht"]]
              qcids <- metingen$qcid
              v1 <- intersect(ids, qcids)
              expect_true(length(v1) == 1)
})
