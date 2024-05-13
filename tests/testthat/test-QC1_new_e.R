test_that("QC1_new_e", {


              data(metingen)

              x <- QC1_new_e(d_metingen = metingen, verbose = FALSE)

              # test if attributes exist
              expect_true(qcout_attrexists(x))
              x_attr <- attr(x, "qcout")
              expect_false(is.null(x_attr[["QC1_new_e"]]))
              expect_true(is.list(x_attr[["QC1_new_e"]][["resultaat"]]))

              ids <- x_attr[["QC1_new_e"]][["oordeel"]][["verdacht"]]
              qcids <- metingen$qcid
              v1 <- intersect(ids, qcids)
              expect_true(length(v1) > 0)
              expect_false(any(v1 != ids))

              expect_true(nrow(metingen) == nrow(x))


})
