# .adapt_range -----------------------------------------------------------------------------------------------

y_min = 0
y_max = 10

# Test ts with NA value
ts_dummy_na  <- c(1, NA, 10)
ts_expect_na <- c(1, NA, 10)

# Test with value that exceeds y_max
ts_dummy_max  <- c(1, NA, 100)
ts_expect_max <- c(1, NA, 10)

# Test with value below y_min
ts_dummy_min  <- c(-100, NA, 10)
ts_expect_min <- c(0, NA, 10)

test_that(".adapt_amp", {
  expect_equal(.adapt_range(ts_dummy_na, y_min, y_max), ts_expect_na)
  expect_equal(.adapt_range(ts_dummy_max, y_min, y_max), ts_expect_max)
  expect_equal(.adapt_range(ts_dummy_min, y_min, y_max), ts_expect_min)
})


# check_ts ---------------------------------------------------------------------------------------------------

valid_min <- 0.5
w_min <- 0.2
y_min = 200
y_max = 10000
amplitude_cutoff <- 2000

# Test with not enough valid values
ts_y_nvalid <- c(200, NA, 250, NA, NA, NA, 600)
ts_d_nvalid <- c(1, NA, 3, NA, NA, NA, 5)
ts_w_nvalid <- c(1, 1, 1, 1, 1, 1, 1)
ts_expect_nvalid <- list(y=c(NA, NA, NA, NA, NA, NA, NA),
                          d=c(NA, NA, NA, NA, NA, NA, NA),
                          w=c(NA, NA, NA, NA, NA, NA, NA))

# Test with NA approximation
ts_y_na <- c(200, NA, 400, NA, NA, 700, 800)
ts_d_na <- c(1, 2, 3, 4, 5, 6, 7)
ts_w_na <- c(1, 1, 1, 1, 1, 1, 1)
ts_expect_na <- list(y=c(200, 300, 400, 500, 600, 700, 800),
                     d=c(1, 2, 3, 4, 5, 6, 7),
                     w=c(1, 0.2, 1, 0.2, 0.2, 1, 1))

# Test with spike approx
ts_y_spk <- c(100, 200, 300, 100000, 500, -100, 700)
ts_d_spk <- c(1, 2, 3, 4, 5, 6, 7)
ts_w_spk <- c(0.2, 1, 1, 0.2, 1, 0.2, 1)
ts_expect_spk <- list(y=c(200, 200, 300, 400, 500, 600, 700),
                      d=c(1, 2, 3, 4, 5, 6, 7),
                      w=c(0.2, 1, 1, 0.2, 1, 0.2, 1))

# test with range adjustment (no spike approx)
ts_y_rg <- c(100, 200, 300, 100000, 500, -100, 700)
ts_d_rg <- c(1, 2, 3, 4, 5, 6, 7)
ts_w_rg <- c(0.2, 1, 1, 0.2, 1, 0.2, 1)
ts_expect_rg <- list(y=c(200, 200, 300, 10000, 500, 200, 700),
                     d=c(1, 2, 3, 4, 5, 6, 7),
                     w=c(0.2, 1, 1, 0.2, 1, 0.2, 1))

# test with amplitude cutoff
ts_y_ac <- c(3000, 4000, 4000, 3000, 3000, 4000, 4000, 3000)
ts_d_ac <- c(1, 2, 3, 4, 1, 2, 3, 4)
ts_w_ac <- c(1, 1, 1, 1, 1, 1, 1, 1)
ts_expect_ac <- list(y=c(NA, NA, NA, NA, NA, NA, NA, NA),
                     d=c(NA, NA, NA, NA, NA, NA, NA, NA),
                     w=c(NA, NA, NA, NA, NA, NA, NA, NA))

# test with different ts lengths
ts_y_len <- c(1, 2, 3)
ts_d_len <- c(1, 2)
ts_w_len <- c(1, 2, 3, 4)

test_that("check_ts", {
  expect_equal(check_ts(ts_y_nvalid, ts_d_nvalid, ts_w_nvalid, valid_min, amplitude_cutoff = 0),
               ts_expect_nvalid)
  expect_equal(check_ts(ts_y_na, ts_d_na, ts_w_na, valid_min, amplitude_cutoff = 0, w_min = 0.2),
               ts_expect_na)
  expect_equal(check_ts(ts_y_spk, ts_d_spk, ts_w_spk, valid_min, amplitude_cutoff = 0, w_min = 0.2,
                        y_min = y_min, y_max = y_max, approx_spike = T), ts_expect_spk)
  expect_equal(check_ts(ts_y_rg, ts_d_rg, ts_w_rg, valid_min, amplitude_cutoff = 0, w_min = 0.2, y_min = y_min,
                        y_max = y_max, approx_spike = F), ts_expect_rg)
  expect_equal(check_ts(ts_y_ac, ts_d_ac, ts_w_ac, valid_min, amplitude_cutoff, w_min = 0.2, y_min = y_min,
                        y_max = y_max, approx_spike = F), ts_expect_ac)
  expect_warning(check_ts(ts_y_len, ts_d_len, ts_w_len))
})
