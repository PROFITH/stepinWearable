library(testthat)
library(dplyr)
library(lubridate)
# Note: You may need to load other internal functions or helpers here.
# For example, if kpis() is internal, you might use stepinWearable:::kpis

# --- SETUP: MOCK DATA AND STATE ---

# Define a temporary directory for testing file persistence
TEMP_TEST_DIR <- tempdir() 

# Mock Data: A small, controlled data frame representing minute-level steps.
# Assumes structure: timestamp (POSIXct), steps (integer)
hr_values = 60:180
hr_range_length <- length(hr_values)
probabilities <- rep(0.002, hr_range_length)
resting_indices <- which(hr_values >= 60 & hr_values <= 75)
probabilities[resting_indices] <- 0.005 + (0.012 * (hr_values[resting_indices] - 60))
moderate_indices <- which(hr_values >= 90 & hr_values <= 110)
probabilities[moderate_indices] <- 0.004 + (0.008 * (1 - abs(hr_values[moderate_indices] - 100) / 10))
peak_suppress_indices <- which(hr_values > 150)
probabilities[peak_suppress_indices] <- 0.0001
probabilities <- probabilities / sum(probabilities)
mock_minute_data <- tibble(
  timestamp = force_tz(seq(
    ymd_hms("2024-05-01 00:00:00"), 
    ymd_hms("2024-05-14 23:59:00"), 
    by = "1 min"
  ), tzone = "Europe/Madrid"),
  steps = sample(c(0, 10, 80, 100, 120), size = 14 * 1440, replace = TRUE, 
                 prob = c(0.7, 0.1, 0.1, 0.05, 0.05)),
  HeartRate = sample(60:180, size = 14 * 1440, replace = TRUE, 
                     prob = probabilities)
)

# Mock State: A typical initial state (t=0) and a state after one intervention (t=1).
mock_initial_state <- list(
  history = list(),
  last_X = NA_integer_,
  last_Y = 0L,
  last_Z = NA_integer_,
  last_steps_factor = 1.0,
  consecutive_fails = 0L,
  consecutive_success = 0L
  
)

mock_t1_state <- list(
  history = list(),
  last_X = NA_integer_,
  last_Y = 0L,
  last_Z = NA_integer_,
  consecutive_fails = 0L,
  consecutive_success = 0L
)

# Mock KPI structure (assuming your kpis() function outputs this)
mock_kpis <- tibble::tibble(
  n_days = 14,
  med_steps_day = 12000,
  med_steps_80plus = 20,
  med_steps_90plus = 10,
  med_steps_100plus = 5
)

mock_kpis_old <- tibble::tibble(
  n_days = 14,
  med_steps_day = 11000,
  med_steps_80plus = 20,
  med_steps_90plus = 10,
  med_steps_100plus = 5
)

mock_kpis_nodata <- tibble::tibble(
  n_days = 14,
  med_steps_day = NA_real_,
  med_steps_80plus = NA_real_,
  med_steps_90plus = NA_real_,
  med_steps_100plus = NA_real_
)

# --- TEST SUITE 1: DATA PROCESSING AND KPI CALCULATION ---
test_that("Data preprocessing returns expected structure and dimensions", {
  # NOTE: Testing 'preprocess_fitbit' requires mocking a file read, which is complex.
  # We focus on the 'daily_summary' output from mock minute data.
  
  # 1. Test daily_summary (assuming it's available for testing)
  dsum_result <- daily_summary(mock_minute_data, tz = "Europe/Madrid")
  
  # Check dimensions for 14 full days
  expect_true(inherits(dsum_result, "data.frame"))
  # 14 days of data are expected
  expect_equal(nrow(dsum_result), 14) 
  
  # Check for essential columns used in plotting/KPIs
  expect_true(all(c("date", "steps_day", "steps_80plus", "steps_100plus") %in% names(dsum_result)))
})

test_that("KPI calculation handles edge cases (NA/zero data)", {
  # 2. Test kpis function (if exported or accessible)
  
  # Test with an empty data frame
  empty_df <- tibble::tibble(date = as_date(character()), 
                             steps_day = integer(),
                             steps_80plus = integer(),
                             steps_90plus = integer(),
                             steps_100plus = integer(),
                             valid_day = logical())
  kpis_empty <- kpis(empty_df)
  expect_true(is.na(kpis_empty$med_steps_day))
  expect_equal(kpis_empty$n_days, 0)
  
  # Test with a window containing some NA data (should use na.rm=TRUE)
  na_df <- tibble(date = as_date(c("2024-01-01", "2024-01-02")),
                  steps_day = c(5000, NA_real_),
                  steps_80plus = c(20, NA_real_),
                  steps_90plus = c(10, NA_real_),
                  steps_100plus = c(5, NA_real_),
                  valid_day = c(TRUE, FALSE))
  kpis_na <- stepinWearable:::kpis(na_df)
  expect_equal(kpis_na$med_steps_day, 5000)
})


# --- TEST SUITE 2: STATE MANAGEMENT ---
test_that("State persistence functions correctly (load/save)", {
  id <- "TEST_001"
  state_path <- .participant_state_path(TEMP_TEST_DIR, id) # Assuming internal helper
  
  # 1. Load: Should return default empty state if file doesn't exist
  state_loaded_new <- load_participant_state(TEMP_TEST_DIR, id)
  expect_equal(state_loaded_new$consecutive_fails, 0L)
  expect_equal(length(state_loaded_new$history), 0)
  
  # 2. Save: Save a mock update
  mock_entry <- list(t_index = 0L, start_date = as_date("2024-01-01"), next_X = 10000)
  
  updated_state <- state_loaded_new
  updated_state$history <- list(mock_entry)
  updated_state$last_X <- 10000
  
  # Assuming save_participant_state is available
  save_participant_state(updated_state, TEMP_TEST_DIR, id) 
  expect_true(file.exists(state_path))
  
  # 3. Load: Load the saved state and verify the update
  state_loaded_saved <- load_participant_state(TEMP_TEST_DIR, id)
  expect_equal(state_loaded_saved$last_X, 10000)
  expect_equal(length(state_loaded_saved$history), 1)
  
  # Cleanup
  if (file.exists(state_path)) file.remove(state_path)
})


# --- TEST SUITE 3: DECISION ENGINE (The Core Logic) ---
test_that("Decision engine selects correct key based on performance", {
  # Test cases based on assumed internal logic of decide_message(st, cur_k, prev_k, ...)
  
  # Scenario 1: Initial state (t=0), very good performance
  X_factor = 1.05
  Y_inc = 5
  res_initial_good <- decide_message(
    state = mock_initial_state,
    cur_k = mock_kpis,
    prev_k = NULL,         # No previous window
    nombre = "TestUser",
    steps_factor = X_factor,
    minutes_inc = Y_inc,
    t = 0
  )
  expect_true(res_initial_good$key == "msg0")
  expect_equal(res_initial_good$consecutive_fails, 0L)
  expect_true(res_initial_good$next_X == mock_kpis$med_steps_day * X_factor)
  
  # Scenario 2: Consecutive fails (3 fails) -> Met even if below target (above X - 0.01)
  X_factor = 1.1
  Y_inc = 3
  state_3fails <- mock_initial_state
  state_3fails$consecutive_fails <- 3L
  state_3fails$last_X <- 8000
  state_3fails$last_steps_factor <- 1.1
  state_3fails$last_minutes_inc <- 3
  state_3fails$history <- list(
    t_index           = 4,
    kpis              = mock_kpis,
    steps_factor      = X_factor ,
    minutes_inc       = Y_inc,
    target_cadence    = 90
  )
  
  res_fails_bad <- decide_message(
    state = state_3fails,
    cur_k = mock_kpis,
    prev_k = mock_kpis_old,
    nombre = "TestUser",
    steps_factor = 1.1,
    minutes_inc = 5,
    t = 1
  )
  # Should not trigger supportive message as cur_k is above prev_k*1.09
  # cur_k is 12000
  # prev_k is 11000
  # prev_k * 1.1 = 12100; prev_k * 1.09 = 11990 --> MET
  expect_true(res_fails_bad$key %in% c("pasos1"))
  expect_equal(res_fails_bad$consecutive_fails, 0) 
  
  # Scenario 3: Consecutive fails (3 fails) -> Doesn't meet -> supportive message
  X_factor = 1.5
  Y_inc = 3
  state_3fails <- mock_initial_state
  state_3fails$consecutive_fails <- 3L
  state_3fails$last_X <- 11000 * X_factor
  state_3fails$last_steps_factor = X_factor
  state_3fails$last_minutes_inc = Y_inc
  state_3fails$history <- list(
    t_index           = 4,
    kpis              = mock_kpis,
    steps_factor      = X_factor ,
    minutes_inc       = Y_inc,
    target_cadence    = 90
  )
  
  res_fails_bad <- decide_message(
    state = state_3fails,
    cur_k = mock_kpis,
    prev_k = mock_kpis_old,
    nombre = "TestUser",
    steps_factor = X_factor,
    minutes_inc = 5,
    t = 1
  )
  
  expect_true(res_fails_bad$key %in% c("pasos2"))    # doesn't reach target
  expect_equal(res_fails_bad$consecutive_fails, state_3fails$consecutive_fails + 1L)  # 1 additional fail
  expect_true(grepl('Si hay algo que quiera comentarnos o cree', res_fails_bad$text))  # supportive msg appended
  
  # Scenario 3: Nodata (handled by the caller function, but good to ensure engine handles it)
  # The server handles nodata separately, but if kpis are all NA, the engine should fail gracefully
  res_nodata <- decide_message(
    state = mock_initial_state,
    cur_k = mock_kpis_nodata,
    prev_k = NULL,
    nombre = "TestUser",
    steps_factor = 1.1,
    minutes_inc = 5,
    t = 4
  )
  # Check that targets are set to default/NA if no data is present
  expect_true(is.na(res_nodata$next_X)) 
})
