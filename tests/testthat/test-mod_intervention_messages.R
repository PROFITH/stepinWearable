test_that("Decision engine caps duration target at 45 min for 120 steps/min", {
  
  # Helper function for intensity KPIs
  gen_kpis_intensity <- function(steps, mins_120) {
    tibble::tibble(
      n_days = 14,
      med_steps_day = steps,
      med_steps_80plus = 60,
      med_steps_90plus = 50,
      med_steps_100plus = 50,
      med_steps_110plus = 50,
      med_steps_120plus = mins_120
    )
  }
  
  # 1. Setup Baseline & Previous state
  baseline_kpis <- gen_kpis_intensity(6000, 0)
  prev_kpis     <- gen_kpis_intensity(10000, 40)
  
  # Participant's last assigned targets were X=10000, Y=40, Z=120
  state_reach_y_cap <- list(
    history = list(
      list(t_index = 0L, kpis = baseline_kpis),
      list(t_index = 5L, kpis = prev_kpis)
    ),
    last_X = 10000,
    last_Y = 20L,
    last_Z = 120L,
    last_steps_factor = 1.0,
    last_minutes_inc = 5L,
    consecutive_fails = 0L
  )
  
  # 2. Current window performance: The participant met the 20 min target at 120
  #    steps/min, so next target should be 25
  cur_kpis_max_intensity <- gen_kpis_intensity(10500, 22)
  
  res_y_capped <- decide_message(
    state = state_reach_y_cap,
    cur_k = cur_kpis_max_intensity,
    prev_k = prev_kpis,
    nombre = "TestUser",
    steps_factor = 1.00, 
    minutes_inc = 5,
    t = 6
  )
  
  # 3. Assertions
  expect_equal(res_y_capped$next_Y, 25L)
  expect_equal(res_y_capped$next_Z, 120L)
  
  # 4. Current window performance: The participant met the 20 min target by far,
  #    accumulating 30 min at 120 steps/min, so next target should be 35
  cur_kpis_max_intensity <- gen_kpis_intensity(10500, 30)
  
  res_y_capped <- decide_message(
    state = state_reach_y_cap,
    cur_k = cur_kpis_max_intensity,
    prev_k = prev_kpis,
    nombre = "TestUser",
    steps_factor = 1.00, 
    minutes_inc = 5,
    t = 6
  )
  
  # 5. Assertions
  expect_equal(res_y_capped$next_Y, 35L)
  expect_equal(res_y_capped$next_Z, 120L)
  
  
  # 6. Current window performance: The participant met the 20 min target by far,
  #    accumulating 49 min at 120 steps/min, as Y/Z target is capped at 
  #    45 min at 120 steps/min, this should be the next target 
  cur_kpis_max_intensity <- gen_kpis_intensity(10500, 49)
  
  res_y_capped <- decide_message(
    state = state_reach_y_cap,
    cur_k = cur_kpis_max_intensity,
    prev_k = prev_kpis,
    nombre = "TestUser",
    steps_factor = 1.00, 
    minutes_inc = 5,
    t = 6
  )
  
  # 7. Assertions
  expect_equal(res_y_capped$next_Y, 45L)
  expect_equal(res_y_capped$next_Z, 120L)
  

})

test_that("Failed step target at t = 5 resets consecutive success streak", {
  
  prev_kpis <- tibble::tibble(
    n_days = 14,
    med_steps_day = 10000,
    med_steps_80plus = 20,
    med_steps_90plus = 10,
    med_steps_100plus = 5,
    med_steps_110plus = 0,
    med_steps_120plus = 0
  )
  
  cur_kpis <- tibble::tibble(
    n_days = 14,
    med_steps_day = 5000,
    med_steps_80plus = 20,
    med_steps_90plus = 10,
    med_steps_100plus = 5,
    med_steps_110plus = 0,
    med_steps_120plus = 0
  )
  
  state <- list(
    history = list(),
    last_X = 10000,
    last_Y = NA_integer_,
    last_Z = NA_integer_,
    last_steps_factor = 1.05,
    last_minutes_inc = NA_integer_,
    consecutive_fails = 0L,
    consecutive_success = 1L
  )
  
  result <- decide_message(
    state = state,
    cur_k = cur_kpis,
    prev_k = prev_kpis,
    nombre = "TestUser",
    steps_factor = 1.05,
    t = 5
  )
  
  expect_equal(result$key, "ambos2")
  expect_false(result$steps_met)
  expect_equal(result$consecutive_success, 0L)
  expect_equal(result$consecutive_fails, 1L)
})