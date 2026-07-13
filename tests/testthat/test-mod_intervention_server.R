test_that("Decision engine caps step target at +5000 from baseline during phase 2", {
  
  # Helper function to generate standalone mock KPIs as a tibble
  gen_kpis <- function(steps) {
    tibble::tibble(
      n_days = 14,
      med_steps_day = steps,
      med_steps_80plus = 60,
      med_steps_90plus = 40,
      med_steps_100plus = 20,
      med_steps_110plus = 0,
      med_steps_120plus = 0
    )
  }
  
  # 1. Setup a baseline at t=0 with 6000 steps
  # This creates a hard ceiling of 11,000 steps (6000 + 5000)
  baseline_kpis <- gen_kpis(6000)
  
  
  # --- Test Case A: Participant's last target was ALREADY at the cap ---
  # Their last target was 11000. Even if their current performance is amazing 
  # (e.g., 12000 steps), the target should remain frozen at 11000.
  
  prev_kpis_A <- gen_kpis(11500) 
  
  state_already_capped <- list(
    history = list(
      list(t_index = 0L, kpis = baseline_kpis),
      list(t_index = 5L, kpis = prev_kpis_A)
    ),
    last_X = 11000,          # Already at the cap
    last_Y = 30L,
    last_Z = 100L,
    last_steps_factor = 1.05,
    last_minutes_inc = 5L,
    consecutive_fails = 0L
  )
  
  cur_kpis_excellent <- gen_kpis(12000)
  
  res_capped <- decide_message(
    state = state_already_capped,
    cur_k = cur_kpis_excellent,
    prev_k = prev_kpis_A,
    nombre = "TestUser",
    steps_factor = 1.05, 
    t = 6
  )
  
  # Expect the next_X to be frozen at the previous target (11000)
  expect_equal(res_capped$next_X, 11000)
  
  
  # --- Test Case B: Participant's current performance triggers the cap ---
  # Their last target was 10000. They walked 11500 steps (which is >= 6000 + 5000).
  # The target should freeze at their last deployed target (10000) instead of increasing.
  
  prev_kpis_B <- gen_kpis(10500)
  
  state_reach_cap <- list(
    history = list(
      list(t_index = 0L, kpis = baseline_kpis),
      list(t_index = 5L, kpis = prev_kpis_B)
    ),
    last_X = 10000,          # Not yet at the cap
    last_Y = 30L,
    last_Z = 100L,
    last_steps_factor = 1.05,
    last_minutes_inc = 5L,
    consecutive_fails = 0L
  )
  
  cur_kpis_over_threshold <- gen_kpis(11500)
  
  res_reach_cap <- decide_message(
    state = state_reach_cap,
    cur_k = cur_kpis_over_threshold,
    prev_k = prev_kpis_B,
    nombre = "TestUser",
    steps_factor = 1.05, 
    t = 6
  )
  
  # Expect the next_X to be frozen at the previously deployed target (10000)
  expect_equal(res_reach_cap$next_X, 10000)
  
})