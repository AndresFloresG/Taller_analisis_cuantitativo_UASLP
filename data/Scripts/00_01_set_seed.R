seed_state_path <- file.path("data", "rng_state.rds")
if (!dir.exists("data")) dir.create("data", recursive = TRUE)
if (file.exists(seed_state_path)) {
  .Random.seed <- readRDS(seed_state_path)
} else {
  set.seed(sample.int(99999, 1))
  saveRDS(.Random.seed, seed_state_path)
}
