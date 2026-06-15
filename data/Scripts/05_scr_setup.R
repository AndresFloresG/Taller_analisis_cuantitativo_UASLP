local({
  librs <- c("tidyverse", "janitor", "psych", "lavaan", "flextable", "officer", "officedown")
  invisible(pacman::p_load(char = librs, update = FALSE, install = FALSE, character.only = TRUE)) 
})