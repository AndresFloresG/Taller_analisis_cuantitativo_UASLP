if(!requireNamespace("pacman", quietly = TRUE)){
  install.packages("pacman")
}

if(!requireNamespace("pak")){
  install.packages("pak")
}

if(!requireNamespace("QuartoHelpers", quietly = TRUE)){
  pak::pak("AndresFloresG/QuartoHelpers")
}
