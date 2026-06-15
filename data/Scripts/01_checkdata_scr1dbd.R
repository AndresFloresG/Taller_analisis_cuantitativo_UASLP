dirpath <- file.path(
  "data", # Nombre de la carpeta
  "_Bases" # Nombre de la subcarpeta
)

basedatos <- file.path(
  dirpath,
  "datos.xlsx"
)

if (!dir.exists(dirpath)) {
  dir.create(dirpath)
}

if (file.exists(basedatos)) {
  rutabase <- basedatos
  db <- read_excel(basedatos)
} else {
  if (interactive()) {
    utils::winDialog(message = "Seleccione su base de datos", "okcancel")
    rutabase <- file.choose()
    db <- read_excel(rutabase)
    writexl::write_xlsx(db, path = basedatos)
  } else {
    stop("Ejecutar el script 01 en modo interactivo")
  }
}

rm(rutabase, dirpath, basedatos)
