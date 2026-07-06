# 1 . Importar datos desde Survey-----------------------------------------------

# Asegurarse de que las credenciales necesarias estén disponibles
if (exists("email") && exists("password") && exists("server") && exists("formid")) {
  message("Credenciales de Survey cargadas correctamente.")
} else {
  stop("No se encontraron las credenciales de Survey. Asegúrate de cargarlas desde el script maestro.")
}


data_ejemplo <-read_excel("raw_data/kiva 2026.xlsx")
vars_needed <- colnames(data_ejemplo)

## Conect to SurveyCTO ----------------------------------------------------------------

API <- paste0('https://',server,'.surveycto.com/api/v2/forms/data/wide/json/',formid,'?date=0')


## Import data -------------------------------------------------------------

max_attempts <- 10
attempt <- 1

repeat {
  
  # 1. Llamada a la API con GET
  response <- GET(
    url = API,
    authenticate(email, password, type = "basic"),
    add_headers("Content-Type" = "application/json")
  )
  
  # 2. Verificar que la respuesta HTTP sea exitosa
  if (status_code(response) == 200) {
    
    # 3. Intentar parsear el contenido
    data <- tryCatch(
      {
        jsonlite::fromJSON(
          content(response, "text", encoding = "UTF-8"),
          flatten = TRUE
        )
      },
      error = function(e) NULL
    )
    
    # 4. Verificar que el resultado sea un data frame válido
    if (is.data.frame(data)) {
      break
    }
  }
  
  # 5. Si se alcanzó el máximo de intentos, detener ejecución
  if (attempt >= max_attempts) {
    stop("Se alcanzó el número máximo de intentos sin obtener un data frame válido.")
  }
  
  # 6. Esperar antes de reintentar
  Sys.sleep(300)
  attempt <- attempt + 1
}

# Transformar base de datos ----------------------------------------------------


for (v in vars_needed) {
  if (!(v %in% names(data))) {
    data[[v]] <- rep(NA, nrow(data))
  }
}

# Organizar variables

# Reordenar y dejar las demás al final
otras_vars <- setdiff(names(data), vars_needed)
data <- data[ , c(vars_needed, otras_vars)]

# Filtrar pilotos --------------------------------------------------------------

data <- data %>%
  filter(username != "testkiva")

data <- data %>%
  mutate(across(where(is.character), ~ na_if(str_trim(.x), "")),
         f2_ocup = as.numeric(f2_ocup))


# Nombres de las variables de opción múltiple
multi_vars <- c(
  "f2_residentes",
  "f2_negocio_close",
  "f2_negocio_deuda_plus",
  "f2_negocio_apoyo",
  "f2_planeacion_tipo",
  "f2_ahorro_medio",
  "f2_ahorro_proposito",
  "f2_deudas_situacion",
  "f2_productos",
  "f2_productos_crezc",
  "f2_uso_prestamo",
  "f2_barreras",
  "f2_inversion",
  "f2_edu_que",
  "f2_menores_edu_mot",
  "f2_mejora_aspectos"
)

for (var in multi_vars) {
  var_cols <- names(data)[startsWith(names(data), paste0(var, "_")) & !grepl("_o$", names(data))]
  
  if (length(var_cols) > 0) {
    data <- data %>%
      rowwise() %>%
      mutate(!!var := {
        vals <- c_across(all_of(var_cols))
        
        if (all(is.na(vals))) {
          NA_character_
        } else {
          activos <- which(vals == 1)
          if (length(activos) == 0) NA_character_ else {
            seleccionados <- gsub(paste0("^", var, "_"), "", var_cols[activos])
            paste(seleccionados, collapse = ",")
          }
        }
      }) %>%
      ungroup()
  }
}




