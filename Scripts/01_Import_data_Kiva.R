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
  filter(!KEY %in% c(
    "uuid:75ab7815-3b0a-4e89-9833-bfcdd2444b13",
    "uuid:0487f901-d48b-45ed-a2c8-d2e6153b3b9a",
    "uuid:0e2ba87c-21d8-428a-8667-28c0940dd0e7",
    "uuid:718f63d4-54f7-4774-a9fc-2902ae13eeeb",
    "uuid:e291cf5c-a8ac-4bad-91cb-c99ecfac7ac6",
    "uuid:5d501855-5af1-4f7d-8fd1-c5231fce6dfb",
    "uuid:43d46bd1-1f95-4a5d-84e3-63a20a99045a",
    "uuid:d1d9870e-e2c4-47d7-a4ad-4b13f1a128fe",
    "uuid:4fb55b14-4c10-4770-b07a-e50cfc2ea50d",
    "uuid:84a68e9e-5b9c-43d6-af96-08bafe256518",
    "uuid:fc3c9f5c-5fd3-4197-a352-5abe21cecb9a",
    "uuid:75aa1ac8-db64-4e2b-a650-c4673b3cf494",
    "uuid:86519d33-1b37-4abf-ae53-31ba1d340944",
    "uuid:2a6531d9-894f-4d70-bfdb-bcceedd82522",
    "uuid:1d64a33d-9da7-43c8-91b4-f2e0622da29c",
    "uuid:a6aab690-2edc-4878-bd8a-d4f7729251c7",
    "uuid:718e3b13-bdc2-472f-88be-2c6158baa417"
  ))

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




