#### Alertas -------------------------------------------------------------------

alertas <- data %>%
  # 1. Calculamos las variables necesarias primero
  mutate(
    duration_minutes = round(as.numeric(duration) / 60, 2),
    part_valido = if_else(f2_consent == 1 & !is.na(caseid), 1, 0,
                          missing = 0)
  ) %>%
  # 2. Agrupamos por ID
  group_by(caseid) %>%
  # 3. Ordenamos: primero los inválidos (0) y luego los válidos (1).
  # Dentro de cada grupo, la última fila será el "último válido".
  # Si no hay válidos, la última fila será simplemente el último registro cronológico.
  arrange(part_valido, .by_group = TRUE) %>%
  # 4. Tomamos el último registro de esa jerarquía
  slice_tail(n = 1) %>%
  ungroup() %>%
  # 5. Creamos la bandera de duración
  mutate(
    flag_duracion = if_else(part_valido == 1 & duration_minutes < 20, 1, 0, missing = 0)
  )

#### Validación de saltos #####
alertas <- alertas %>%
  mutate(
    s_f2_status_1 = if_else(!is.na(f2_status_1) & !(f2_call_ans == 2), 1, 0),
    s_f2_contact_phone = if_else(!is.na(f2_contact_phone) & !(f2_status_1 == 6), 1, 0),
    s_f2_reagendar_hora = if_else(!is.na(f2_reagendar_hora) & !(f2_status_1 == 4), 1, 0),
    s_f2_consent = if_else(!is.na(f2_consent) & !(f2_call_ans == 1), 1, 0),
    s_f2_cel_yn = if_else(!is.na(f2_cel_yn) & !(f2_consent == 1), 1, 0),
    s_f2_cel_new = if_else(!is.na(f2_cel_new) & !(f2_consent == 1 & f2_cel_yn == 2), 1, 0),
    s_f2_cel2 = if_else(!is.na(f2_cel2) & !(f2_consent == 1), 1, 0),
    s_f2_cel2_person = if_else(!is.na(f2_cel2_person) & !(f2_consent == 1 & f2_cel2 != "NA"), 1, 0),
    s_f2_cel2_person_otro = if_else(!is.na(f2_cel2_person_otro) & !(f2_consent == 1 & f2_cel2_person == 66), 1, 0),
    s_f2_correo = if_else(!is.na(f2_correo) & !(f2_consent == 1 & (is.na(contact_correo) | contact_correo == "NA")), 1, 0),
    s_f2_correo_yn = if_else(!is.na(f2_correo_yn) & !(f2_consent == 1 & (!is.na(contact_correo) & contact_correo != "NA")), 1, 0),
    s_f2_correo_new = if_else(!is.na(f2_correo_new) & !(f2_consent == 1 & f2_correo_yn == 2), 1, 0),
    s_f2_canal_pref = if_else(!is.na(f2_canal_pref) & !(f2_consent == 1), 1, 0),
    s_f2_edad = if_else(!is.na(f2_edad) & !(f2_consent == 1), 1, 0),
    s_f2_estcivil = if_else(!is.na(f2_estcivil) & !(f2_consent == 1), 1, 0),
    s_f2_pais = if_else(!is.na(f2_pais) & !(f2_consent == 1), 1, 0),
    s_f2_pais_new = if_else(!is.na(f2_pais_new) & !(f2_consent == 1 & f2_pais == 2), 1, 0),
    s_f2_dpto = if_else(!is.na(f2_dpto) & !(f2_consent == 1 & f2_pais == 1), 1, 0),
    s_f2_dpto_now = if_else(!is.na(f2_dpto_now) & !(f2_consent == 1 & (f2_dpto == 2 & f2_pais == 1)), 1, 0),
    s_f2_ciudad = if_else(!is.na(f2_ciudad) & !(f2_consent == 1 & (f2_dpto == 1 & f2_pais == 1)), 1, 0),
    s_f2_ciudad_now = if_else(!is.na(f2_ciudad_now) & !(f2_consent == 1 & (f2_pais == 1 & (f2_dpto == 2 | f2_ciudad == 2))), 1, 0),
    s_f2_credito = if_else(!is.na(f2_credito) & !(f2_consent == 1), 1, 0),
    s_f2_credito_control = if_else(!is.na(f2_credito_control) & !(f2_consent == 1 & (f2_grupo == 2 & f2_credito == 1)), 1, 0),
    s_f2_resp_financiero = if_else(!is.na(f2_resp_financiero) & !(f2_consent == 1 & a8_jefe == 1), 1, 0),
    s_f2_id = if_else(!is.na(f2_id) & !(f2_consent == 1), 1, 0),
    s_f2_permanencia = if_else(!is.na(f2_permanencia) & !(f2_consent == 1), 1, 0),
    s_f2_educacion = if_else(!is.na(f2_educacion) & !(f2_consent == 1), 1, 0),
    s_f2_member = if_else(!is.na(f2_member) & !(f2_consent == 1), 1, 0),
    s_f2_residentes = if_else(!is.na(f2_residentes) & !(f2_consent == 1 & f2_pais == 1), 1, 0),
    s_f2_hacinamiento = if_else(!is.na(f2_hacinamiento) & !(f2_consent == 1), 1, 0),
    s_f2_mujeres = if_else(!is.na(f2_mujeres) & !(f2_consent == 1), 1, 0),
    s_f2_menores = if_else(!is.na(f2_menores) & !(f2_consent == 1), 1, 0),
    s_f2_trabajadores = if_else(!is.na(f2_trabajadores) & !(f2_consent == 1), 1, 0),
    s_f2_ingresos = if_else(!is.na(f2_ingresos) & !(f2_consent == 1), 1, 0),
    s_f2_monto = if_else(!is.na(f2_monto) & !(f2_consent == 1), 1, 0),
    s_f2_monto_check = if_else(!is.na(f2_monto_check) & !(f2_consent == 1), 1, 0),
    s_f2_resp_relacion = if_else(!is.na(f2_resp_relacion) & !(f2_consent == 1 & f2_resp_financiero == 2), 1, 0),
    s_f2_resp_relacion_otro = if_else(!is.na(f2_resp_relacion_otro) & !(f2_consent == 1 & f2_resp_relacion == 66), 1, 0),
    s_f2_resp_genero = if_else(!is.na(f2_resp_genero) & !(f2_consent == 1 & f2_resp_financiero == 2), 1, 0),
    s_f2_resp_edad = if_else(!is.na(f2_resp_edad) & !(f2_consent == 1 & f2_resp_financiero == 2), 1, 0),
    s_f2_resp_educ = if_else(!is.na(f2_resp_educ) & !(f2_consent == 1 & f2_resp_financiero == 2), 1, 0),
    s_f2_ocup = if_else(!is.na(f2_ocup) & !(f2_consent == 1), 1, 0),
    s_f2_ocup_otro = if_else(!is.na(f2_ocup_otro) & !(f2_consent == 1 & f2_ocup == 66), 1, 0),
    s_f2_negocio_continua = if_else(!is.na(f2_negocio_continua) & !(f2_consent == 1 & b2_negocio == 1), 1, 0),
    s_f2_negocio = if_else(!is.na(f2_negocio) & !(f2_consent == 1 & ((f2_ocup == 1 | f2_ocup == 2) & b2_negocio != 1)), 1, 0),
    s_f2_negocio_close = if_else(!is.na(f2_negocio_close) & !(f2_consent == 1 & f2_negocio_continua == 2), 1, 0),
    s_f2_negocio_deuda = if_else(!is.na(f2_negocio_deuda) & !(f2_consent == 1 & (f2_negocio_close_2 == 1 & (f2_grupo == 1 | f2_credito_control == 1))), 1, 0),
    s_f2_negocio_deuda_plus = if_else(!is.na(f2_negocio_deuda_plus) & !(f2_consent == 1 & f2_negocio_close_2 == 1), 1, 0),
    s_f2_negocio_close_otro = if_else(!is.na(f2_negocio_close_otro) & !(f2_consent == 1 & f2_negocio_close_66 == 1), 1, 0),
    s_f2_ocup_tiempo = if_else(!is.na(f2_ocup_tiempo) & !(f2_consent == 1 & f2_ocup < 8), 1, 0),
    s_f2_ocup_tiempo_meses = if_else(!is.na(f2_ocup_tiempo_meses) & !(f2_consent == 1 & f2_ocup_tiempo == 1), 1, 0),
    s_f2_ocup_tiempo_anios = if_else(!is.na(f2_ocup_tiempo_anios) & !(f2_consent == 1 & f2_ocup_tiempo == 2), 1, 0),
    s_f2_negocio_gasto = if_else(!is.na(f2_negocio_gasto) & !(f2_consent == 1 & (f2_negocio_continua == 1 | f2_negocio == 1)), 1, 0),
    s_f2_negocio_formal = if_else(!is.na(f2_negocio_formal) & !(f2_consent == 1 & (f2_negocio_continua == 1 | f2_negocio == 1)), 1, 0),
    s_f2_negocio_apoyo_yn = if_else(!is.na(f2_negocio_apoyo_yn) & !(f2_consent == 1 & (f2_negocio_formal == 1 & (f2_grupo == 1 | f2_credito_control == 1))), 1, 0),
    s_f2_negocio_apoyo = if_else(!is.na(f2_negocio_apoyo) & !(f2_consent == 1 & (f2_negocio_apoyo_yn == 1 & (f2_grupo == 1 | f2_credito_control == 1))), 1, 0),
    s_f2_negocio_apoyo_otro = if_else(!is.na(f2_negocio_apoyo_otro) & !(f2_consent == 1 & f2_negocio_apoyo_66 == 1), 1, 0),
    s_f2_negocio_lugar = if_else(!is.na(f2_negocio_lugar) & !(f2_consent == 1 & (f2_negocio_continua == 1 | f2_negocio == 1)), 1, 0),
    s_f2_negocio_empleados = if_else(!is.na(f2_negocio_empleados) & !(f2_consent == 1 & (f2_negocio_continua == 1 | f2_negocio == 1)), 1, 0),
    s_f2_ocup_sector = if_else(!is.na(f2_ocup_sector) & !(f2_consent == 1 & f2_ocup < 7), 1, 0),
    s_f2_ocup_sector_otro = if_else(!is.na(f2_ocup_sector_otro) & !(f2_consent == 1 & f2_ocup_sector == 66), 1, 0),
    s_f2_ocup_contrato = if_else(!is.na(f2_ocup_contrato) & !(f2_consent == 1 & f2_ocup < 7), 1, 0),
    s_f2_ocup_contrato_tipo = if_else(!is.na(f2_ocup_contrato_tipo) & !(f2_consent == 1 & (f2_ocup < 7 & f2_ocup_contrato == 1)), 1, 0),
    s_f2_ocup_contrato_tipo_otro = if_else(!is.na(f2_ocup_contrato_tipo_otro) & !(f2_consent == 1 & f2_ocup_contrato_tipo == 66), 1, 0),
    s_f2_ocup_estabilidad = if_else(!is.na(f2_ocup_estabilidad) & !(f2_consent == 1 & f2_ocup < 7), 1, 0),
    s_f2_ocup_ingresos = if_else(!is.na(f2_ocup_ingresos) & !(f2_consent == 1 & f2_ocup < 7), 1, 0),
    s_f2_ocup_ingresos_monto = if_else(!is.na(f2_ocup_ingresos_monto) & !(f2_consent == 1 & f2_ocup < 7), 1, 0),
    s_f2_ocup_ingresos_monto_check = if_else(!is.na(f2_ocup_ingresos_monto_check) & !(f2_consent == 1 & f2_ocup < 7), 1, 0),
    s_f2_ocup_ingresos_freq = if_else(!is.na(f2_ocup_ingresos_freq) & !(f2_consent == 1 & f2_ocup < 7), 1, 0),
    s_f2_ocup_ingresos_medio = if_else(!is.na(f2_ocup_ingresos_medio) & !(f2_consent == 1 & f2_ocup < 7), 1, 0),
    s_b7_ingresos_medio_otro = if_else(!is.na(b7_ingresos_medio_otro) & !(f2_consent == 1 & f2_ocup_ingresos_medio == 66), 1, 0),
    s_f2_planeacion = if_else(!is.na(f2_planeacion) & !(f2_consent == 1), 1, 0),
    s_f2_planeacion_tipo = if_else(!is.na(f2_planeacion_tipo) & !(f2_consent == 1 & f2_planeacion == 1), 1, 0),
    s_f2_planeacion_tipo_otro = if_else(!is.na(f2_planeacion_tipo_otro) & !(f2_consent == 1 & f2_planeacion_tipo_66 == 1), 1, 0),
    s_f2_ahorro = if_else(!is.na(f2_ahorro) & !(f2_consent == 1), 1, 0),
    s_f2_ahorro_medio = if_else(!is.na(f2_ahorro_medio) & !(f2_consent == 1 & f2_ahorro == 1), 1, 0),
    s_f2_ahorro_medio_otro = if_else(!is.na(f2_ahorro_medio_otro) & !(f2_consent == 1 & f2_ahorro_medio_66 == 1), 1, 0),
    s_f2_ahorro_proposito = if_else(!is.na(f2_ahorro_proposito) & !(f2_consent == 1 & f2_ahorro == 1), 1, 0),
    s_f2_ahorro_status = if_else(!is.na(f2_ahorro_status) & !(f2_consent == 1 & c2_ahorro == 1), 1, 0),
    s_f2_ahorro_status_worse = if_else(!is.na(f2_ahorro_status_worse) & !(f2_consent == 1 & f2_ahorro_status == 2), 1, 0),
    s_f2_ahorro_status_worse_otro = if_else(!is.na(f2_ahorro_status_worse_otro) & !(f2_consent == 1 & f2_ahorro_status_worse == 66), 1, 0),
    s_f2_razon_noahorra = if_else(!is.na(f2_razon_noahorra) & !(f2_consent == 1 & f2_ahorro == 0), 1, 0),
    s_f2_monto_emerg = if_else(!is.na(f2_monto_emerg) & !(f2_consent == 1), 1, 0),
    s_f2_prestamo_persona = if_else(!is.na(f2_prestamo_persona) & !(f2_consent == 1), 1, 0),
    s_f2_gastos = if_else(!is.na(f2_gastos) & !(f2_consent == 1), 1, 0),
    s_f2_cuentas = if_else(!is.na(f2_cuentas) & !(f2_consent == 1), 1, 0),
    s_f2_deudas = if_else(!is.na(f2_deudas) & !(f2_consent == 1), 1, 0),
    s_f2_deudas_num = if_else(!is.na(f2_deudas_num) & !(f2_consent == 1 & f2_deudas != 1), 1, 0),
    s_f2_deudas_pago = if_else(!is.na(f2_deudas_pago) & !(f2_consent == 1 & f2_deudas != 1), 1, 0),
    s_f2_deudas_carga = if_else(!is.na(f2_deudas_carga) & !(f2_consent == 1 & f2_deudas != 1), 1, 0),
    s_f2_deudas_situacion = if_else(!is.na(f2_deudas_situacion) & !(f2_consent == 1 & f2_deudas != 1), 1, 0),
    s_f2_productos = if_else(!is.na(f2_productos) & !(f2_consent == 1), 1, 0),
    s_f2_productos_crezc = if_else(!is.na(f2_productos_crezc) & !(f2_consent == 1 & f2_grupo == 1), 1, 0),
    s_f2_tasa = if_else(!is.na(f2_tasa) & !(f2_consent == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1))), 1, 0),
    s_f2_apoyo = if_else(!is.na(f2_apoyo) & !(f2_consent == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1))), 1, 0),
    s_f2_apoyo_crezc = if_else(!is.na(f2_apoyo_crezc) & !(f2_consent == 1 & (f2_apoyo == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)))), 1, 0),
    s_f2_mejora_vida = if_else(!is.na(f2_mejora_vida) & !(f2_consent == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1))), 1, 0),
    s_f2_uso_prestamo = if_else(!is.na(f2_uso_prestamo) & !(f2_consent == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1))), 1, 0),
    s_f2_uso_prestamo_otro = if_else(!is.na(f2_uso_prestamo_otro) & !(f2_consent == 1 & f2_uso_prestamo_66 == 1), 1, 0),
    s_f2_barreras = if_else(!is.na(f2_barreras) & !(f2_consent == 1 & f2_productos_12 == 1), 1, 0),
    s_e5_barreras_otro = if_else(!is.na(e5_barreras_otro) & !(f2_consent == 1 & f2_barreras_66 == 1), 1, 0),
    s_f2_inversion = if_else(!is.na(f2_inversion) & !(f2_consent == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1))), 1, 0),
    s_f2_acceso = if_else(!is.na(f2_acceso) & !(f2_consent == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1))), 1, 0),
    s_f2_facilidad = if_else(!is.na(f2_facilidad) & !(f2_consent == 1 & (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)), 1, 0),
    s_f2_solicitante = if_else(!is.na(f2_solicitante) & !(f2_consent == 1 & (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)), 1, 0),
    s_f2_independencia = if_else(!is.na(f2_independencia) & !(f2_consent == 1 & (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)), 1, 0),
    s_f2_recomendacion = if_else(!is.na(f2_recomendacion) & !(f2_consent == 1 & ((f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1) & a1_genero == 2)), 1, 0),
    s_f2_prestamo_plan = if_else(!is.na(f2_prestamo_plan) & !(f2_consent == 1 & ((f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)) & (f2_negocio_continua == 1 | f2_negocio == 1))), 1, 0),
    s_f2_prestamo_uso = if_else(!is.na(f2_prestamo_uso) & !(f2_consent == 1 & ((f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)) & (f2_negocio_continua == 1 | f2_negocio == 1))), 1, 0),
    s_f2_mejora_inventario = if_else(!is.na(f2_mejora_inventario) & !(f2_consent == 1 & ((f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)) & (f2_negocio_continua == 1 | f2_negocio == 1))), 1, 0),
    s_f2_mejora_activofijo = if_else(!is.na(f2_mejora_activofijo) & !(f2_consent == 1 & ((f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)) & (f2_negocio_continua == 1 | f2_negocio == 1))), 1, 0),
    s_f2_mejora_flujo = if_else(!is.na(f2_mejora_flujo) & !(f2_consent == 1 & ((f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)) & (f2_negocio_continua == 1 | f2_negocio == 1))), 1, 0),
    s_f2_prestamo_ganancia = if_else(!is.na(f2_prestamo_ganancia) & !(f2_consent == 1 & ((f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)) & (f2_negocio_continua == 1 | f2_negocio == 1))), 1, 0),
    s_f2_interes = if_else(!is.na(f2_interes) & !(f2_consent == 1), 1, 0),
    s_f2_usura = if_else(!is.na(f2_usura) & !(f2_consent == 1), 1, 0),
    s_f2_inflacion1 = if_else(!is.na(f2_inflacion1) & !(f2_consent == 1), 1, 0),
    s_f2_inflacion2 = if_else(!is.na(f2_inflacion2) & !(f2_consent == 1), 1, 0),
    s_f2_porc_edu = if_else(!is.na(f2_porc_edu) & !(f2_consent == 1), 1, 0),
    s_f2_edu_quien = if_else(!is.na(f2_edu_quien) & !(f2_consent == 1 & f2_porc_edu > 0), 1, 0),
    s_f2_edu_que = if_else(!is.na(f2_edu_que) & !(f2_consent == 1 & f2_porc_edu > 0), 1, 0),
    s_f2_menores_edu = if_else(!is.na(f2_menores_edu) & !(f2_consent == 1 & f2_menores > 0), 1, 0),
    s_f2_menores_edu_mot = if_else(!is.na(f2_menores_edu_mot) & !(f2_consent == 1 & f2_menores_edu == 2), 1, 0),
    s_f2_comidas = if_else(!is.na(f2_comidas) & !(f2_consent == 1), 1, 0),
    s_f2_hog_dificultad = if_else(!is.na(f2_hog_dificultad) & !(f2_consent == 1), 1, 0),
    s_f2_salud = if_else(!is.na(f2_salud) & !(f2_consent == 1), 1, 0),
    s_f2_atencion_med = if_else(!is.na(f2_atencion_med) & !(f2_consent == 1), 1, 0),
    s_f2_gasto_salud = if_else(!is.na(f2_gasto_salud) & !(f2_consent == 1), 1, 0),
    s_f2_sit_eco = if_else(!is.na(f2_sit_eco) & !(f2_consent == 1), 1, 0),
    s_f2_mejora = if_else(!is.na(f2_mejora) & !(f2_consent == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1))), 1, 0),
    s_f2_mejora_aspectos = if_else(!is.na(f2_mejora_aspectos) & !(f2_consent == 1 & ((f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)) & (f2_mejora == 1 | f2_mejora == 2))), 1, 0))

## Sumar total de saltos irregulares
variables_salto <- names(alertas %>%
                           select(matches("^s_")))
alertas <- alertas %>%
  mutate(
    total_saltos = rowSums(alertas[, variables_salto], na.rm = T),
    flag_skips   = if_else(total_saltos > 0, 1, 0)
  )

#### Validación de preguntas obligatorias (missings) #####
alertas <- alertas %>%
  mutate(
    m_f2_call_ans = if_else(is.na(f2_call_ans), 1, 0),
    m_f2_message = if_else(is.na(f2_message), 1, 0),
    m_f2_status_1 = if_else(is.na(f2_status_1) & f2_call_ans == 2, 1, 0),
    m_f2_contact_phone = if_else(is.na(f2_contact_phone) & f2_status_1 == 6, 1, 0),
    m_f2_reagendar_hora = if_else(is.na(f2_reagendar_hora) & f2_status_1 == 4, 1, 0),
    m_f2_consent = if_else(is.na(f2_consent) & f2_call_ans == 1, 1, 0),
    m_f2_cel_yn = if_else(is.na(f2_cel_yn) & f2_consent == 1, 1, 0),
    m_f2_cel_new = if_else(is.na(f2_cel_new) & f2_consent == 1 & f2_cel_yn == 2, 1, 0),
    m_f2_cel2 = if_else(is.na(f2_cel2) & f2_consent == 1, 1, 0),
    m_f2_cel2_person = if_else(is.na(f2_cel2_person) & f2_consent == 1 & f2_cel2 != "NA", 1, 0),
    m_f2_cel2_person_otro = if_else(is.na(f2_cel2_person_otro) & f2_consent == 1 & f2_cel2_person == 66, 1, 0),
    m_f2_correo = if_else(is.na(f2_correo) & f2_consent == 1 & (is.na(contact_correo) | contact_correo == "NA"), 1, 0),
    m_f2_correo_yn = if_else(is.na(f2_correo_yn) & f2_consent == 1 & (!is.na(contact_correo) & contact_correo != "NA"), 1, 0),
    m_f2_correo_new = if_else(is.na(f2_correo_new) & f2_consent == 1 & f2_correo_yn == 2, 1, 0),
    m_f2_canal_pref = if_else(is.na(f2_canal_pref) & f2_consent == 1, 1, 0),
    m_f2_edad = if_else(is.na(f2_edad) & f2_consent == 1, 1, 0),
    m_f2_estcivil = if_else(is.na(f2_estcivil) & f2_consent == 1, 1, 0),
    m_f2_pais = if_else(is.na(f2_pais) & f2_consent == 1, 1, 0),
    m_f2_pais_new = if_else(is.na(f2_pais_new) & f2_consent == 1 & f2_pais == 2, 1, 0),
    m_f2_dpto = if_else(is.na(f2_dpto) & f2_consent == 1 & f2_pais == 1, 1, 0),
    m_f2_dpto_now = if_else(is.na(f2_dpto_now) & f2_consent == 1 & (f2_dpto == 2 & f2_pais == 1), 1, 0),
    m_f2_ciudad = if_else(is.na(f2_ciudad) & f2_consent == 1 & (f2_dpto == 1 & f2_pais == 1), 1, 0),
    m_f2_ciudad_now = if_else(is.na(f2_ciudad_now) & f2_consent == 1 & (f2_pais == 1 & (f2_dpto == 2 | f2_ciudad == 2)), 1, 0),
    m_f2_credito = if_else(is.na(f2_credito) & f2_consent == 1, 1, 0),
    m_f2_credito_control = if_else(is.na(f2_credito_control) & f2_consent == 1 & (f2_grupo == 2 & f2_credito == 1), 1, 0),
    m_f2_resp_financiero = if_else(is.na(f2_resp_financiero) & f2_consent == 1 & a8_jefe == 1, 1, 0),
    m_f2_id = if_else(is.na(f2_id) & f2_consent == 1, 1, 0),
    m_f2_permanencia = if_else(is.na(f2_permanencia) & f2_consent == 1, 1, 0),
    m_f2_educacion = if_else(is.na(f2_educacion) & f2_consent == 1, 1, 0),
    m_f2_member = if_else(is.na(f2_member) & f2_consent == 1, 1, 0),
    m_f2_residentes = if_else(is.na(f2_residentes) & f2_consent == 1 & f2_pais == 1, 1, 0),
    m_f2_hacinamiento = if_else(is.na(f2_hacinamiento) & f2_consent == 1, 1, 0),
    m_f2_mujeres = if_else(is.na(f2_mujeres) & f2_consent == 1, 1, 0),
    m_f2_menores = if_else(is.na(f2_menores) & f2_consent == 1, 1, 0),
    m_f2_trabajadores = if_else(is.na(f2_trabajadores) & f2_consent == 1, 1, 0),
    m_f2_ingresos = if_else(is.na(f2_ingresos) & f2_consent == 1, 1, 0),
    m_f2_monto = if_else(is.na(f2_monto) & f2_consent == 1, 1, 0),
    m_f2_monto_check = if_else(is.na(f2_monto_check) & f2_consent == 1, 1, 0),
    m_f2_resp_relacion = if_else(is.na(f2_resp_relacion) & f2_consent == 1 & f2_resp_financiero == 2, 1, 0),
    m_f2_resp_relacion_otro = if_else(is.na(f2_resp_relacion_otro) & f2_consent == 1 & f2_resp_relacion == 66, 1, 0),
    m_f2_resp_genero = if_else(is.na(f2_resp_genero) & f2_consent == 1 & f2_resp_financiero == 2, 1, 0),
    m_f2_resp_edad = if_else(is.na(f2_resp_edad) & f2_consent == 1 & f2_resp_financiero == 2, 1, 0),
    m_f2_resp_educ = if_else(is.na(f2_resp_educ) & f2_consent == 1 & f2_resp_financiero == 2, 1, 0),
    m_f2_ocup = if_else(is.na(f2_ocup) & f2_consent == 1, 1, 0),
    m_f2_ocup_otro = if_else(is.na(f2_ocup_otro) & f2_consent == 1 & f2_ocup == 66, 1, 0),
    m_f2_negocio_continua = if_else(is.na(f2_negocio_continua) & f2_consent == 1 & b2_negocio == 1, 1, 0),
    m_f2_negocio = if_else(is.na(f2_negocio) & f2_consent == 1 & ((f2_ocup == 1 | f2_ocup == 2) & b2_negocio != 1), 1, 0),
    m_f2_negocio_close = if_else(is.na(f2_negocio_close) & f2_consent == 1 & f2_negocio_continua == 2, 1, 0),
    m_f2_negocio_deuda = if_else(is.na(f2_negocio_deuda) & f2_consent == 1 & (f2_negocio_close_2 == 1 & (f2_grupo == 1 | f2_credito_control == 1)), 1, 0),
    m_f2_negocio_deuda_plus = if_else(is.na(f2_negocio_deuda_plus) & f2_consent == 1 & f2_negocio_close_2 == 1, 1, 0),
    m_f2_negocio_close_otro = if_else(is.na(f2_negocio_close_otro) & f2_consent == 1 & f2_negocio_close_66 == 1, 1, 0),
    m_f2_ocup_tiempo = if_else(is.na(f2_ocup_tiempo) & f2_consent == 1 & f2_ocup < 8, 1, 0),
    m_f2_ocup_tiempo_meses = if_else(is.na(f2_ocup_tiempo_meses) & f2_consent == 1 & f2_ocup_tiempo == 1, 1, 0),
    m_f2_ocup_tiempo_anios = if_else(is.na(f2_ocup_tiempo_anios) & f2_consent == 1 & f2_ocup_tiempo == 2, 1, 0),
    m_f2_negocio_gasto = if_else(is.na(f2_negocio_gasto) & f2_consent == 1 & (f2_negocio_continua == 1 | f2_negocio == 1), 1, 0),
    m_f2_negocio_formal = if_else(is.na(f2_negocio_formal) & f2_consent == 1 & (f2_negocio_continua == 1 | f2_negocio == 1), 1, 0),
    m_f2_negocio_apoyo_yn = if_else(is.na(f2_negocio_apoyo_yn) & f2_consent == 1 & (f2_negocio_formal == 1 & (f2_grupo == 1 | f2_credito_control == 1)), 1, 0),
    m_f2_negocio_apoyo = if_else(is.na(f2_negocio_apoyo) & f2_consent == 1 & (f2_negocio_apoyo_yn == 1 & (f2_grupo == 1 | f2_credito_control == 1)), 1, 0),
    m_f2_negocio_apoyo_otro = if_else(is.na(f2_negocio_apoyo_otro) & f2_consent == 1 & f2_negocio_apoyo_66 == 1, 1, 0),
    m_f2_negocio_lugar = if_else(is.na(f2_negocio_lugar) & f2_consent == 1 & (f2_negocio_continua == 1 | f2_negocio == 1), 1, 0),
    m_f2_negocio_empleados = if_else(is.na(f2_negocio_empleados) & f2_consent == 1 & (f2_negocio_continua == 1 | f2_negocio == 1), 1, 0),
    m_f2_ocup_sector = if_else(is.na(f2_ocup_sector) & f2_consent == 1 & f2_ocup < 7, 1, 0),
    m_f2_ocup_sector_otro = if_else(is.na(f2_ocup_sector_otro) & f2_consent == 1 & f2_ocup_sector == 66, 1, 0),
    m_f2_ocup_contrato = if_else(is.na(f2_ocup_contrato) & f2_consent == 1 & f2_ocup < 7, 1, 0),
    m_f2_ocup_contrato_tipo = if_else(is.na(f2_ocup_contrato_tipo) & f2_consent == 1 & (f2_ocup < 7 & f2_ocup_contrato == 1), 1, 0),
    m_f2_ocup_contrato_tipo_otro = if_else(is.na(f2_ocup_contrato_tipo_otro) & f2_consent == 1 & f2_ocup_contrato_tipo == 66, 1, 0),
    m_f2_ocup_estabilidad = if_else(is.na(f2_ocup_estabilidad) & f2_consent == 1 & f2_ocup < 7, 1, 0),
    m_f2_ocup_ingresos = if_else(is.na(f2_ocup_ingresos) & f2_consent == 1 & f2_ocup < 7, 1, 0),
    m_f2_ocup_ingresos_monto = if_else(is.na(f2_ocup_ingresos_monto) & f2_consent == 1 & f2_ocup < 7, 1, 0),
    m_f2_ocup_ingresos_monto_check = if_else(is.na(f2_ocup_ingresos_monto_check) & f2_consent == 1 & f2_ocup < 7, 1, 0),
    m_f2_ocup_ingresos_freq = if_else(is.na(f2_ocup_ingresos_freq) & f2_consent == 1 & f2_ocup < 7, 1, 0),
    m_f2_ocup_ingresos_medio = if_else(is.na(f2_ocup_ingresos_medio) & f2_consent == 1 & f2_ocup < 7, 1, 0),
    m_b7_ingresos_medio_otro = if_else(is.na(b7_ingresos_medio_otro) & f2_consent == 1 & f2_ocup_ingresos_medio == 66, 1, 0),
    m_f2_planeacion = if_else(is.na(f2_planeacion) & f2_consent == 1, 1, 0),
    m_f2_planeacion_tipo = if_else(is.na(f2_planeacion_tipo) & f2_consent == 1 & f2_planeacion == 1, 1, 0),
    m_f2_planeacion_tipo_otro = if_else(is.na(f2_planeacion_tipo_otro) & f2_consent == 1 & f2_planeacion_tipo_66 == 1, 1, 0),
    m_f2_ahorro = if_else(is.na(f2_ahorro) & f2_consent == 1, 1, 0),
    m_f2_ahorro_medio = if_else(is.na(f2_ahorro_medio) & f2_consent == 1 & f2_ahorro == 1, 1, 0),
    m_f2_ahorro_medio_otro = if_else(is.na(f2_ahorro_medio_otro) & f2_consent == 1 & f2_ahorro_medio_66 == 1, 1, 0),
    m_f2_ahorro_proposito = if_else(is.na(f2_ahorro_proposito) & f2_consent == 1 & f2_ahorro == 1, 1, 0),
    m_f2_ahorro_status = if_else(is.na(f2_ahorro_status) & f2_consent == 1 & c2_ahorro == 1, 1, 0),
    m_f2_ahorro_status_worse = if_else(is.na(f2_ahorro_status_worse) & f2_consent == 1 & f2_ahorro_status == 2, 1, 0),
    m_f2_ahorro_status_worse_otro = if_else(is.na(f2_ahorro_status_worse_otro) & f2_consent == 1 & f2_ahorro_status_worse == 66, 1, 0),
    m_f2_razon_noahorra = if_else(is.na(f2_razon_noahorra) & f2_consent == 1 & f2_ahorro == 0, 1, 0),
    m_f2_monto_emerg = if_else(is.na(f2_monto_emerg) & f2_consent == 1, 1, 0),
    m_f2_prestamo_persona = if_else(is.na(f2_prestamo_persona) & f2_consent == 1, 1, 0),
    m_f2_gastos = if_else(is.na(f2_gastos) & f2_consent == 1, 1, 0),
    m_f2_cuentas = if_else(is.na(f2_cuentas) & f2_consent == 1, 1, 0),
    m_f2_deudas = if_else(is.na(f2_deudas) & f2_consent == 1, 1, 0),
    m_f2_deudas_num = if_else(is.na(f2_deudas_num) & f2_consent == 1 & f2_deudas != 1, 1, 0),
    m_f2_deudas_pago = if_else(is.na(f2_deudas_pago) & f2_consent == 1 & f2_deudas != 1, 1, 0),
    m_f2_deudas_carga = if_else(is.na(f2_deudas_carga) & f2_consent == 1 & f2_deudas != 1, 1, 0),
    m_f2_deudas_situacion = if_else(is.na(f2_deudas_situacion) & f2_consent == 1 & f2_deudas != 1, 1, 0),
    m_f2_productos = if_else(is.na(f2_productos) & f2_consent == 1, 1, 0),
    m_f2_productos_crezc = if_else(is.na(f2_productos_crezc) & f2_consent == 1 & f2_grupo == 1, 1, 0),
    m_f2_tasa = if_else(is.na(f2_tasa) & f2_consent == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)), 1, 0),
    m_f2_apoyo = if_else(is.na(f2_apoyo) & f2_consent == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)), 1, 0),
    m_f2_apoyo_crezc = if_else(is.na(f2_apoyo_crezc) & f2_consent == 1 & (f2_apoyo == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1))), 1, 0),
    m_f2_mejora_vida = if_else(is.na(f2_mejora_vida) & f2_consent == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)), 1, 0),
    m_f2_uso_prestamo = if_else(is.na(f2_uso_prestamo) & f2_consent == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)), 1, 0),
    m_f2_uso_prestamo_otro = if_else(is.na(f2_uso_prestamo_otro) & f2_consent == 1 & f2_uso_prestamo_66 == 1, 1, 0),
    m_f2_barreras = if_else(is.na(f2_barreras) & f2_consent == 1 & f2_productos_12 == 1, 1, 0),
    m_e5_barreras_otro = if_else(is.na(e5_barreras_otro) & f2_consent == 1 & f2_barreras_66 == 1, 1, 0),
    m_f2_inversion = if_else(is.na(f2_inversion) & f2_consent == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)), 1, 0),
    m_f2_acceso = if_else(is.na(f2_acceso) & f2_consent == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)), 1, 0),
    m_f2_facilidad = if_else(is.na(f2_facilidad) & f2_consent == 1 & (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1), 1, 0),
    m_f2_solicitante = if_else(is.na(f2_solicitante) & f2_consent == 1 & (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1), 1, 0),
    m_f2_independencia = if_else(is.na(f2_independencia) & f2_consent == 1 & (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1), 1, 0),
    m_f2_recomendacion = if_else(is.na(f2_recomendacion) & f2_consent == 1 & ((f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1) & a1_genero == 2), 1, 0),
    m_f2_prestamo_plan = if_else(is.na(f2_prestamo_plan) & f2_consent == 1 & ((f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)) & (f2_negocio_continua == 1 | f2_negocio == 1)), 1, 0),
    m_f2_prestamo_uso = if_else(is.na(f2_prestamo_uso) & f2_consent == 1 & ((f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)) & (f2_negocio_continua == 1 | f2_negocio == 1)), 1, 0),
    m_f2_mejora_inventario = if_else(is.na(f2_mejora_inventario) & f2_consent == 1 & ((f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)) & (f2_negocio_continua == 1 | f2_negocio == 1)), 1, 0),
    m_f2_mejora_activofijo = if_else(is.na(f2_mejora_activofijo) & f2_consent == 1 & ((f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)) & (f2_negocio_continua == 1 | f2_negocio == 1)), 1, 0),
    m_f2_mejora_flujo = if_else(is.na(f2_mejora_flujo) & f2_consent == 1 & ((f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)) & (f2_negocio_continua == 1 | f2_negocio == 1)), 1, 0),
    m_f2_prestamo_ganancia = if_else(is.na(f2_prestamo_ganancia) & f2_consent == 1 & ((f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)) & (f2_negocio_continua == 1 | f2_negocio == 1)), 1, 0),
    m_f2_interes = if_else(is.na(f2_interes) & f2_consent == 1, 1, 0),
    m_f2_usura = if_else(is.na(f2_usura) & f2_consent == 1, 1, 0),
    m_f2_inflacion1 = if_else(is.na(f2_inflacion1) & f2_consent == 1, 1, 0),
    m_f2_inflacion2 = if_else(is.na(f2_inflacion2) & f2_consent == 1, 1, 0),
    m_f2_porc_edu = if_else(is.na(f2_porc_edu) & f2_consent == 1, 1, 0),
    m_f2_edu_quien = if_else(is.na(f2_edu_quien) & f2_consent == 1 & f2_porc_edu > 0, 1, 0),
    m_f2_edu_que = if_else(is.na(f2_edu_que) & f2_consent == 1 & f2_porc_edu > 0, 1, 0),
    m_f2_menores_edu = if_else(is.na(f2_menores_edu) & f2_consent == 1 & f2_menores > 0, 1, 0),
    m_f2_menores_edu_mot = if_else(is.na(f2_menores_edu_mot) & f2_consent == 1 & f2_menores_edu == 2, 1, 0),
    m_f2_comidas = if_else(is.na(f2_comidas) & f2_consent == 1, 1, 0),
    m_f2_hog_dificultad = if_else(is.na(f2_hog_dificultad) & f2_consent == 1, 1, 0),
    m_f2_salud = if_else(is.na(f2_salud) & f2_consent == 1, 1, 0),
    m_f2_atencion_med = if_else(is.na(f2_atencion_med) & f2_consent == 1, 1, 0),
    m_f2_gasto_salud = if_else(is.na(f2_gasto_salud) & f2_consent == 1, 1, 0),
    m_f2_sit_eco = if_else(is.na(f2_sit_eco) & f2_consent == 1, 1, 0),
    m_f2_mejora = if_else(is.na(f2_mejora) & f2_consent == 1 & (f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)), 1, 0),
    m_f2_mejora_aspectos = if_else(is.na(f2_mejora_aspectos) & f2_consent == 1 & ((f2_grupo == 1 | (f2_grupo == 2 & f2_credito == 1 & f2_credito_control == 1)) & (f2_mejora == 1 | f2_mejora == 2)), 1, 0))

## Sumar total missings
variables_missing <- names(alertas %>%
                             select(matches("^m_")))
alertas <- alertas %>%
  mutate(
    total_missing = rowSums(alertas[, variables_missing], na.rm = T),
    flag_missing  = if_else(total_missing > 0, 1, 0)
  )

table(alertas$flag_missing)
table(alertas$flag_skips)


# Alertas de duplicados --------------------------------------




# Alertas de case id vacío --------------------------------------

alertas <- alertas %>%
  mutate(flag_case_id = if_else(is.na(caseid),1,0))


# Alerta de valores numéricos

alertas <- alertas %>%
  mutate(across(c(f2_monto, f2_ocup_ingresos_monto), as.numeric))

q1_monto  <- quantile(alertas$f2_monto, 0.25, na.rm = TRUE)
q3_monto  <- quantile(alertas$f2_monto, 0.75, na.rm = TRUE)
iqr_monto <- IQR(alertas$f2_monto, na.rm = TRUE)

lim_inf_monto <- q1_monto - 1.5 * iqr_monto
lim_sup_monto <- q3_monto + 1.5 * iqr_monto

alertas <- alertas %>%
  mutate(
    ex_f2_monto = if_else(
      f2_monto < lim_inf_monto | f2_monto > lim_sup_monto,
      1,
      0,
      missing = 0
    )
  )

q1_ocup_ingresos_monto  <- quantile(alertas$f2_ocup_ingresos_monto, 0.25, na.rm = TRUE)
q3_ocup_ingresos_monto  <- quantile(alertas$f2_ocup_ingresos_monto, 0.75, na.rm = TRUE)
iqr_ocup_ingresos_monto <- IQR(alertas$f2_ocup_ingresos_monto, na.rm = TRUE)

lim_inf_ocup_ingresos_monto <- q1_ocup_ingresos_monto - 1.5 * iqr_ocup_ingresos_monto
lim_sup_ocup_ingresos_monto <- q3_ocup_ingresos_monto + 1.5 * iqr_ocup_ingresos_monto

alertas <- alertas %>%
  mutate(
    ex_f2_ocup_ingresos_monto = if_else(
      f2_ocup_ingresos_monto < lim_inf_ocup_ingresos_monto | 
        f2_ocup_ingresos_monto > lim_sup_ocup_ingresos_monto,
      1,
      0,
      missing = 0
    )
  )


alertas <- alertas %>%
  mutate(flag_extremes = if_else(ex_f2_monto == 1 | ex_f2_ocup_ingresos_monto == 1,1,0,missing = 0))


# Consolidar alertas -----------------------------------------------------------


alertas <- alertas %>%
  mutate(total_encuestas = n(),
         Exitos = if_else(flag_duracion== 0 & flag_missing == 0 & flag_skips == 0 &  
                            flag_extremes == 0 & flag_case_id == 0 & part_valido == 1,1,0),
         Alertas = if_else(flag_duracion == 1 | flag_missing == 1 | flag_skips == 1 |   
                             flag_extremes == 1 | flag_case_id == 1,1,0),
         Rechazo = if_else(f2_consent == 2 | f2_status_1 == 5,1,0,missing = NA),
         no_contesta = if_else(f2_status_1 == 1,1,0,missing = NA),
         num_equiv = if_else(f2_status_1 == 2,1,0,missing = NA),
         num_fuera = if_else(f2_status_1 == 3,1,0,missing = NA),
         reagendam = if_else(f2_status_1 == 4,1,0,missing = NA),
         otro_numeroco = if_else(f2_status_1 == 6,1,0,missing = NA)
  )



