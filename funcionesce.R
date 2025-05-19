#====================================#
# Descargar bases y libro de códigos #
#====================================#

descargar_bases <- function(año_seleccionado) {
  
  # Crear carpeta si no existe #
  #============================#
  
  carpeta_destino <- "01 Bases"
  if (!dir.exists(carpeta_destino)) dir.create(carpeta_destino, recursive = TRUE)
  
  # Descargar libro de códigos #
  #============================#
  
  # URL del archivo
  url <- "https://raw.githubusercontent.com/Manumarc/Indicadores-censo-escolar/main/Libro_de_codigos.xlsx"
  
  # Ruta de destino
  destino <- "01 Bases/Libro_de_codigos.xlsx"
  
  # Descargar el archivo
  download.file(url, destfile = destino, mode = "wb")
  
  # Descargar bases de datos #
  #==========================#
  
  # Listas de URLs y nombres
  nom_url24 <- c(
    "https://escale.minedu.gob.pe/documents/10156/6ab386c8-4b5c-4cd1-b3c1-3b44e0137085", # Padron 
    "https://escale.minedu.gob.pe/documents/10156/ac666dee-3231-452e-a655-57ec95c4e894", # Loc_lineal_2
    "https://escale.minedu.gob.pe/documents/10156/bf381a61-2f33-49c7-ae16-d4671c1d77da", # Loc_P108_SaFiLe
    "https://escale.minedu.gob.pe/documents/10156/d9ba6b10-c116-4573-9d24-e2d60ac6f069", # Loc_P200_ServBa
    "https://escale.minedu.gob.pe/documents/10156/87f92989-ef23-46a5-b850-7e1cfb0bd7a2", # Loc_P501_Aulas
    "https://escale.minedu.gob.pe/documents/10156/7933ed8e-c751-41d0-958a-ac0a215a38c6", # Loc_P601_OtEsp
    "https://escale.minedu.gob.pe/documents/10156/641cdd5a-9cbd-4f1c-a9cf-91ff7a5bc557"  # Loc_P701_SSHH
  )
  nombres24 <- c("Padron_24", "Loc_lineal_2_24", "Loc_P108_SaFiLe_24", "Loc_P200_ServBa_24",
                 "Loc_P501_Aulas_24", "Loc_P601_OtEsp_24", "Loc_P701_SSHH_24")
  
  nom_url23 <- c(
    "https://escale.minedu.gob.pe/documents/10156/feb2b10b-9c8d-4686-8342-345bc2abb87d", # Padron 
    "https://escale.minedu.gob.pe/documents/10156/a0bd77de-fdcc-4d33-a71b-712a4728cebb", # Loc_lineal_2
    "https://escale.minedu.gob.pe/documents/10156/43ee710b-9503-40d8-acdd-06c043e1b8ac", # Loc_P108_SaFiLe
    "https://escale.minedu.gob.pe/documents/10156/9940b8c2-b00b-4454-bfd0-ed5ef3ab5a45", # Loc_P200_ServBa
    "https://escale.minedu.gob.pe/documents/10156/2a5071e9-218b-4b24-870c-c51ce81a5563", # Loc_P501_Aulas
    "https://escale.minedu.gob.pe/documents/10156/fcaaff07-6a8e-486b-bc3d-5cb6e217c0d8", # Loc_P601_OtEsp
    "https://escale.minedu.gob.pe/documents/10156/1e1df25d-c815-47d4-bd4b-cf1d67c4ae4e"  # Loc_P701_SSHH
  )
  
  nombres23 <- c("Padron_23", "Loc_lineal_2_23", "Loc_P108_SaFiLe_23", "Loc_P200_ServBa_23",
                 "Loc_P501_Aulas_23", "Loc_P601_OtEsp_23", "Loc_P701_SSHH_23")
  
  # Función para descargar, descomprimir y renombrar archivos #
  #===========================================================#
  
  descargar_y_procesar <- function(urls, nombres) {
    
    for (i in seq_along(urls)) {
      
      url <- urls[i]
      nombre_logico <- nombres[i]
      nombre_zip <- paste0(nombre_logico, ".zip")
      ruta_zip <- file.path(carpeta_destino, nombre_zip)
      
      # Descargar zip
      
      download.file(url, destfile = ruta_zip, mode = "wb")
      message("Descargado: ", nombre_zip)
      
      # Descomprimir y renombrar
      
      temp_dir <- tempdir()
      archivos_extraidos <- unzip(ruta_zip, exdir = temp_dir)
      
      for (archivo in archivos_extraidos) {
        
        ext <- tools::file_ext(archivo)
        nuevo_nombre <- paste0(nombre_logico, ".", ext)
        file.copy(from = archivo, to = file.path(carpeta_destino, nuevo_nombre), overwrite = TRUE)
        message("Extraído y renombrado: ", nuevo_nombre)
        
      }
    }
  }
  
  # Ejecutar según año seleccionado #
  #=================================#
  
  if ("2024" %in% año_seleccionado) {
    
    message("Procesando archivos 2024...")
    descargar_y_procesar(nom_url24, nombres24)
    
  }
  
  if ("2023" %in% año_seleccionado) {
    
    message("Procesando archivos 2023...")
    descargar_y_procesar(nom_url23, nombres23)
    
  }
  
  message("Todos los archivos seleccionados han sido descargados y procesados.")
}

#=========================================================================#
# Función para el cálculo de indicadores de infraestructura de la escuela #
#=========================================================================#

cal_indicador <- function(nom_indicador,nom_año){
  
  # Libro de códigos #
  #==================#
  
  libcod <- read.xlsx("01 Bases/Libro_de_codigos.xlsx", sheet = 3)
  
  # Selección de grupo de indicadores a calcular #
  #==============================================#
  
  # Filtro de datos 
  
  base <- libcod %>% 
    filter(indicador %in% nom_indicador) %>% 
    filter(año %in% nom_año)
  
  # Nombre de la base de datos a llamar 
  
  nombre_base <- base %>% 
    select(nom_base) %>% 
    pull() %>% 
    unique()
  
  # Nombre de la variable 
  
  nombre_preg <- base %>% 
    select(nom_pregunta) %>% 
    pull() %>% 
    unique()
  
  # Cambio de nombre de variables #
  #===============================#
  
  # Nombre del cuadro 
  
  cuadro <- base %>% 
    select(CUADRO) %>% 
    pull() %>% 
    unique()
  
  uniq_cuadro <- base %>% 
    select(nom_cuadro) %>% 
    pull() %>% 
    unique()
  
  # Nombre de las preguntas 
  
  preguntas <- base %>% 
    select(codigo_preg, nom_pregunta) %>% 
    mutate_at(vars(codigo_preg,nom_pregunta),
              ~str_trim(., "right"))
  
  nombres_a_renombrar <- preguntas$nom_pregunta
  names(nombres_a_renombrar) <- preguntas$codigo_preg
  
  # Llamado de base de datos con nombres uniformizados #
  #====================================================#
  
  bd_1 <- read.dbf(paste0("01 Bases/",nombre_base), as.is = TRUE) %>%
    { 
      bd_1 <- .
      if (all(c("CUADRO","NROCED") %in% names(bd_1))) {
        
        bd_1 <- bd_1 %>%
          mutate(CUADRO = case_when(CUADRO %in% cuadro ~ uniq_cuadro,
                                    TRUE ~ CUADRO)) %>%
          filter(NROCED %in% "11") %>%
          filter(CUADRO %in% uniq_cuadro)
      }
      bd_1
    } %>%
    rename_with(~ nombres_a_renombrar[.x],
                .cols = names(nombres_a_renombrar)[names(nombres_a_renombrar) %in% names(.)])
  
  if (nom_indicador %in% "Saneamiento"){
    
    # Construcción de base de datos final #
    #=====================================#
    
    # Rango de total de terrenos que tiene el local escolar 
    
    rango <- bd_1 %>% 
      select(ID_TERR) %>% 
      pull() %>% 
      unique()
    
    #Base de datos con el indicador
    
    bd_final <- bd_1 %>% 
      select(CODLOCAL,ID_TERR,!!sym(nombre_preg)) %>% 
      pivot_wider(names_from = ID_TERR,
                  values_from = !!sym(nombre_preg)) %>% 
      rowwise() %>%
      mutate(no_na = sum(!is.na(c_across(first(rango):last(rango)))),
             tot_1 = sum(c_across(first(rango):last(rango)) == "1", na.rm = TRUE)) %>%
      ungroup() %>% 
      filter(!no_na %in% 0) %>% 
      mutate(avance = redondear((tot_1/no_na)*100,1)) %>% 
      mutate(saneamiento = case_when(avance %in% 100 ~ "Saneado",
                                     avance %in% 0 ~ "No saneado",
                                     TRUE ~ "Saneado parcial")) %>% 
      select(CODLOCAL,!!paste0("saneamiento",nom_año) := saneamiento)
    
    return(bd_final)
    
  } else if(nom_indicador %in% c("Agua","Desagüe","Luz")){
    
    # Construcción de base de datos final #
    #=====================================#
    
    claves <- base %>%
      select(codigo_resp) %>%
      str_split("; ") %>% 
      as.data.frame() %>% 
      pull()
    
    serbas_1 <- bd_1 %>% 
      mutate(conexion = case_when(!!sym(nombre_preg) %in% claves[[1]] ~ "Conexión red pública",
                                  TRUE ~ "Sin conexión red pública")) %>%
      select(CODLOCAL, conexion)
    
    # Rango de variables duplicadas #
    #-------------------------------#
    
    rango <- serbas_1 %>% 
      group_by(CODLOCAL) %>%
      mutate(duplicado = row_number()) %>%
      ungroup() %>% 
      select(duplicado) %>% 
      unique() %>% 
      pull()
    
    # Cálculo de base por código de local único #
    #-------------------------------------------#
    
    if (length(rango) == 1) {
      
      serbas_2 <- serbas_1 %>% 
        group_by(CODLOCAL) %>%
        mutate(duplicado = row_number()) %>%
        ungroup() %>% 
        pivot_wider(names_from = duplicado,
                    values_from = conexion) %>% 
        mutate(tot_rp = case_when(
          !!sym(as.character(rango)) == "Conexión red pública" ~ 1,
          TRUE ~ 0
        )) %>% 
        mutate(serbas = case_when(
          tot_rp == 0 ~ "Sin conexión red pública",
          TRUE ~ "Conexión red pública"
        )) %>% 
        select(CODLOCAL, !!paste0(nombre_preg,nom_año) := serbas)
      
    } else {
      
      serbas_2 <- serbas_1 %>% 
        group_by(CODLOCAL) %>%
        mutate(duplicado = row_number()) %>%
        ungroup() %>% 
        pivot_wider(names_from = duplicado,
                    values_from = conexion) %>% 
        rowwise() %>%
        mutate(tot_rp = sum(
          c_across(all_of(as.character(rango))) == "Conexión red pública",
          na.rm = TRUE
        )) %>%
        ungroup() %>% 
        mutate(serbas = case_when(
          tot_rp == 0 ~ "Sin conexión red pública",
          TRUE ~ "Conexión red pública"
        )) %>% 
        select(CODLOCAL, !!paste0(nombre_preg,nom_año) := serbas)
    }
    
    return(serbas_2)
    
  } else if (nom_indicador %in% "Infraestructura aulas"){
    
    # Construcción de base de datos final #
    #=====================================#
    
    var_be <- c(nombre_preg[[4]],nombre_preg[[8]],nombre_preg[[9]],nombre_preg[[10]],"ventanabe")
    var_tot <- c(nombre_preg[[3]],nombre_preg[[5]])
    
    infraula_1 <- bd_1 %>% 
      filter(aula_filtro_1 %in% "1") %>% 
      mutate(ventanabe = case_when(marcovenbe %in% c("01") & vidriobe %in% c("01") ~ "01",
                                   is.na(marcovenbe) | is.na(vidriobe) ~ NA_character_,
                                   TRUE ~ "00")) %>% 
      mutate(across(
        all_of(var_be),
        ~ case_when(.x == "01" ~ 1, TRUE ~ 0),
        .names = "buen_estado_{.col}"
      )) %>% 
      select(CODLOCAL,var_be,var_tot) %>% 
      group_by(CODLOCAL) %>% 
      summarise(
        tot1 = n(),
        across(
          all_of(var_tot),
          ~sum(., na.rm = T),
          .names = "{.col}"
        ),
        across(
          all_of(var_be),
          ~sum(.x == "01", na.rm = TRUE),
          .names = "buen_estado_{.col}"
        ),
        .groups = "drop"  # opcional, elimina el agrupamiento
      ) %>% 
      mutate(
        !!paste0("aulapiso",nom_año) := redondear(buen_estado_aulapiso/tot1*100,1),
        !!paste0("aulatecho",nom_año) := redondear(buen_estado_aulatecho/tot1*100,1),
        !!paste0("aulapared",nom_año) := redondear(buen_estado_aulapared/tot1*100,1),
        !!paste0("puerta",nom_año) := redondear(buen_estado_puertabe/puertatot*100,1),
        !!paste0("ventana",nom_año) := redondear(buen_estado_ventanabe/ventanatot*100,1)
      ) %>% 
      select(CODLOCAL,!!paste0("aulapiso",nom_año):!!paste0("ventana",nom_año))
    
    return(infraula_1)
    
    
  } else if(nom_indicador %in% "Internet"){
    
    # Construcción de base de datos final #
    #=====================================#
    
    bd_final <- bd_1 %>% 
      select(CODLOCAL, internet) %>% 
      mutate(internet = case_when(internet %in% "2" ~ "NO",
                                  internet %in% "1" ~ "Sí",
                                  TRUE ~ internet)) %>% 
      rename(!!paste0("internet",nom_año) := internet)
    
    return(bd_final)
    
  } else if(nom_indicador %in% "Mobiliario aulas"){
    
    # Construcción de base de datos final #
    #=====================================#
    
    # Nombre de las variables 
    
    nombre_preg2 <- nombre_preg[-1]
    
    # Base con id
  
    bd_id <- bd_1 %>% 
      select(CODLOCAL) %>% 
      unique()
    
    # Base de datos con variables para construir indicadores
    
    temp1 <- bd_1 %>% 
      filter(NROCED %in% "11") %>% 
      filter(CUADRO %in% "CAULAS") %>% 
      filter(aula_filtro_1 %in% "1") %>% 
      group_by(CODLOCAL) %>% 
      summarise_at(vars(nombre_preg2),
                   ~sum(., na.rm = T))  
    
    # Construcción de indicador de mesas 
    
    mesas <- temp1 %>%
      mutate_at(vars(mesastot_1,mesastot_2),
                ~case_when(. %in% 0 ~ NA,
                           TRUE ~ .)) %>% 
      mutate(mesastot = rowSums(across(c(mesastot_1, mesastot_2)), na.rm = TRUE),
             mesasbe = rowSums(across(c(mesasbe_1, mesasbe_2)), na.rm = TRUE)) %>% 
      filter(!mesastot %in% 0) %>% 
      mutate(mesas = redondear(mesasbe/mesastot*100,1)) %>% 
      select(CODLOCAL,mesas)
    
    # Construcción de indicador de sillas
    
    sillas <- temp1 %>% 
      group_by(CODLOCAL) %>% 
      summarise_at(vars(sillastot,sillasbe),
                   ~sum(., na.rm = T)) %>% 
      filter(!sillastot %in% 0) %>% 
      mutate(sillas = redondear(sillasbe/sillastot*100,1)) %>% 
      select(CODLOCAL, sillas)
    
    # Construcción de indicador de pizarras
    
    pizarras <- temp1 %>% 
      group_by(CODLOCAL) %>% 
      summarise_at(vars(pizarratot,pizarrabe),
                   ~sum(., na.rm = T)) %>% 
      filter(!pizarratot %in% 0) %>% 
      mutate(pizarras = redondear(pizarrabe/pizarratot*100,1)) %>% 
      select(CODLOCAL, pizarras)
    
    # Integración de indicadores
    
    mobaula_1 <- reduce(list(bd_id,mesas,sillas,pizarras), left_join, by = "CODLOCAL") %>% 
      rename(!!paste0("mesas",nom_año) := mesas, !!paste0("sillas",nom_año) := sillas, !!paste0("pizarras",nom_año) := pizarras)
    
    return(mobaula_1)
    
  } else if(nom_indicador %in% "Espacios educativos"){
    
    # Construcción de base de datos final #
    #=====================================#
    
    ambientes <- c("004", "005",                              # Salas
                   "011", "012", "013", "015",                # Bibliotecas y ambientes de innovación pedagógica
                   "061", "062",                              # Espacios deportivos
                   "066", "067", "070", "072",                # Bienestar
                   "084", "088",                              # Servicios generales
                   "097", "098", "099", "100", "103","104")   # Gestión administrativa y pedagógica
    
    nom_ambientes <- c("Sala de usos múltiples", "Auditorio", 
                       "Biblioteca tipo 1", "Biblioteca tipo 2", "Biblioteca tipo 3", "Aula de innovación pedagógica", 
                       "Gimnasio", "Coliseo deportivo", 
                       "Quiosco", "Cafetería", "Tópico", "Espacio temporal para docente", 
                       "Depósito general", "Cuarto de limpieza", 
                       "Dirección", "Subdirección", "Administración", "Oficina de coordinación pedagógica", "Sala de docentes", "Sala de reuniones")
    
    nombres_a_renombrar <-  setNames(paste0(nom_ambientes,nom_año),ambientes)
    
    amb_1 <- bd_1 %>% 
      filter(espacio_1 %in% ambientes) %>% 
      filter(espacio_2 %in% "1") %>% 
      filter(str_detect(espacio_3, "B0|F0")) %>% 
      mutate(valor = 1) %>% 
      select(CODLOCAL,espacio_1,valor) %>% 
      distinct(CODLOCAL, espacio_1, .keep_all = TRUE) %>% 
      pivot_wider(
        names_from = espacio_1,
        values_from = valor,
        values_fill = list(valor = 0)
      ) %>% 
      mutate(biblioteca = case_when(`011` %in% 1 | `012` %in% 1 | `013` %in% 1 ~ 1,
                                    TRUE ~ 0)) %>% 
      select(-c(`011`,`012`,`013`))
    
    amb_2 <- amb_1 %>% 
      rename_with(~ nombres_a_renombrar[.x], 
                  .cols = all_of(intersect(names(amb_1), ambientes))) %>% 
      rename(!!paste0("biblioteca",nom_año) := biblioteca)
    
    return(amb_2)
    
  } else if(nom_indicador %in% "Infraestructura SSHH"){
    
    # Construcción de base de datos final #
    #=====================================#
    
    # Construcción de base general
    
    infra_sshh <- bd_1 %>% 
      filter(sshh_1 %in% c("133","134","135","136","137","138","139")) %>% 
      filter(str_detect(sshh_2, "B0|F0")) %>% 
      select(CODLOCAL,NUMERO,sshh_pared,sshh_techo,sshh_piso) %>% 
      mutate_at(vars(sshh_pared,sshh_techo,sshh_piso),
                ~case_when(. %in% "01" ~ "1",
                           is.na(.) ~ NA_character_,
                           TRUE ~ "0")) %>% 
      mutate_at(vars(sshh_pared,sshh_techo,sshh_piso),
                funs(as.numeric))
    
    # Base con id
    
    bd_id <- infra_sshh %>% 
      select(CODLOCAL) %>% 
      unique()
    
    # Rango de CODLOCAL duplicados 
    
    rango <- infra_sshh %>% 
      select(NUMERO) %>% 
      unique() %>% 
      pull()
    
    # Pared en buen estado 
    
    bd_pared <- infra_sshh %>% 
      select(CODLOCAL,NUMERO,sshh_pared) %>% 
      pivot_wider(names_from = NUMERO,
                  values_from = c(sshh_pared)) %>% 
      rowwise() %>% 
      mutate(no_na = sum(!is.na(c_across(min(rango):max(rango)))),
             tot_1 = sum(c_across(min(rango):max(rango)) == "1", na.rm = TRUE)) %>% 
      mutate(paredsshh := redondear(tot_1/no_na * 100, 1)) %>% 
      select(CODLOCAL,paredsshh)
    
    # Techo en buen estado 
    
    bd_techo <- infra_sshh %>% 
      select(CODLOCAL,NUMERO,sshh_techo) %>% 
      pivot_wider(names_from = NUMERO,
                  values_from = c(sshh_techo)) %>% 
      rowwise() %>% 
      mutate(no_na = sum(!is.na(c_across(min(rango):max(rango)))),
             tot_1 = sum(c_across(min(rango):max(rango)) == "1", na.rm = TRUE)) %>% 
      mutate(techosshh := redondear(tot_1/no_na * 100, 1)) %>% 
      select(CODLOCAL,techosshh)
    
    # Piso en buen estado 
    
    bd_piso <- infra_sshh %>% 
      select(CODLOCAL,NUMERO,sshh_piso) %>% 
      pivot_wider(names_from = NUMERO,
                  values_from = c(sshh_piso)) %>% 
      rowwise() %>% 
      mutate(no_na = sum(!is.na(c_across(min(rango):max(rango)))),
             tot_1 = sum(c_across(min(rango):max(rango)) == "1", na.rm = TRUE)) %>% 
      mutate(pisosshh := redondear(tot_1/no_na * 100, 1)) %>% 
      select(CODLOCAL,pisosshh)
    
    # Integración de bases de datos
    
    infra_baños <- reduce(list(bd_id,bd_pared,bd_techo,bd_piso), left_join, by = "CODLOCAL") %>% 
      rename(!!paste0("pisosshh",nom_año) := pisosshh,!!paste0("techosshh",nom_año) := techosshh,!!paste0("paredsshh",nom_año) := paredsshh)
    
    return(infra_baños)
    
  } else if (nom_indicador %in% "Mobiliario SSHH"){
    
    # Base con id
    
    bd_id <- bd_1 %>% 
      select(CODLOCAL) %>% 
      unique()
    
    # Construcción de base de datos final #
    #=====================================#
    
    mobiliariobaño_fun <- function(bd_datos, nom_mobiliario){
      
      if (nom_mobiliario %in% "Baños"){
        
        nom_tot1 <- "letrinatot"
        nom_tot1_be <- "letrinabe"
        nom_tot2 <- "inodorotot"
        nom_tot2_be <- "inodorobe"   
        nom_baño_var <- "baños"
        
      } else if (nom_mobiliario %in% "Lavatorios"){
        
        nom_tot1 <- "lavatoriotot_1"
        nom_tot1_be <- "lavatoriobe_1"
        nom_tot2 <- "lavatoriotot_2"
        nom_tot2_be <- "lavatoriobe_2"   
        nom_baño_var <- "lavatorios"
        
      } else if (nom_mobiliario %in% "Urinarios"){
        
        nom_tot1 <- "urinariotot_1"
        nom_tot1_be <- "urinariobe_1"
        nom_tot2 <- "urinariotot_2"
        nom_tot2_be <- "urinariobe_2"   
        nom_baño_var <- "urinarios"
        
      }
      
      bd_mobbaños <- bd_datos %>% 
        filter(sshh_1 %in% c("133","134","135","136","137","138","139")) %>% 
        filter(str_detect(sshh_2, "B0|F0")) %>% 
        select(CODLOCAL, !!sym(nom_tot1), !!sym(nom_tot1_be),!!sym(nom_tot2),!!sym(nom_tot2_be)) %>% 
        group_by(CODLOCAL) %>% 
        summarise(!!sym(nom_tot1) := sum(!!sym(nom_tot1), na.rm = T),
                  !!sym(nom_tot1_be) := sum(!!sym(nom_tot1_be), na.rm = T),
                  !!sym(nom_tot2) := sum(!!sym(nom_tot2), na.rm = T),
                  !!sym(nom_tot2_be) := sum(!!sym(nom_tot2_be), na.rm = T)) %>% 
        mutate(tot = !!sym(nom_tot1) + !!sym(nom_tot2),
               tot_1 = !!sym(nom_tot1_be) + !!sym(nom_tot2_be)) %>% 
        mutate(!!sym(nom_baño_var) := redondear(tot_1/tot*100,1)) %>% 
        select(CODLOCAL,!!sym(nom_baño_var))
      
      return(bd_mobbaños)
      
    }
    
    baños <- mobiliariobaño_fun(bd_1,"Baños")
    
    lavatorios <- mobiliariobaño_fun(bd_1,"Lavatorios")
    
    urinarios <- mobiliariobaño_fun(bd_1,"Urinarios")
    
    mobiliario_baños <- reduce(list(bd_id,baños,lavatorios,urinarios), left_join, by = "CODLOCAL") %>% 
      rename(!!paste0("baños",nom_año) := baños, !!paste0("lavatorios",nom_año) := lavatorios, !!paste0("urinarios",nom_año) := urinarios)

    return(mobiliario_baños)
    
  } else {
    
    "Sigue intentando"
    
  }
  
}
