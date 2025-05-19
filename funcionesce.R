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
  
  nombre_base <-base %>% 
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
    
    
  } else {
    
    "Sigue intentando"
    
  }
  
}
