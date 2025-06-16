#========================================================#
# Funciones varias que ayudan a manejar la base de datos #
#========================================================#

# Conteo de missing en las variables #
#------------------------------------#

funmiss <- function(x) {
  
  sum(is.na(x))/length(x)*100
  
  }

# Media pesada # 
#--------------#

weighted_mean <- function(x, w, ..., na.rm = FALSE){
  
  if(na.rm){
    
    df_omit <- na.omit(data.frame(x, w))
    
    return(weighted.mean(df_omit$x, df_omit$w, ...))
    
  } 
  
  weighted.mean(x, w, ...)
}

# Desviación estándar pesada #
#----------------------------#

weighted_sd <- function(x, weights) {
  # Eliminar valores faltantes en x y en los pesos correspondientes
  complete_cases <- complete.cases(x, weights)
  x <- x[complete_cases]
  weights <- weights[complete_cases]
  
  # Calcular la desviación estándar ponderada
  weighted_var <- sum(weights * (x - weighted.mean(x, w = weights))^2) / sum(weights)
  sqrt(weighted_var)
}

# Cálculo de la moda con pesos #
#------------------------------#

lamoda_pesos <- function(x, weights = NULL) {
  if (is.null(weights)) {
    u <- unique(x)
    tab <- tabulate(match(x, u))
    u[tab == max(tab)]
  } else {
    # Verificar que la longitud de 'x' y 'weights' sea la misma
    if (length(x) != length(weights)) {
      stop("La longitud de 'x' y 'weights' debe ser la misma.")
    }
    
    # Crear una tabla de frecuencia ponderada
    df <- data.frame(x, weights)
    df <- df[order(df$x), ]
    df <- df[!duplicated(df$x), ]
    df$weighted_freq <- ave(df$weights, df$x, FUN = sum)
    
    # Encontrar la moda basada en las frecuencias ponderadas
    moda <- df[df$weighted_freq == max(df$weighted_freq), "x"]
    moda
  }
}

# Redondear #
#-----------#

redondear<-function(x,d=0){
  (floor(x*10**d)+as.numeric((x*10**d-floor(x*10**d))>=0.5))/10**d
}

# Cambiar punto por coma decimal #
#--------------------------------#

puntocoma2<-function(x,dec=0){
  if(is.numeric(x)){
    if(length(dim(x))==2){
      gsub("\\.",",",apply(redondear(x,dec), 2, sprintf,fmt=paste0("%.",dec,"f")))
    }else{gsub("\\.",",",sprintf(paste0("%.",dec,"f"), redondear(x,dec)))}
  }else{
    if(length(dim(x))==2){
      redondear(apply(gsub(",","\\.",x),2,as.numeric),dec)
    }else{redondear(as.numeric(gsub(",","\\.",x)),dec)}
  }
  
} 

# Ver en Excel #
#--------------#

show_in_excel <- function(.data){
  tmp <- paste0(tempfile(),".xlsx")
  write.xlsx(.data,tmp)
  browseURL(url=tmp)
}

# Contar NA en las filas #
#------------------------#

rowSumsNA <- function(x) rowSums(is.na(x)) 

# Acortar oraciones en variables tipo factor #
#--------------------------------------------#

str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}

# Colores UMC #
#-------------#

color_pre_in <- '#A5A5A5' # Color "Previo al inicio"
color_inicio <- '#A74D4B' # Color "En inicio"
color_proces <- '#F79646' # Color "En proceso"
color_satisf <- '#9BBB59' # Color "Satisfactorio"

# Duplicar texto del "eje Y" hacia la derecha del gráfico #
#---------------------------------------------------------#

guide_axis_label_trans <- function(label_trans = identity, ...) {
  axis_guide <- guide_axis(...)
  axis_guide$label_trans <- rlang::as_function(label_trans)
  class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
  axis_guide
}

# Crear percentiles pesados #
#---------------------------#

percentil_umc <- function(var, weights, percentil){
  
  if (percentil == "tercil") {
    
    case_when(
      {{var}} <= wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.33), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q1",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.33), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.67), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q2",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.67), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q3",
      TRUE ~ NA_character_
    )
    
  } else if (percentil == "cuartil") {
    
    case_when(
      {{var}} <= wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.25), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q1",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.25), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.50), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q2",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.50), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.75), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q3",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.75), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q4",
      TRUE ~ NA_character_
    )
    
  } else if (percentil == "quintil") {
    
    case_when(
      {{var}} <= wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.20), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q1",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.20), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.40), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q2",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.40), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.60), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q3",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.60), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.80), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q4",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.80), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q5",
      TRUE ~ NA_character_
    )
    
  } else if (percentil == "decil") {
    
    case_when(
      {{var}} <= wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.10), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q1",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.10), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.20), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q2",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.20), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.30), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q3",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.30), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.40), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q4",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.40), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.50), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q5",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.50), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.60), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q6",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.60), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.70), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q7",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.70), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.80), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q8",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.80), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.90), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q9",
      {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.90), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "Grupo Q10",
      TRUE ~ NA_character_
    )
    
  } else if (percentil == "nse"){
    
    case_when({{var}} <= wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.35), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "NSE muy bajo",
              {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.35), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.60), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "NSE bajo",
              {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.60), na.rm = TRUE, type=c('(i-1)/(n-1)')) & var <= wtd.quantile(var, weights = {{weights}}, probs = c(0.85), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "NSE medio",
              {{var}} > wtd.quantile({{var}}, weights = {{weights}}, probs = c(0.85), na.rm = TRUE, type=c('(i-1)/(n-1)')) ~ "NSE alto",
    )
    
  }
  
}

# Función para cálculo de PCA #
#-----------------------------#

pca_umc <- function(x, corr = NULL, puntajes = TRUE){

  #toma un data.frame y aplica PCA
  #devuelve las varianza explicada, cargas del primer componente y alpha de cronbach
  
  # x: data.frame
  # corr: por defecto aplica correlacion de pearson, podemos colocar 'poly' para indicar
  #       correlacion policorica
  # puntajes: logical si es que se quiere puntajes

  if(sum(sapply(x, function(xx) sum(is.na(xx)))) > 0) stop("Es preferible que no hayan NAs en las columnas =)")

  if(is.null(corr)){
    ee <- eigen(cor(x), symmetric = FALSE) # symmetric=FALSE previene cargas negativas [espero]
  }else{
    cor_pol <- psych::polychoric(x)$rho
    ee <- eigen(cor_pol, symmetric = FALSE)
    alpha <- psych::alpha(cor_pol, warnings = FALSE)$feldt$alpha[[1]] #Confiabilidad
  }

  #calculamos varianza (val), cargas (l), pesos (w)
  val <- ee$values
  val_sq <- sqrt(val) #desviacion
  l <- ee$vectors %*% diag(val_sq)
  w <- ee$vectors %*% diag(1/val_sq)

  if(all(l[, 1] < 0)) {l[, 1] <- l[, 1]*-1; w[, 1] <- w[, 1]*-1} # ¿por que? U_U

  if(puntajes == TRUE){
    z <- as.matrix(scale(x)) # datos estandarizados y matrix
    s <- z %*% l # datos estandarizados por sus cargas
    s <- scale(s)
  }

  cargas <- data.frame(Item = names(x), Pesos = w[, 1], Cargas = l[, 1])
  vr <- c("Pesos", "Cargas")
  cargas[vr] <- apply(cargas[vr], 2, function(x) format(round(x, 3), decimal.mark = ","))
  varex <- format(round(val[1]/sum(val)*100, 2), decimal.mark = ",")

  if(puntajes == TRUE){
    return(list(puntajes = s[, 1], indicadores = varex, cargas = cargas, confiabilidad = alpha))}
  else {
    return(list(indicadores = varex, cargas = cargas, confiabilidad = alpha))}

}
                      
# Espacio decimal en números #
#----------------------------#

decimal_format <- function(x) {
  sapply(x, function(n) {
    if (is.na(n)) return(NA)
    
    entero <- floor(abs(n))  # parte entera sin decimales
    ndig <- nchar(as.character(entero))
    
    if (ndig <= 4) {
      return(as.character(n))  # lo dejamos igual
    } else {
      # Agregar espacio como separador de miles a la parte entera
      entero_txt <- format(entero, big.mark = " ", scientific = FALSE, trim = TRUE)
      return(entero_txt)
    }
  })
}
                      
#====================================#
# Descargar bases y libro de códigos #
#====================================#

descargar_bases <- function(año_seleccionado) {
  
  #============================#
  # Crear carpeta si no existe #
  #============================#
  
  carpeta_destino <- "01 Bases"
  if (!dir.exists(carpeta_destino)) dir.create(carpeta_destino, recursive = TRUE)
  
  #============================#
  # Descargar libro de códigos #
  #============================#
  
  # URL del archivo
  url <- "https://raw.githubusercontent.com/Manumarc/Indicadores-censo-escolar/main/Libro_de_codigos.xlsx"
  
  # Ruta de destino
  destino <- "01 Bases/Libro_de_codigos.xlsx"
  
  # Descargar el archivo
  download.file(url, destfile = destino, mode = "wb")
  
  # Abrir archivo de libro de códigos
  
  libcod <- read.xlsx("01 Bases/Libro_de_codigos.xlsx", sheet = 4) %>% 
    filter(año %in% año_seleccionado)
  
  #==========================#
  # Descargar bases de datos #
  #==========================#
  
  # Listas de URLs y nombres
  
  nom_url <- libcod %>% 
    dplyr::select(link) %>% 
    unique() %>% 
    pull()
  
  nombres_var <- libcod %>% 
    dplyr::select(nom_base2) %>% 
    unique() %>% 
    pull()
  
  # Función para descargar, descomprimir y renombrar archivos
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
  
  # Ejecutar según el o los años seleccionados
  
    message("Procesando archivos ...")
    descargar_y_procesar(nom_url, nombres_var)
  
  message("Todos los archivos seleccionados han sido descargados y procesados.")
  
}

#=========================================================================#
# Función para el cálculo de indicadores de infraestructura de la escuela #
#=========================================================================#

cal_indicador <- function(nom_indicador,nom_año){
  
  # Libro de códigos #
  #==================#
  
  libcod <- read.xlsx("01 Bases/Libro_de_codigos.xlsx", sheet = 4)
  
  # Selección de grupo de indicadores a calcular #
  #==============================================#
  
  # Filtro de datos 
  
  base <- libcod %>% 
    filter(indicador %in% nom_indicador) %>% 
    filter(año %in% nom_año)
  
  # Nombre de la base de datos a llamar 
  
  nombre_base <- base %>% 
    dplyr::select(nom_base) %>% 
    pull() %>% 
    unique()
  
  # Nombre de la variable 
  
  nombre_preg <- base %>% 
    dplyr::select(nom_pregunta) %>% 
    pull() %>% 
    unique()
  
  # Cambio de nombre de variables #
  #===============================#
  
  # Nombre del cuadro 
  
  cuadro <- base %>% 
    dplyr::select(CUADRO) %>% 
    pull() %>% 
    unique()
  
  uniq_cuadro <- base %>% 
    dplyr::select(nom_cuadro) %>% 
    pull() %>% 
    unique()
  
  # Nombre de las preguntas 
  
  preguntas <- base %>% 
    dplyr::select(codigo_preg, nom_pregunta) %>% 
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
          mutate(NROCED = case_when(NROCED %in% "C11" ~ "11",
                            TRUE ~ NROCED)) %>% 
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
      dplyr::select(!!sym(nombre_preg[[1]])) %>% 
      pull() %>% 
      unique()
    
    #Base de datos con el indicador
    
    bd_final <- bd_1 %>% 
      mutate(!!sym(nombre_preg[[2]]) := case_when(!!sym(nombre_preg[[2]]) %in% "SI" ~ "1",
                                   !!sym(nombre_preg[[2]]) %in% "NO" ~ "2",
                                   is.na(!!sym(nombre_preg[[2]])) ~ NA_character_,
                                   TRUE ~ !!sym(nombre_preg[[2]]))) %>% 
      dplyr::select(CODLOCAL,!!sym(nombre_preg[[1]]),!!sym(nombre_preg[[2]])) %>% 
      pivot_wider(names_from = !!sym(nombre_preg[[1]]),
                  values_from = !!sym(nombre_preg[[2]])) %>% 
      rowwise() %>%
      mutate(no_na = sum(!is.na(c_across(first(rango):last(rango)))),
             tot_1 = sum(c_across(first(rango):last(rango)) == "1", na.rm = TRUE)) %>%
      ungroup() %>% 
      filter(!no_na %in% 0) %>% 
      mutate(avance = redondear((tot_1/no_na)*100,1)) %>% 
      mutate(saneamiento = case_when(avance %in% 100 ~ "Saneado",
                                     avance %in% 0 ~ "No saneado",
                                     TRUE ~ "Saneado parcial")) %>% 
      dplyr::select(CODLOCAL,!!paste0("saneamiento",nom_año) := saneamiento)
    
    return(bd_final)
    
  } else if(nom_indicador %in% c("Agua","Desagüe","Luz")){
    
    # Construcción de base de datos final #
    #=====================================#
    
    claves <- base %>%
      dplyr::select(codigo_resp) %>%
      str_split("; ") %>% 
      as.data.frame() %>% 
      pull()
    
    serbas_1 <- bd_1 %>% 
      mutate(conexion = case_when(!!sym(nombre_preg) %in% claves[[1]] ~ "Conexión red pública",
                                  TRUE ~ "Sin conexión red pública")) %>%
      dplyr::select(CODLOCAL, conexion)
    
    # Rango de variables duplicadas #
    #-------------------------------#
    
    rango <- serbas_1 %>% 
      group_by(CODLOCAL) %>%
      mutate(duplicado = row_number()) %>%
      ungroup() %>% 
     dplyr::select(duplicado) %>% 
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
        dplyr::select(CODLOCAL, !!paste0(nombre_preg,nom_año) := serbas)
      
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
        dplyr::select(CODLOCAL, !!paste0(nombre_preg,nom_año) := serbas)
    }
    
    return(serbas_2)
    
  } else if (nom_indicador %in% "Infraestructura aulas"){
    
    # Construcción de base de datos final #
    #=====================================#
    
    var_be <- c(nombre_preg[[4]],nombre_preg[[8]],nombre_preg[[9]],nombre_preg[[10]],"ventanabe")
    var_tot <- c(nombre_preg[[3]],nombre_preg[[5]])
    
    infraula_1 <- bd_1 %>% 
      mutate(!!sym(nombre_preg[[2]]) := case_when(!!sym(nombre_preg[[2]]) %in% "SI" ~ "1",
                                       !!sym(nombre_preg[[2]]) %in% "NO" ~ "2",
                                       is.na(!!sym(nombre_preg[[2]])) ~ NA_character_,
                                       TRUE ~ !!sym(nombre_preg[[2]]))) %>% 
      filter(!!sym(nombre_preg[[2]]) %in% "1") %>% 
      mutate(ventanabe = case_when(marcovenbe %in% c("01") & vidriobe %in% c("01") ~ "01",
                                   is.na(marcovenbe) | is.na(vidriobe) ~ NA_character_,
                                   TRUE ~ "00")) %>% 
      mutate(across(
        all_of(var_be),
        ~ case_when(.x == "01" ~ 1, TRUE ~ 0),
        .names = "buen_estado_{.col}"
      )) %>% 
      dplyr::select(CODLOCAL,var_be,var_tot) %>% 
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
      dplyr::select(CODLOCAL,!!paste0("aulapiso",nom_año):!!paste0("ventana",nom_año))
    
    return(infraula_1)
    
  } else if(nom_indicador %in% "Internet"){
    
    # Construcción de base de datos final #
    #=====================================#
    
    bd_final <- bd_1 %>% 
      dplyr::select(CODLOCAL, internet) %>% 
      mutate(internet = case_when(internet %in% "2" ~ "No",
                                  internet %in% "1" ~ "Sí",
                                  internet %in% "NO" ~ "No",
                                  internet %in% "SI" ~ "Sí",
                                  is.na(internet) ~ NA_character_,
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
      dplyr::select(CODLOCAL) %>% 
      unique()
    
    # Base de datos con variables para construir indicadores
    
    temp1 <- bd_1 %>% 
      filter(NROCED %in% "11") %>% 
      filter(CUADRO %in% "CAULAS") %>% 
      mutate(aula_filtro_1 = case_when(aula_filtro_1 %in% "SI" ~ "1",
                                   aula_filtro_1 %in% "NO" ~ "2",
                                   is.na(aula_filtro_1) ~ NA_character_,
                                   TRUE ~ aula_filtro_1)) %>% 
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
      dplyr::select(CODLOCAL,mesas)
    
    # Construcción de indicador de sillas
    
    sillas <- temp1 %>% 
      group_by(CODLOCAL) %>% 
      summarise_at(vars(sillastot,sillasbe),
                   ~sum(., na.rm = T)) %>% 
      filter(!sillastot %in% 0) %>% 
      mutate(sillas = redondear(sillasbe/sillastot*100,1)) %>% 
      dplyr::select(CODLOCAL, sillas)
    
    # Construcción de indicador de pizarras
    
    pizarras <- temp1 %>% 
      group_by(CODLOCAL) %>% 
      summarise_at(vars(pizarratot,pizarrabe),
                   ~sum(., na.rm = T)) %>% 
      filter(!pizarratot %in% 0) %>% 
      mutate(pizarras = redondear(pizarrabe/pizarratot*100,1)) %>% 
      dplyr::select(CODLOCAL, pizarras)
    
    # Integración de indicadores
    
    mobaula_1 <- reduce(list(bd_id,mesas,sillas,pizarras), left_join, by = "CODLOCAL") %>% 
      rename(!!paste0("mesas",nom_año) := mesas, !!paste0("sillas",nom_año) := sillas, !!paste0("pizarras",nom_año) := pizarras)
    
    return(mobaula_1)
    
  } else if(nom_indicador %in% "Espacios educativos"){
    
    # Construcción de base de datos final #
    #=====================================#
    
    ambientes <- base %>% 
      filter(nom_pregunta %in% "espacio_1") %>% 
      dplyr::select(codigo_resp) %>% 
      mutate(codigo_resp = str_trim(codigo_resp, side = "right")) %>% 
      str_split("; ") %>% 
      as.data.frame() %>% 
      pull()
    
    nom_ambientes <- base %>% 
      filter(nom_pregunta %in% "espacio_1") %>% 
      dplyr::select(respuesta) %>% 
      mutate(respuesta = str_trim(respuesta, side = "right")) %>% 
      str_split("; ") %>% 
      as.data.frame() %>% 
      pull()
    
    # Salas
    # Bibliotecas y ambientes de innovación pedagógica
    # Espacios deportivos
    # Bienestar
    # Servicios generales
    # Gestión administrativa y pedagógica
    
    nombres_a_renombrar <-  setNames(paste0(nom_ambientes,nom_año),ambientes)
    
    amb_1 <- bd_1 %>% 
      filter(espacio_1 %in% ambientes) %>% 
      mutate(espacio_2 = case_when(espacio_2 %in% "SI" ~ "1",
                                   espacio_2 %in% "NO" ~ "2",
                                   is.na(espacio_2) ~ NA_character_,
                                   TRUE ~ espacio_2)) %>% 
      filter(espacio_2 %in% "1") %>% 
      filter(str_detect(espacio_3, "B0|F0")) %>% 
      mutate(valor = 1) %>% 
      dplyr::select(CODLOCAL,espacio_1,valor) %>% 
      distinct(CODLOCAL, espacio_1, .keep_all = TRUE) %>% 
      pivot_wider(
        names_from = espacio_1,
        values_from = valor,
        values_fill = list(valor = 0)
      ) %>% 
      mutate(biblioteca = case_when(`011` %in% 1 | `012` %in% 1 | `013` %in% 1 ~ 1,
                                    TRUE ~ 0)) %>% 
      dplyr::select(-c(`011`,`012`,`013`))
    
    amb_2 <- amb_1 %>% 
      rename_with(~ nombres_a_renombrar[.x], 
                  .cols = all_of(intersect(names(amb_1), ambientes))) %>% 
      rename(!!paste0("biblioteca",nom_año) := biblioteca)
    
    return(amb_2)
    
  } else if(nom_indicador %in% "Infraestructura SSHH"){
    
    # Construcción de base de datos final #
    #=====================================#
    
    # Tipos de baño considerados 
    
    tip_baño <- base %>% 
      filter(nom_pregunta %in% "sshh_1") %>% 
      dplyr::select(codigo_resp) %>% 
      mutate(codigo_resp = str_trim(codigo_resp, side = "right")) %>% 
      str_split("; ") %>% 
      as.data.frame() %>% 
      pull()
    
    # Construcción de base general
    
    infra_sshh <- bd_1 %>% 
      filter(sshh_1 %in% tip_baño) %>% 
      filter(str_detect(sshh_2, "B0|F0")) %>% 
      dplyr::select(CODLOCAL,NUMERO,sshh_pared,sshh_techo,sshh_piso) %>% 
      mutate_at(vars(sshh_pared,sshh_techo,sshh_piso),
                ~case_when(. %in% "01" ~ "1",
                           is.na(.) ~ NA_character_,
                           TRUE ~ "0")) %>% 
      mutate_at(vars(sshh_pared,sshh_techo,sshh_piso),
                funs(as.numeric))
    
    # Base con id
    
    bd_id <- infra_sshh %>% 
      dplyr::select(CODLOCAL) %>% 
      unique()
    
    # Rango de CODLOCAL duplicados 
    
    rango <- infra_sshh %>% 
      dplyr::select(NUMERO) %>% 
      unique() %>% 
      pull()
    
    # Pared en buen estado 
    
    bd_pared <- infra_sshh %>% 
      dplyr::select(CODLOCAL,NUMERO,sshh_pared) %>% 
      pivot_wider(names_from = NUMERO,
                  values_from = c(sshh_pared)) %>% 
      rowwise() %>% 
      mutate(no_na = sum(!is.na(c_across(min(rango):max(rango)))),
             tot_1 = sum(c_across(min(rango):max(rango)) == "1", na.rm = TRUE)) %>% 
      mutate(paredsshh := redondear(tot_1/no_na * 100, 1)) %>% 
      dplyr::select(CODLOCAL,paredsshh)
    
    # Techo en buen estado 
    
    bd_techo <- infra_sshh %>% 
      dplyr::select(CODLOCAL,NUMERO,sshh_techo) %>% 
      pivot_wider(names_from = NUMERO,
                  values_from = c(sshh_techo)) %>% 
      rowwise() %>% 
      mutate(no_na = sum(!is.na(c_across(min(rango):max(rango)))),
             tot_1 = sum(c_across(min(rango):max(rango)) == "1", na.rm = TRUE)) %>% 
      mutate(techosshh := redondear(tot_1/no_na * 100, 1)) %>% 
      dplyr::select(CODLOCAL,techosshh)
    
    # Piso en buen estado 
    
    bd_piso <- infra_sshh %>% 
      dplyr::select(CODLOCAL,NUMERO,sshh_piso) %>% 
      pivot_wider(names_from = NUMERO,
                  values_from = c(sshh_piso)) %>% 
      rowwise() %>% 
      mutate(no_na = sum(!is.na(c_across(min(rango):max(rango)))),
             tot_1 = sum(c_across(min(rango):max(rango)) == "1", na.rm = TRUE)) %>% 
      mutate(pisosshh := redondear(tot_1/no_na * 100, 1)) %>% 
      dplyr::select(CODLOCAL,pisosshh)
    
    # Integración de bases de datos
    
    infra_baños <- reduce(list(bd_id,bd_pared,bd_techo,bd_piso), left_join, by = "CODLOCAL") %>% 
      rename(!!paste0("pisosshh",nom_año) := pisosshh,!!paste0("techosshh",nom_año) := techosshh,!!paste0("paredsshh",nom_año) := paredsshh)
    
    return(infra_baños)
    
  } else if (nom_indicador %in% "Mobiliario SSHH"){
    
    # Base con id
    
    bd_id <- bd_1 %>% 
      dplyr::select(CODLOCAL) %>% 
      unique()
    
    # Tipos de baño considerados 
    
    tip_baño <- base %>% 
      filter(nom_pregunta %in% "sshh_1") %>% 
      dplyr::select(codigo_resp) %>% 
      mutate(codigo_resp = str_trim(codigo_resp, side = "right")) %>% 
      str_split("; ") %>% 
      as.data.frame() %>% 
      pull()
    
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
        filter(sshh_1 %in% tip_baño) %>% 
        filter(str_detect(sshh_2, "B0|F0")) %>% 
        dplyr::select(CODLOCAL, !!sym(nom_tot1), !!sym(nom_tot1_be),!!sym(nom_tot2),!!sym(nom_tot2_be)) %>% 
        group_by(CODLOCAL) %>% 
        summarise(!!sym(nom_tot1) := sum(!!sym(nom_tot1), na.rm = T),
                  !!sym(nom_tot1_be) := sum(!!sym(nom_tot1_be), na.rm = T),
                  !!sym(nom_tot2) := sum(!!sym(nom_tot2), na.rm = T),
                  !!sym(nom_tot2_be) := sum(!!sym(nom_tot2_be), na.rm = T)) %>% 
        mutate(tot = !!sym(nom_tot1) + !!sym(nom_tot2),
               tot_1 = !!sym(nom_tot1_be) + !!sym(nom_tot2_be)) %>% 
        mutate(!!sym(nom_baño_var) := redondear(tot_1/tot*100,1)) %>% 
        dplyr::select(CODLOCAL,!!sym(nom_baño_var))
      
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
