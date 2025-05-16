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
  url <- "https://raw.githubusercontent.com/Manumarc/Indicadores-censo-escolar/main/Libro%20de%20c%C3%B3digos.xlsx"
  
  # Ruta de destino
  destino <- "01 Bases/Libro_de_codigos.xlsx"
  
  # Descargar el archivo
  download.file(url, destfile = destino, mode = "wb")
  
  # Descargar bases de datos #
  #==========================#
  
  # Listas de URLs y nombres
  nom_url24 <- c(
    "https://escale.minedu.gob.pe/documents/10156/6ab386c8-4b5c-4cd1-b3c1-3b44e0137085", # Padron 
    "https://escale.minedu.gob.pe/documents/10156/ac666dee-3231-452e-a655-57ec95c4e894", # Loc_linea_2
    "https://escale.minedu.gob.pe/documents/10156/bf381a61-2f33-49c7-ae16-d4671c1d77da", # Loc_P108_SaFiLe
    "https://escale.minedu.gob.pe/documents/10156/d9ba6b10-c116-4573-9d24-e2d60ac6f069", # Loc_P200_ServBa
    "https://escale.minedu.gob.pe/documents/10156/87f92989-ef23-46a5-b850-7e1cfb0bd7a2", # Loc_P501_Aulas
    "https://escale.minedu.gob.pe/documents/10156/7933ed8e-c751-41d0-958a-ac0a215a38c6", # Loc_P601_OtEsp
    "https://escale.minedu.gob.pe/documents/10156/641cdd5a-9cbd-4f1c-a9cf-91ff7a5bc557"  # Loc_P701_SSHH
  )
  nombres24 <- c("Padron_24", "Loc_linea_2_24", "Loc_P108_SaFiLe_24", "Loc_P200_ServBa_24",
                 "Loc_P501_Aulas_24", "Loc_P601_OtEsp_24", "Loc_P701_SSHH_24")
  
  nom_url23 <- c(
    "https://escale.minedu.gob.pe/documents/10156/feb2b10b-9c8d-4686-8342-345bc2abb87d", # Padron 
    "https://escale.minedu.gob.pe/documents/10156/a0bd77de-fdcc-4d33-a71b-712a4728cebb", # Loc_linea_2
    "https://escale.minedu.gob.pe/documents/10156/43ee710b-9503-40d8-acdd-06c043e1b8ac", # Loc_P108_SaFiLe
    "https://escale.minedu.gob.pe/documents/10156/9940b8c2-b00b-4454-bfd0-ed5ef3ab5a45", # Loc_P200_ServBa
    "https://escale.minedu.gob.pe/documents/10156/2a5071e9-218b-4b24-870c-c51ce81a5563", # Loc_P501_Aulas
    "https://escale.minedu.gob.pe/documents/10156/fcaaff07-6a8e-486b-bc3d-5cb6e217c0d8", # Loc_P601_OtEsp
    "https://escale.minedu.gob.pe/documents/10156/1e1df25d-c815-47d4-bd4b-cf1d67c4ae4e"  # Loc_P701_SSHH
  )
  
  nombres23 <- c("Padron_23", "Loc_linea_2_23", "Loc_P108_SaFiLe_23", "Loc_P200_ServBa_23",
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
