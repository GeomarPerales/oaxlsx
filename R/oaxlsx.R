#' function for reading, sorting and convert a data file downloaded from ANA in dataframe.
#'
#' function for reading, sorting and convert a data file downloaded from ANA in dataframe.
#' @param x a file (xlsx) downloaded from ANA (Peru)
#'
#' @importFrom openxlsx read.xlsx
#' @import openxlsx
#'
#' @export
#'
#' @author Geomar Perales Apaico
#'
#' @name oaxlsx

oaxlsx <- function(x) {
  file.xlsx <- x
  data <- read.xlsx( file.xlsx, sheet = "Reporte")

  ll <- length(unlist(strsplit(names(data)[1],"[.]")))
  nl <-  nchar(unlist(strsplit(names(data)[1],"[.]"))[ll])

  #parametros
  codigo <- substr(unlist(strsplit(names(data)[1],"[.]"))[ll], 1, nl-1)

  #nombre estacion
  lon.name <- length(unlist(strsplit(names(data)[1],"[.]")))

  if (lon.name == 4){
    nombre.estacion <- unlist(strsplit(names(data)[1],"[.]"))[2]
  } else if(lon.name == 5){
    nombre.estacion <- paste(unlist(strsplit(names(data)[1],"[.]"))[2],
                             unlist(strsplit(names(data)[1],"[.]"))[3], sep = " ")
  } else if(lon.name == 6){
    nombre.estacion <- paste(unlist(strsplit(names(data)[1],"[.]"))[2],
                             unlist(strsplit(names(data)[1],"[.]"))[3],
                             unlist(strsplit(names(data)[1],"[.]"))[4], sep = " ")
  } else if(lon.name == 7){
    nombre.estacion <- paste(unlist(strsplit(names(data)[1],"[.]"))[2],
                             unlist(strsplit(names(data)[1],"[.]"))[3],
                             unlist(strsplit(names(data)[1],"[.]"))[4],
                             unlist(strsplit(names(data)[1],"[.]"))[5], sep = " ")
  } else if(lon.name == 8){
    nombre.estacion <- paste(unlist(strsplit(names(data)[1],"[.]"))[2],
                             unlist(strsplit(names(data)[1],"[.]"))[3],
                             unlist(strsplit(names(data)[1],"[.]"))[4],
                             unlist(strsplit(names(data)[1],"[.]"))[5], sep = " ")
  }

  operador <- data[2,2]
  variable <- data[1,1]

  #coordenadas
  src <- "WGS 84"
  latitud <- strsplit(data[3,2]," ")[[1]][3]
  longitud <- strsplit(data[3,2]," ")[[1]][6]
  altitud <- strsplit(data[3,2]," ")[[1]][9]

  #tipo
  tipo1 <- strsplit(data[4,2]," ")[[1]][1]
  tipo2 <- strsplit(data[4,2]," ")[[1]][3]

  #amb. politico
  dpto <- strsplit(data[5,2]," ")[[1]][2]
  prov <- strsplit(data[5,2]," ")[[1]][5]
  dist <- strsplit(data[5,2]," ")[[1]][8]

  #amb. admin del ANA (por evaluar)
  aaa <- strsplit(data[6,2]," ")[[1]][2] #1
  aaa
  ala <- strsplit(data[6,2]," ")[[1]][5] #2
  ala

  #unidad hidrografica
  lon.uh <- length(strsplit(data[7,2]," ")[[1]])
  if(lon.uh == 2){
    uh <- strsplit(data[7,2]," ")[[1]][2]
  } else if(lon.uh == 3){
    uh <- paste(strsplit(data[7,2]," ")[[1]][2],
                strsplit(data[7,2]," ")[[1]][3], sep = " ")
  } else if(lon.uh == 4){
    uh <- paste(strsplit(data[7,2]," ")[[1]][2],
                strsplit(data[7,2]," ")[[1]][3],
                strsplit(data[7,2]," ")[[1]][4], sep = " ")
  } else if(lon.uh == 5){
    uh <- paste(strsplit(data[7,2]," ")[[1]][2],
                strsplit(data[7,2]," ")[[1]][3],
                strsplit(data[7,2]," ")[[1]][4],
                strsplit(data[7,2]," ")[[1]][5], sep = " ")
  }

  #data pluviometrica
  df_pp <- data[11:nrow(data),]
  colnames(df_pp) <- c("Fecha", "Hora", "valor")

  #generacion del dataframe de ts de info station
  df <- data.frame(codigo, nombre.estacion, operador, variable,
                   src, latitud, longitud, altitud, tipo1, tipo2,
                   dpto, prov, dist, uh, df_pp)
  return(df)
}
