#rm() #clean temporary variables

librariesUse <- c("RJDBC", "tidyverse")
install.packages(librariesUse)
lapply(librariesUse, require, character.only = TRUE)

# seteo del directorio por default, tengo que dejar las salidas en algún lugar comón
setwd("/home/david/rstudio")
getwd()


#drive <- JDBC("oracle.jdbc.OracleDriver", classPath="/home/david/ojdbc8.jar")
#connect <- dbConnect(drive,"jdbc:oracle:thin:@//172.18.0.1:1521/xe", "sys as sysdba", "mysecurepassword")

data <- raw.data <- read.csv(file = '/home/david/Datos_pareto.csv', stringsAsFactors=FALSE, fileEncoding="latin1", na.strings=c(""))

glimpse(data)

sapply(data, function(x) sum(is.na(x)))

dataClean <- na.omit(data)

glimpse(dataClean)

count.decimals.numbers <- function(value) {
  if (isTRUE(grep(".", value, fixed = TRUE)==1)){
    number <- nchar(gsub("\\D+", "", sub("\\..*", "", value)))
    decimal <- nchar(sub("^.+[.]","",value))
  }else{
    decimal = 0
    number <- nchar(value)
  }
  return(list(number, decimal))
}

for(column in 1:(ncol(dataClean))){
  enterNumber <- list()
  decimalNumber <- list()
  for (element in unique(dataClean[column])){
    if(class(element)=="numeric"){
      for (number in element){
        number <- count.decimals.numbers(toString(number))
        enterNumber <- c(enterNumber, number[1])
        decimalNumber <- c(decimalNumber, number[2])
      }
      value <- unlist(list(max(unlist(unique(enterNumber)), na.rm=TRUE), max(unlist(unique(decimalNumber)), na.rm=TRUE)))
    }else{
      value <- max(nchar(element), na.rm=TRUE)
    }
  }
  cat(" ",sapply(data[column], typeof),names(data[column]), ":", value)
}

