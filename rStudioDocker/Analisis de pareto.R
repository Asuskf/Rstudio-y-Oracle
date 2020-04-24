#rm() #clean temporary variables

librariesUse <- c("RJDBC", "tidyverse","gridExtra", "grid")
install.packages(librariesUse)
lapply(librariesUse, require, character.only = TRUE)

# seteo del directorio por default, tengo que dejar las salidas en algún lugar comón
setwd("/home/david/rstudio")
getwd()


drive <- JDBC("oracle.jdbc.OracleDriver", classPath="/home/david/ojdbc8.jar")
connect <- dbConnect(drive,"jdbc:oracle:thin:@//172.19.0.1:1521/xe", "SYSTEM", "Oracle18")

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

createTable <- "CREATE TABLE ventas (
    id number PRIMARY key,
    rtl_loc_id NUMBER(2) NOT NULL,
    dept_name VARCHAR(53) NOT NULL,
    item_id NUMBER (9) NOT NULL,
    descriptions VARCHAR(131) NOT NULL,
    quantity NUMBER (7,2) NOT NULL,
    net_amt NUMBER (9,2) NOT NULL
  )"

createSequece <- "CREATE SEQUENCE lastSale
START WITH 1
INCREMENT BY 1"

createTrigger <- "CREATE TRIGGER registerSale
BEFORE INSERT ON ventas
FOR EACH ROW
BEGIN
SELECT lastSale.NEXTVAL INTO : NEW.id FROM DUAL;
END;"


dbSendUpdate(connect, createTable)
dbSendUpdate(connect, createSequece)
dbSendUpdate(connect, createTrigger)


for (rowData in 1:nrow(dataClean)){
  ptint(rowData)
  dbSendUpdate(connect, "insert into ventas (rtl_loc_id, dept_name, item_id, descriptions, quantity, net_amt) values (?, ?, ?, ?, ?, ?)", dataClean[['RTL_LOC_ID']][rowData], dataClean[['DEPT_NAME']][rowData], dataClean[['ITEM_ID']][rowData], dataClean[['DESCRIPCION']][rowData], dataClean[['QUANTITY']][rowData], dataClean[['NET_AMT']][rowData])
}

querySales <- "SELECT RTL_LOC_ID,SUM(NET_AMT) as totalMoney,SUM(quantity) AS totalProductsSold FROM ventas where NET_AMT > 0 GROUP BY RTL_LOC_ID ORDER BY totalProductsSold DESC"
queryCategory <- "SELECT DEPT_NAME,SUM(quantity) as sumQuantity,SUM(NET_AMT) AS sumNeramt FROM ventas where NET_AMT > 0 GROUP BY DEPT_NAME ORDER BY sumQuantity DESC"
queryCategoryReturn <- "SELECT DEPT_NAME, sum(QUANTITY) as sumQuantity, SUM(NET_AMT) as sumNetAMT FROM ventas where NET_AMT < 0 GROUP BY DEPT_NAME ORDER BY sumNetAMT"
queryLocalRoyalties <- "SELECT DEPT_NAME, COUNT (Quantity) as countQuanty FROM ventas where NET_AMT = 0 GROUP BY DEPT_NAME ORDER BY countQuanty DESC"
queryLocalReturn <- "SELECT RTL_LOC_ID, sum(QUANTITY) as sumQuantity, SUM(NET_AMT) as sumNetAMT FROM ventas where NET_AMT < 0 GROUP BY RTL_LOC_ID ORDER BY sumNetAMT"
queryCategoryRoyalties <- "SELECT rtl_loc_id, COUNT (Quantity) as countRtllocId FROM ventas where NET_AMT = 0 GROUP BY rtl_loc_id ORDER BY countRtllocId DESC"



Sales <- dbGetQuery(connect, querySales)
Category <- dbGetQuery(connect, queryCategory)
CategoryReturn <- dbGetQuery(connect, queryCategoryReturn)
LocalRoyalties <- dbGetQuery(connect, queryCategoryRoyalties)
LocalReturn <- dbGetQuery(connect, queryLocalReturn)
CategoryRoyalties <- dbGetQuery(connect, queryLocalRoyalties)

#dbDisconnect(connect)

orderColumnDesc <- function(dataFrame, keepNameColumn, orderNameColumn){
  orderedDataframeSales <- dataFrame %>% select_(keepNameColumn, orderNameColumn) %>% arrange(desc(.[[orderNameColumn]]))
  return(orderedDataframeSales)
}


graphBars <- function(dataFrame, keepNameColumn, orderNameColumn, title){
  moreImportant <- dataFrame%>%head(10)
  
  graphImportat <- moreImportant%>%
    ggplot(aes_string(x=keepNameColumn, y=orderNameColumn)) +
    geom_bar(stat = "identity")+labs(title = title)
  
  tableMoreImportat <- gtable_combine(tableGrob(moreImportant),along=1)
  grid.arrange(tableMoreImportat, graphImportat, ncol=2, nrow =1)
  
}

#Tienda con mayor venta/productos 
totalMoneyRaised <- orderColumnDesc(Sales, "RTL_LOC_ID", "TOTALMONEY")
totalProductRaised <- Sales %>%select(RTL_LOC_ID, TOTALPRODUCTSSOLD)

graphBars(totalMoneyRaised, "RTL_LOC_ID", "TOTALMONEY", "More money in sales")
graphBars(totalProductRaised, "RTL_LOC_ID", "TOTALPRODUCTSSOLD", "More product in sales")

#Categoria con mayor venta/productos
totalMoneyCategory <- orderColumnDesc(Category, "DEPT_NAME", "SUMNERAMT")
totalProductCategory <- Category %>%select(DEPT_NAME, SUMQUANTITY)

graphBars(totalMoneyCategory, "DEPT_NAME", "SUMNERAMT", "More money in category")
graphBars(totalProductCategory, "DEPT_NAME", "SUMQUANTITY", "More product in category")

#Categoria con mayor retorno venta/productos
categoryMostReturns <- orderColumnDesc(CategoryReturn, "DEPT_NAME", "SUMQUANTITY")
categoryMostReturnsQuantity <- CategoryReturn%>%select(DEPT_NAME, SUMNETAMT)

graphBars(categoryMostReturns, "DEPT_NAME", "SUMQUANTITY", "Category with the most returns(product)")
graphBars(categoryMostReturnsQuantity, "DEPT_NAME", "SUMNETAMT", "Category with the most returns(money)")

#Local con mayor retorno venta/productos
localMostReturns <- orderColumnDesc(LocalReturn, "RTL_LOC_ID", "SUMQUANTITY")
localMostReturnsQuantity <- LocalReturn%>%select(RTL_LOC_ID, SUMNETAMT)

graphBars(localMostReturns, "RTL_LOC_ID", "SUMQUANTITY", "Local with higher returns(quantity)")
graphBars(localMostReturnsQuantity, "RTL_LOC_ID", "SUMNETAMT", "Local with higher returns(money)")

#Regalias por local
graphBars(LocalRoyalties, "RTL_LOC_ID", "COUNTRTLLOCID", "Local with higher royalties")

#Regalias por categoria
graphBars(CategoryRoyalties, "DEPT_NAME", "COUNTQUANTY", "Category with higher royalties")

dataClean %>%head(100)%>%
  count(DEPT_NAME, sort = TRUE)  %>%
  mutate(DEPT_NAME = fct_reorder(DEPT_NAME, n)) %>%   
  ggplot(aes(x = DEPT_NAME, y = n, lable = n)) +
  geom_col() + 
  #geom_text(hjust = 1, col = "white") +
  coord_flip() + 
  labs(tittle = 'Cantidad de Departamentos',
       x = "DEPT_NAME",
       y = "Frecuencia Absoluta",
       caption = "Datos de Venta ERP") 


Perc_redondeado <- function(x){
  paste(round(x,3)*100, "%", sep = "")
  
}

#Datos acumulados
dataClean %>%head(100)%>%
  count(DEPT_NAME, sort = TRUE)  %>%  
  mutate(perc = n /sum(n),
         perc_acum = cumsum(perc),
         DEPT_NAME = fct_reorder(DEPT_NAME, -n)) %>% #view()
  
ggplot(aes(DEPT_NAME, perc)) +
  geom_col() + 
  geom_point(aes(x = DEPT_NAME , y = perc_acum, group = 1)) + 
  geom_line(aes(x = DEPT_NAME , y = perc_acum, group = 1)) +
  geom_text(aes(x = DEPT_NAME , y = perc_acum, group = 1, label = Perc_redondeado(perc_acum)),
            vjust = -1, angle = 30, size = 2 ) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(tittle = 'Pareto de ventas por Departamnto',
       x = "DEPT_NAME",
       y = "Frecuencia relativa",
       caption = "Datos de Venta ERP")  #%>% view()






