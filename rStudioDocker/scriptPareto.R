library(RJDBC)
#driver <- JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = "/home/david/kitematic/ojdbc8.jar")
drive <- JDBC("oracle.jdbc.OracleDriver", classPath="/home/david/kitematic/ojdbc8.jarr")
connect <- dbConnect(drive,"jdbc:oracle:thin:@//172.17.0.2:1521/xe", "sys as sysdba", "mysecurepassword")


