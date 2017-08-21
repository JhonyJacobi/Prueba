rm(list = ls())

setwd("D:/Mining/Negocios Fijo/Contactabilidad/01. Modelo BTTC/04. Modelos")
#getwd()

# Conección ODBC
con_ODBC <- odbcConnect(dsn="SB_MIN",uid="JJACOBIR",pwd="cobijja") 

Library(rpart)

i <- 0

for (i in 1:4) {
    
  query <- paste0("SELECT * FROM JJR_REPLICA_CONTACTO_R",i)
  
  data <- sqlQuery(con_ODBC, as.is = F, query, believeNRows=FALSE)
  data <- filter(data,ZONAS!='')
  
  data$PERIODO <-  as.Date.character(data$PERIODO)
  data$DUR_LLAMADA_SAL_M1 <-  as.numeric(data$DUR_LLAMADA_SAL_M1)
  data$DUR_LLAMADA_SAL_M2 <-  as.numeric(data$DUR_LLAMADA_SAL_M2)
  data$DUR_LLAMADA_SAL_M3 <-  as.numeric(data$DUR_LLAMADA_SAL_M3)
  data$DUR_LLAMADA_ENT_M1 <-  as.numeric(data$DUR_LLAMADA_ENT_M1)
  data$DUR_LLAMADA_ENT_M2 <-  as.numeric(data$DUR_LLAMADA_ENT_M2)
  data$DUR_LLAMADA_ENT_M3 <-  as.numeric(data$DUR_LLAMADA_ENT_M3)
  data$DESTINOS_OUT_M1 <-  as.integer(data$DESTINOS_OUT_M1)
  data$DESTINOS_OUT_M2 <-  as.integer(data$DESTINOS_OUT_M2)
  data$DESTINOS_OUT_M3 <-  as.integer(data$DESTINOS_OUT_M3)
  data$DESTINOS_IN_M1 <-  as.integer(data$DESTINOS_IN_M1)
  data$DESTINOS_IN_M2 <-  as.integer(data$DESTINOS_IN_M2)
  data$DESTINOS_IN_M3 <-  as.integer(data$DESTINOS_IN_M3)
  data$DUR_LLAMADA_ENT_COMP_M1 <-  as.numeric(data$DUR_LLAMADA_ENT_COMP_M1)
  data$DUR_LLAMADA_ENT_COMP_M2 <-  as.numeric(data$DUR_LLAMADA_ENT_COMP_M2)
  data$DESTINOS_IN_COMP_M1 <-  as.numeric(data$DESTINOS_IN_COMP_M1)
  data$DESTINOS_IN_COMP_M2 <-  as.numeric(data$DESTINOS_IN_COMP_M2)
  data$CABLE_TENENCIA <-  as.factor(data$CABLE_TENENCIA)
  data$MACROSEGMENTO <-  as.factor(data$MACROSEGMENTO)
  data$CLUSTER_ <-  as.factor(data$CLUSTER_)
  data$TRIODUO <-  as.factor(data$TRIODUO)
  data$ANTIGUEDAD_LINEA <-  as.integer(data$ANTIGUEDAD_LINEA)
  data$ZONAS <-  factor(data$ZONAS)
  data$FREQ_ENVIO <-  as.integer(data$FREQ_ENVIO)
  
  data[,"TRAFICO_SAL"] <- apply(data[,3:5], 1, sum, na.rm = T)/3
  data[,"TRAFICO_ENT"] <- apply(data[,6:8], 1, sum, na.rm = T)/3
  data[,"DEST_OUT"] <- apply(data[,9:11], 1, sum, na.rm = T)/3
  data[,"DEST_IN"] <- apply(data[,12:14], 1, sum, na.rm = T)/3
  data[,"TRAFICO_ENT_MOV"] <- apply(data[,15:16], 1, sum, na.rm = T)/3
  data[,"DEST_IN_MOV"] <- apply(data[,17:18], 1, sum, na.rm = T)/3
  data$TRAFICO_SAL <- replace(data$TRAFICO_SAL,NA,0)
  data$TRAFICO_ENT <- replace(data$TRAFICO_ENT,NA,0)
  data$DEST_OUT <- replace(data$DEST_OUT,NA,0)
  data$DEST_IN <- replace(data$DEST_IN,NA,0)
  data$TRAFICO_ENT_MOV <- replace(data$TRAFICO_ENT_MOV,NA,0)
  data$DEST_IN_MOV <- replace(data$DEST_IN_MOV,NA,0)
  
  attach(data)
  data$FLAG_TRAFICO_SAL[TRAFICO_SAL == 0] <- "0"
  data$FLAG_TRAFICO_SAL[TRAFICO_SAL != 0] <- "1"
  
  data$FLAG_TRAFICO_ENT[TRAFICO_ENT == 0] <- "0"
  data$FLAG_TRAFICO_ENT[TRAFICO_ENT != 0] <- "1"
  
  data$FLAG_TRAFICO_ENT_MOV[TRAFICO_ENT_MOV == 0] <- "0"
  data$FLAG_TRAFICO_ENT_MOV[TRAFICO_ENT_MOV != 0] <- "1"
  
  data$FLAG_TRAFICO_SAL <- as.factor(data$FLAG_TRAFICO_SAL)
  data$FLAG_TRAFICO_ENT <- as.factor(data$FLAG_TRAFICO_ENT)
  data$FLAG_TRAFICO_ENT_MOV <- as.factor(data$FLAG_TRAFICO_ENT_MOV)
  detach(data)
  
  if (i == 1) {
    # Grupo de contacto R1
    q_TRAF_SAL = c(-Inf,0,0.4,1.27,2.6,4.67,7.87,13.4,25.83,Inf)
    q_TRAF_ENT = c(-Inf,0,0.06,0.43,1.2,2.83,6.3,14.9,Inf)
    q_TRAF_ENT_MOV = c(-Inf,0,0.13,2,5.73,11.03,18.2,27.73,41.13,62.1,103.57,Inf)  
  }
  
  if (i == 2) { 
    # Grupo de contacto R2
    q_TRAF_SAL <-c(-Inf,0,0.2,1.23,2.93,5.4,8.97,14.4,23.6,43.2,Inf)
    q_TRAF_ENT <-c(-Inf,0,0.06,0.43,1.27,2.93,5.97,11.63,24.7,Inf)
    q_TRAF_ENT_MOV <-c(-Inf,0,0.1,1.833,5.47,10.67,17.57,26.83,39.87,60.07,100.3,Inf)
  }
  
  if (i == 3) {
    # Grupo de contacto R3
    q_TRAF_SAL <-c(-Inf,0,0.17,1.07,2.5,4.6,7.57,11.97,19.17,33.83,Inf)
    q_TRAF_ENT <-c(-Inf,0,0.33,1,2.27,4.53,8.73,18.17,Inf)
    q_TRAF_ENT_MOV <-c(-Inf,0,0.17,2.17,6.1,11.57,18.73,28.23,41.4,61.93,102.3,Inf)
  }
  
  if (i == 4) {
    # Grupo de contacto R4
    q_TRAF_SAL <-c(-Inf,0,0.73,2.63,5.47,9.4,14.93,23.17,36.67,64.2,Inf)
    q_TRAF_ENT <-c(-Inf,0,0.3,1.13,2.83,5.83,10.83,19.5,38.37,Inf)
    q_TRAF_ENT_MOV <-c(-Inf,0,0.23,2.43,6.47,12,19.17,28.63,41.73,62.17,102.5,Inf)
  }
  
  data$CAT_TRAFICO_SAL <-cut(data$TRAFICO_SAL,unique(q_TRAF_SAL),include.lowest=TRUE)
  data$CAT_TRAFICO_ENT <-cut(data$TRAFICO_ENT,unique(q_TRAF_ENT),include.lowest=TRUE)
  data$CAT_TRAFICO_ENT_MOV <-cut(data$TRAFICO_ENT_MOV,unique(q_TRAF_ENT_MOV),include.lowest=TRUE)
  
  # Identificar las variables independientes
  
  imp <- c(1:2,19:39)
  data <- data[,imp]
  
  #colSums(is.na(data))
  data<-na.omit(data)
  
  PERIODO <- data$PERIODO
  TELEFONO <- data$TELEFONO
  
  data$PERIODO <- NULL
  data$TELEFONO <- NULL
  
  set.seed(123)
  
  # Cargar el modelo
  modelo_file_name <- paste0("modelo_r",i,".RData")
  load(file = modelo_file_name)
  
  Modelo.RF.predictions<-predict(Modelo.RF,newdata=data,type="prob")
  PERIODO <- PERIODO %m+% months(2)
  score_cont <- data.frame(PERIODO,TELEFONO,PROB_CONT=Modelo.RF.predictions[,2])
  
  score_file_name <- paste0("score_r",i,".csv")
  write.csv(score_cont,file = score_file_name,row.names = F,quote=F)
}

?print



