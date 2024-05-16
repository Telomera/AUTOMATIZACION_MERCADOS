rm(list= ls())
# dev.off()
gc()
cat("\014")
options(scipen=999)

package <- function(x){
  if(!(require(x, character.only=TRUE))){
    install.packages(paste(x))}
  require(x, character.only = T)
  
}
## PAQUETES de R ####


library(dplyr)
library(reshape2)
package("lubridate")
package("data.table")
package("openxlsx")
package("stringr")


## PARAMETROS ####

user = "YesikaDíazRodriguez"



mes <- as.Date(paste0(year(Sys.Date()), "-", format(Sys.Date() %m-% months(1), "%m"),"-01"))
mes_texto <- gsub("\\.","",str_to_title(format(mes, "%b")))
mes_actual <- as.Date(paste0(year(Sys.Date()), "-", format(Sys.Date(), "%m"),"-01"))

trim <- as.Date(paste0(year(Sys.Date()), "-", format(Sys.Date() %m-% months(3), "%m"),"-01"))
tam1 <- as.Date(paste0(year(Sys.Date())-1, "-", format(Sys.Date() %m-% months(12), "%m"),"-01"))
tam2 <- as.Date(paste0(year(Sys.Date())-2, "-", format(Sys.Date() %m-% months(12), "%m"),"-01"))


## RUTAS ####
path <- paste0("C:/Users/",user,"/Telomera S.L/AUTOMATIZACION MERCADOS - General/IPCSK9/")
output <- paste0(path, "output")
input <- paste0(path, "input")




## Leemos el fichero ####
list.files(input)

d_raw <- fread("Data/RX_IPCSK9 2305_2404.csv")
d <- fread("Data/RX_IPCSK9 2305_2404.csv")
d2_raw <- fread("Data/RX_IPCSK9_SWFROM 2305_2404.csv")
d2 <- fread("Data/RX_IPCSK9_SWFROM 2305_2404.csv")

names(d) <- tolower(names(d))
names(d2) <- tolower(names(d2))



## añadimos los switchs FROM

d2 <- d2[,.(pat_id, esp, con_date = `con_date+`, mast_prd_name = `mast_prd_name-`, recod_prd = `recod_prd-`, status = "SWITCHES FROM", tam)]

d <- rbind(d, d2, fill = T)

## Arreglamos fechas para poder hacer filtros

d[, con_date := as.Date(as.character(con_date), "%d/%m/%Y")]
d[, con_date_end := as.Date(as.character(con_date_end), "%d/%m/%Y")]

if(d[is.na(con_date),.N] > 0){warning(paste0("Tenemos ",d[is.na(con_date),.N]," registros sin fecha 'con_date'"))}


# d_raw[TAM == "TAMAbr24", uniqueN(pat_id) ]
# vec1 <- d[con_date >= as.Date("2023-05-01") & con_date < as.Date("2024-05-01"), pat_id]
# d[con_date >= as.Date("2023-05-01") & con_date < as.Date("2024-05-01"), uniqueN(pat_id)]
# 
# 
# vec2 <- d[tam == "TAMAbr24", pat_id]
# d[tam == "TAMAbr24", uniqueN(pat_id)]
# 
# setdiff(vec1, vec2)
# setdiff(vec2, vec1)
# # summary(d[tam == "TAMAbr24"]$con_date)




## ABANDONO ####

{
  d[, abandono := gsub("ene", "01", abandono)]
  d[, abandono := gsub("feb", "02", abandono)]
  d[, abandono := gsub("mar", "03", abandono)]
  d[, abandono := gsub("abr", "04", abandono)]
  d[, abandono := gsub("may", "05", abandono)]
  d[, abandono := gsub("jun", "06", abandono)]
  d[, abandono := gsub("jul", "07", abandono)]
  d[, abandono := gsub("ago", "08", abandono)]
  d[, abandono := gsub("sep", "09", abandono)]
  d[, abandono := gsub("oct", "10", abandono)]
  d[, abandono := gsub("nov", "11", abandono)]
  d[, abandono := gsub("dic", "12", abandono)]
}


d[, abandono := as.Date(paste0("01-", abandono), format = "%d-%m-%y")]
d[abandono != "",.N, abandono]
summary(d$abandono)

aban0 <- d[con_date >= trim & con_date < mes_actual & abandono >= trim & abandono < mes_actual]  ## abandono trim
aban0[, `:=` (status = "ABANDONO", tam = "TRIM")]

aban1 <- d[con_date >= tam1 & con_date < mes_actual & abandono >= tam1 & abandono < mes_actual]  ## abandono TAM1
summary(aban1$abandono)
aban1[, `:=` (status = "ABANDONO", tam = paste0("TAM",mes_texto,format(mes_actual,"%y")))]

aban2 <- d[con_date >= tam2 & con_date < tam1 & abandono >= tam2 & abandono < tam1]  ## abandono TAM2
summary(aban2$abandono)
aban2[, `:=` (status = "ABANDONO", tam = paste0("TAM",mes_texto,format(tam1,"%y")))]

d <- rbind(d, aban0, aban1, aban2)



## MOLECULA ####

d_mas <- copy(d)
d_prd <- copy(d)

d_mas[,mast_prd_name := NULL]
d_prd[,recod_prd := NULL]


setnames(d_prd, 'mast_prd_name', 'recod_prd')

d <- rbind(d_mas, d_prd)


## STATUS ####

d[,.N, status]
d_dynamic <- d[status %in% c("SWITCHES TO","NUEVO PACIENTE")]
d_dynamic[, status := "DYNAMIC"]

d <- rbind(d, d_dynamic)

d[,.N, status]




### TAM ####
##### ESCENARIOS POR TAM Y MES ACTUAL
## los TAM ya estan calculados, ahora hacer el TRIM

# d_trim <- d[(con_date >= trim & con_date < mes_actual) | abandono >= trim & abandono < mes_actual,]
d_trim <- d[(con_date >= trim & con_date < mes_actual),]
d_trim[, tam :=  "TRIM"]


# d_tam1 <- d[(con_date >= tam1 & con_date < mes_actual) | (abandono >= tam1 & abandono < mes_actual), ]
d_tam1 <- d[(con_date >= tam1 & con_date < mes_actual), ]
d_tam1[, tam := paste0("TAM",mes_texto,format(mes_actual,"%y"))]

# d_tam2 <- d[(con_date >= tam2 & con_date < tam1) | (abandono >= tam2 & abandono < tam1), ]
d_tam2 <- d[(con_date >= tam2 & con_date < tam1), ]
d_tam2[, tam := paste0("TAM",mes_texto,format(tam1,"%y"))]

d <- rbind(d_trim, d_tam1, d_tam2)







## ARREGLAMOS ALGUNAS VARIABLES
d[, esp := ifelse(esp %in% c("CAR","END","MIV","NEF","NRL"), esp, "RESTO")]
d[,.N, esp]

## los que no tienen status de hecho es porque son fuera de periodo, asi que los eliminamos
## mejor eliminamos los que no tienen TAM que son más
d <- d[tam != ""]

# d <- d[status != ""]

a <- c(unique(d$esp))
b <- c(unique(d$status))
c <- c(unique(d$recod_prd))
e <- c(unique(d$tam, na.rm = T))


txt <- expand.grid(a,b,c, e)


## SIN TOTALES
for (i in 1:nrow(txt)){
  txt[i,5] <- d[esp == txt[i,1] & status == txt[i,2] & recod_prd == txt[i,3] & tam == txt[i,4], uniqueN(pat_id)]
}


### TOTALES INDIVIDUALES ####
### TOTAL Primera variable
txt1 <- expand.grid("TOTAL ESP",b,c, e)
for (i in 1:nrow(txt1)){
  txt1[i,5] <- d[status == txt1[i,2] & recod_prd == txt1[i,3] & tam == txt1[i,4], uniqueN(pat_id)]
}

### TOTAL Segunda variable
txt2 <- expand.grid(a,"Total Patients",c, e)
for (i in 1:nrow(txt2)){
  txt2[i,5] <- d[esp == txt2[i,1] & recod_prd == txt2[i,3] & tam == txt2[i,4], uniqueN(pat_id)]
}

### TOTAL Tercera variable
txt3 <- expand.grid(a,b, "TOTAL", e)
for (i in 1:nrow(txt3)){
  txt3[i,5] <- d[esp == txt3[i,1] & status == txt3[i,2] & tam == txt3[i,4] , uniqueN(pat_id)]
}




### TOTALES POR PAREJAS ####
### TOTAL Primera y Segunda variable
txt12 <- expand.grid("TOTAL ESP","Total Patients", c, e)
for (i in 1:nrow(txt12)){
  txt12[i,5] <- d[recod_prd == txt12[i,3] & tam == txt12[i,4] , uniqueN(pat_id)]
}

### TOTAL Primera y Tercera variable
txt13 <- expand.grid("TOTAL ESP",b, "TOTAL", e)
for (i in 1:nrow(txt13)){
  txt13[i,5] <- d[status == txt13[i,2] & tam == txt13[i,4], uniqueN(pat_id)]
}

### TOTAL Segunda y Tercera variable
txt23 <- expand.grid(a,"Total Patients", "TOTAL", e)
for (i in 1:nrow(txt23)){
  txt23[i,5] <- d[esp == txt23[i,1] & tam == txt23[i,4] , uniqueN(pat_id)]
}



### TOTALES POR TRIOS ####
### TOTAL Primera Segunda Tercera variable

txt123 <- expand.grid("TOTAL ESP","Total Patients", "TOTAL", e)
for (i in 1:nrow(txt123)){
  txt123[i,5] <- d[tam == txt123[i,4], uniqueN(pat_id)]
}
names(txt)



### JUNtamOS LOS RESULTADOS ####
total_txt <- setDT(Reduce(function(...) rbind(..., fill = T), mget(ls(pattern = "^txt"))))
## quitamos unas filas de NA que se generan y los registros que tienen 0
total_txt <- total_txt[!is.na(Var1) & V5 > 0,]

total_txt[,.N, Var4]

names(total_txt) <- c("Var2", "Var3", "Var4", "Var1", "Contador_r")

## comprobacion
# total_txt2 <- rbind(txt, txt1, txt2, txt3, txt13,txt23, txt12, txt123)
# total_txt
# 
# 
# diferencias <- setDT(merge(total_txt, total_txt2, by = c("Var1", "Var2", "Var3", "Var4"), suffixes = c("reduce", "rbind"), all = T))
# diferencias[is.na(V5rbind)]
# diferencias[V5rbind != V5reduce]

total_txt[Var1 == "TRIM", Var1 := "Feb-Abr24"]

total_txt <- total_txt[Var3 != "REPETICION"]


out_previo <- setDT(read.xlsx("out/out_abril24.xlsx", "Hoja1"))

out_previo[, c("X7", "X8", "X9") := NULL]

diferencias <- setDT(merge(total_txt, out_previo, by = c("Var1", "Var2", "Var3", "Var4"), all = T))


dif <- diferencias[Contador_r != Contador | is.na(Contador_r) | is.na(Contador)]
igual <- diferencias[Contador_r == Contador]

# dif[,.N, Var1]
# dif[,.N, Var2]
# dif[,.N, Var3]
# dif[,.N, Var4]
# 
# igual[,.N, Var1]
# igual[,.N, Var2]
# igual[,.N, Var3]
# igual[,.N, Var4]

#    Var1      Var2           Var3         Var4     Contador_r                             Concatenado Contador
# TAMAbr24 TOTAL ESP Total Patients        TOTAL        735        TOTAL ESPTotal PatientsTOTALTAMAbr24      714

# d[tam == "TAMAbr24", uniqueN(pat_id)]
# d[con_date >= tam1 & con_date < mes_actual, uniqueN(pat_id)]

# Feb-Abr24       CAR       DYNAMIC     PRALUENT        125               CARDYNAMICPRALUENTFeb-Abr24       13

d[tam == "TRIM" & esp == "CAR" & recod_prd == "PRALUENT", uniqueN(pat_id), status]



# Feb-Abr24       CAR  SWITCHES FROM     PRALUENT          2          CARSWITCHES FROMPRALUENTFeb-Abr24        3



# d2[pat_id %in% c("308526", "438805", "134702342")]
# d[pat_id %in% c("308526", "438805", "134702342") & status == "SWITCHES FROM" & tam == "TRIM",.N, pat_id]

# 25:  TAMAbr23       END       ABANDONO PRALUENT 150          2              ENDABANDONOPRALUENT 150TAMAbr23        1
d[tam == "TRIM" & esp == "END" & recod_prd == "PRALUENT 150" & status == "ABANDONO",]

names(d)
