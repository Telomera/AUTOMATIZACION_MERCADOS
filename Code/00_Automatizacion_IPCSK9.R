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


## PARAMETROS ####

user = "YesikaDíazRodriguez"


## RUTAS ####
path <- paste0("C:/Users/",user,"/Telomera S.L/AUTOMATIZACION MERCADOS - General/IPCSK9/")
output <- paste0(path, "output")
input <- paste0(path, "input")

## PAQUETES de R ####

package("data.table")
package("openxlsx")
library(dplyr)
library(reshape2)


## Leemos el fichero ####
list.files(input)

d <- fread("Data/RX_IPCSK9 2305_2404.csv", )
d2 <- fread("Data/RX_IPCSK9_SWFROM 2305_2404.csv", )

d[, con_date := as.Date(as.character(con_date), "%d/%m/%Y")]
d[, con_date_end := as.Date(as.character(con_date_end), "%d/%m/%Y")]

if(d[is.na(con_date) | is.na(con_date_end),.N] > 0){warning(paste0("Tenemos ",d[is.na(con_date),.N]," registros sin fecha 'con_date' y ",d[is.na(con_date_end),.N]," sin fecha 'con_date_end'"))}


## Contadores ####
# Feb-Abr24	TOTAL ESP	Total Patients	TOTAL	        638
# Feb-Abr24	TOTAL ESP	Total Patients	PRALUENT	    359
# Feb-Abr24	TOTAL ESP	Total Patients	REPATHA	      282
# Feb-Abr24	TOTAL ESP	Total Patients	PRALUENT 150	122
# Feb-Abr24	TOTAL ESP	Total Patients	PRALUENT 300	77
# Feb-Abr24	TOTAL ESP	Total Patients	PRALUENT 75	  165
# Feb-Abr24	TOTAL ESP	Total Patients	REPATHA 140	  282

## 620 lineas


### FILTRO STATUS NOT REP = DYNAMIC

d[con_date >= as.Date('2024-01-01') & con_date <= as.Date('2024-04-30'),uniqueN(pat_id)]

d[con_date >= as.Date('2024-01-01') & con_date <= as.Date('2024-04-30') & ESP == "CAR",.N]
summary(d)


## Primero creamos el doc en blanco sin contadores
d[, ESP2 := ifelse(ESP %in% c("CAR","END","MIV","NEF","NRL"), ESP, "RESTO")]
d[, STATUS2 := ifelse(STATUS != "REPETICION", STATUS, "DYNAMIC")]

d[con_date >= as.Date('2024-01-01') & con_date <= as.Date('2024-04-30'), month := "Mes actual"]
d[,.N, .(ESP, ESP2)]



a <- c( unique(d$ESP2))
b <- c( unique(d[STATUS2 != ""]$STATUS2), "ABANDONO")
c <- c(unique(d$RECOD_PRD), unique(d$MAST_PRD_NAME))
e <- c(unique(d[TAM != ""]$TAM, na.rm = T), "Mes actual")


txt <- expand.grid(a,b,c, e)



## primera variable
d[,.N, ESP]

## SEgunda variable
d[,.N, STATUS]

## TERCERA Variable
d[,.N, MAST_PRD_NAME]

## cuarta Variable
d[,.N, RECOD_PRD]
d[,.N, ABANDONO]




## funciones

# txt <- setDT(txt)

# txt[Var1 == "TOTAL" & Var2 == "TOTAL" & Var3 == "TOTAL"]


# txt1 <- txt[!(Var1 == "TOTAL" & Var2 == "TOTAL" & Var3 == "TOTAL")]

{
d[, ABANDONO := gsub("ene", "01", ABANDONO)]
d[, ABANDONO := gsub("feb", "02", ABANDONO)]
d[, ABANDONO := gsub("mar", "03", ABANDONO)]
d[, ABANDONO := gsub("abr", "04", ABANDONO)]
d[, ABANDONO := gsub("may", "05", ABANDONO)]
d[, ABANDONO := gsub("jun", "06", ABANDONO)]
d[, ABANDONO := gsub("jul", "07", ABANDONO)]
d[, ABANDONO := gsub("ago", "08", ABANDONO)]
d[, ABANDONO := gsub("sep", "09", ABANDONO)]
d[, ABANDONO := gsub("oct", "10", ABANDONO)]
d[, ABANDONO := gsub("nov", "11", ABANDONO)]
d[, ABANDONO := gsub("dic", "12", ABANDONO)]
}


d[, ABANDONO := as.Date(paste0("01-", ABANDONO), format = "%d-%m-%y")]
d[ABANDONO != "",.N, ABANDONO]
summary(d$ABANDONO)


# d[, ABANDONO_TAM1 := ]
# d[, ABANDONO_TAM2 := ]
# d[, ABANDONO_MES := ]


### PONEMOS LAS BBDD EN FILA con RBIND aunque esten duplicadas














## SIN TOTALES
for (i in 1:nrow(txt)){
  txt[i,5] <- d[ESP2 == txt[i,1] & STATUS == txt[i,2] & (RECOD_PRD == txt[i,3] | MAST_PRD_NAME == txt[i,3]) & (TAM == txt[i,4] | month == txt[i,4]), uniqueN(pat_id)]
}


### TOTALES INDIVIDUALES ####
### TOTAL Primera variable
txt1 <- expand.grid("TOTAL",b,c, e)
for (i in 1:nrow(txt1)){
  txt1[i,5] <- d[STATUS == txt1[i,2] & (RECOD_PRD == txt1[i,3] | MAST_PRD_NAME == txt1[i,3]) & (TAM == txt1[i,4] | month == txt1[i,4]), uniqueN(pat_id)]
}

### TOTAL Segunda variable
txt2 <- expand.grid(a,"TOTAL",c, e)
for (i in 1:nrow(txt2)){
  txt2[i,5] <- d[ESP2 == txt2[i,1] & (RECOD_PRD == txt2[i,3] | MAST_PRD_NAME == txt2[i,3]) & (TAM == txt2[i,4] | month == txt2[i,4]), uniqueN(pat_id)]
}

### TOTAL Tercera variable
txt3 <- expand.grid(a,b, "TOTAL", e)
for (i in 1:nrow(txt3)){
  txt3[i,5] <- d[ESP2 == txt3[i,1] & STATUS == txt3[i,2] & (TAM == txt3[i,4] | month == txt3[i,4]), uniqueN(pat_id)]
}

### TOTAL Primera y Segunda variable
txt4 <- expand.grid(a,b, "TOTAL", e)
for (i in 1:nrow(txt4)){
  txt4[i,5] <- d[ESP2 == txt4[i,1] & STATUS == txt4[i,2] & (TAM == txt4[i,4] | month == txt4[i,4]), uniqueN(pat_id)]
}

### TOTALES POR GRUPOS ####
### TOTAL Primera y Tercera variable
txt13 <- expand.grid("TOTAL",b, "TOTAL", e)
for (i in 1:nrow(txt13)){
  txt13[i,5] <- d[STATUS == txt13[i,2] & (TAM == txt13[i,4] | month == txt13[i,4]), uniqueN(pat_id)]
}


### TOTAL Segunda y Tercera variable
txt23 <- expand.grid(a,"TOTAL", "TOTAL", e)
for (i in 1:nrow(txt23)){
  txt23[i,5] <- d[ESP2 == txt23[i,1] & (TAM == txt23[i,4] | month == txt23[i,4]), uniqueN(pat_id)]
}

### TOTAL Primera y Segunda variable
txt12 <- expand.grid("TOTAL","TOTAL", c, e)
for (i in 1:nrow(txt12)){
  txt12[i,5] <- d[ESP2 == txt12[i,1] & STATUS == txt12[i,2] & (TAM == txt12[i,4] | month == txt12[i,4]), uniqueN(pat_id)]
}


### TOTALES POR TRIOS ####
### TOTAL Primera y Tercera variable

txt123 <- expand.grid("TOTAL","TOTAL", "TOTAL", e)
for (i in 1:nrow(txt123)){
  txt123[i,5] <- d[(TAM == txt123[i,4] | month == txt123[i,4]), uniqueN(pat_id)]
}
names(txt)



### JUNTAMOS LOS RESULTADOS ####
total_txt <- Reduce(function(...) rbind(..., fill = T), mget(ls(pattern = "txt")))

rbind(txt, txt1, txt2, txt3, txt13,txt4, txt12, txt123)





