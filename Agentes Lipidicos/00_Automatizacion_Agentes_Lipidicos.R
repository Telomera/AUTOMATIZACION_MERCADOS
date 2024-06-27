rm(list= ls())
gc()
cat("\014")
options(scipen=999)

### PAQUETES DE R ####

package <- function(x){
  if(!(require(x, character.only=TRUE))){
    install.packages(paste(x))}
  require(x, character.only = T)
  
}

package("data.table")
package("lubridate")    ## para restar meses cómodamente
package("stringr")      ## para poder poner los formatos de meses correspondientes
package("rstudioapi")   ## showPrompt
package("readxl")       ## excel_sheets
package("openxlsx")     ## para leer el excel



## PARAMETROS ####
path <- "../Agentes lipidicos/"

mes <- as.Date(paste0(year(Sys.Date()), "-", format(Sys.Date() %m-% months(1), "%m"),"-01"))
mes_actual <- as.Date(paste0(year(Sys.Date()), "-", format(Sys.Date(), "%m"),"-01"))

trim <- as.Date(paste0(year(Sys.Date()), "-", format(Sys.Date() %m-% months(3), "%m"),"-01"))
tam1 <- as.Date(paste0(year(Sys.Date())-1, "-", format(Sys.Date() %m-% months(12), "%m"),"-01"))
tam2 <- as.Date(paste0(year(Sys.Date())-2, "-", format(Sys.Date() %m-% months(12), "%m"),"-01"))

mes_texto <- gsub("\\.","",str_to_title(format(mes, "%b")))
mes_texto_completo <- paste0(str_to_title(format(mes, "%B"))," ",year(mes))
mes_texto0 <- gsub("\\.","",str_to_title(format(trim, "%b")))

year_extract <- format(mes,"%y")
year_extract_long <- year(mes)

## RUTAS ####
path_input <- paste0(path,"Input/",format(mes,"%Y%m"),"/")

file_mercado <- list.files(path_input)[grepl(".xlsx",list.files(path_input))]

if(length(file_mercado) != 1) stop("Revisar los documentos de entrada. Deberían haber solo 1 documento")

(nombres_hojas <- excel_sheets(paste0(path,"Input/",format(mes,"%Y%m"),"/",file_mercado)))
nombres_hojas <- nombres_hojas[!toupper(nombres_hojas) %in% "RESULTADOS"]


## creamos la carpeta en caso de que no exista

path_output <- paste0(path,"Output/",format(mes,"%Y%m"),"/")
ifelse(!dir.exists(path_output), dir.create(path_output), F)

## Leemos el fichero ####

d <- setDT(read.xlsx(paste0(path_input,file_mercado), 
                     sheet = nombres_hojas[!grepl("tx", ignore.case = T, nombres_hojas)], 
                     detectDates = T))
d_raw <- setDT(read.xlsx(paste0(path_input,file_mercado), 
                     sheet = nombres_hojas[!grepl("tx", ignore.case = T, nombres_hojas)], 
                     detectDates = T))


## siempre tiene TX en el nombre
d2 <- setDT(read.xlsx(paste0(path_input,file_mercado), 
                      sheet = nombres_hojas[grepl("tx", ignore.case = T, nombres_hojas)], 
                      detectDates = T))


names(d) <- tolower(names(d))
names(d2) <- tolower(names(d2))


d2_raw <- copy(d2)


if(sum(c("pat_id","esp" , "con_date",  "class", "recod_prd_name", "status", "abandono") %in% names(d)) != 7) {
  stop(paste0("Revisar los nombres del archivo, pestaña: ",nombres_hojas[grepl("tx|switches", ignore.case = T, nombres_hojas)],"\n 
              deberían ser: pat_id, esp, con_date,  class, recod_prd_name,  status, abandono"))
}


if(sum(c("pat_id", "esp" , "con_date","recod_prd_name-" , "class-", "segmento.final") %in% names(d2)) != 6) {
  stop(paste0("Revisar los nombres del archivo, pestaña: ",nombres_hojas[grepl("tx|switches", ignore.case = T, nombres_hojas)],"\n 
              deberían ser: pat_id,esp , con_date, recod_prd_name- , class-, segmento final"))
}


temp <- rstudioapi::showQuestion("AGENTES LIPIDICOS",paste0("Agentes Lipídicos. Mes a ejecutar: ", mes_texto_completo))



## añadimos los switchs FROM
names(d)
names(d2)

d2 <- d2[segmento.final != "OUT",.(pat_id, esp, con_date, recod_prd_name = `recod_prd_name-`, class = `class-`, segmento.final,
            status = "SWITCHES FROM")]
d2 <- d2[!is.na(pat_id)]

d <- rbind(d, d2, fill = T)

# draw0 <- copy(d)

## Arreglamos fechas para poder hacer filtros

if(d[is.na(con_date),.N] > 0){warning(paste0("Tenemos ",d[is.na(con_date),.N]," registros sin fecha 'con_date'"))}

if(format(max(d$con_date),"%b") != format(mes_actual-1,"%b")) stop("Los datos no corresponden con el mes indicado.")



## ABANDONO ####

if(min(as.numeric(d$abandono), na.rm = T) != "Inf"){
  d[, abandono := as.numeric(abandono)]
  d[, abandono := as.Date(abandono, origin = "1899-12-30")]
} else{
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
  d[, abandono := as.Date(paste0("01-", abandono), format = "%d-%m-%y")]
}


d[abandono != "",.N, abandono]
summary(d$abandono)

aban0 <- d[abandono >= trim & abandono < mes_actual]  ## abandono trim
aban0[, `:=` (con_date = as.Date("1999-01-01"), status = "ABANDONOS", tam = "TRIM")]

aban1 <- d[abandono >= tam1 & abandono < mes_actual]  ## abandono TAM1
aban1[,.N, esp]
summary(aban1$abandono)
aban1[, `:=` (con_date = as.Date("1999-01-01"), status = "ABANDONOS", tam = paste0("TAM",mes_texto,format(mes_actual,"%y")))]

aban2 <- d[abandono >= tam2 & abandono < tam1]  ## abandono TAM2
summary(aban2$abandono)
aban2[, `:=` (con_date = as.Date("1999-01-01"), status = "ABANDONOS", tam = paste0("TAM",mes_texto,format(tam1,"%y")))]


## nos quedamos con el ultimo registro de cada id en función del estatus

d_last <- d[order(pat_id, -con_date)][, orden := rleid(con_date), .(pat_id)][orden == 1,][, orden := NULL]
d_last[, periodo := "last"]
d_last_dinamic <- d[order(pat_id, -con_date)][status %in% c("SWITCHES TO", "ADDONS", "NUEVO PACIENTE","SWITCHES FROM"), orden := rleid(con_date), .(pat_id)][orden == 1,][, orden := NULL]
d_last_dinamic[, periodo := "last dinamic"]

# d <- rbind(d, aban0, aban1, aban2, fill = T)
d <- rbind(d_last, d_last_dinamic, aban0, aban1, aban2, fill = T)




# ## MOLECULA ####

d[,.N, .(class, recod_prd_name)]

d_mas <- copy(d)
d_prd <- copy(d)

d_mas[,class := NULL]
d_prd[,recod_prd_name := NULL]


setnames(d_prd, 'class', 'recod_prd_name')

d <- rbind(d_mas, d_prd)







### TAM ####
##### ESCENARIOS POR TAM Y MES ACTUAL
## los TAM ya estan calculados, ahora hacer el TRIM

d_trim <- d[(con_date >= trim & con_date < mes_actual),]
d_trim[, tam :=  "TRIM"]

d_tam1 <- d[(con_date >= tam1 & con_date < mes_actual), ]
d_tam1[, tam := paste0("TAM",mes_texto,format(mes_actual,"%y"))]

d_tam2 <- d[(con_date >= tam2 & con_date < tam1), ]
d_tam2[, tam := paste0("TAM",mes_texto,format(tam1,"%y"))]

d_abandonos <- d[status == "ABANDONOS"]

d <- rbind(d_trim, d_tam1,d_tam2, d_abandonos)



## Para los casos de tipo de paciente switche, nuevo,... solo considerar el ultimo registro

d[,.N, status]
d <- d[order(pat_id, -con_date)]
# d[status %in% c("SWITCHES TO", "ADDONS", "NUEVO PACIENTE"), orden := .GRP, .(pat_id, con_date)]
d[status %in% c("SWITCHES TO", "ADDONS", "NUEVO PACIENTE"), orden := rleid(con_date), .(pat_id)]


d[ orden > 1, ig := 1]

## STATUS ####

d[,.N, status]
d_dynamic <- d[status %in% c("SWITCHES TO","NUEVO PACIENTE","ADDONS")]
d_dynamic[, status := "DYNAMIC"]

d <- rbind(d, d_dynamic)

d[,.N, status]



## ARREGLAMOS ALGUNAS VARIABLES


## los que no tienen status de hecho es porque son fuera de periodo, asi que los eliminamos
## mejor eliminamos los que no tienen TAM que son más
d <- d[tam != ""]
d[,.N, status]
# d <- d[status != ""]
setnames(d, "segmento.final", "segmento_final")

a <- c(unique(d$esp))
b <- c(unique(d$status))
c <- c(unique(d[!is.na(recod_prd_name),]$recod_prd_name))
e <- c(unique(d$tam, na.rm = T))
f <- c(unique(d[!is.na(segmento_final)]$se))


txt <- expand.grid(a,b,c, e,f)


## para poder filtrar los ultimos registros
dfiltr <- d[is.na(ig)]

dfiltr_last1 <- d[order(pat_id, tam, -con_date) & status != "ABANDONOS"]
dfiltr_last1 <- dfiltr_last1[, orden := rleid(con_date), .(pat_id, tam)][orden == 1,]
dfiltr_last2 <- d[order(pat_id, -con_date) & status == "ABANDONOS"]
dfiltr_last <- rbind(dfiltr_last1, dfiltr_last2)

# dfiltr <- dfiltr[valores == 1,]

## SIN TOTALES
for (i in 1:nrow(txt)){
  if(txt[i,2] == "ABANDONOS"){
    # i=1
    txt[i,6] <- dfiltr[esp == txt[i,1] & status == txt[i,2] & recod_prd_name == txt[i,3] & tam == txt[i,4] & segmento_final == txt[i,5], uniqueN(pat_id)]
  }
  if(txt[i,2] != "ABANDONOS"){
    txt[i,6] <- dfiltr[esp == txt[i,1] & status == txt[i,2] & recod_prd_name == txt[i,3] & tam == txt[i,4] & segmento_final == txt[i,5] & status != "ABANDONOS", uniqueN(pat_id)]
  }
}


### TOTALES INDIVIDUALES ####
### TOTAL Primera variable
txt1 <- expand.grid("TOTAL ESP",b,c, e, f)
for (i in 1:nrow(txt1)){
  # i = 166
  if(txt1[i,2] == "ABANDONOS"){
    txt1[i,6] <- dfiltr[status == txt1[i,2] & recod_prd_name == txt1[i,3] & tam == txt1[i,4] & segmento_final == txt1[i,5], uniqueN(pat_id)]
  }
  if(txt1[i,2] != "ABANDONOS"){
    txt1[i,6] <- dfiltr[status == txt1[i,2] & recod_prd_name == txt1[i,3] & tam == txt1[i,4] & status != "ABANDONOS" & segmento_final == txt1[i,5], uniqueN(pat_id)]
  }
}

# d[status == "NUEVO PACIENTE" & recod_prd_name == "REPATHA" & tam == "TRIM" & segmento_final == "H+ECVA", uniqueN(pat_id)]
# txt1[txt1$Var1 == "NUEVO PACIENTE" & txt1$Var2 == "REPATHA" & txt1$Var4 == "TRIM" & txt1$Var5 == "H+ECVA", ]


### TOTAL Segunda variable
txt2 <- expand.grid(a,"Total Patients",c, e, f)
for (i in 1:nrow(txt2)){
  txt2[i,6] <- dfiltr_last[esp == txt2[i,1] & recod_prd_name == txt2[i,3] & tam == txt2[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM") & segmento_final == txt2[i,5], uniqueN(pat_id)]
}

# txt2_v <- expand.grid(a,"Total Patients",c, e, f)
# for (i in 1:nrow(txt2_v)){
#   txt2_v[i,6] <- dfiltr_last[esp == txt2_v[i,1] & recod_prd_name == txt2_v[i,3] & tam == txt2_v[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM") & segmento_final == txt2_v[i,5], uniqueN(pat_id)]
# }
# 
# txt_c <- merge(txt2, txt2_v, by = c("Var1", "Var2", "Var3", "Var4", "Var5"), all = T)
# txt_c <- merge(txt_c, diferencias2, by = c("Var1", "Var2", "Var3", "Var4", "Var5"), all.x = T)

### TOTAL Tercera variable
txt3 <- expand.grid(a,b, "TOTAL", e, f)
for (i in 1:nrow(txt3)){
  # i=1
  if(txt3[i,2] == "ABANDONOS"){
  txt3[i,6] <- dfiltr[esp == txt3[i,1] & status == txt3[i,2] & tam == txt3[i,4] & segmento_final == txt3[i,5] , uniqueN(pat_id)]
  }
  if(txt3[i,2] != "ABANDONOS"){
  txt3[i,6] <- dfiltr[esp == txt3[i,1] & status == txt3[i,2] & tam == txt3[i,4] & status != "ABANDONOS" & segmento_final == txt3[i,5], uniqueN(pat_id)]
  }
}

### TOTAL cuarta variable
txt4 <- expand.grid(a,b, c, e, "Total indicaciones")
for (i in 1:nrow(txt4)){
  # i=1
  if(txt4[i,2] == "ABANDONOS"){
  txt4[i,6] <- dfiltr[esp == txt4[i,1] & status == txt4[i,2] & recod_prd_name == txt4[i,3] & tam == txt4[i,4] , uniqueN(pat_id)]
  }
  if(txt4[i,2] != "ABANDONOS"){
  txt4[i,6] <- dfiltr[esp == txt4[i,1] & status == txt4[i,2] & recod_prd_name == txt4[i,3] & tam == txt4[i,4] & status != "ABANDONOS" , uniqueN(pat_id)]
  }
}




### TOTALES POR PAREJAS ####
### TOTAL Primera y Segunda variable
txt12 <- expand.grid("TOTAL ESP","Total Patients", c, e, f)
for (i in 1:nrow(txt12)){
  txt12[i,6] <- dfiltr_last[recod_prd_name == txt12[i,3] & tam == txt12[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM") & segmento_final == txt12[i,5], uniqueN(pat_id)]
}

# # dfiltr[recod_prd_name == "NUSTENDI" & tam == "TRIM" & segmento_final == "HF"& !status %in% c("ABANDONOS", "SWITCHES FROM"), uniqueN(pat_id)]
# vec <- c(18725,     44212,   367314,   528117,  548848,    698005,   140096846)
# setdiff(dfiltr[recod_prd_name == "NUSTENDI" & tam == "TRIM" & segmento_final == "HF"& !status %in% c("ABANDONOS", "SWITCHES FROM"),]$pat_id,vec)

### TOTAL Primera y Tercera variable
txt13 <- expand.grid("TOTAL ESP",b, "TOTAL", e, f)
for (i in 1:nrow(txt13)){
  if(txt13[i,2] == "ABANDONOS"){
    txt13[i,6] <- dfiltr[status == txt13[i,2] & tam == txt13[i,4] & segmento_final == txt13[i,5], uniqueN(pat_id)]
  }
  if(txt13[i,2] != "ABANDONOS"){
    txt13[i,6] <- dfiltr[status == txt13[i,2] & tam == txt13[i,4] & status != "ABANDONOS" & segmento_final == txt13[i,5], uniqueN(pat_id)]
  }
}

### TOTAL Primera y Cuarta variable
txt14 <- expand.grid("TOTAL ESP",b, c, e, "Total indicaciones")
for (i in 1:nrow(txt14)){
  if(txt14[i,2] == "ABANDONOS"){
    txt14[i,6] <- dfiltr[status == txt14[i,2] & tam == txt14[i,4] &  recod_prd_name == txt14[i,3], uniqueN(pat_id)]
  }
  if(txt14[i,2] != "ABANDONOS"){
    txt14[i,6] <- dfiltr[status == txt14[i,2] & tam == txt14[i,4] & status != "ABANDONOS" & recod_prd_name == txt14[i,3], uniqueN(pat_id)]
  }
}


### TOTAL Segunda y Tercera variable
txt23 <- expand.grid(a,"Total Patients", "TOTAL", e, f)
for (i in 1:nrow(txt23)){
  txt23[i,6] <- dfiltr_last[esp == txt23[i,1] & tam == txt23[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM") & segmento_final == txt23[i,5], uniqueN(pat_id)]
}


### TOTAL Segunda y cuarta variable
txt24 <- expand.grid(a,"Total Patients", c, e, "Total indicaciones")
for (i in 1:nrow(txt24)){
    txt24[i,6] <- dfiltr_last[esp == txt24[i,1] & recod_prd_name == txt24[i,3] & tam == txt24[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM"), uniqueN(pat_id)]
}

### TOTAL Tercera y cuarta variable
txt34 <- expand.grid(a,b, "TOTAL", e, "Total indicaciones")
for (i in 1:nrow(txt34)){
  if(txt34[i,2] == "ABANDONOS"){
    txt34[i,6] <- dfiltr[esp == txt34[i,1] & status == txt34[i,2] & tam == txt34[i,4], uniqueN(pat_id)]
  }
  if(txt34[i,2] != "ABANDONOS"){
    txt34[i,6] <- dfiltr[esp == txt34[i,1] & status == txt34[i,2] & tam == txt34[i,4] & status != "ABANDONOS", uniqueN(pat_id)]
  }
}










### TOTALES POR TRIOS ####
### TOTAL Primera Segunda Tercera variable

txt123 <- expand.grid("TOTAL ESP","Total Patients", "TOTAL", e, f)
for (i in 1:nrow(txt123)){
  txt123[i,6] <- dfiltr_last[tam == txt123[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM") & segmento_final == txt123[i,5], uniqueN(pat_id)]
}


txt234 <- expand.grid(a,"Total Patients", "TOTAL", e, "Total indicaciones")
for (i in 1:nrow(txt234)){
  txt234[i,6] <- dfiltr_last[tam == txt234[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM") & esp == txt234[i,1], uniqueN(pat_id)]
}


txt124 <- expand.grid("TOTAL ESP","Total Patients", c, e, "Total indicaciones")
for (i in 1:nrow(txt124)){
  # i=1
  txt124[i,6] <- dfiltr_last[tam == txt124[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM") & recod_prd_name == txt124[i,3], uniqueN(pat_id)]
}

txt134 <- expand.grid("TOTAL ESP", b, "TOTAL", e, "Total indicaciones")
for (i in 1:nrow(txt134)){
  if(txt134[i,2] == "ABANDONOS"){
    txt134[i,6] <- dfiltr[tam == txt134[i,4] & status == txt134[i,2], uniqueN(pat_id)]
  }
  if(txt134[i,2] != "ABANDONOS"){
    txt134[i,6] <- dfiltr[tam == txt134[i,4] & status == txt134[i,2] & tam == txt134[i,4] & status != "ABANDONOS", uniqueN(pat_id)]
  }
}




### TOTALES POR CUARTETO ####
### TOTAL Primera Segunda Tercera cuarta variable


txt1234 <- expand.grid("TOTAL ESP","Total Patients", "TOTAL", e, "Total indicaciones")
for (i in 1:nrow(txt1234)){
  txt1234[i,6] <- dfiltr_last[tam == txt1234[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM"), uniqueN(pat_id)]
}




### Juntamos LOS RESULTADOS ####
total_txt <- setDT(Reduce(function(...) rbind(..., fill = T), mget(ls(pattern = "^txt"))))

## quitamos unas filas de NA que se generan y los registros que tienen 0
total_txt <- total_txt[!is.na(Var1) & V6 > 0,]


total_txt[,.N, Var4]

names(total_txt) <- c("Var3", "Var4", "Var5", "Var1", "Var2","Contador_r")

{
if(mes_texto %in% c("Mar"))  total_txt[Var1 == "TRIM", Var1 := paste0("Q1 ",year_extract_long)]
if(mes_texto %in% c("Jun"))  total_txt[Var1 == "TRIM", Var1 := paste0("Q2 ",year_extract_long)]
if(mes_texto %in% c("Sep"))  total_txt[Var1 == "TRIM", Var1 := paste0("Q3 ",year_extract_long)]
if(mes_texto %in% c("Dic"))  total_txt[Var1 == "TRIM", Var1 := paste0("Q4 ",year_extract_long)]
if(!mes_texto %in% c("Mar","Jun","Sep","Dic")) total_txt[Var1 == "TRIM", Var1 := paste0(mes_texto0,"-",mes_texto,year_extract)]
}

total_txt <- total_txt[!grepl("^REPETICION",Var4)]
total_txt[, Concatenado := paste0(Var3,Var4,Var2,Var5,Var1)]

total_txt <- total_txt[,.(Concatenado, Var1,Var2,Var3,Var4,Var5, Contador_r)]


## Exportamos los resultados del periodo ####
write.xlsx(total_txt, paste0(path_output,"Agentes_lipidicos_",format(mes,"%Y%m"),".xlsx"))
file_name <- paste0("Agentes_lipidicos_",format(mes,"%Y%m"),".xlsx")
if(paste0("Agentes_lipidicos_",format(mes,"%Y%m"),".xlsx") %in% list.files(path_output)){cat("Se ha exportado correctamente el archivo:\n",file_name)}else{warning("Hay un problema con la exportación")}


