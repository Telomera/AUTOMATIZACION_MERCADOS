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
path <- "../IPCSK9/"
mes <- as.Date(paste0(year(Sys.Date()), "-", format(Sys.Date() %m-% months(1), "%m"),"-01"))
mes_actual <- as.Date(paste0(year(Sys.Date()), "-", format(Sys.Date(), "%m"),"-01"))

trim <- as.Date(paste0(year(Sys.Date()), "-", format(Sys.Date() %m-% months(3), "%m"),"-01"))
tam1 <- as.Date(paste0(year(Sys.Date())-1, "-", format(Sys.Date() %m-% months(12), "%m"),"-01"))
tam2 <- as.Date(paste0(year(Sys.Date())-2, "-", format(Sys.Date() %m-% months(12), "%m"),"-01"))

mes_texto <- gsub("\\.","",str_to_title(format(mes, "%b")))
mes_texto_completo <- paste0(str_to_title(format(mes, "%B"))," ",year(mes))
mes_texto0 <- gsub("\\.","",str_to_title(format(trim, "%b")))

year_extract <- format(mes,"%y")

## RUTAS ####
path_input <- paste0(path,"input/",format(mes,"%Y%m"),"/")
file_mercado <- list.files(path_input)[grepl(".xlsx",list.files(path_input))]

if(length(file_mercado) != 1) stop("Revisar los documentos de entrada. Deberían haber solo 1 documento")

(nombres_hojas <- excel_sheets(paste0(path,"Input/",format(mes,"%Y%m"),"/",file_mercado)))
nombres_hojas <- nombres_hojas[!toupper(nombres_hojas) %in% "RESULTADOS"]


## creamos la carpeta en caso de que no exista

path_output <- paste0(path,"output/",format(mes,"%Y%m"),"/")
ifelse(!dir.exists(path_output), dir.create(path_output), F)

## Leemos los fichero ####
# list.files(path_input)
d <- setDT(read.xlsx(paste0(path_input,file_mercado), 
                     sheet = nombres_hojas[!grepl("tx|switches", ignore.case = T, nombres_hojas)], 
                     detectDates = T))
d_raw <- setDT(read.xlsx(paste0(path_input,file_mercado), 
                         sheet = nombres_hojas[!grepl("tx|switches", ignore.case = T, nombres_hojas)], 
                         detectDates = T))


## siempre tiene TX en el nombre
d2 <- setDT(read.xlsx(paste0(path_input,file_mercado), 
                      sheet = nombres_hojas[grepl("tx|switches", ignore.case = T, nombres_hojas)], 
                      detectDates = T))


names(d) <- tolower(names(d))
names(d2) <- tolower(names(d2))

setnames(d2, "mol_name-", "recod_prd-")




if(sum(c("pat_id","esp" , "con_date",  "mast_prd_name", "recod_prd","tam", "status", "abandono") %in% names(d)) != 8) {
  stop(paste0("Revisar los nombres del archivo, pestaña: ",nombres_hojas[grepl("tx|switches", ignore.case = T, nombres_hojas)],"\n 
              deberían ser: pat_id, esp, con_date, mast_prd_name, recod_prd, tam, status, abandono"))
}


if(sum(c("pat_id","esp" , "con_date+","recod_prd-" , "mast_prd_name-", "tam") %in% names(d2)) != 6) {
  stop(paste0("Revisar los nombres del archivo, pestaña: ",nombres_hojas[grepl("tx|switches", ignore.case = T, nombres_hojas)],"\n 
              deberían ser: pat_id,esp , con_date+,recod_prd- , mast_prd_name-, tam"))
  }



temp <- rstudioapi::showQuestion("IPCSK9",paste0("IPCSK9 Mes a ejecutar: ", mes_texto_completo))


## añadimos los switchs FROM

d2 <- d2[,.(pat_id, esp, con_date = `con_date+`, mast_prd_name = `mast_prd_name-`, recod_prd = `recod_prd-`, status = "SWITCHES FROM", tam)]

d <- rbind(d, d2, fill = T)


if(d[is.na(con_date),.N] > 0){warning(paste0("Tenemos ",d[is.na(con_date),.N]," registros sin fecha 'con_date'"))}
max(d$con_date)
if(format(max(d$con_date, na.rm = T),"%b") != format(mes_actual-1,"%b")) stop("Los datos no corresponden con el mes indicado.")


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
# summary(d$abandono)   ## comprobación

aban0 <- d[abandono >= trim & abandono < mes_actual]  ## abandono trim
aban0[, `:=` (con_date = as.Date("1999-01-01"), status = "ABANDONO", tam = "TRIM")]

aban1 <- d[abandono >= tam1 & abandono < mes_actual]  ## abandono TAM1
aban1[,.N, esp]
summary(aban1$abandono)
aban1[, `:=` (con_date = as.Date("1999-01-01"), status = "ABANDONO", tam = paste0("TAM",mes_texto,format(mes_actual,"%y")))]

aban2 <- d[abandono >= tam2 & abandono < tam1]  ## abandono TAM2
summary(aban2$abandono)
aban2[, `:=` (con_date = as.Date("1999-01-01"), status = "ABANDONO", tam = paste0("TAM",mes_texto,format(tam1,"%y")))]

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

d_trim <- d[(con_date >= trim & con_date < mes_actual),]
d_trim[, tam :=  "TRIM"]

d_tam1 <- d[(con_date >= tam1 & con_date < mes_actual), ]
d_tam1[, tam := paste0("TAM",mes_texto,format(mes_actual,"%y"))]

d_tam2 <- d[(con_date >= tam2 & con_date < tam1), ]
d_tam2[, tam := paste0("TAM",mes_texto,format(tam1,"%y"))]

d_abandonos <- d[status == "ABANDONO"]

d <- rbind(d_trim, d_tam1, d_tam2, d_abandonos)







## ARREGLAMOS ALGUNAS VARIABLES
d[, esp := ifelse(esp %in% c("CAR","END","MIV","NEF","NRL"), esp, "RESTO")]
d[,.N, esp]

## los que no tienen status de hecho es porque son fuera de periodo, asi que los eliminamos
## mejor eliminamos los que no tienen TAM que son más
d <- d[tam != ""]

a <- c(unique(d$esp))
b <- c(unique(d$status))
c <- c(unique(d$recod_prd))
e <- c(unique(d$tam, na.rm = T))


txt <- expand.grid(a,b,c, e)


## SIN TOTALES
for (i in 1:nrow(txt)){
  if(txt[i,2] == "ABANDONO"){
    txt[i,5] <- d[esp == txt[i,1] & status == txt[i,2] & recod_prd == txt[i,3] & tam == txt[i,4], uniqueN(pat_id)]
  }
  if(txt[i,2] != "ABANDONO"){
    txt[i,5] <- d[esp == txt[i,1] & status == txt[i,2] & recod_prd == txt[i,3] & tam == txt[i,4] & status != "ABANDONO", uniqueN(pat_id)]
  }
}


### TOTALES INDIVIDUALES ####
### TOTAL Primera variable
txt1 <- expand.grid("TOTAL ESP",b,c, e)
for (i in 1:nrow(txt1)){
  if(txt1[i,2] == "ABANDONO"){
    txt1[i,5] <- d[status == txt1[i,2] & recod_prd == txt1[i,3] & tam == txt1[i,4], uniqueN(pat_id)]
  }
  if(txt1[i,2] != "ABANDONO"){
    txt1[i,5] <- d[status == txt1[i,2] & recod_prd == txt1[i,3] & tam == txt1[i,4] & status != "ABANDONO", uniqueN(pat_id)]
  }
}

### TOTAL Segunda variable
txt2 <- expand.grid(a,"Total Patients",c, e)
for (i in 1:nrow(txt2)){
  txt2[i,5] <- d[esp == txt2[i,1] & recod_prd == txt2[i,3] & tam == txt2[i,4] & !status %in% c("ABANDONO", "SWITCHES FROM"), uniqueN(pat_id)]
}

### TOTAL Tercera variable
txt3 <- expand.grid(a,b, "TOTAL", e)
for (i in 1:nrow(txt3)){
  # i=1
  if(txt3[i,2] == "ABANDONO"){
  txt3[i,5] <- d[esp == txt3[i,1] & status == txt3[i,2] & tam == txt3[i,4] , uniqueN(pat_id)]
  }
  if(txt3[i,2] != "ABANDONO"){
  txt3[i,5] <- d[esp == txt3[i,1] & status == txt3[i,2] & tam == txt3[i,4] & status != "ABANDONO" , uniqueN(pat_id)]
  }
}



### TOTALES POR PAREJAS ####
### TOTAL Primera y Segunda variable
txt12 <- expand.grid("TOTAL ESP","Total Patients", c, e)
for (i in 1:nrow(txt12)){
  txt12[i,5] <- d[recod_prd == txt12[i,3] & tam == txt12[i,4] & !status %in% c("ABANDONO", "SWITCHES FROM"), uniqueN(pat_id)]
}

### TOTAL Primera y Tercera variable
txt13 <- expand.grid("TOTAL ESP",b, "TOTAL", e)
for (i in 1:nrow(txt13)){
  if(txt13[i,2] == "ABANDONO"){
    txt13[i,5] <- d[status == txt13[i,2] & tam == txt13[i,4], uniqueN(pat_id)]
  }
  if(txt13[i,2] != "ABANDONO"){
    txt13[i,5] <- d[status == txt13[i,2] & tam == txt13[i,4] & status != "ABANDONO", uniqueN(pat_id)]
  }
}

### TOTAL Segunda y Tercera variable
txt23 <- expand.grid(a,"Total Patients", "TOTAL", e)
for (i in 1:nrow(txt23)){
  txt23[i,5] <- d[esp == txt23[i,1] & tam == txt23[i,4] & !status %in% c("ABANDONO", "SWITCHES FROM"), uniqueN(pat_id)]
}



### TOTALES POR TRIOS ####
### TOTAL Primera Segunda Tercera variable

txt123 <- expand.grid("TOTAL ESP","Total Patients", "TOTAL", e)
for (i in 1:nrow(txt123)){
  txt123[i,5] <- d[tam == txt123[i,4] & !status %in% c("ABANDONO", "SWITCHES FROM"), uniqueN(pat_id)]
}


### Juntamos LOS RESULTADOS ####
total_txt <- setDT(Reduce(function(...) rbind(..., fill = T), mget(ls(pattern = "^txt"))))

## quitamos unas filas de NA que se generan y los registros que tienen 0
total_txt <- total_txt[!is.na(Var1) & V5 > 0,]

total_txt[,.N, Var4]

names(total_txt) <- c("Var2", "Var3", "Var4", "Var1", "Contador_r")

{
  if(mes_texto %in% c("Mar"))  total_txt[Var1 == "TRIM", Var1 := paste0("Q1 ",year_extract_long)]
  if(mes_texto %in% c("Jun"))  total_txt[Var1 == "TRIM", Var1 := paste0("Q2 ",year_extract_long)]
  if(mes_texto %in% c("Sep"))  total_txt[Var1 == "TRIM", Var1 := paste0("Q3 ",year_extract_long)]
  if(mes_texto %in% c("Dic"))  total_txt[Var1 == "TRIM", Var1 := paste0("Q4 ",year_extract_long)]
  if(!mes_texto %in% c("Mar","Jun","Sep","Dic")) total_txt[Var1 == "TRIM", Var1 := paste0(mes_texto0,"-",mes_texto,year_extract)]
}

total_txt <- total_txt[Var3 != "REPETICION"]

total_txt[, Concatenado := paste0(Var2, Var3, Var4, Var1)]
total_txt <- total_txt[,.(Concatenado, Var2,Var3,Var4,Var1, Contador_r)]


{
  # ## comprobacions 202404
  # require(openxlsx)
  # out_previo <- setDT(read.xlsx("IPCSK9/out/out_abril24.xlsx", "Hoja1"))
  # out_previo2 <- setDT(read.xlsx("IPCSK9/Data/Libro1.xlsx", "Hoja1"))  ## output arreglado los abandonos
  # out_previo[, c("X7", "X8", "X9") := NULL]
  # 
  # names(out_previo2) <- names(out_previo)
  # 
  # diferencias <- setDT(merge(total_txt, out_previo2, by = c("Concatenado","Var1", "Var2", "Var3", "Var4"), all = T))
  # 
  # dif <- diferencias[Contador_r != Contador | is.na(Contador_r) | is.na(Contador)]
  # igual <- diferencias[Contador_r == Contador]
}


## Exportamos los resultados del periodo ####
# fwrite(total_txt, paste0(path_output,"IPCSK9_",format(mes,"%Y%m"),".csv"))
write.xlsx(total_txt, paste0(path_output,"IPCSK9_",format(mes,"%Y%m"),".xlsx"))
file_name <- paste0("IPCSK9_",format(mes,"%Y%m"),".xlsx")
if(paste0("IPCSK9_",format(mes,"%Y%m"),".xlsx") %in% list.files(path_output)){cat("Se ha exportado correctamente el archivo:\n",file_name)}else{warning("Hay un problema con la exportación")}
