rm(list= ls())
gc()
cat("\014")
options(scipen=999)


## PARAMETROS USUARIO EXTERNO ####
user = "YesikaDíazRodriguez"
path <- paste0("C:/Users/",user,"/Telomera S.L/AUTOMATIZACION MERCADOS - General/Agentes lipidicos/")



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
aux <- as.Date("2024-05-15")
# aux <- Sys.Date()

mes <- as.Date(paste0(year(aux), "-", format(aux %m-% months(1), "%m"),"-01"))
mes_actual <- as.Date(paste0(year(aux), "-", format(aux, "%m"),"-01"))

trim <- as.Date(paste0(year(aux), "-", format(aux %m-% months(3), "%m"),"-01"))
tam1 <- as.Date(paste0(year(aux)-1, "-", format(aux %m-% months(12), "%m"),"-01"))
tam2 <- as.Date(paste0(year(aux)-2, "-", format(aux %m-% months(12), "%m"),"-01"))

mes_texto <- gsub("\\.","",str_to_title(format(mes, "%b")))
mes_texto_completo <- paste0(str_to_title(format(mes, "%B"))," ",year(mes))
mes_texto0 <- gsub("\\.","",str_to_title(format(trim, "%b")))

year_extract <- format(mes,"%y")

## RUTAS ####
path_input <- paste0(path,"Input/",format(mes,"%Y%m"),"/")
# path_input <- "C:/Users/YesikaDíazRodriguez/Telomera S.L/AUTOMATIZACION MERCADOS - General/Agentes lipidicos/Input/202404/"
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

# temp <- rstudioapi::showQuestion("AGENTES LIPIDICOS",paste0("Agentes Lipídicos. Mes a ejecutar: ", mes_texto_completo))



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

d[, mol_complex := paste0(class,"-", recod_prd_name)]

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
  txt2[i,6] <- dfiltr[esp == txt2[i,1] & recod_prd_name == txt2[i,3] & tam == txt2[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM") & segmento_final == txt2[i,5], uniqueN(pat_id)]
}



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
  txt12[i,6] <- dfiltr[recod_prd_name == txt12[i,3] & tam == txt12[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM") & segmento_final == txt12[i,5], uniqueN(pat_id)]
}

# dfiltr[recod_prd_name == "NUSTENDI" & tam == "TRIM" & segmento_final == "HF"& !status %in% c("ABANDONOS", "SWITCHES FROM"), uniqueN(pat_id)]
vec <- c(18725,     44212,   367314,   528117,  548848,    698005,   140096846)
setdiff(dfiltr[recod_prd_name == "NUSTENDI" & tam == "TRIM" & segmento_final == "HF"& !status %in% c("ABANDONOS", "SWITCHES FROM"),]$pat_id,vec)

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
  txt23[i,6] <- dfiltr[esp == txt23[i,1] & tam == txt23[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM") & segmento_final == txt23[i,5], uniqueN(pat_id)]
}


### TOTAL Segunda y cuarta variable
txt24 <- expand.grid(a,"Total Patients", c, e, "Total indicaciones")
for (i in 1:nrow(txt24)){
    txt24[i,6] <- dfiltr[esp == txt24[i,1] & recod_prd_name == txt24[i,3] & tam == txt24[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM"), uniqueN(pat_id)]
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
  txt123[i,6] <- dfiltr[tam == txt123[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM") & segmento_final == txt123[i,5], uniqueN(pat_id)]
}


txt234 <- expand.grid(a,"Total Patients", "TOTAL", e, "Total indicaciones")
for (i in 1:nrow(txt234)){
  txt234[i,6] <- dfiltr[tam == txt234[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM") & esp == txt234[i,1], uniqueN(pat_id)]
}


txt124 <- expand.grid("TOTAL ESP","Total Patients", c, e, "Total indicaciones")
for (i in 1:nrow(txt124)){
  # i=1
  txt124[i,6] <- dfiltr[tam == txt124[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM") & recod_prd_name == txt124[i,3], uniqueN(pat_id)]
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
  txt1234[i,6] <- dfiltr[tam == txt1234[i,4] & !status %in% c("ABANDONOS", "SWITCHES FROM"), uniqueN(pat_id)]
}




### Juntamos LOS RESULTADOS ####
total_txt <- setDT(Reduce(function(...) rbind(..., fill = T), mget(ls(pattern = "^txt"))))

## quitamos unas filas de NA que se generan y los registros que tienen 0
total_txt <- total_txt[!is.na(Var1) & V6 > 0,]

total_txt[,.N, Var4]

names(total_txt) <- c("Var3", "Var4", "Var5", "Var1", "Var2","Contador_r")

## comprobacion
# total_txt2 <- rbind(txt, txt1, txt2, txt3, txt13,txt23, txt12, txt123)
# total_txt
# 
# 
# diferencias <- setDT(merge(total_txt, total_txt2, by = c("Var1", "Var2", "Var3", "Var4"), suffixes = c("reduce", "rbind"), all = T))
# diferencias[is.na(V5rbind)]
# diferencias[V5rbind != V5reduce]

# total_txt[Var1 == "TRIM", Var1 := "Feb-Abr24"]
total_txt[Var1 == "TRIM", Var1 := paste0(mes_texto0,"-",mes_texto,year_extract)]

total_txt <- total_txt[!grepl("^REPETICION",Var4)]

total_txt[, Concatenado := paste0(Var3,Var4,Var2,Var5,Var1)]
total_txt <- total_txt[,.(Concatenado, Var1,Var2,Var3,Var4,Var5, Contador_r)]

{
# ## comprobacions 202404
# require(openxlsx)
out_previo <- setDT(read.xlsx(paste0(path_input,file_mercado), 
                              sheet = "Resultados", 
                              detectDates = T))


diferencias <- setDT(merge(total_txt, out_previo, by = c("Concatenado","Var1", "Var2", "Var3", "Var4", "Var5"), all = T))
diferencias[,.N, Var1]
diferencias <- diferencias[Var1 %in% c("Feb-Abr24", "TAMAbr24")]
diferencias <- diferencias[Var5 != "LEQVIO"]
dif <- diferencias[Contador_r != VALOR | is.na(Contador_r) | is.na(VALOR)]
igual <- diferencias[Contador_r == VALOR]

dif[, magnitud := abs(VALOR-Contador_r)]
}


out_previo[,.N, Var4]

## Exportamos los resultados del periodo ####
# fwrite(total_txt, paste0(path_output,"IPCSK9_",format(mes,"%Y%m"),".csv"))
















dif[Concatenado == "ENDTotal PatientsTotal indicacionesiPCSK9TAMAbr24",]
# Concatenado                                               Var1          Var2        Var3        Var4     Var5     Contador_r VALOR magnitud
# 1: ENDTotal PatientsTotal indicacionesiPCSK9TAMAbr24    TAMAbr24 Total indicaciones  END Total Patients iPCSK9        271     257       14

dfiltr[tam == "TAMAbr24" & esp == "END" & recod_prd_name == "iPCSK9" & status != "ABANDONOS", uniqueN(pat_id)]


vec1 <- c(3401,        7561,       9089,       10317,        12114,       13940,        23762,       26116,        31383,       34283,        34486,       
          42413,        53276,       54252,        54351,       54447,        54675,       59105,        73812,       73915,        74660,       80857,        
          82853,       84525,        86341,       96999,        103089,       103107,       103184,      103559,        104640,       120200,       123877,      
          125212,        132391,       132774,       136260,      142631,        143609,       146808,       146989,      149914,        154101,       157378,       
          157610,      160626,        162622,       163529,       166771,      169770,        173057,       176292,       176632,      183252,        184832,       
          195764,       198968,      209675,        210853,       214606,       215211,      215561,        223376,       225506,       226637,      229936,        
          230929,       235990,       249492,      253496,        254193,       257264,       257405,      263503,        264253,       265274,       269114,      
          272132,        276332,       282156,       286582,      290751,        303195,       303298,       316357,      317636,        320048,       320557,       
          321503,      324606,        325126,       326064,       327171,      327207,        327529,       335443,       341785,      347446,        348242,       
          349058,       352114,      354802,        371549,       374329,       376655,      380108,        380921,       383401,       397612,      401296,        
          402332,       402837,       404655,      405511,        413424,       422233,       428975,      430641,        438531,       440390,       440845,      
          448638,        449177,       457877,       460657,      461088,        464780,       467548,       473538,      484277,        486337,       488123,       
          492375,      492544,        493015,       493427,       499695,      511153,        511980,       513691,       513733,      514845,        521502,       
          533303,       533417,      533467,        540715,       541802,       564964,      568201,        583935,       587598,       589699,      594236,        
          597375,       604238,       610293,      610886,        646214,       652818,       653873,      656930,        661798,       662309,       662846,      
          665133,        667477,       670758,       674221,      676523,        683816,       686736,       688716,      694693,        712385,       712425,       
          716776,      718765,        720338,       728766,       730460,      736037,        737407,       739992,       741276,      750405,        757810,       
          763611,       764414,      770817,        775297,       789050,       790093,      802619,        808481,       815212,       816529,      816603,        
          816721,       819082,       820845,      830248,        830379,       832597,       833049,      833840,        836995,       846021,       848571,      
          850998,        862511,       904128,       930749,      932491,        2011754,       91008353,      91008846,        91027411,       110267298,       
          110395108,       116064813,       122218308,       122514193,       122632443,       123015942,       123183236,       127360290,       131811909,       
          132205415,       137074752,       139159631,       139193391,       139195917,       139439353,       139495610,       139496260,       139524511,       
          139880873,       140221423,       140518599,       142367730,       144788137,       152490333,       153546640,       162456135,       163703835,       
          164347235,       170636035,       171647935,       173498736,       175918236,       188388038,       189308036,       190748037,       193823636,       
          195681141,       198703942)

setdiff(dfiltr[tam == "TAMAbr24" & esp == "END" & recod_prd_name == "iPCSK9" & status != "ABANDONOS",]$pat_id, vec1)


















# {
# # > dif[Concatenado == "CARDynamicTotal indicacionesREPATHAMar-May24"]
# # Concatenado      Var1               Var2 Var3    Var4    Var5                                      Contador_r Contador
# # 1: CARDynamicTotal indicacionesREPATHAMar-May24 Mar-May24 Total indicaciones  CAR Dynamic REPATHA         11       10
# 
# d[tam == "TRIM" & esp == "CAR" & status == "DYNAMIC" & recod_prd_name == "REPATHA", ]
# 
# vec1 <- c(45058,
#           254920,
#           426916,
#           467465,
#           778559,
#           832352,
#           837396,
#           915771,
#           116723963,
#           139713349)
# 
# setdiff(d[tam == "TRIM" & esp == "CAR" & status == "DYNAMIC" & recod_prd_name == "REPATHA", ]$pat_id, vec1)
# }
# 
# 
# {
# dif[Concatenado == "ENDDynamicHFNUSTENDITAMMay24"]
# Concatenado     Var1 Var2 Var3    Var4     Var5 Contador_r Contador
# 1: ENDDynamicHFNUSTENDITAMMay24 TAMMay24   HF  END Dynamic NUSTENDI          8       11

# vec1 <- c(18725,
#           367314,
#           436560,
#           528117,
#           548848,
#           583935,
#           687998,
#           697958,
#           698005,
#           140096846,
#           140518599)
# 
# 
# 
# dfiltr[tam == "TAMMay24" & esp == "END" & status == "DYNAMIC" & recod_prd_name == "NUSTENDI" & segmento_final == "HF", uniqueN(pat_id) ]
# 
# setdiff(vec1, 
#         dfiltr[tam == "TAMMay24" & esp == "END" & status == "DYNAMIC" & recod_prd_name == "NUSTENDI" & segmento_final == "HF", ]$pat_id)
# }



# dif[Concatenado == "ENDDynamicHFREPATHATAMMay24"]
# Concatenado     Var1 Var2 Var3    Var4    Var5 Contador_r Contador
# 1: ENDDynamicHFREPATHATAMMay24 TAMMay24   HF  END Dynamic REPATHA         23       24

# dfiltr[tam == "TAMMay24" & esp == "END" & status == "DYNAMIC" & recod_prd_name == "REPATHA" & segmento_final == "HF", uniqueN(pat_id) ]
# 
# 
# vec1 <- c(7561,
#           53276,
#           120200,
#           215211,
#           223376,
#           341785,
#           376655,
#           397612,
#           401296,
#           404655,
#           438531,
#           449177,
#           465811,
#           492375,
#           528117,
#           587598,
#           652818,
#           656930,
#           718765,
#           720338,
#           802619,
#           816529,
#           833049,
#           131811909)
# setdiff(vec1, 
#         dfiltr[tam == "TAMMay24" & esp == "END" & status == "DYNAMIC" & recod_prd_name == "REPATHA" & segmento_final == "HF", ]$pat_id)

