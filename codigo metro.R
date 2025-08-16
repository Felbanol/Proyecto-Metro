library(plm)
library(readxl)

DatosT <- read_excel("C:/Users/Felipe Bañol/Downloads/Datosmetroconteo (1).xlsx", 
                     sheet = "Conteo general")
DatosH <- read_excel("C:/Users/Felipe Bañol/Downloads/Datosmetroconteo (1).xlsx", 
                     sheet = "Hombres")
DatosM <- read_excel("C:/Users/Felipe Bañol/Downloads/Datosmetroconteo (1).xlsx", 
                     sheet = "Mujer")

Datosjovengeneral <- read_excel("C:/Users/Felipe Bañol/Downloads/Datosmetroconteojoven.xlsx", 
                                sheet = "Joven general")
Jovenhombre <- read_excel("C:/Users/Felipe Bañol/Downloads/Datosmetroconteojoven.xlsx", 
                          sheet = "Joven hombres")

Jovenmujer <- read_excel("C:/Users/Felipe Bañol/Downloads/Datosmetroconteojoven.xlsx", 
                         sheet = "Joven mujer")
Hombres21M <- read_excel("C:/Users/Felipe Bañol/Downloads/Datosmetroconteojoven21.xlsx", 
                         sheet = ">21 hombre")
Mujer21 <- read_excel("C:/Users/Felipe Bañol/Downloads/Datosmetroconteojoven21.xlsx", 
                      sheet = "<=21 mujer")
Hombre21 <- read_excel("C:/Users/Felipe Bañol/Downloads/Datosmetroconteojoven21.xlsx", 
                       sheet = "<=21 hombre")
Mujer21M <- read_excel("C:/Users/Felipe Bañol/Downloads/Datosmetroconteojoven21.xlsx", 
                       sheet = ">21 mujer")
General21J <- read_excel("C:/Users/Felipe Bañol/Downloads/Datosmetroconteojoven21.xlsx", 
                         sheet = "<=21 general")
X21generalmayor <- read_excel("C:/Users/Felipe Bañol/Downloads/Datosmetroconteojoven21.xlsx", 
                              sheet = ">21 general")




#General
didreg = lm( CONTEO~ Treat  + Time2020 + TimeT2020, data = DatosT)
summary(didreg)
didreg3=lm(CONTEO~Treat*Time2020+ELEVADO,data = DatosT)
summary(didreg3)
didreg2 = lm( CONTEO~ Treat + Time2022 + TimeT2022, data = DatosT)
summary(didreg2)
#Hombres
didreg = lm( CONTEO~ Treat + Time2020 + TimeT2020, data = DatosH)
summary(didreg)
didreg2 = lm( CONTEO~ Treat + Time2022 + TimeT2022, data = DatosH)
summary(didreg2)
#Mujeres
didreg = lm( CONTEO~ Treat + Time2020 + TimeT2020, data = DatosM)
summary(didreg)
didreg2 = lm( CONTEO~ Treat + Time2022 + TimeT2022, data = DatosM)
summary(didreg2)
#Efectos fijos
#Hombre
data_panelH <- pdata.frame(DatosH, index = c("ESTACION", "AÑOT"))
data_panelHe <- pdata.frame(DatosH, index = c("ESTACION"))
dd3 <- plm(CONTEO ~ +ELEVADO+Treat+Time2020+TimeT2020, 
           data = data_panelH, model = "within", effect = "individual")
dd3e2020 <- plm(CONTEO ~ Treat+Time2020+TimeT2020, 
           data = data_panelHe, model = "within", effect = "individual")
dd3e2022 <- plm(CONTEO ~ Treat+Time2022+TimeT2022, 
            data = data_panelHe, model = "within", effect = "individual")
summary(dd3e2020)
summary(dd3e2022)
summary(dd3)
#Intercepto
efectos_fijos <- fixef(dd3e2020)

# Calcular la constante promedio (promedio de los efectos fijos)
constante <- mean(efectos_fijos)
print(constante)
efectos_fijos <- fixef(dd3e2022)

# Calcular la constante promedio (promedio de los efectos fijos)
constante <- mean(efectos_fijos)
print(constante)

#Mujer
data_panelM <- pdata.frame(DatosM, index = c("ESTACION"))
dd32020 <- plm(CONTEO ~ Treat+Time2020+TimeT2020, 
           data = data_panelM, model = "within", effect = "individual")
summary(dd32020)
dd32022 <- plm(CONTEO ~ Treat+Time2022+TimeT2022, 
           data = data_panelM, model = "within", effect = "individual")
summary(dd32022)

efectos_fijos <- fixef(dd32020)

# Calcular la constante promedio (promedio de los efectos fijos)
constante <- mean(efectos_fijos)
print(constante)

efectos_fijos <- fixef(dd32022)

# Calcular la constante promedio (promedio de los efectos fijos)
constante <- mean(efectos_fijos)
print(constante)
#total
data_panel <- pdata.frame(DatosT, index = c("ESTACION"))
dd3 <- plm(CONTEO ~ Treat+Time2020+TimeT2020, 
           data = data_panel, model = "within", effect = "individual")
summary(dd3)
dd32022t <- plm(CONTEO ~ Treat+Time2022+TimeT2022, 
           data = data_panel, model = "within", effect = "individual")
summary(dd32022t)

efectos_fijos <- fixef(dd3)

# Calcular la constante promedio (promedio de los efectos fijos)
constante <- mean(efectos_fijos)
print(constante)
efectos_fijos <- fixef(dd32022t)

# Calcular la constante promedio (promedio de los efectos fijos)
constante <- mean(efectos_fijos)
print(constante)

#Ahora para 2022
#Hombre
data_panelH <- pdata.frame(DatosH, index = c("ESTACION", "AÑOT"))
dd3 <- plm(CONTEO ~ factor(ESTRATO)+ELEVADO+ESCUCHADERO+Time2022+TimeT2022, 
           data = data_panelH, model = "within", effect = "twoways")
summary(dd3)
#Mujer
data_panelM <- pdata.frame(DatosM, index = c("ESTACION", "AÑOT"))
dd3 <- plm(CONTEO ~ factor(ESTRATO)+ELEVADO+ESCUCHADERO+Time2022+TimeT2022, 
           data = data_panelM, model = "within", effect = "twoways")
summary(dd3)
#total
data_panel <- pdata.frame(DatosT, index = c("ESTACION", "AñoT"))
dd3 <- plm(CONTEO ~ Treat+Time2022+TimeT2022, 
           data = data_panelM, model = "within", effect = "twoways")
summary(dd3)
#se podria pensar un diff para la variable conteo
#Jovenes
#General
didreg = lm( CONTEO~ Treat  + Time2020 + TimeT2020, data = Datosjovengeneral)
summary(didreg)
didreg3=lm(CONTEO~Treat*Time2020+ELEVADO,data = Datosjovengeneral)
summary(didreg3)
didreg2 = lm( CONTEO~ Treat + Time2022 + TimeT2022, data = Datosjovengeneral)
summary(didreg2)
#Hombres
didreg = lm( CONTEO~ Treat + Time2020 + TimeT2020, data = Jovenhombre)
summary(didreg)
didreg2 = lm( CONTEO~ Treat + Time2022 + TimeT2022, data = Jovenhombre)
summary(didreg2)
#Mujeres
didreg = lm( CONTEO~ Treat + Time2020 + TimeT2020, data = Jovenmujer)
summary(didreg)
didreg2 = lm( CONTEO~ Treat + Time2022 + TimeT2022, data = Jovenmujer)
summary(didreg2)
#Efectos fijos
#Hombre
data_panelH <- pdata.frame(Jovenhombre, index = c("ESTACION", "AÑOT"))
dd3 <- plm(CONTEO ~ Treat+Time2020+TimeT2020, 
           data = data_panelH, model = "within", effect = "individual")
summary(dd3)
#Mujer
data_panelM <- pdata.frame(Jovenmujer, index = c("ESTACION", "AÑOT"))
dd3 <- plm(CONTEO ~ Treat+Time2020+TimeT2020, 
           data = data_panelM, model = "within", effect = "twoways")
summary(dd3)
#total
data_panel <- pdata.frame(Datosjovengeneral, index = c("ESTACION", "AÑOT"))
dd3 <- plm(CONTEO ~ Treat+Time2020+TimeT2020, 
           data = data_panelM, model = "within", effect = "twoways")
summary(dd3)
#Ahora para 2022
#Hombre
data_panelH <- pdata.frame(Jovenhombre, index = c("ESTACION", "AÑOT"))
dd3 <- plm(CONTEO ~ data_panelH$Treat+Time2022+TimeT2022, 
           data = data_panelH, model = "within", effect = "twoways")
summary(dd3)
#Mujer
data_panelM <- pdata.frame(Jovenmujer, index = c("ESTACION", "AÑOT"))
dd3 <- plm(CONTEO ~ factor(ESTRATO)+ELEVADO+Treat+Time2022+TimeT2022, 
           data = data_panelM, model = "within", effect = "twoways")
summary(dd3)
#total
data_panel <- pdata.frame(Datosjovengeneral, index = c("ESTACION", "AÑOT"))
dd3 <- plm(CONTEO ~ Treat+Time2022+TimeT2022, 
           data = data_panelM, model = "within", effect = "twoways")
summary(dd3)

#Mayor 21 anos hombre
didreg = lm( Conteo~ Treat  + Time2020 + TimeT2020, data = Hombres21M)
summary(didreg)
didreg2 = lm( Conteo~ Treat + Time2022 + TimeT2022, data = Hombres21M)
summary(didreg2)
#Mayor 21 anos Mujer
didreg = lm( Conteo~ Treat  + Time2020 + TimeT2020, data = Mujer21M)
summary(didreg)
didreg2 = lm( Conteo~ Treat + Time2022 + TimeT2022, data = Mujer21M)
summary(didreg2)
#Joven 21 anos mujer
didreg = lm( Conteo~ Treat  + Time2020 + TimeT2020, data = Mujer21)
summary(didreg)
didreg2 = lm( Conteo~ Treat + Time2022 + TimeT2022, data = Mujer21)
summary(didreg2)
#Joven 21 anos hombre
didreg = lm( Conteo~ Treat  + Time2020 + TimeT2020, data = Hombre21)
summary(didreg)
didreg2 = lm( Conteo~ Treat + Time2022 + TimeT2022, data = Hombre21)
summary(didreg2)
#Joven 21 general
didreg = lm( Conteo~ Treat  + Time2020 + TimeT2020, data = General21J)
summary(didreg)
didreg2 = lm( Conteo~ Treat + Time2022 + TimeT2022, data = General21J)
summary(didreg2)
#Joven 21 mayor general
didreg = lm( Conteo~ Treat  + Time2020 + TimeT2020, data = X21generalmayor)
summary(didreg)
didreg2 = lm( Conteo~ Treat + Time2022 + TimeT2022, data = X21generalmayor)
summary(didreg2)
