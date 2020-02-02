library (foreign)
library(sf)
library(dplyr)
library (tmap)
library(DescTools)
library(PropCIs)
library(DCluster)
library(readr)
library(naniar)
library(sp)
library(spdep)
library(lme4)
library(sjlabelled)
library(sjmisc)
library(sjstats)
library(sjPlot)
library(summarytools)
library(ggplot2)
library(ggthemes)
library(GPArotation) 
library(psych)
library(ggrepel)
library(expss)


 ## Download 2017 survey dataset, crop and operationalise variables##

urlfile <- 'https://www.dropbox.com/s/yajr7jrsax1scn3/BDA_ECPRASP_ME_V06_20181011.sav?dl=0'
download.file(urlfile, 'paidsexdataset.sav')
unzip('paidsexdataset.zip', exdir = 'paidsexdataset')
paidsexdataset<- read.spss("paidsexdataset/BDA_ECPRASP_ME_V06_20181011.sav", to.data.frame=TRUE)
paidsext <- subset(paidsexdataset, paidsexdataset$P002=='Mujeres')
paidsex <- paidsext[-c(paidsext$ID_VERIF == 1688), ]
paidsex[paidsex ==99] <- NA
paidsex$code <- NA
##Outcome Variables##
paidsex$clientsexualviolence <- as.numeric(paidsex$P066A =='Sí')
paidsex$clientphysicalviolence<- as.numeric(paidsex$P066D =='Sí')
paidsex$policeabuse <- as.numeric(paidsex$P066B =='Sí')
paidsex$ht <- as.numeric(paidsex$P066C =='Sí')
paidsex$violencepimp <- as.numeric(paidsex$P066E =='Sí')
paidsex$violenceothersexworker <- as.numeric(paidsex$P066F =='Sí')
paidsex$withheldID <- as.numeric(paidsex$P066G =='Sí')
paidsex$freedomofmovement<- as.numeric(paidsex$P066H =='Sí')
paidsex$withheldpayment <- as.numeric(paidsex$P066I =='Sí')
paidsex$violence <- as.numeric(paidsex$clientphysicalviolence| paidsex$clientsexualviolence)
# Independent Varaibles##
paidsex$code[paidsex$COD_LOC=="Usaquén"] <- 1
paidsex$code[paidsex$COD_LOC=="Chapinero"] <- 2
paidsex$code[paidsex$COD_LOC=="Santa Fe"] <- 3
paidsex$code[paidsex$COD_LOC=="San Cristobal"] <-4
paidsex$code[paidsex$COD_LOC=="Usme"] <- 5
paidsex$code[paidsex$COD_LOC=="Tunjuelito"] <- 6
paidsex$code[paidsex$COD_LOC=="Bosa"] <- 7
paidsex$code[paidsex$COD_LOC=="Kennedy"] <- 8
paidsex$code[paidsex$COD_LOC=="Fontibón"] <- 9
paidsex$code[paidsex$COD_LOC=="Engativá"] <- 10
paidsex$code[paidsex$COD_LOC=="Suba"] <- 11
paidsex$code[paidsex$COD_LOC=="Barrios Unidos"] <- 12
paidsex$code[paidsex$COD_LOC=="Teusaquillo"] <- 13
paidsex$code[paidsex$COD_LOC=="Los Mártires"] <- 14
paidsex$code[paidsex$COD_LOC=="Antonio Nariño"] <- 15
paidsex$code[paidsex$COD_LOC=="Puente Aranda"] <- 16
paidsex$code[paidsex$COD_LOC=="La Candelaria"] <- 17
paidsex$code[paidsex$COD_LOC=="Rafael Uribe Uribe"] <- 18
paidsex$code[paidsex$COD_LOC=="Ciudad Bolívar"] <- 19
paidsex$Usaquen <- as.numeric(paidsex$code==1)
paidsex$Chapinero <- as.numeric(paidsex$code==2)
paidsex$Sancris <- as.numeric(paidsex$code==4)
paidsex$Usme <- as.numeric(paidsex$code==5)
paidsex$Tunjuelito <- as.numeric(paidsex$code==6)
paidsex$Bosa <- as.numeric(paidsex$code==7)
paidsex$Kennedy <- as.numeric(paidsex$code==8)
paidsex$Fontibon <- as.numeric(paidsex$code==9)
paidsex$Engativa <- as.numeric(paidsex$code== 10)
paidsex$Suba <- as.numeric(paidsex$code== 11)
paidsex$BarrriosUnidos <- as.numeric(paidsex$code== 12)
paidsex$Teusaquillos<- as.numeric(paidsex$code== 13)
paidsex$Martires <- as.numeric(paidsex$code==14)
paidsex$Antonio <- as.numeric(paidsex$code== 15)
paidsex$Puente <- as.numeric(paidsex$code==16)
paidsex$Sancan <- as.numeric(paidsex$code==3 | paidsex$code==17)
paidsex$Rafael <- as.numeric(paidsex$code==18)
paidsex$Ciubo <- as.numeric(paidsex$code==19)
paidsex$numclients <- as.numeric(paidsex$P056)
paidsex$payperclient <- as.numeric(paidsex$P057)/1000
summary(paidsex$payperclient)
paidsex$contactphone <- as.numeric(paidsex$P051A =='Sí')
paidsex$contactinternet <- as.numeric(paidsex$P051B =='Sí')
paidsex$contactstreet <- as.numeric(paidsex$P051D =='Sí')
paidsex$contactbrothel <- as.numeric(paidsex$P051C =='Sí')
paidsex$street <- as.numeric(paidsex$P050G =='Sí')
paidsex$car <- as.numeric(paidsex$P050F== 'Sí')
paidsex$streetcar <- as.numeric(paidsex$P050G =='Sí'|paidsex$P050F== 'Sí')
paidsex$motel <- as.numeric(paidsex$P050B == 'Sí')
paidsex$brothel <- as.numeric(paidsex$P050C == 'Sí')
paidsex$clienthome <- as.numeric(paidsex$P050A == 'Sí')
paidsex$clientmental <- as.numeric(paidsex$P064CA =='Sí')
paidsex$clientphydis <- as.numeric(paidsex$P064BA =='Sí')
paidsex$clientolder <- as.numeric(paidsex$P064DA =='Sí')
paidsex$clientminor <- as.numeric(paidsex$P064EA =='Sí')
paidsex$clientwomen <- as.numeric(paidsex$P064GA =='Sí')
paidsex$clientprison <-as.numeric(paidsex$P064FA =='Sí')
paidsex$venezolana <- as.numeric(paidsex$P007== 'Otro país')
paidsex$notfrombog <- as.numeric(paidsex$P007== 'Otro municipio del país')
paidsex$drinks <- as.numeric(paidsex$P069AD == 'Todos los días')
paidsex$age <-paidsex$P001
paidsex$agec <- paidsex$P001 - mean(paidsex$P001)
paidsex$agecsq <- paidsex$agec * paidsex$agec
paidsex$agestarted <-paidsex$P044
paidsex$yearsworking <- as.numeric(paidsex$age - paidsex$agestarted)
paidsex$educ <- as.numeric(paidsex$P037C)
paidsex$educ[paidsex$educ == 11] <-NA
paidsex$trainingshh <- as.numeric(paidsex$P089A =='Sí')
paidsex$trainingroutes <- as.numeric(paidsex$P089H=='Sí')
paidsex$trainingvio <- as.numeric(paidsex$P089I == 'Sí')
paidsex$trainingsti <- as.numeric(paidsex$P089K == 'Sí')
paidsex$vio2 <- as.numeric(paidsex$P089I == 'Sí' | paidsex$P089I == 'Sí')
paidsex$sti2 <- as.numeric(paidsex$P089A == 'Sí'| paidsex$P089K == 'Sí')
paidsex$trainingcombo <- as.numeric(paidsex$vio2 & paidsex$sti2)
paidsex$typeofclients <-as.numeric(paidsex$clientmental + paidsex$clientphydis + paidsex$clientminor + paidsex$clientolder)
paidsex$placesofwork <- as.numeric(paidsex$brothel + paidsex$motel + paidsex$clienthome + paidsex$streetcar)
paidsex$notdrink <-as.numeric(paidsex$P069AD =='Menos de dos veces por semana')
paidsex$stigma <- as.numeric(paidsex$P068 == 'Sí')
paidsex$sti <- as.numeric(paidsex$P109 == 'Sí')
paidsex$health <- as.numeric(paidsex$P075 == 'SÍ')
paidsex$SEC <- as.numeric(paidsex$agestarted < 18)
paidsex$educ1 <- as.numeric(paidsex$educ < 5)
paidsex$educ2 <- as.numeric(paidsex$educ == 5)
paidsex$educ3 <-  as.numeric(paidsex$educ > 5)
paidsex$interviewstreet <- as.numeric(paidsex$F_LUG== "Calle")


new <- paidsex [, c("ID_VERIF", "code", "COD_LOC", "SEC", "health", "clientsexualviolence", "clientphysicalviolence", "policeabuse", "Martires","venezolana", "notfrombog", "drinks","notdrink", "stigma", "contactphone", "contactinternet", "contactstreet","contactbrothel", "motel", "brothel", "clienthome", "streetcar", "clientmental", "clientphydis", "clientolder", "clientminor", "clientwomen", "clientprison", "age", "educ1", "educ2", "educ3", "numclients","payperclient", "yearsworking", "interviewstreet")]

##Descriptive statistics continuous variables##

conti <- paidsex[, c( "age","numclients","payperclient", "yearsworking")]

(conti)


tis <- cbind(Freq = table(paidsex$interviewstreet), Perc = prop.table(table(paidsex$interviewstreet)),
             Cum = cumsum(prop.table(table(paidsex$interviewstreet))))
tis

tmar <- cbind(Freq = table(paidsex$Martires), Perc = prop.table(table(paidsex$Martires)),
              Cum = cumsum(prop.table(table(paidsex$Martires))))
tmar


tpa<- cbind(Freq = table(paidsex$policeabuse), Perc = prop.table(table(paidsex$policeabuse)),
              Cum = cumsum(prop.table(table(paidsex$policeabuse))))
tpa


tven <- cbind(Freq = table(paidsex$venezolana), Perc = prop.table(table(paidsex$venezolana)),
              Cum = cumsum(prop.table(table(paidsex$venezolana))))
tven


tnfb <- cbind(Freq = table(paidsex$notfrombog), Perc = prop.table(table(paidsex$notfrombog)),
              Cum = cumsum(prop.table(table(paidsex$notfrombog))))
tnfb

tsti <- cbind(Freq = table(paidsex$stigma), Perc = prop.table(table(paidsex$stigma)),
              Cum = cumsum(prop.table(table(paidsex$stigma))))
tsti

tcp <-  cbind(Freq = table(paidsex$contactphone), Perc = prop.table(table(paidsex$contactphone)),
              Cum = cumsum(prop.table(table(paidsex$contactphone))))
tcp

tcin <-  cbind(Freq = table(paidsex$contactinternet), Perc = prop.table(table(paidsex$contactcontactinternet)),
              Cum = cumsum(prop.table(table(paidsex$contactinternet))))
tcin

tcbr <-  cbind(Freq = table(paidsex$contactbrothel), Perc = prop.table(table(paidsex$contactbrothel)),
              Cum = cumsum(prop.table(table(paidsex$contactbrothel))))
tcbr

tcst<-  cbind(Freq = table(paidsex$contactstreet), Perc = prop.table(table(paidsex$contactstreet)),
               Cum = cumsum(prop.table(table(paidsex$contactstreet))))
tcst

tcbro <-  cbind(Freq = table(paidsex$brothel), Perc = prop.table(table(paidsex$brothel)),
              Cum = cumsum(prop.table(table(paidsex$brothel))))
tcbro

tcmot <-  cbind(Freq = table(paidsex$motel), Perc = prop.table(table(paidsex$motel)),
               Cum = cumsum(prop.table(table(paidsex$motel))))
tcmot

tclh <-  cbind(Freq = table(paidsex$clienthome), Perc = prop.table(table(paidsex$clienthome)),
               Cum = cumsum(prop.table(table(paidsex$clienthome))))
tclh

tcsc <-  cbind(Freq = table(paidsex$streetcar), Perc = prop.table(table(paidsex$streetcar)),
               Cum = cumsum(prop.table(table(paidsex$streetcar))))
tcsc


tedu1 <- cbind(Freq = table(paidsex$educ1), Perc = prop.table(table(paidsex$educ1)),
              Cum = cumsum(prop.table(table(paidsex$educ1))))
tedu1

tedu2 <- cbind(Freq = table(paidsex$educ2), Perc = prop.table(table(paidsex$educ2)),
               Cum = cumsum(prop.table(table(paidsex$educ2))))
tedu2

tedu3 <- cbind(Freq = table(paidsex$educ3), Perc = prop.table(table(paidsex$educ3)),
               Cum = cumsum(prop.table(table(paidsex$educ3))))
tedu3

tcmen <- cbind(Freq = table(paidsex$clientmental), Perc = prop.table(table(paidsex$clientmental)),
             Cum = cumsum(prop.table(table(paidsex$clientmental))))
tcmen

tcphy <- cbind(Freq = table(paidsex$clientphydis), Perc = prop.table(table(paidsex$clientphydis)),
             Cum = cumsum(prop.table(table(paidsex$clientphydis))))
tcphy

tol <- cbind(Freq = table(paidsex$clientolder), Perc = prop.table(table(paidsex$clientolder)),
             Cum = cumsum(prop.table(table(paidsex$clientolder))))
tcol

tcun <- cbind(Freq = table(paidsex$clientminor), Perc = prop.table(table(paidsex$clientminor)),
             Cum = cumsum(prop.table(table(paidsex$clientminor))))
tcun

tcfe <- cbind(Freq = table(paidsex$clientwomen), Perc = prop.table(table(paidsex$clientwomen)),
             Cum = cumsum(prop.table(table(paidsex$clientwomen))))
tcfe

tcpri <- cbind(Freq = table(paidsex$clientprison), Perc = prop.table(table(paidsex$clientprison)),
             Cum = cumsum(prop.table(table(paidsex$clientprison))))
tcpri


tcsv <- cbind(Freq = table(paidsex$clientsexualviolence), Perc = prop.table(table(paidsex$clientsexualviolence)),
              Cum = cumsum(prop.table(table(paidsex$clientsexualviolence))))
tcsv

tcpv <- cbind(Freq = table(paidsex$clientphysicalviolence), Perc = prop.table(table(paidsex$clientphysicalviolence)),
              Cum = cumsum(prop.table(table(paidsex$clientphysicalviolence))))
tcpv


##Determinants Factor analysis##


factoran <- paidsex[,  c("Martires","venezolana", "notfrombog", "drinks", "stigma", "contactphone", "contactinternet", "contactstreet","contactbrothel", "motel", "brothel", "clienthome", "streetcar", "clientmental", "clientphydis", "clientolder", "clientminor", "clientwomen", "clientprison", "age", "educ", "numclients","payperclient", "yearsworking")]

summary(paidsex$code)



new_css <- list(css.table = "border: 2px solid;", css.thead = "border: 1px solid; text-align:centre;",
                css.tr = "border: 1px solid; color:black;", css.td = "border: 1px solid;")

facto <- sjt.fa(factoran, nmbr.fctr = 3, rotation = c("promax"), method = "minres", CSS = new_css)

facto
fact <- psych::fa(r = new, nfactors = 5, rotate = "promax", fm="pa")

psych::fa.diagram(fact,
                  sort=TRUE,
                  cut=.5,
                  simple=FALSE,
                  digits=1)

pl <- c(
  `(Intercept)` = "Intercept",
  mdp = "Multidimensional Poverty",
  lpsumbog = "Physical Violence against Women rate",
  dsumbog = "Sexual Violence against Women rate",
  Martires = "Martires tolerance zone",
  policeabuse = "Police Harassment",
  venezolana = "Venezuelan",
  notfrombog = "Colombian not from Bogota",
  age = "Age",
  educ1 = "From no education to incomplete secondary",
  educ2 = "Finished secondary studies",
  educ3 = "Started or completed post-secondary studies",
  drinks = "Drinks alchohol more than 5 times a week",
  stigma = "Feels stigmatised by their community",
  contactphone ="Contacts clients by phone",
  contactinternet ="Contacts clients online",
  contactstreet ="Contacts clients on the street",
  contactbrothel ="Contacts clients inside brothels",
  motel= "Provides services at motels and hotels",
  brothel= "Provides services at a brothels",
  clienthome= "Provides services at a clients' homes",
  streetcar= "Provides services on the street and inside cars",
  clientmental= "clients with mental disabilities",
  clientphydis= " clients with physical disabilities",
  clientolder= " older adult clients",
  clientminor= " underage clients",
  clientwomen= " female clients",
  clientprison= "clients in prison",
  numclients = "Number of clients per day",
  payperclient = "Payment per service in COP",
  yearsworking = "Years in sex work",
  interviewstreet ="Was interviewed on the street"
)

## Hypothesis 1 - Tolerance Zones##

fitca <- glm(clientsexualviolence ~ Martires, data = paidsex, family = binomial("logit"))
summary(fitca)
fitcsmar <- glm (clientsexualviolence ~ Martires + age + educ2 + educ3  + numclients + payperclient + yearsworking + clientmental + clientphydis + clientolder + clientminor + clientwomen +  clientprison, data = paidsex, family = binomial(logit))
summary (fitcsmar)

fitphmarn <- glm(clientphysicalviolence ~ Martires, data = paidsex, family = binomial("logit"))
summary(fitphmarn)
fitphmar <- glm (clientphysicalviolence ~ Martires + age + educ2 + educ3 + numclients + payperclient + yearsworking + clientmental + clientphydis + clientolder + clientminor + clientwomen +  clientprison, data = paidsex, family = binomial(logit))
summary (fitphmar)


tab_model(
  fitcsmar, fitphmar,
  pred.labels = pl)
  


##Hypothesis 2 - Police Harassment##

fitpa <- glm(clientsexualviolence ~ policeabuse, data = paidsex, family = binomial("logit"))
summary(fitcpa)
fitcspa <- glm (clientsexualviolence ~ policeabuse + age + educ2 + educ3  + numclients + payperclient + yearsworking + clientmental + clientphydis + clientolder + clientminor + clientwomen +  clientprison, data = paidsex, family = binomial(logit))
summary (fitcspa)

fitphpan <- glm(clientphysicalviolence ~ policeabuse, data = paidsex, family = binomial("logit"))
summary(fitphpan)
fitphpa <- glm (clientphysicalviolence ~ policeabuse + age + educ2 + educ3 + numclients + payperclient + yearsworking + clientmental + clientphydis + clientolder + clientminor + clientwomen +  clientprison, data = paidsex, family = binomial(logit))
summary (fitphmpa)


tab_model(
  fitcspa, fitphpa,
  pred.labels = pl)



##Hypothesis 3 - Migration##

fitcami <- glm(clientsexualviolence ~ venezolana + notfrombog, data = paidsex, family = binomial("logit"))
summary(fitcami)
fitcsmi <- glm (clientsexualviolence ~ venezolana + notfrombog + age + educ2 + educ3 + stigma + numclients + payperclient + yearsworking + clientmental + clientphydis + clientolder + clientminor + clientwomen +  clientprison, data = paidsex, family = binomial(logit))
summary (fitcsmar)

fitphmi <- glm(clientphysicalviolence ~ venezolana + notfrombog, data = paidsex, family = binomial("logit"))
summary(fitphmi)
fitphmi <- glm (clientphysicalviolence ~ venezolana + notfrombog + age + educ2 + educ3 + numclients + payperclient + yearsworking + clientmental + clientphydis + clientolder + clientminor + clientwomen +  clientprison, data = paidsex, family = binomial(logit))
summary (fitphmi)


tab_model(
  fitcsmi, fitphmi,
  pred.labels = pl)


## Hypothesis4 - Stigma##

fitsti <- glm(clientsexualviolence ~ stigma, data = paidsex, family = binomial("logit"))
summary(fitsti)
fitcssti <- glm (clientsexualviolence ~ stigma + age + educ2 + educ3  + numclients + payperclient + yearsworking + clientmental + clientphydis + clientolder + clientminor + clientwomen +  clientprison, data = paidsex, family = binomial(logit))
summary (fitcssti)

fitphsti <- glm(clientphysicalviolence ~ stigma, data = paidsex, family = binomial("logit"))
summary(fitphsti)
fitphstit <- glm (clientphysicalviolence ~ stigma + age + educ2 + educ3 + numclients + payperclient + yearsworking + clientmental + clientphydis + clientolder + clientminor + clientwomen +  clientprison, data = paidsex, family = binomial(logit))
summary (fitphstit)


tab_model(
  fitcssti, fitphstit,
  pred.labels = pl)



## Hypothesis5 - Environmental##


fitcsen <- glm (clientsexualviolence ~  contactphone + contactinternet + contactbrothel + contactstreet + brothel + motel + clienthome + streetcar + interviewstreet + age + educ2 + educ3  + numclients + payperclient + yearsworking + clientmental + clientphydis + clientolder + clientminor + clientwomen +  clientprison, data = paidsex, family = binomial(logit))
summary (fitcsen)



fitphent <- glm (clientphysicalviolence ~ contactphone + contactinternet + contactbrothel + contactstreet + brothel + motel + clienthome + streetcar + interviewstreet + age + educ2 + educ3 + numclients + payperclient + yearsworking + clientmental + clientphydis + clientolder + clientminor + clientwomen +  clientprison, data = paidsex, family = binomial(logit))
summary (fitphent)


tab_model(
  fitcsen, fitphent,
  pred.labels = pl)





## Hypothesis 6 - spatial violence##


sexviol <- subset(paidsex, paidsex$clientsexualviolence==1)
physicalvio <- subset(paidsex, paidsex$clientphysicalviolence==1)
policeabuse <-subset(paidsex, paidsex$policeabuse ==1)


sexloc <- sexviol %>%
  group_by(code) %>%
  summarise(sexloc1=n())



phyloc <- physicalvio %>%
  group_by(code) %>%
  summarise(physicalvio=n())


tot <-paidsex %>%
  group_by(code) %>%
  summarise(tot=n())



polloc <- policeabuse %>%
  group_by(code)%>%
  summarise(polloc1=n())


violence1 <- left_join(phyloc, sexloc, by = c("code"="code"))
violence2 <- left_join(tot, violence1, by = c("code"="code"))
violence3 <- left_join(violence2, polloc, by = c("code"="code"))

violence3[is.na(violence3)] <- 0


violence3[nrow(violence3) + 1,] = list(code = "4", tot = 1, physicalvio = 0, sexloc = 0, polloc =0)


violence3$ratephyvio <- as.numeric(violence3$physicalvio/violence3$tot *10000)

violence3$ratesexvio <- as.numeric(violence3$sexloc1/violence3$tot *10000)

##mapping rates##

urlfile <- 'https://www.dropbox.com/s/9cbu47fjetjlqb1/localidades2.zip?dl=0'
download.file(urlfile, 'localidades2.zip')
unzip('localidades.zip', exdir = 'loca')



shp <- 'loca/localidades.shp'
bog <- st_read(shp)
bogo <- bog[-c(15), ]

BOG_WGS84 <- st_transform(bogo, 4326)
st_crs(BOG_WGS84)
plot(st_geometry(BOG_WGS84))


bogospatial <- left_join(BOG_WGS84, violence3, by = c("CODIGO_LOC" ="code"))


 current_style <- tmap_style("col_blind")
plot(st_geometry(bogospatial))
tmap_mode("view")
map1 <- tm_shape(bogospatial) +
  tm_fill("tot", style="quantile", title = "count n=2625", palette = "Greys") +
  tm_layout(main.title = "FSW per municipality", main.title.size = 0.9, legend.position = c("right", "bottom"),
            legend.title.size = 0.8,
            legend.text.size = 0.7) +
  tm_text('CODIGO_LOC')
map1

map2 <- tm_shape(bogospatial) +
  tm_fill("ratesexvio", style="jenks", title = "rate per 10,000 FSW", palette = "Greys") +
  tm_layout(main.title = "Reports of Client Sexual Violence", main.title.size = 0.7, legend.position = c("right", "bottom"),
            legend.title.size = 0.8,
            legend.text.size = 0.7) +
  tm_text('CODIGO_LOC')
map2
map3 <- tm_shape(bogospatial) +
  tm_fill("ratephyvio", style="jenks", title = "rate per 10,000 FSW", palette = "Greys") +
  tm_layout(main.title = "Reports of Client Physical Violence", main.title.size = 0.7, legend.position = c("right", "bottom"),
            legend.title.size = 0.8,
            legend.text.size = 0.7) +
  tm_text('CODIGO_LOC')

map3

tmap_arrange(map2, map3)

## Testing global spatial autocorrelation##

bogotspatial <- as(bogospatial, "Spatial")
bogotspatial$ratesexvio <-as.numeric(bogotspatial$ratesexvio)
w <- poly2nb(bogotspatial, row.names=bogotspatial$CODIGO_LOC)
class(w)
str(w)
plot(bogotspatial, col='gray', border='blue', lwd=2)

xy <- coordinates(bogotspatial)

library(spdep)

plot(w, xy, col='red', lwd=2, add=TRUE)
wm <- nb2mat(w, style='B')
wm
ww <-  nb2listw(w, style='B')
ww
moran(bogotspatial$sexloc1, ww, n=length(ww$neighbours), S0=Szero(ww))

moran(bogotspatial$ratesexvio, ww, n=length(ww$neighbours), S0=Szero(ww))

moran(bogotspatial$physicalvio, ww, n=length(ww$neighbours), S0=Szero(ww))


set.seed(1234)
mun_moranmc_results1 <- moran.mc(bogotspatial$ratesexvio, ww, nsim=99)
mun_moranmc_results1
mun_moranmc_results2 <- moran.mc(bogotspatial$ratephyvio, ww, nsim=99)
mun_moranmc_results2

 
## Testing for local spatial autocorrelation##

wm <- nb2mat(w, style='B')
wm
rwm <- mat2listw(wm, style='W')
mat <- listw2mat(rwm)
apply(mat, 1, sum)[1:15]
moran(bogotspatial$ratesexvio, ww, n=length(ww$neighbours), S0=Szero(ww))
moran.mc(bogotspatial$ratesexvio, ww, nsim=99999)
moran(bogotspatial$ratephyvio, ww, n=length(ww$neighbours), S0=Szero(ww))
moran.mc(bogotspatial$ratephyvio, ww, nsim=99999)


