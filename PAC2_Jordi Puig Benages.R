if(!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('circlize')) install.packages('circlize'); library('circlize')

df_poblacio <- read.csv(file='Registre_central_de_poblaci__del_CatSalut__poblaci__per_municipi.csv',fileEncoding = "UTF-8")

#Data for chart 1
df_poblacio<- df_poblacio %>% mutate(grupEdat=case_when(
  df_poblacio$edat<18  ~ '0-17',
  df_poblacio$edat>=18 &  df_poblacio$edat<40 ~ '18-39',
  df_poblacio$edat>=40 &  df_poblacio$edat<65 ~ '40-64',
  df_poblacio$edat>=65 ~'65+'))

poblacio_agrupada <- df_poblacio %>% group_by(any,grupEdat) %>% summarize(total_poblacio=sum(població.oficial))

poblacio_agrupada_provincia_poblacio <- df_poblacio %>% filter(any==2021) %>% group_by(província, comarca)%>%summarize(total_poblacio=sum(població.oficial))

write.csv(poblacio_agrupada, 'dades_grÃ fic_barres.csv')

write.csv(poblacio_agrupada_provincia_poblacio, 'dades_barres2.csv')

#Data for Chart 2

poblacio_agrupada2 <- df_poblacio  %>%  group_by(any,província, gènere) %>% summarize(mitjana_edat=sum(poblacio*edat)/sum(poblacio))
poblacio_agrupada2 <- poblacio_agrupada2 %>% filter(província!='SENSE ESPECIFICAR')
write.csv(poblacio_agrupada2, 'dades_grÃ fic_small_multiple.csv')

#Chart3

migraciones <- read.csv(file='Migraciones_Barcelona_2021.csv',fileEncoding = "UTF-8")
migraciones_agrupadas <- migraciones %>% group_by(Nom_Districte_baixa, Nom_Districte_alta) %>% summarize(total=sum(Nombre))

migraciones_agrupadas <- migraciones_agrupadas  %>% filter(Nom_Districte_baixa!=Nom_Districte_alta)

##Create Matrix
### get names for row and columns
nameVals <- sort(unique(unlist(migraciones_agrupadas[1:2])))
### construct 0 matrix of correct dimensions with row and column names
myMat <- matrix(0, length(nameVals), length(nameVals), dimnames = list(nameVals, nameVals))

### fill in the matrix with matrix indexing on row and column names
myMat[as.matrix(migraciones_agrupadas[c("Nom_Districte_baixa", "Nom_Districte_alta")])] <- migraciones_agrupadas[["total"]]

##Chord diagram
par(cex=0.85)
chordDiagram(myMat, transparency = 0.5)

