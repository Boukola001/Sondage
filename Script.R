#######################################################
# Thématique : Analyse des données de l'enquête sur "Lycéens et la pratique sportive"
# SAE 1 - Mise en oeuvre d'une enquête (Les lycéens et la pratique sportive)
# Auteur : Groupe 02 BONDON Evan - DEMBELE Salimata - GBAYE Boukola - JOUVENCEL Paolo - OTCHOFFA Karline
# Source : Enquête sur le lycée Paul-Cornu de Lisieux
######################################################

# Debut du script

#####################################################
# ---- Chargement des données ----
#####################################################

readLines(con = "../Data/Groupe02-Donnees_Enquete.csv",
          n = 10)

dataset = read.table(file = "../Data/Groupe02-Donnees_Enquete.csv",
                     sep = ";",
                     header = TRUE,
                     encoding = "latin1",
                     quote = "\"",
                     comment.char = "")
head(dataset)

str(dataset)

within(dataset,
       {
         Sexe = as.factor(Sexe);
         Niveau = factor(Niveau, ordered = TRUE);
         Filiere = as.factor(Filiere);
         Sport_trimestre = as.factor(Sport_trimestre);
         Etat = factor(Etat, ordered = TRUE);
         Sport_lycee = as.factor(Sport_lycee);
         Sport_etude = as.factor(Sport_etude);
         Sport_dehors = as.factor(Sport_dehors);
         Foot = as.factor(Foot);
         Basket = as.factor(Basket);
         Volley = as.factor(Volley);
         Boxe = as.factor(Boxe);
         Tennis_de_table = as.factor(Tennis_de_table);
         Autre1 = as.factor(Autre1);
         Lycee = as.factor(Lycee);
         Club = as.factor(Club);
         Chez_soi = as.factor(Chez_soi);
         Salle_sport = as.factor(Salle_sport);
         Exterieur = as.factor(Exterieur);
         Interieur = as.factor(Interieur);
         Competition = as.factor(Competition);
       }) -> data
    
str(data)
summary(data)

##########################################################################
# Pourcentage d'hommes et de femmes interrogés
##########################################################################

table1 = round(100*prop.table(table(data$Sexe)),2)
print(table1)
graph1 = barplot(table1,
                 col="orange",
                 ylim =c(0,70),
                 xlab = "Lycéens",
                 main = "Répartition des lycéens interrogés par sexe",
                 ylab = "Pourcentages",
                 names.arg = c("Femmes","Hommes"))

text(graph1,
     table1+2,
     labels = paste0(table1,"%"))

##########################################################################
#Répartition des interrogés par classe
##########################################################################

table2 = round(100*prop.table(table(data$Niveau)),2)
print(table2)
graph2 = barplot(table2,
                 col="blue",
                 ylim =c(0,50),
                 main = "Répartition des lycéens interrogés \npar niveaau",
                 ylab = "Pourcentages",
                 xlab = ("Niveaux"),
                 names.arg = c("Seconde","Premiere","Terminale")
)
text(graph2,
     table2+2,
     labels = paste0(table2,"%"))



##########################################################################
#------------------------ Le sport au lycée -----------------------------
##########################################################################

#répartition du sport trimestriel en fonction du niveau

table3 = table( subset(data,
                       select = c("Niveau","Sport_trimestre")))
#Profils lignes :
rows1 = round(100*prop.table(table3,1),2)
print(rows1)
graph3 = barplot(t(rows1),
                xlab = "Niveau",
                names.arg = c("Seconde","Première","Terminale"),
                beside = FALSE,
                ylab = "Pourcentages",
                main = "Répartition du sport trimestriel en fonction du niveau",
                las = 1,
                col = c("grey70","chocolate","salmon","skyblue", "orange","gold","green"),
                #col = c("antiquewhite4", "antiquewhite3",  "antiquewhite2",
                ylim = c(0,110),
                legend.text = TRUE,
                args.legend = list( x= "top",
                                    legend = rev(c("Volleyball", "Basket ball", "Handball", "Badminton", "Athlétisme",
                                              "Tennis de table", "Autre")),
                                    horiz = TRUE,
                                    bty = "n",
                                    xpd = TRUE,
                                    cex = 0.6)
)
percent = apply(t(rows1),2,function(x) c(x[1]/2, head(cumsum(x),-1)+tail(x,-1)/2))
non_zero_percent = t(rows1) > 0
print(non_zero_percent)
text(x = rep(graph3, each = nrow(x = percent))[non_zero_percent],
     y = c(percent)[non_zero_percent],
     labels = (c(paste0(t(rows1)[non_zero_percent],"%"))), 
     cex = 0.8, 
     col = "black")

# Top sports souhaités
table4 = sort(table(data$Ajout_.sport),decreasing = TRUE)[1:5]
graph4 = barplot(rev(table4),
                 xlab = "Effectifs",
                 horiz = TRUE,
                 ylab = "",
                 main = "Top 5 des sports souhaités",
                 las = 1,
                 col = c("pink"))
text(rev(table4)-0.5,
     graph4,
     rev(table4))

# Notes attribuées aux cours d'EPS
table5 = table(data$Sport_lycee)
print(table5)
graph5 = barplot(table5,
                 col = "red2",
                 xlab = "Notes",
                 ylab = "Effectifs",
                 ylim = c(0,30),
                 main = "Répartition des notes attribuées au cours d'EPS")
text(graph5,
     table5+1,
     labels = table5
)

# Ceux qui veulent continuer dans une filière sportive

table6 = round(100*prop.table(table(data$Sport_etude)),2)
print(table6)
graph6 = barplot(table6,
                 xlab = "Etudes sportives",
                 names.arg = c("Non","Oui"),
                 ylab = "Pourcentages",
                 main = "Continuer dans une filière sportive",
                 las = 1,
                 col = c("darkblue","lightblue3"),
                 ylim = c(0,110))

text(graph6,
     table6+2.9,
     labels = paste0(table6,"%"))

##########################################################################
#--------------------- sport en dehors du lycée---------------------------
##########################################################################

# Proportion de pratique sportive
sport_hors_lycée = table(data$Sport_dehors)
print(sport_hors_lycée)
graph_sport_dehors = barplot(sport_hors_lycée,
                             ylab = "Pourcentages",
                             names.arg = c("Non","Oui"),
                             main = "Répartition des lycéens en fonction \ndu sport en dehors du lycée",
                             las = 1,
                             col = c("darkblue","lightblue3"),
                             ylim = c(0,70))

text(graph_sport_dehors,
     sport_hors_lycée+2,
     labels = paste0(sport_hors_lycée, '%'))

# Qui fait du sport en dehors du lycée en fonction du niveau


table7 = table( subset(data,
                       select = c("Niveau","Sport_dehors")))
#Profils lignes :
rows2 = round(100*prop.table(table7,1),2)
print(rows2)
graph7 = barplot(t(rows2),
                xlab = "Niveau",
                names.arg = c("Seconde","Premiere","Terminale"),
                beside = FALSE,
                ylab = "Pourcentages",
                main = "Répartition du sport en dehors du lycée en fonction du niveau",
                las = 1,
                col = c("darkblue","lightblue3"),
                ylim = c(0,110),
                legend.text = TRUE,
                args.legend = list( x= "top",
                                    legend =c("Oui","Non"),
                                    horiz = TRUE,
                                    bty = "n",
                                    xpd = TRUE,
                                    cex = 0.8)
)
percent = apply(t(rows2),2,function(x) c(x[1]/2, head(cumsum(x),-1)+tail(x,-1)/2))
text(x = rep(graph7, each = nrow(x = percent)),
     y = c(percent),
     labels = (c(paste0(t(rows2),"%"))), 
     cex = 0.8, 
     col = "red")

# Qui fait du sport en dehors du lycée en fonction du sexe


table8 = table( subset(data,
                       select = c("Sexe","Sport_dehors")))
#Profils lignes :
rows3 = round(100*prop.table(table8,1),2)
print(rows3)
graph8 = barplot(t(rows3),
                xlab = "Sexe",
                names.arg = c("Homme","Femme"),
                beside = TRUE,
                ylab = "Pourcentages",
                main = "Répartition du sport en dehors du lycée en fonction du Sexe",
                las = 1,
                col = c("darkblue","lightblue3"),
                ylim = c(0,90),
                legend.text = TRUE,
                args.legend = list( x= "top",
                                    legend =c("Oui","Non"),
                                    horiz = TRUE,
                                    bty = "n",
                                    xpd = TRUE,
                                    cex = 0.8)
)
text(x =graph8,
     y = c(t(rows3))+3,
     labels = paste0(c(t(rows3)),"%"),
     cex = 0.8,
     col = "red")


# Répartion des lycéens sportifs selon qu'ils ont fait ou pas de la competiton 


sportifs = subset(data,
                      subset = Sport_dehors == "1")
table9 = round(100*prop.table(table(sportifs$Competition)),2)
graph9 = barplot(table9,
                 xlab = "Compétition",
                 names.arg = c("Oui","Non"),
                 ylab = "Pourcentages",
                 main = "Sportifs ayant participé à des compétitions ou non",
                 las = 1,
                 col = c("darkblue","lightblue3"),
                 ylim = c(0,60))

text(graph9,
     table9+2,
     labels = paste0(table9,"%"))


# Top 5 sports en dehors du lycée

# Récupération des sports autre que ceux proposés
strsplit(x = data$Precisez2,
         split = ",") -> mylist1
str(mylist1)

data.frame(Precisez2 = unlist(mylist1))->sports
table_autres_sp = table(sports$Precisez2)
Foot = table(data$Foot)
Basket = table(data$Basket)
Volley = table(data$Volley)
Boxe = table(data$Boxe)
Ten = table(data$Tennis_de_table)

#Récapitulatifs des nombres d'occurences des sports proposés.
sports_prop = c(Foot["1"],Ten["1"],Basket["1"],Volley["1"],Boxe["1"])
names(sports_prop) = c("Football","Tennis de table","Basketball", "Volley","Boxe")

# Tous les sports pratiqués (proposés comme autres)
tous_sports = c(sports_prop, table_autres_sp)
print(tous_sports)

top5_sports_dehors = sort(tous_sports, decreasing = TRUE)[1:5]
print(top5_sports_dehors)
par(mar = c(5.1,5.5,4.1,2.1))
graph_sp_deh = barplot(rev(top5_sports_dehors),
                       col = "thistle",
                       horiz = TRUE,
                       xlab = "Effectifs",
                       ylab = "",
                       width = 0.15,
                       las = 1,
                       main = "Top 5 des sports pratiqués en dehors du lycée"
)
text(rev(top5_sports_dehors) - 0.5,
     graph_sp_deh,
     labels = rev(top5_sports_dehors))


# Fréquence hebdomadaire de sport
table_freq = round(100*prop.table(table(sportifs$Nbre_heure)),2)
print(table_freq)
frequence_sport = barplot(table_freq,
                          names.arg = c("entre 1 et 2h","entre 2 et 3h","entre 3 et 4h","entre 4 et 5h","entre 5 et 6h", "plus de 7h"),
                          ylab = "Fréquences",
                          main = "Fréquence de l'activité sportive",
                          col = "lightblue",
                          ylim = c(0,40))
text(frequence_sport,
     table_freq+1,
     labels = paste0(table_freq, "%"))


# Top des lieux de pratique sportive

club = table(sportifs$Club)
maison = table(sportifs$Chez_soi)
salle = table(sportifs$Salle_sport)
ext = table(sportifs$Extérieur)
int = table(sportifs$Intérieur)

lieux = c(club["1"],maison["1"],salle["1"],ext["1"],int["1"])
names(lieux)=c("Club", "Chez soi", "Salle", "Extérieur", "Intérieur")
print(lieux)
top3_lieux = sort(lieux, decreasing = TRUE)[1:3]
print(top3_lieux)
graph_lieux = barplot(top3_lieux,
                       col = "purple",
                       #horiz = TRUE,
                       ylab = "Effectifs",
                       xlab = "",
                       width = 0.15,
                       ylim = c(0,60),
                       las = 1,
                       main = "Top 3 des lieux de pratique sportive \n en dehors du lycée"
)
text(graph_lieux,
     top3_lieux+2,
     labels = top3_lieux)



# Top des raisons évoquées

par(mar = c(5.1,7.8,4.1,1.1))
#Sporifs
r_sportifs = sort(table(sportifs$Pourquoi), decreasing = TRUE)[1:3]
print(r_sportifs)
graph_r_sportifs= barplot(rev(r_sportifs),
                       col = "green2",
                       horiz = TRUE,
                       xlab = "Effectifs",
                       ylab = "",
                       width = 0.05,
                       las = 1,
                       main = "Top 3 des raisons de pratique du sport en dehors du lycée",
                       xlim = c(0,35)
)
text(rev(r_sportifs) - 0.5,
     graph_r_sportifs,
     labels = rev(r_sportifs))

#Non sporifs

non_sportifs = subset(data,
                      subset = Sport_dehors == "0")
r_nsportifs = sort(table(non_sportifs$Pourquoi), decreasing = TRUE)[1:3]
print(r_nsportifs)
graph_r_nsportifs= barplot(rev(r_nsportifs),
                          col = "red2",
                          horiz = TRUE,
                          xlab = "Effectifs",
                          ylab = "",
                          width = 0.05,
                          las = 1,
                          main = "Top 3 des raisons de non pratique du sport en dehors du lycée",
                          xlim = c(0,16)
)
text(rev(r_nsportifs) - 0.5,
     graph_r_nsportifs,
     labels = rev(r_nsportifs),
     cex = 1.2)

