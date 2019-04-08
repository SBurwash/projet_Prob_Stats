#Nos données
ourData <- read.csv("myData.csv", TRUE, ",")
na_vec <- which(!complete.cases(ourData))
ourData_noNA<- ourData[-na_vec,]
n <- length(tauxCroissance)


tauxCroissance <- ourData_noNA$Taux_Croissance_Pop
consommationAlcool <- ourData_noNA$Consommation_Alcool_Par_Habitant
alphabetisation <- ourData_noNA$Taux_Alphabetisation

#Partie 1

#Taux de croissance
boxplot(tauxCroissance, col="red", horizontal = T)
hist(tauxCroissance, col = "red")
qqnorm(tauxCroissance, main ="Diagramme de probabilités normal - ou Q-Q Plot of x")
qqline(tauxCroissance, col = "red")

res <- boxplot(tauxCroissance, col="red", horizontal = T)
tauxCroissanceNoAber <- (tauxCroissance[!tauxCroissance %in% res$out])

boxplot(tauxCroissanceNoAber, col = "red", horizontal =T)
hist(tauxCroissanceNoAber, col = "red")
qqnorm(tauxCroissanceNoAber, main ="Diagramme de probabilités normal - ou Q-Q Plot of x")
qqline(tauxCroissanceNoAber, col = "red")


shapiro.test(tauxCroissanceNoAber)


#parameters

Scroissance <- sd(tauxCroissance)

#MOYENNE
L_moy_croi <- mean(tauxCroissance) - qt(1-0.025,n-1) * Scroissance / sqrt(n)
U_moy_croi <- mean(tauxCroissance) + qt(1-0.025,n-1) * Scroissance / sqrt(n)

#VARIANCE

L_var_croi <- (n-1)*Scroissance^2/qchisq(1-0.025,n-1)
U_var_croi <- (n-1)*Scroissance^2/qchisq(0.025,n-1)


#Consommation d'alcool

boxplot(consommationAlcool, col = "blue", horizontal =T)
hist(consommationAlcool, col = "blue")
qqnorm(consommationAlcool, main ="Diagramme de probabilités normal - ou Q-Q Plot of x")
qqline(consommationAlcool, col = "blue")
















#Analphabétisation 
resAlphabet <- boxplot(alphabetisation, col="green", horizontal = T)
alphabetisationNoAber <- (alphabetisation[!alphabetisation %in% resAlphabet$out])

boxplot(alphabetisationNoAber, col = "green", horizontal =T)
hist(alphabetisationNoAber, col = "green")
qqnorm(alphabetisationNoAber, main ="Diagramme de probabilités normal - ou Q-Q Plot of x")
qqline(alphabetisationNoAber, col = "green")








