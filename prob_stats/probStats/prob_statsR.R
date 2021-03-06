#Nos données
ourData <- read.csv("test3.csv", TRUE, ",")
na_vec <- which(!complete.cases(ourData))
ourData_noNA<- ourData[-na_vec,]
n <- length(tauxCroissance)


tauxCroissance <- ourData_noNA$Taux_Croissance_Pop
consommationAlcool <- ourData_noNA$Consommation_Alcool_Par_Habitant
alphabetisation <- ourData_noNA$Taux_Alphabetisation

#Partie 1

#Taux de croissance
res <- boxplot(tauxCroissance, col="red", horizontal = T)
tauxCroissanceNoAber <- (tauxCroissance[!tauxCroissance %in% res$out])

boxplot(tauxCroissanceNoAber, col = "red", horizontal =T, main = "Diagramme en boîte de taux de croissance", xlab = "Taux de croissance annuel (%)")
hist(tauxCroissanceNoAber, col = "red", xlab = "Quantile théorique", ylab = "Quantile observé", main = "Histogramme du taux de croissance")
qqnorm(tauxCroissanceNoAber, main ="Diagramme quantile-quantile du taux de croissance annuel (%)")
qqline(tauxCroissanceNoAber, col = "red")

#Test de normalité
shapiro.test(tauxCroissanceNoAber)

#Paramètres
Scroissance <- sd(tauxCroissance)

#MOYENNE
L_moy_croi <- mean(tauxCroissance) - qt(1-0.025,n-1) * Scroissance / sqrt(n)
U_moy_croi <- mean(tauxCroissance) + qt(1-0.025,n-1) * Scroissance / sqrt(n)

#VARIANCE

L_var_croi <- (n-1)*Scroissance^2/qchisq(1-0.025,n-1)
U_var_croi <- (n-1)*Scroissance^2/qchisq(0.025,n-1)

#Tableau
paramCroi <- matrix(c(L_moy_croi,U_moy_croi,L_var_croi,U_var_croi),ncol=2,byrow=TRUE)
colnames(paramCroi) <- c("Lower","Upper")
rownames(paramCroi) <- c("Moyenne","Variance")
paramCroi <- as.table(paramCroi)
paramCroi



#Consommation d'alcool
boxplot(consommationAlcool, col = "blue", horizontal =T, main = "Diagramme en boîte de la consommation d'alcool annuelle", xlab = "Consommation d'alcool annuelle par personne (L)")
hist(consommationAlcool, col = "blue", xlab = "Consommation annuelle d'alcool pure par personne (L)", ylab = "Fréquence", main = "Histogramme de la consommation d'alcool annuelle")
qqnorm(consommationAlcool, main ="Diagramme quantile-quantile de la consommation d'alcool annuelle par personne", xlab = "Quantile théorique", ylab = "Quantile observé")
qqline(consommationAlcool, col = "blue")


shapiro.test(consommationAlcool)
resConso <- boxplot(consommationAlcool, col="green", horizontal = T)
consoAlcoolNoAber <- (consommationAlcool[!consommationAlcool %in% resConso$out])


#Paramètres
Salcool <- sd(consoAlcoolNoAber)

#MOYENNE
L_moy_alcool <- mean(consoAlcoolNoAber) - qt(1-0.025,n-1) * Salcool / sqrt(n)
U_moy_alcool <- mean(consoAlcoolNoAber) + qt(1-0.025,n-1) * Salcool / sqrt(n)

#VARIANCE

L_var_alcool <- (n-1)*Salcool^2/qchisq(1-0.025,n-1)
U_var_alcool <- (n-1)*Salcool^2/qchisq(0.025,n-1)

#Tableau
paramAlcool <- matrix(c(L_moy_alcool,U_moy_alcool,L_var_alcool,U_var_alcool),ncol=2,byrow=TRUE)
colnames(paramAlcool) <- c("Lower","Upper")
rownames(paramAlcool) <- c("Moyenne","Variance")
paramAlcool <- as.table(paramAlcool)
paramAlcool

#Alphabétisation
resAlphabet <- boxplot(alphabetisation, col="green", horizontal = T)
alphabetisationNoAber <- (alphabetisation[!alphabetisation %in% resAlphabet$out])

boxplot(alphabetisationNoAber, col = "green", horizontal =T, main = "Diagramme en boîte du taux d'alphabétisation", xlab = "Taux d'alphabétisation de la population (%)")
hist(alphabetisationNoAber, col = "green", xlab = "Taux d'alphabétisation de la population (%)", ylab = "Fréquence", main = "Histogramme du taux d'alphabétisation")
qqnorm(alphabetisationNoAber, main ="Diagramme quantile-quantile du taux d'alphabétisation de la population", xlab = "Quantile théorique", ylab = "Quantile observé")
qqline(alphabetisationNoAber, col = "green")

shapiro.test(alphabetisationNoAber)


#parameters

Salpha <- sd(alphabetisationNoAber)

#MOYENNE
L_moy_alpha <- mean(alphabetisationNoAber) - qt(1-0.025,n-1) * Salpha / sqrt(n)
U_moy_alpha <- mean(alphabetisationNoAber) + qt(1-0.025,n-1) * Salpha / sqrt(n)

#VARIANCE
L_var_alpha <- (n-1)*Salpha^2/qchisq(1-0.025,n-1)
U_var_alpha <- (n-1)*Salpha^2/qchisq(0.025,n-1)

#Tableau
paramAlpha <- matrix(c(L_moy_alpha,U_moy_alpha,L_var_alpha,U_var_alpha),ncol=2,byrow=TRUE)
colnames(paramAlpha) <- c("Lower","Upper")
rownames(paramAlpha) <- c("Moyenne","Variance")
paramAlpha <- as.table(paramAlpha)
paramAlpha







#Tests d'hypothèse à critères (Europe vs reste du monde)
ourDataEurope <- read.csv("projet_Donnee_europe.csv", TRUE, ",")
na_vec_eur <- which(!complete.cases(ourDataEurope))
ourData_noNA_eur<- ourDataEurope[-na_vec_eur,]


d_Europe_croissance <- ourData_noNA_eur$Taux_Croissance_Pop[ourData_noNA_eur$Europe == "1"]
d_nonEurope_croissance <- ourData_noNA_eur$Taux_Croissance_Pop[!ourData_noNA_eur$Europe == "1"]

d_Europe_alcool <- ourData_noNA_eur$Consommation_Alcool_Par_Habitant[ourData_noNA_eur$Europe == "1"]
d_nonEurope_alcool <- ourData_noNA_eur$Consommation_Alcool_Par_Habitant[!ourData_noNA_eur$Europe == "1"]

d_Europe_alphabet <- ourData_noNA_eur$Taux_Alphabetisation[ourData_noNA_eur$Europe == "1"]
d_nonEurope_alphabet <- ourData_noNA_eur$Taux_Alphabetisation[!ourData_noNA_eur$Europe == "1"]

#Test de variance
var.test(d_Europe_alcool, d_nonEurope_alcool)
var.test(d_Europe_croissance, d_nonEurope_croissance)
var.test(d_Europe_alphabet, d_nonEurope_alphabet)

#T-test
t.test(d_Europe_alcool, d_nonEurope_alcool)
t.test(d_Europe_croissance, d_nonEurope_croissance)
t.test(d_Europe_alphabet, d_nonEurope_alphabet)


#Regression lineaire entre le taux dalpha et la conso
hist(tauxCroissanceNoAber)
linModel <- lm(alphabetisation ~ tauxCroissanceNoAber, data=ourData)
summary(linModel)
confint(linModel)

#Graphique de notre régression linéaire
plot(tauxCroissanceNoAber, alphabetisation)
abline(linModel, col="red")

#Combinaison de graphes
par(mfrow = c(2,2))
plot(linModel)






