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

paramCroi <- matrix(c(L_moy_croi,U_moy_croi,L_var_croi,U_var_croi),ncol=2,byrow=TRUE)
colnames(paramCroi) <- c("Lower","Upper")
rownames(paramCroi) <- c("Moyenne","Variance")
paramCroi <- as.table(paramCroi)
paramCroi
#Consommation d'alcool

boxplot(consommationAlcool, col = "blue", horizontal =T)
hist(consommationAlcool, col = "blue")
qqnorm(consommationAlcool, main ="Diagramme de probabilités normal - ou Q-Q Plot of x")
qqline(consommationAlcool, col = "blue")


shapiro.test(consommationAlcool)
resConso <- boxplot(consommationAlcool, col="green", horizontal = T)
consoAlcoolNoAber <- (consommationAlcool[!consommationAlcool %in% resConso$out])


#parameters

Salcool <- sd(consoAlcoolNoAber)

#MOYENNE
L_moy_alcool <- mean(consoAlcoolNoAber) - qt(1-0.025,n-1) * Salcool / sqrt(n)
U_moy_alcool <- mean(consoAlcoolNoAber) + qt(1-0.025,n-1) * Salcool / sqrt(n)

#VARIANCE

L_var_alcool <- (n-1)*Salcool^2/qchisq(1-0.025,n-1)
U_var_alcool <- (n-1)*Salcool^2/qchisq(0.025,n-1)

paramAlcool <- matrix(c(L_moy_alcool,U_moy_alcool,L_var_alcool,U_var_alcool),ncol=2,byrow=TRUE)
colnames(paramAlcool) <- c("Lower","Upper")
rownames(paramAlcool) <- c("Moyenne","Variance")
paramAlcool <- as.table(paramAlcool)
paramAlcool












#Analphabétisation 
resAlphabet <- boxplot(alphabetisation, col="green", horizontal = T)
alphabetisationNoAber <- (alphabetisation[!alphabetisation %in% resAlphabet$out])

boxplot(alphabetisationNoAber, col = "green", horizontal =T)
hist(alphabetisationNoAber, col = "green")
qqnorm(alphabetisationNoAber, main ="Diagramme de probabilités normal - ou Q-Q Plot of x")
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

paramAlpha <- matrix(c(L_moy_alpha,U_moy_alpha,L_var_alpha,U_var_alpha),ncol=2,byrow=TRUE)
colnames(paramAlpha) <- c("Lower","Upper")
rownames(paramAlpha) <- c("Moyenne","Variance")
paramAlpha <- as.table(paramAlpha)
paramAlpha







#TESTS d'hypothèse

ourDataEurope <- read.csv("projet_Donnee_europe.csv", TRUE, ",")
na_vec_eur <- which(!complete.cases(ourDataEurope))
ourData_noNA_eur<- ourDataEurope[-na_vec_eur,]

d_Europe_croissance <- ourData_noNA_eur$Taux_Croissance_Pop[ourData_noNA_eur$Europe == "1"]
d_nonEurope_croissance <- ourData_noNA_eur$Taux_Croissance_Pop[!ourData_noNA_eur$Europe == "1"]

d_Europe_alcool <- ourData_noNA_eur$Consommation_Alcool_Par_Habitant[ourData_noNA_eur$Europe == "1"]
d_nonEurope_alcool <- ourData_noNA_eur$Consommation_Alcool_Par_Habitant[!ourData_noNA_eur$Europe == "1"]

d_Europe_alphabet <- ourData_noNA_eur$Taux_Alphabetisation[ourData_noNA_eur$Europe == "1"]
d_nonEurope_alphabet <- ourData_noNA_eur$Taux_Alphabetisation[!ourData_noNA_eur$Europe == "1"]


var.test(d_Europe_alcool, d_nonEurope_alcool)
var.test(d_Europe_croissance, d_nonEurope_croissance)
var.test(d_Europe_alphabet, d_nonEurope_alphabet)

t.test(d_Europe_alcool, d_nonEurope_alcool)
t.test(d_Europe_croissance, d_nonEurope_croissance)
t.test(d_Europe_alphabet, d_nonEurope_alphabet)
#Regression lineaire entre le taux dalpha et la conso
hist(tauxCroissanceNoAber)
linModel <- lm(alphabetisation ~ tauxCroissanceNoAber, data=ourData)
summary(linModel)
confint(linModel)

plot(tauxCroissanceNoAber, alphabetisation)
abline(linModel, col="red")


#EXPO
#REGRESSION
#PARAMETRE - VERIFICATION







