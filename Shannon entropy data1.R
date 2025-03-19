#####Install readxl package to read excel files#####
install.packages("readxl")
library(readxl)
#####Find the path to the excel file#######
file.choose()
#########Data analysis of entropy data Nasopharyngeal samples (S1.xlsx)##################
#########Read excel file de Nasopharyngeal entropy data ##########
read_excel("/../S1.xlsx")
##########Name the excel file###############
nasofaringeal<-read_excel("/../S1.xlsx")
#########View excel file##########
View(nasofaringeal)
######Tracheal aspirates data #######
file.choose()
#########Data analysis of entropy data Tracheal samples (S2.xlsx)##################
#########Read excel file de Tracheal entropy data ##########
read_excel("/../S2.xlsx")
##########Name the excel file###############
lung<-read_excel("/../S2.xlsx")
#########View excel file##########
View(lung)
#########Box plot of entropies in the different types of samples############
nasofaringeal
lung
##########Box plot for all entropy values##########
boxplot(nasofaringeal$Entropy,lung$Entropy,
        ylab= "Diversity",names = c("Nasopharyngeal",
"Tracheal aspirate"),col = rainbow(ncol(trees)),ylim=c(0.0,1.0))
#Histogram for all shannon entropy values in two samples#
hist(nasofaringeal$Entropy,ylim=c(0.0,300),main=NULL,xlab="Shannon entropy")
hist(lung$Entropy,ylim=c(0.0,120),main=NULL,xlab = "Shannon entropy")
########Filter for entropy >=0.3 in Nasopharyngeal###########
subset(x=nasofaringeal,subset = Entropy>=0.3)
######assign name to subset function##########
nasoentropy2<-subset(x=nasofaringeal,subset = Entropy>=0.3)
########Call vector##########
nasoentropy2
########Filter for entropy >=0.5 in tracheal aspirates ###########
subset(x=lung,subset = Entropy>=0.3)
######assign name to subset function##########
lungentropy<-subset(x=lung,subset = Entropy>=0.3)
########Call vector##########
lungentropy
#######Box plot for entropy both samples####
boxplot(nasoentropy2$Entropy,lungentropy$Entropy,
        ylab= "Diversity",names = c("Nasopharyngeal",
                                    "Tracheal aspirate"),col = rainbow(ncol(trees)))
##Summary##
summary(nasoentropy)
summary(lungentropy)
###Histogram for distribution of lungentropy###
hist(lungentropy$Entropy)
########Wilcoxon test#########
wilcox.test(nasoentropy$Entropy,lungentropy$Entropy)
####End###
