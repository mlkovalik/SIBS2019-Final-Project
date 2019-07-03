# Libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)


# Basic Tables & Plots ----------------------------------------------------

table(drugConsumption$Age)
plot(drugConsumption$Age)

table(drugConsumption$Gender)
plot(drugConsumption$Gender)

table(drugConsumption$Education)
plot(drugConsumption$Education)

table(drugConsumption$Country)
plot(drugConsumption$Country)

table(drugConsumption$Ethnicity)
plot(drugConsumption$Ethnicity)

table(drugConsumption$Neuroticism)
plot(drugConsumption$Neuroticism)

table(drugConsumption$Extraversion)
plot(drugConsumption$Extraversion)

table(drugConsumption$OpennessToExperience)
plot(drugConsumption$OpennessToExperience)

table(drugConsumption$Agreeableness)
plot(drugConsumption$Agreeableness)

table(drugConsumption$Conscientiousness)
plot(drugConsumption$Conscientiousness)

table(drugConsumption$Alcohol)
plot(drugConsumption$Alcohol)

table(drugConsumption$Amphetamines)
plot(drugConsumption$Amphetamines)

table(drugConsumption$AmylNitrite)
plot(drugConsumption$AmylNitrite)

table(drugConsumption$Benzodiazepine)
plot(drugConsumption$Benzodiazepine)

table(drugConsumption$Caffeine)
plot(drugConsumption$Caffeine)

table(drugConsumption$Cannabis)
plot(drugConsumption$Cannabis)

table(drugConsumption$Chocolate)
plot(drugConsumption$Chocolate)

table(drugConsumption$Cocaine)
plot(drugConsumption$Cocaine)

table(drugConsumption$Crack)
plot(drugConsumption$Crack)

table(drugConsumption$Ectasy)
plot(drugConsumption$Ectasy)

table(drugConsumption$Heroin)
plot(drugConsumption$Heroin)

table(drugConsumption$Ketamine)
plot(drugConsumption$Ketamine)

table(drugConsumption$LegalHighs)
plot(drugConsumption$LegalHighs)

table(drugConsumption$LSD)
plot(drugConsumption$LSD)

table(drugConsumption$Methadone)
plot(drugConsumption$Methadone)

table(drugConsumption$MagicMushrooms)
plot(drugConsumption$MagicMushrooms)

table(drugConsumption$Nicotine)
plot(drugConsumption$Nicotine)

table(drugConsumption$FakeSemeron)
plot(drugConsumption$FakeSemeron)

table(drugConsumption$VolatileSubstanceAbuse)
plot(drugConsumption$VolatileSubstanceAbuse)


# Looking at Frequencies --------------------------------------------------

#Countries
  countryFreq <- drugConsumption %>%
    group_by(Country) %>%
    summarise(Counts = n())

  ggplot(countryFreq, aes(Country, Counts)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=Counts), vjust=-0.3)

# Graphical Comparisons ---------------------------------------------------
  
#Alcohol

  #Alcohol consumption ~ gender
    ggplot(drugConsumption, aes(Alcohol, color=Gender, fill=Gender)) +
      geom_bar(position="dodge") +
      labs(title="Alcohol Consumption by Gender", x="Latest Alcohol Consumption", fill="Gender") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
    ggplot(drugConsumption, aes(Alcohol)) +
      geom_bar(position = "dodge") +
      facet_grid(. ~ Gender) +
      labs(title="Alcohol Consumption by Gender", x="Latest Alcohol Consumption") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
  #Alcohol consumption ~ age
    ggplot(drugConsumption, aes(Alcohol, fill=Age)) +
      geom_bar(position = "dodge") +
      labs(title="Alcohol Consumption by Age", x="Latest Alcohol Consumption", fill="Age Group") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal")
    
    ggplot(drugConsumption, aes(Alcohol)) +
      geom_bar(position = "dodge") +
      facet_grid(. ~ Age) +
      labs(title="Alcohol Consumption by Age", x="Latest Alcohol Consumption") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal")
    
  #Alcohol consumption ~ country
    ggplot(drugConsumption, aes(Alcohol, fill=Country)) +
      geom_bar(position = "dodge") +
      labs(title="Alcohol Consumption by Country", x="Latest Alcohol Consumption", fill="Country") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal")
    
    ggplot(drugConsumption, aes(Alcohol)) +
      geom_bar(position = "dodge") +
      facet_grid(. ~ Country) +
      labs(title="Alcohol Consumption by Country", x="Latest Alcohol Consumption") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal")

#Cannabis
    
  #Cannabis consumption ~ gender
    ggplot(drugConsumption, aes(Cannabis, fill=Gender)) +
      geom_bar(position="dodge") +
      labs(title="Cannabis Consumption by Gender", x="Latest Cannabis Consumption", fill="Gender") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
    ggplot(drugConsumption, aes(Cannabis)) +
      geom_bar(position = "dodge") +
      facet_grid(. ~ Gender) +
      labs(title="Cannabis Consumption by Gender", x="Latest Cannabis Consumption") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
  #Cannabis consumption ~ age
    ggplot(drugConsumption, aes(Cannabis, fill=Age)) +
      geom_bar(position="dodge") +
      labs(title="Cannabis Consumption by Age", x="Latest Cannabis Consumption", fill="Age Group") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
    ggplot(drugConsumption, aes(Cannabis)) +
      geom_bar(position = "dodge") +
      facet_grid(. ~ Age) +
      labs(title="Cannabis Consumption by Age", x="Latest Cannabis Consumption") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
  #Cannabis consumption ~ country
    ggplot(drugConsumption, aes(Cannabis, fill=Country)) +
      geom_bar(position="dodge") +
      labs(title="Cannabis Consumption by Country", x="Latest Cannabis Consumption", fill="Age Group") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
    ggplot(drugConsumption, aes(Cannabis)) +
      geom_bar(position = "dodge") +
      facet_grid(. ~ Country) +
      labs(title="Cannabis Consumption by Country", x="Latest Cannabis Consumption") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
#Nicotine
    
  #Nicotine consumption ~ gender
    ggplot(drugConsumption, aes(Nicotine, fill=Gender)) +
      geom_bar(position="dodge") +
      labs(title="Nicotine Consumption by Gender", x="Latest Nicotine Consumption", fill="Gender") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
    ggplot(drugConsumption, aes(Nicotine)) +
      geom_bar(position = "dodge") +
      facet_grid(. ~ Gender) +
      labs(title="Nicotine Consumption by Gender", x="Latest Nicotine Consumption") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
  #Nicotine consumption ~ age
    ggplot(drugConsumption, aes(Nicotine, fill=Age)) +
      geom_bar(position="dodge") +
      labs(title="Nicotine Consumption by Age", x="Latest Nicotine Consumption", fill="Age Group") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
    ggplot(drugConsumption, aes(Nicotine)) +
      geom_bar(position = "dodge") +
      facet_grid(. ~ Age) +
      labs(title="Nicotine Consumption by Age", x="Latest Nicotine Consumption") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
  #Nicotine consumption ~ country
    ggplot(drugConsumption, aes(Nicotine, fill=Country)) +
      geom_bar(position="dodge") +
      labs(title="Nicotine Consumption by Country", x="Latest Nicotine Consumption", fill="Age Group") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    
    ggplot(drugConsumption, aes(Nicotine)) +
      geom_bar(position = "dodge") +
      facet_grid(. ~ Country) +
      labs(title="Nicotine Consumption by Country", x="Latest Nicotine Consumption") +
      theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size = 20, face="bold.italic"), legend.position="bottom", 
            legend.box = "horizontal") 
    

