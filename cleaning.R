# load packages ----

library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(dplyr)


# read in data ----

titanic <- read.table(file = here("data", "TitanicMess.tsv"), sep = '\t', header = TRUE)



View(titanic)     # podgląd danych w oknie
dim(titanic)      # ilość danych
str(titanic)      # struktura danych
glimpse(titanic)  # struktura danych - lepiej uporządkowana (for human readable)
head(titanic)     # głowa danych
tail(titanic)     # ogon danych
summary(titanic)  # podsumowanie - rodzaje danych w kolumnach
skim(titanic)     # podsumowanie - rodzaje danych w kolumnach - lepiej uporządkowane



# Czyszczenie danych ----
## Naprawa błędów ----

skim(titanic)
unique(titanic[c("Embarked")])
unique(titanic[c("Sex")])
unique(titanic[c("Fare")])


#     - kolumna Embarked - usunięcie wierszy, gdzie brakuje wartości
#     - kolumna Embarked - przycięcie błędnych wartości do jednego znaku
#     - kolumna Embarked - pełne nazwy
titanic <- titanic[-which(titanic$Embarked == ""), ]
titanic$Embarked <- strtrim(titanic$Embarked, 1)
titanic$Embarked[titanic$Embarked=="C"]<-"Cherbourg"
titanic$Embarked[titanic$Embarked=="Q"]<-"Queenstown"
titanic$Embarked[titanic$Embarked=="S"]<-"Southampton"

#     - kolumna Sex - naprawa błędnych wartości
titanic$Sex[titanic$Sex=="femmale"]<-"female"
titanic$Sex[titanic$Sex=="fem"]<-"female"
titanic$Sex[titanic$Sex=="mal"]<-"male"
titanic$Sex[titanic$Sex=="malef"]<-"male"

#     - kolumna Survived - opisowe
titanic$Survived[titanic$Survived==0]<-"Died"
titanic$Survived[titanic$Survived==1]<-"Survived"

#     - kolumna Fare - usunięcie liter, zamiana , na .
#     - kolumna Fare - konwersja typu
#     - kolumna Fare - grupowanie jako kwartyle
#     - kolumna Fare - usunięcie liter
titanic$Fare <- gsub("[^0-9.-]", "", titanic$Fare)
titanic$Fare <- gsub(",", ".", titanic$Fare)
titanic$Fare <- as.numeric(titanic$Fare)
titanic$Fare <- 
  ifelse(titanic$Fare < quantile(titanic$Fare, prob=c(.25)),"Q1",
  ifelse(titanic$Fare >= quantile(titanic$Fare, prob=c(.25)) & titanic$Fare < quantile(titanic$Fare, prob=c(.5)),"Q2",
         ifelse(titanic$Fare >= quantile(titanic$Fare, prob=c(.75)),"Q4","Q3")))
titanic$Fare <- gsub("[^0-9.-]", "", titanic$Fare)
titanic$Fare <- as.numeric(titanic$Fare)


#     - kolumna Age - usunięcie wszystkiego po przecinku
#     - kolumna Age - konwersja typu
#     - kolumna Age - usunięcie wartości powyżej 100 i poniżej 0
titanic$Age <- gsub("(.*),.*", "\\1", titanic$Age)
titanic$Age <- as.numeric(titanic$Age)
titanic <- titanic[-which(titanic$Age > 100), ]
titanic <- titanic[-which(titanic$Age < 0), ]

#     - kolumna Family - dodanie kolumny na podstawie kolumn SibSp i Parch
titanic$Family <- titanic$SibSp + titanic$Parch + 1
titanic <- titanic %>% relocate(Family, .after = Age)

# usuwanie zbędnych kolumn
titanic = subset(titanic, select = -c(Cabin,ship,Ticket,PassengerId,Name) )
titanic = subset(titanic, select = -c(SibSp,Parch) )










# uzupełnienie brakujących wartości zerami
#   - dotyczących nr-u biletu
#   - wieku
# titanic$Age[is.na(titanic$Age)] <- 0



# usuwanie powtórzeń w rzędach
titanic <- titanic %>% distinct()

# zapis pliku
write.table(titanic, file = here("data", "TitanicCleaned.tsv"), row.names=FALSE, sep="\t")














# Analiza eksploracyjna ----
library(ggplot2)
library(patchwork)
library(corrplot)
library(RColorBrewer)



## wstępne sprawdzenie korelacji
titanicCor <- read.table(file = here("data", "TitanicCleaned.tsv"), sep = '\t', header = TRUE)


#   - zmiana danych w kolumna na numeryczne (do korelacji)

titanicCor$Sex[titanicCor$Sex=="female"]<-0
titanicCor$Sex[titanicCor$Sex=="male"]<-1
titanicCor$Sex <- as.numeric(titanicCor$Sex)
titanicCor$Embarked <- ifelse(titanicCor$Embarked == "Cherbourg","1",ifelse(titanicCor$Embarked == "Queenstown","2","3"))
titanicCor$Embarked <- as.numeric(titanicCor$Embarked)
titanicCor$Survived <- ifelse(titanicCor$Survived == "Survived","1","0")
titanicCor$Survived <- as.numeric(titanicCor$Survived)
titanicCor$Age[is.na(titanicCor$Age)] <- 0

hSurvived <- titanicCor %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) + 
geom_histogram(bins=30)
hPclass <- titanicCor %>%
  na.omit() %>%
  ggplot(aes(x = Pclass)) + 
  geom_histogram(bins=30)
hSex <- titanicCor %>%
  na.omit() %>%
  ggplot(aes(x = Sex)) + 
  geom_histogram(bins=30)
hAge <- titanicCor %>%
  na.omit() %>%
  ggplot(aes(x = Age)) + 
  geom_histogram(bins=30)
hFamily <- titanicCor %>%
  na.omit() %>%
  ggplot(aes(x = Family)) + 
  geom_histogram(bins=30)
hFare <- titanicCor %>%
  na.omit() %>%
  ggplot(aes(x = Fare)) + 
  geom_histogram(bins=30)
hEmbarked <- titanicCor %>%
  na.omit() %>%
  ggplot(aes(x = Embarked)) + 
  geom_histogram(bins=30)


(hSurvived + hFamily + hSex)/ (hEmbarked + hPclass + hFare) / hAge

corrplot(cor(titanicCor), type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))





titanicclean <- read.table(file = here("data", "TitanicCleaned.tsv"), sep = '\t', header = TRUE)



## 1
# zginęło i przeżyło w liczabach bezwzg.
titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) + 
  geom_bar(aes(fill = Survived), position = "dodge") +
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by Sex")


# zginęło i przeżyło procentowo bedacych na pokladzie
titanicclean %>% 
  na.omit() %>%
  ggplot(aes(Survived)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = Survived)) + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Percent")





# zginęło i przeżyło w ze wzg na plec - liczby bezwzg. 
titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) + 
  geom_bar(aes(fill = Sex), position = "dodge") +
  facet_wrap(~ Sex)+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by Sex")

# zginęło i przeżyło w ze wzg na plec - procentowo 
titanicclean %>%
  ggplot(aes(x = Survived,  group = Sex)) + 
  geom_bar(aes(y = ..prop.., fill = Sex), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~Sex) +
  scale_y_continuous(labels = scales::percent)

# grupa, która zginęła/przeżyła składała się z tylu kobiet/mezczyzn - procentowo
titanicclean %>%
  ggplot(aes(x = Sex,  group = Survived)) + 
  geom_bar(aes(y = ..prop.., fill = Survived), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~ Survived) +
  scale_y_continuous(labels = scales::percent)





# zginęło i przeżyło w liczbach bezwzg. ze wzg na klase
titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) + 
  geom_bar(aes(fill = Pclass), position = "dodge") + 
  facet_wrap(~ Pclass)+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by Pclass")

# zginęło i przeżyło procentowo pasażerów wg klas 
titanicclean %>%
  ggplot(aes(x = Survived,  group = Pclass)) + 
  geom_bar(aes(y = ..prop.., fill = Pclass), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~ Pclass) +
  scale_y_continuous(labels = scales::percent)

# grupa, która zginęła/przeżyła składała się z tylu procent pasażerów klas
titanicclean %>%
  ggplot(aes(x = Pclass,  group = Survived)) + 
  geom_bar(aes(y = ..prop.., fill = Survived), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~Survived) +
  scale_y_continuous(labels = scales::percent)






# zginęło i przeżyło w liczbach bezwzg. ze wzg na ilosc osób w rodzinie
titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) + 
  geom_bar(aes(fill = Family), position = "dodge")+
  facet_wrap(~ Family)+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by Family")

# zginęło i przeżyło procentowo rodzin będacych na pokładzie
titanicclean %>%
  ggplot(aes(x = Survived,  group = Family)) + 
  geom_bar(aes(y = ..prop.., fill = Family), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~ Family) +
  scale_y_continuous(labels = scales::percent)

# grupa, która zginęła/przeżyła składała się z tylu procent rodzin
titanicclean %>%
  ggplot(aes(x = Family,  group = Survived)) + 
  geom_bar(aes(y = ..prop.., fill = Survived), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~Survived) +
  scale_y_continuous(labels = scales::percent)








# zginęło i przeżyło w liczbach bezwzg. ze wzg na oplate
titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) + 
  geom_bar(aes(fill = Fare), position = "dodge")+
  facet_wrap(~ Fare)+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by Fare")

# zginęło i przeżyło procentowo za wzg na oplate
titanicclean %>%
  ggplot(aes(x = Survived,  group = Fare)) + 
  geom_bar(aes(y = ..prop.., fill = Fare), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~ Fare) +
  scale_y_continuous(labels = scales::percent)

# grupa, która zginęła/przeżyła składała się z tylu procent za wzg na oplate
titanicclean %>%
  ggplot(aes(x = Fare,  group = Survived)) + 
  geom_bar(aes(y = ..prop.., fill = Survived), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~Survived) +
  scale_y_continuous(labels = scales::percent)




# zginęło i przeżyło w liczbach bezwzg. ze wzg na wiek
titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) + 
  geom_bar(aes(fill = Age), position = "dodge")+
  facet_wrap(~ Age)+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by Age")




# zginęło i przeżyło w liczbach bezwzg. ze wzg na port załadunku
titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) + 
  geom_bar(aes(fill = Embarked), position = "dodge")+
  facet_wrap(~ Embarked)+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by Embarked")


# zginęło i przeżyło procentowo za wzg na port załadunku
titanicclean %>%
  ggplot(aes(x = Survived,  group = Embarked)) + 
  geom_bar(aes(y = ..prop.., fill = Embarked), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~ Embarked) +
  scale_y_continuous(labels = scales::percent)

# grupa, która zginęła/przeżyła składała się z tylu procent za wzg na port załadunku
titanicclean %>%
  ggplot(aes(x = Embarked,  group = Survived)) + 
  geom_bar(aes(y = ..prop.., fill = Survived), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~Survived) +
  scale_y_continuous(labels = scales::percent)





## 2 violin
titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Pclass, y = Age)) + 
  geom_violin()+
  labs(title  = str_wrap("Pclass",80), 
       subtitle = "by city")

titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Pclass, y = Age)) + 
  geom_violin()+
  facet_grid(~Pclass) +
  labs(title  = str_wrap("Pclass",80), 
       subtitle = "by city")

titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Family, y = Age)) + 
  geom_violin()+
  facet_grid(~ Family) +
  labs(title  = str_wrap("Family",80), 
       subtitle = "by city")

titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Family, y = Age)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  facet_grid(~ Embarked) +
  labs(title  = str_wrap("Family",80), 
       subtitle = "by city")

titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Embarked, y = Age)) + 
  geom_violin()+
  facet_grid(~ Family) +
  labs(title  = str_wrap("Family",80), 
       subtitle = "by city")





## 3
# ile osób zginęło wraz z rodzinami
titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Survived, y = Family)) + 
  geom_jitter()



# średni wiek ludzi w poszczególnych portach
titanicclean %>%
  na.omit() %>%
  group_by(Embarked) %>%
  summarise(meanage = mean(Age)) %>%
  ggplot(aes(x = Embarked, y = meanage)) +
  geom_col()+
  labs(title  = str_wrap("Mean Age",80), 
       subtitle = "by city")

# średni wiek ludzi z poszczególnych portów, którzy przeżyli
titanicclean %>%
  na.omit() %>%
  filter(Survived == "Survived") %>%
  group_by(Embarked) %>%
  summarise(meanage = mean(Age)) %>%
  ggplot(aes(x = Embarked, y = meanage)) +
  geom_col()+
  labs(title  = str_wrap("Mean Age Survivors",80), 
       subtitle = "by city")

# średni wiek ludzi z poszczególnych portów, którzy zgineli
titanicclean %>%
  na.omit() %>%
  filter(Survived == "Died") %>%
  group_by(Embarked) %>%
  summarise(meanage = mean(Age)) %>%
  ggplot(aes(x = Embarked, y = meanage)) +
  geom_col()+
  labs(title  = str_wrap("Mean Age Died",80), 
       subtitle = "by city")

# histogramy
ggplot(titanicclean, aes(x = Age, fill = Survived)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x = "Age with binwidth 5 years",
       title = "Titanic passengers Survived distribution")

ggplot(titanicclean, aes(x = Age, fill = Embarked)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x = "Age with binwidth 5 years",
       title = "Titanic passengers Embarked distribution")

ggplot(titanicclean, aes(x = Age, fill = Sex)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x = "Age with binwidth 5 years",
       title = "Titanic passengers Sex distribution")





## 4
# klasa ze wzg na wiek
titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Pclass)) + 
  geom_bar(aes(fill = Age), position = "dodge")+
  facet_wrap(~ Age)+
  labs(title  = str_wrap("Age",80), 
       subtitle = "by Age")

# które klasy przezyły
titanicclean %>%
  ggplot(aes(x = Pclass, y=Age, group = Pclass)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(y = "Percent") +
  facet_grid(~ Survived)

# które klasy kupowane w których portach
titanicclean %>%
  ggplot(aes(x = Pclass, y=Age, group = Pclass)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(y = "Percent") +
  facet_grid(~ Embarked)





## 5
# w jakim wieku byli ludzie w poszczególnych portach
titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Embarked, y = Age)) + 
  geom_violin()+
  labs(title  = str_wrap("People",80), 
       subtitle = "by city")

# w jakim wieku byli ludzie, którzy zginęli dla poszczególnych portów
titanicclean %>%
  na.omit() %>%
  filter(Survived == "Died") %>%
  ggplot(aes(x = Embarked, y = Age)) + 
  geom_violin()+
  labs(title  = str_wrap("Died",80), 
       subtitle = "by city")

# w jakim wieku byli ludzie, którzy przeżyli dla poszczególnych portów
titanicclean %>%
  na.omit() %>%
  filter(Survived == "Survived") %>%
  ggplot(aes(x = Embarked, y = Age)) + 
  geom_violin()+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by city")




# ile osób w rodzinie dla poszczególnych portów
titanicclean %>%
  na.omit() %>%
  ggplot(aes(x = Embarked, y = Family)) + 
  geom_violin()+
  labs(title  = str_wrap("People",80), 
       subtitle = "by city")

# ile osób zgineło w rodzinie dla poszczególnych portów
titanicclean %>%
  na.omit() %>%
  filter(Survived == "Died") %>%
  ggplot(aes(x = Embarked, y = Family)) + 
  geom_violin()+
  labs(title  = str_wrap("Died",80), 
       subtitle = "by city")

# ile osób przezyło w rodzinie dla poszczególnych portów
titanicclean %>%
  na.omit() %>%
  filter(Survived == "Survived") %>%
  ggplot(aes(x = Embarked, y = Family)) + 
  geom_violin()+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by city")










































