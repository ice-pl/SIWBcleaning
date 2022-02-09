# pakiety ----

library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(dplyr)
library(ggplot2)
library(patchwork)
library(corrplot)
library(RColorBrewer)
library(cowplot)


# funkcje ----
char2num<-function(x){ 
  groups = unique(x) 
  as.numeric(factor(x, levels=groups)) 
}


# Czyszczenie ----
# ladowanie danych

titanic <- read.table(file = here("data", "TitanicMess.tsv"), sep = '\t', header = TRUE)




# Celem poniższej analizy jest ocena sytuacji w jakiej znaleźli się pasażerowie 
# statku Titanic na podstawie danych statystycznych dotyczących uczestników 
# feralnego rejsu oraz próba odpowiedzi na pytanie co warunkowało szansę na 
# przetrwanie pasażerów.

# Mamy do dyspozycji zbiór o nazwie "Titanic - Machine Learning from Disaster" ze strony
# https://www.kaggle.com/c/titanic/data. Charakeryzuje się on dużą ilością zniekształceń 
# co utrudnia poszukiwanie wzorców.

skim(titanic)     # podsumowanie - rodzaje danych w kolumnach - lepiej uporządkowane

# Zbiór zawiera zmienne numeryczne oraz nazwy. Zaczynając od analizy nazw można 
# zauważyć, że dla kolumn Age i Cabin występuje zaskakująco dużo pustych wartości, 
# oraz że kolumna ship ma tylko jedną unikalną wartość.
# W celu lepszego wglądu w sytuację warto bardziej przyjrzeć się danym.

glimpse(titanic)  # struktura danych - lepiej uporządkowana (for human readable)




# Na tym etapie można wymienić następujące cechy dancy poszczególnych kolumn: kolumna 
# PassangerId zawira kolejne wartości numeryczne, Ticket - numery biletów, składające 
# się z nazw i numerów utrudniających ich jednolitą kategoryzację, kolumna Cabin to 
# numery kajut przypisane do pasażerów, lecz posiadająca ilość pustych wartości większą 
# niż połowa zbioru, kolumna ship posiada jedną unikatową wartość.
# Cechy wymienione powyżej decydują o bezużyteczności poszczególnych kolumn dla dalszej 
# analizy, w związku z czym muszą zostać usunięte.

#     - tabela - usuwanie kolumn
titanic = subset(titanic, select = -c(PassengerId, Ticket, Cabin, ship) )




skim(titanic)     # podsumowanie - rodzaje danych w kolumnach - lepiej uporządkowane

# Można zauważyć, że pozostało sześć unikalnych wartości określających płeć, a także 
# siedem wartości opisujących port załadunku, chociaż dane na stronie wskazują, że 
# powinny być trzy, a także dwie puste wartości.

unique(titanic[c("Sex")])
unique(titanic[c("Embarked")])

#     - kolumna Sex - naprawa błędnych wartości
titanic$Sex[titanic$Sex=="femmale"]<-"female"
titanic$Sex[titanic$Sex=="fem"]<-"female"
titanic$Sex[titanic$Sex=="mal"]<-"male"
titanic$Sex[titanic$Sex=="malef"]<-"male"

#     - kolumna Embarked - usunięcie wierszy, gdzie brakuje wartości
#     - kolumna Embarked - przycięcie błędnych wartości do jednego znaku
titanic <- titanic[-which(titanic$Embarked == ""), ]
titanic$Embarked <- strtrim(titanic$Embarked, 1)




skim(titanic)     # podsumowanie - rodzaje danych w kolumnach - lepiej uporządkowane

# Jeśli chodzi o kolumnę Age to część wartości jest pusta, część jest poniżej zera 
# lub powyżej sto, a tak, że sama zmienna nie jest wartością numeryczną. Kolumna Fare 
# opisuje cenę, więc warotści powinnybyć dodatnie.
# W kolumnie Fare odcinam wartości po przecinku, a ujemne zastępuje wartością absolutną.

#     - kolumna Age - usunięcie wszystkiego po przecinku
#     - kolumna Age - konwersja typu
titanic$Age <- gsub("(.*),.*", "\\1", titanic$Age)
titanic$Age <- as.numeric(titanic$Age)

titanic$Age <- ifelse(titanic$Age < 0, abs(titanic$Age), titanic$Age )
titanic <- titanic[-which(titanic$Age > 100), ]

#     - kolumna Fare - usunięcie wszystkiego po przecinku
#     - kolumna Fare - konwersja typu
#     - kolumna Fare - zmiana ujemnych wartości
titanic$Fare <- gsub("(.*),.*", "\\1", titanic$Fare)
titanic$Fare <- as.numeric(titanic$Fare)
titanic$Fare <- ifelse(titanic$Fare < 0, abs(titanic$Fare), titanic$Fare )




# Na końcu usuwam duplikaty i kolumnę Name.

#     - tabela - usuwanie powtórzeń w rzędach
titanic <- titanic %>% distinct()
titanic = subset(titanic, select = -c(Name) )




# zapis pliku
write.table(titanic, file = here("data", "TitanicCleaned.tsv"), row.names=FALSE, sep="\t")


















# Analiza eksploracyjna ----



# ladowanie danych
analysis <- read.table(file = here("data", "TitanicCleaned.tsv"), sep = '\t', header = TRUE)

#     - kolumna Survived - opisowe nazwy
analysis$Survived[analysis$Survived==0]<-"Died"
analysis$Survived[analysis$Survived==1]<-"Survived"

#     - kolumna Embarked - opisowe nazwy
analysis$Embarked[analysis$Embarked=="C"]<-"Cherbourg"
analysis$Embarked[analysis$Embarked=="Q"]<-"Queenstown"
analysis$Embarked[analysis$Embarked=="S"]<-"Southampton"

#     - kolumna Family - dodanie kolumny na podstawie kolumn SibSp i Parch
analysis$Family <- analysis$SibSp + analysis$Parch + 1





# Wstęp, sformułowanie celu, analiza


# Celem poniższej analizy jest ocena sytuacji w jakiej znaleźli się pasażerowie 
# statku Titanic na podstawie danych statystycznych dotyczących uczestników 
# feralnego rejsu oraz próba odpowiedzi na pytanie co warunkowało szansę na 
# przetrwanie pasażerów.


# Mamy do dyspozycji zbiór o nazwie "Titanic - Machine Learning from Disaster" ze strony
# https://www.kaggle.com/c/titanic/data. Charakeryzuje się on dużą ilością zniekształceń 
# co utrudnia poszukiwanie wzorców.





# Wyniki analiz, podsumowanie


# W nocy z 14 na 15 kwietnia 1912 roku, podczas dziewiczego rejsu na trasie 
# Southampton – Cherbourg – Queenstown – Nowy Jork, zderzył się z górą lodową i zatonął. 
# Jego katastrofa spowodowała nowelizację zasad bezpieczeństwa morskiego.


# Podczas katastrofy zginęło ponad 60% pasażerów pochodzących z portów Southampton, 
# Cherbourg, Queenstown.


# Ogólnie
# zginęło i przeżyło w liczabach bezwzg.
p1 <- analysis %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) + 
  geom_bar(aes(fill = Survived), position = "dodge")+
  xlab("")+
  theme(legend.position = "none")+
  labs(title  = str_wrap("Survivors",80), subtitle = "")

# zginęło i przeżyło procentowo
p2 <- analysis %>% 
  na.omit() %>%
  ggplot(aes(Survived), show.legend = FALSE) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = Survived)) + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Percent")+xlab("")+
  theme(legend.position = "none")+
  labs(title  = str_wrap("Survivors",80), subtitle = "(percentage)")

p <- plot_grid(p1, p2, ncol = 2, nrow = 1)

title <- ggdraw() + draw_label("Total", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
rm(p,p1,p2)
rm(title)



# Najwięcej osób wsiadło w porcie Southampton i to z tego miasta zginęło najwięcej osób.
# Jedynym portem, z którego uratowało się więcej osób niż zginęło był Cherbourg.

# zginęło i przeżyło w liczbach bezwzg. ze wzg na port załadunku
p1 <- analysis %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) + 
  geom_bar(aes(fill = Embarked), position = "dodge")+
  facet_wrap(~ Embarked)+xlab("")+
  labs(title  = str_wrap("Survivors due to the city",80), subtitle = "in absolute numbers")

# zginęło i przeżyło w liczbach bezwzg. ze wzg na port załadunku
p2 <- analysis %>%
  na.omit() %>%
  ggplot(aes(x = Embarked)) + 
  geom_bar(aes(fill = Survived), position = "dodge")+
  facet_wrap(~ Survived)+xlab("")+
  labs(title  = str_wrap("Survivors due to the city",80), subtitle = "in absolute numbers")

# zginęło i przeżyło procentowo za wzg na port załadunku
p3 <- analysis %>%
  ggplot(aes(x = Survived,  group = Embarked)) + 
  geom_bar(aes(y = ..prop.., fill = Embarked), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~ Embarked) +
  scale_y_continuous(labels = scales::percent)+xlab("")+
  labs(title  = str_wrap("Survivors due to the city",80), subtitle = "percentage")

# grupa, która zginęła/przeżyła składała się z tylu procent za wzg na port załadunku
p4 <- analysis %>%
  ggplot(aes(x = Embarked,  group = Survived["Died"])) + 
  geom_bar(aes(y = ..prop.., fill = Survived), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~Survived) +
  scale_y_continuous(labels = scales::percent)+xlab("")+
  labs(title  = str_wrap("Survivors due to the city",80), subtitle = "percentage")

plot_grid(p1, p2, p3, p4, ncol = 2, nrow = 2)
rm(p1, p2, p3, p4)





# Port Cherbourg na tle innych miast wyróżniał się najlepszą sprzedażą 
# biletów klasy pierwszej na tle inncyh miast.

# naczęściej kupowana klasa biletu wg miasta procentowo
p1 <- analysis %>%
  ggplot(aes(x = Pclass,  group = Embarked)) + 
  geom_bar(aes(y = ..prop.., fill = Embarked), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~ Embarked) +
  scale_y_continuous(labels = scales::percent)+
  labs(title  = str_wrap("The most frequently bought ticket class by city",80), subtitle = "percentage")+ 
  theme(legend.position="bottom")

# naczęściej kupowana klasa biletu wg miasta
p2 <- analysis %>%
  na.omit() %>%
  ggplot(aes(x = Pclass)) + 
  geom_bar(aes(fill = Embarked), position = "dodge") + 
  facet_wrap(~ Embarked)+
  labs(title  = str_wrap("The most frequently bought ticket class by city",80), subtitle = "in absolute numbers")+ 
  theme(legend.position="bottom")

plot_grid(p1, p2, ncol = 2, nrow = 1)
rm(p1, p2)




# Najlepsze miejsca na statku zostały wykupione przez mieszkańców
# Southampton i Cherbourga, odpowiednio w 60 i 40 procentach, najgorsze
# z kolei w ponad 70% przez mieszkańców Southampton, spowodowane to
# było faktem, że większość pasażerów statku stanowili mieszkańcy 
# Southampton, którzy byłi najbardziej zróżnicowaną grupą pod wzgędem 
# zamożności, gdyż klasa biletu była bezpośrednio powiązana z jego ceną.
# Dane wskazują również, że zakup najdrożej klasy biletu był najbardziej 
# opłacalny, gdyż uratowała się największa liczba pasażerów posiadająca
# tą klasę biletu - ponad 60 procent, podczas gdy los pasażerów posiadających
# najtańsze bilety był przesądzony - ich sznasa na przetrwanie wynosiła tylko 
# niecałe 25 procent ponieważ taka ilość posiadaczy tych biletów zdołało się uratować,
# co jeszcze dobitniej obrazuje fakt, że grupa osób, które nie zdołały się
# uratować w aż ponad 67 procentach składa się z pasażerów posiadających 
# najtańszą klasę biletu.

# sprzedaż klas z uwzglednieniem miast
p11 <- analysis %>%
  ggplot(aes(x = Embarked,  group = Pclass)) + 
  geom_bar(aes(y = ..prop.., fill = Pclass), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~ Pclass) +
  scale_y_continuous(labels = scales::percent)+xlab("")+
  labs(title  = str_wrap("Sale of ticket classes including cities",80), subtitle = "percentage")

# pasażerowie w liczbach bezwzg. ze wzg na port załadunku
p12 <- analysis %>%
  na.omit() %>%
  ggplot(aes(x = Embarked)) + 
  geom_bar(aes(fill = Embarked), position = "dodge")+xlab("")+
  labs(title  = str_wrap("Number of tickets sold",80), subtitle = "in absolute numbers")

# klasa a cena biletu
p13 <- ggplot(analysis, aes(x=Survived, y=Fare)) + 
  geom_boxplot() +
  facet_grid(~Pclass)+xlab("")+
  labs(title  = str_wrap("Tickets price",80), subtitle = "in absolute numbers")

# zginęło i przeżyło procentowo pasażerów wg klas 
# szansa na przetrwanie
p41 <- analysis %>%
  ggplot(aes(x = Survived,  group = Pclass)) + 
  geom_bar(aes(y = ..prop.., fill = Pclass), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~ Pclass) +
  scale_y_continuous(labels = scales::percent)+xlab("")+
  theme(legend.position = "none")+
  labs(title  = str_wrap("Chance of survival due to the ticket class",80), subtitle = "percentage")

# grupa, która zginęła/przeżyła składała się z tylu procent pasażerów klas
p42 <- analysis %>%
  ggplot(aes(x = Pclass,  group = Survived)) + 
  geom_bar(aes(y = ..prop.., fill = Survived), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~Survived) +
  scale_y_continuous(labels = scales::percent)+
  theme(legend.position = "none")+
  labs(title  = str_wrap("Chance of survival due to the ticket class",80), subtitle = "percentage")

p1 <- plot_grid(p11,p12,p13, ncol = 3, nrow = 1)
p4 <- plot_grid(p41,p42, ncol = 2, nrow = 1)

plot_grid(p1, p4,  ncol = 1, nrow = 2)
rm(p1, p11, p12, p13, p4, p41,p42)







# Dane wskazują na to, że większość uratowanych stanowiły kobiety, odwrotna sytuacja miała miejsce w 
# przypadku mężczyzn. Kobiety również zachowały większą szansę na przetrwanie ponieważ około 75 procent 
# kobiet zostało uratowanych, podczas gdy dla mężczyzn ten wskaźnik wyniósł niecałe 20 procent.


# grupa, która zginęła/przeżyła składała się z tylu kobiet/mezczyzn - procentowo
p11 <- analysis %>%
  ggplot(aes(x = Sex,  group = Survived)) + 
  geom_bar(aes(y = ..prop.., fill = Survived), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  # facet_grid(~ Sex) +
  facet_wrap(~ Survived)+
  scale_y_continuous(labels = scales::percent)+xlab("")+ 
  theme(legend.position="bottom")+
  labs(title  = str_wrap("Survivors",80), subtitle = "percentage")

# zginęło i przeżyło w ze wzg na plec - procentowo 
p12 <- analysis %>%
  ggplot(aes(x = Survived,  group = Sex)) + 
  geom_bar(aes(y = ..prop.., fill = Sex), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~Sex) +
  scale_y_continuous(labels = scales::percent)+xlab("")+ 
  theme(legend.position="bottom")+
  labs(title  = str_wrap("The sex of the surviovors",80), subtitle = "percentage")

# zginęło i przeżyło w ze wzg na plec - histogram 
p2 <- ggplot(analysis, aes(x = Age, fill = Survived)) +
  theme_bw() +
  geom_histogram(binwidth = 5, position = "dodge") +
  facet_wrap(~ Sex)+
  labs(y = "Passenger Count")+
  labs(title  = str_wrap("Surviovors",80), subtitle = "in absolute numbers")

p1 <- plot_grid(p11, p12, ncol = 2, nrow = 1)
plot_grid(p1, p2, ncol = , nrow = 2)
rm(p11, p12, p1, p2)





# Biorąc pod uwagę różnicę w przeżywalności ze względu na płeć, to można powiedzieć, że pasażerowie 
# podróżujący z rodzinami nie zwiększali swojej szansy na przeżycie z tego powodu.

# zginęło i przeżyło w liczbach bezwzg. ze wzg na ilosc osób w rodzinie
analysis %>%
  na.omit() %>%
  ggplot(aes(x = Family)) + 
  geom_bar(aes(fill = Survived), position = "dodge")+
  facet_wrap(~ Survived)+
  labs(title  = str_wrap("The size of the survivor's family",80), subtitle = "in absolute numbers")






# Wnioski

# Największą szansę na przetrwanie miały osoby pochodzące z Cherbourg, które kupiły bilety
# pierwszej klasy i będące kobietami niezależnie od tego czy podróżowały z rodziną czy bez
# ich szansa na przetwanie wynosiła 97 procent.


# Ogólnie
# zginęło i przeżyło w liczabach bezwzg.
p11 <- analysis %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) + 
  geom_bar(aes(fill = Survived), position = "dodge")+
  xlab("")+
  theme(legend.position = "none")+
  labs(title  = str_wrap("Survivors",80), subtitle = "")

# zginęło i przeżyło procentowo
p12 <- analysis %>% 
  na.omit() %>%
  ggplot(aes(Survived), show.legend = FALSE) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = Survived)) + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Percent")+xlab("")+
  theme(legend.position = "none")+
  labs(title  = str_wrap("Survivors",80), subtitle = "(percentage)")

# z filtrami
# zginęło i przeżyło w liczabach bezwzg.
p21 <- analysis %>%
  na.omit() %>%
  filter(Embarked == "Cherbourg") %>%
  filter(Pclass == 1) %>%
  filter(Sex == "female") %>%
  ggplot(aes(x = Survived)) + 
  geom_bar(aes(fill = Survived), position = "dodge")+
  xlab("")+
  theme(legend.position = "none")+
  labs(title  = str_wrap("Survivors",80), subtitle = "Female embarked in Cherbourg with ticket class 1")

# zginęło i przeżyło procentowo
p22 <- analysis %>% 
  na.omit() %>%
  filter(Embarked == "Cherbourg") %>%
  filter(Pclass == 1) %>%
  filter(Sex == "female") %>%
  ggplot(aes(Survived), show.legend = FALSE) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = Survived)) + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Percent")+xlab("")+
  theme(legend.position = "none")+
  labs(title  = str_wrap("Survivors",80), subtitle = "Female embarked in Cherbourg with ticket class 1 (percentage)")


plot_grid(p11, p12, p21, p22, ncol = 2, nrow = 2)
rm(p11, p12, p21, p22)


























####################
# niewykorzystane
####################

# wykorzystane %
# zginęło i przeżyło w liczbach bezwzg. ze wzg na klase
analysis %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) + 
  geom_bar(aes(fill = Pclass), position = "dodge") + 
  facet_wrap(~ Pclass)+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by Pclass")

# wykorzystane %
# zginęło i przeżyło w liczbach bezwzg. ze wzg na klase
analysis %>%
  na.omit() %>%
  ggplot(aes(x = Pclass)) + 
  geom_bar(aes(fill = Survived), position = "dodge") + 
  facet_wrap(~ Survived)+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by Pclass")

# wykorzystany inny typ grafu
# które klasy kupowane w których portach
analysis %>%
  ggplot(aes(x = Pclass, y=Age, group = Pclass)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(y = "Percent") +
  facet_grid(~ Embarked)

# wykorzystane
# zginęło i przeżyło w liczbach bezwzg. ze wzg na oplate
analysis %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) + 
  geom_bar(aes(fill = Fare), position = "dodge")+
  facet_wrap(~ Embarked)+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by Fare")

# wykorzystane %
# zginęło i przeżyło w ze wzg na plec - liczby bezwzg.
analysis %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) +
  geom_bar(aes(fill = Sex), position = "dodge") +
  facet_wrap(~ Sex)+
  labs(title  = str_wrap("Survived",80),
       subtitle = "by Sex")

# wykorzystany inny typ grafu
ggplot(analysis, aes(x = Age, fill = Sex)) +
  theme_bw() +
  geom_histogram(binwidth = 5, position = "dodge") +
  facet_wrap(~ Survived)+
  labs(y = "Passenger Count",
       x = "Age with binwidth 5 years",
       title = "Titanic passengers Sex distribution")

# wykorzystany prostszy typ wykresu
analysis %>%
  na.omit() %>%
  filter(Sex == "male") %>%
  filter(Survived == "Died") %>%
  ggplot(aes(x = Age, group = Age)) + 
  geom_bar(aes(fill = Age), position = "dodge")+
  facet_wrap(~ Survived)+
  labs(title  = str_wrap("Survived male",80), 
       subtitle = "with family")

# wykorzystany prostszy typ wykresu
analysis %>%
  na.omit() %>%
  ggplot(aes(x = Sex, group = Age)) + 
  geom_bar(aes(fill = Age), position = "dodge")+
  facet_wrap(~ Survived)+
  labs(title  = str_wrap("Survived male",80), 
       subtitle = "with family")

# wykorzystany prostszy typ wykresu
analysis %>%
  na.omit() %>%
  # filter(Age > 18) %>%
  filter(Sex == "male") %>%
  filter(Survived == "Died") %>%
  ggplot(aes(x = Sex, group = Age)) + 
  geom_bar(aes(fill = Age), position = "dodge")+
  facet_wrap(~ Survived)+
  labs(title  = str_wrap("Survived male",80), 
       subtitle = "with family")


# wykorzystany prostszy typ wykresu
# zginęło i przeżyło w liczbach bezwzg. ze wzg na wiek
analysis %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) + 
  geom_bar(aes(fill = Age), position = "dodge")+
  facet_wrap(~ Age)+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by Age")

# wykorzystany prostszy typ wykresu
# klasa ze wzg na wiek
analysis %>%
  na.omit() %>%
  ggplot(aes(x = Pclass)) + 
  geom_bar(aes(fill = Age), position = "dodge")+
  facet_wrap(~ Age)+
  labs(title  = str_wrap("Age",80), 
       subtitle = "by Age")

# wykorzystany prostszy typ wykresu
# które klasy przezyły
analysis %>%
  ggplot(aes(x = Pclass, y=Age, group = Pclass)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(y = "Percent") +
  facet_grid(~ Survived)


# nic nie da się odczytać
# ile osób zginęło wraz z rodzinami
analysis %>%
  na.omit() %>%
  ggplot(aes(x = Survived, y = Family)) + 
  geom_jitter()


# wykorzystany prostszy typ wykresu
# w jakim wieku byli ludzie w poszczególnych portach
analysis %>%
  na.omit() %>%
  ggplot(aes(x = Embarked, y = Age)) + 
  geom_violin()+
  labs(title  = str_wrap("People",80), 
       subtitle = "by city")

# wykorzystany prostszy typ wykresu
# w jakim wieku byli ludzie, którzy zginęli dla poszczególnych portów
analysis %>%
  na.omit() %>%
  filter(Survived == "Died") %>%
  ggplot(aes(x = Embarked, y = Age)) +
  geom_violin()+
  labs(title  = str_wrap("Died",80), 
       subtitle = "by city")

# wykorzystany prostszy typ wykresu
# w jakim wieku byli ludzie, którzy przeżyli dla poszczególnych portów
analysis %>%
  na.omit() %>%
  filter(Survived == "Survived") %>%
  ggplot(aes(x = Embarked, y = Age)) + 
  geom_violin()+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by city")


# wykorzystany prostszy typ wykresu
# survived/died female with family
analysis %>%
  na.omit() %>%
  filter(Sex == "female") %>%
  ggplot(aes(x = Family)) + 
  geom_bar(aes(fill = Survived), position = "dodge")+
  facet_wrap(~ Survived)+
  labs(title  = str_wrap("Survived female",80), 
       subtitle = "with family")
# survived/died male with family
analysis %>%
  na.omit() %>%
  filter(Sex == "male") %>%
  ggplot(aes(x = Family)) + 
  geom_bar(aes(fill = Survived), position = "dodge")+
  facet_wrap(~ Survived)+
  labs(title  = str_wrap("Survived male",80), 
       subtitle = "with family")
# survived/died female with family in %
analysis %>%
  na.omit() %>%
  filter(Sex == "female") %>%
  ggplot(aes(x = Family,  group = Sex)) + 
  geom_bar(aes(y = ..prop.., fill = Survived), stat="count", position = "dodge") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~Survived) +
  scale_y_continuous(labels = scales::percent)+
  labs(title  = str_wrap("Survived female",80), 
       subtitle = "with family")
# survived/died male with family in %
analysis %>%
  na.omit() %>%
  filter(Sex == "male") %>%
  ggplot(aes(x = Family,  group = Sex)) + 
  geom_bar(aes(y = ..prop.., fill = Survived), stat="count", position = "dodge") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~Survived) +
  scale_y_continuous(labels = scales::percent)+
  labs(title  = str_wrap("Survived male",80), 
       subtitle = "with family")


# wykorzystany inny typ grafu
# sprzedaż klas w poszczególnych miastach
analysis %>%
  na.omit() %>%
  ggplot(aes(x = Embarked)) + 
  geom_bar(aes(fill = Embarked), position = "dodge") + 
  facet_wrap(~ Pclass)+xlab("")


# nic nie da się odczytać
# ile osób w rodzinie dla poszczególnych portów
analysis %>%
  na.omit() %>%
  ggplot(aes(x = Embarked, y = Family)) + 
  geom_violin()+
  labs(title  = str_wrap("People",80), 
       subtitle = "by city")
# ile osób zgineło w rodzinie dla poszczególnych portów
analysis %>%
  na.omit() %>%
  filter(Survived == "Died") %>%
  ggplot(aes(x = Embarked, y = Family)) + 
  geom_violin()+
  labs(title  = str_wrap("Died",80), 
       subtitle = "by city")
# ile osób przezyło w rodzinie dla poszczególnych portów
analysis %>%
  na.omit() %>%
  filter(Survived == "Survived") %>%
  ggplot(aes(x = Embarked, y = Family)) + 
  geom_violin()+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by city")


# wykorzystany inny typ grafu
# histogramy
ggplot(analysis, aes(x = Age, fill = Survived)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x = "Age with binwidth 5 years",
       title = "Titanic passengers Survived distribution")

ggplot(analysis, aes(x = Age, fill = Embarked)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x = "Age with binwidth 5 years",
       title = "Titanic passengers Embarked distribution")


# wykorzystany prostszy typ wykresu
# zginęło i przeżyło w liczbach bezwzg. ze wzg na ilosc osób w rodzinie
analysis %>%
  na.omit() %>%
  ggplot(aes(x = Survived)) + 
  geom_bar(aes(fill = Family), position = "dodge")+
  facet_wrap(~ Family)+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by Family")

# wykorzystany prostszy typ wykresu
# zginęło i przeżyło procentowo rodzin będacych na pokładzie
analysis %>%
  ggplot(aes(x = Survived,  group = Family)) + 
  geom_bar(aes(y = ..prop.., fill = Family), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(~ Family) +
  scale_y_continuous(labels = scales::percent)


# wykorzystany prostszy typ wykresu
# zginęło i przeżyło w liczbach bezwzg. ze wzg na ilosc osób w rodzinie
analysis %>%
  na.omit() %>%
  ggplot(aes(x = Family)) + 
  geom_bar(aes(fill = Survived), position = "dodge")+
  facet_wrap(~ Sex)+
  labs(title  = str_wrap("Survived",80), 
       subtitle = "by Family")


# na tym typie wykresu trudniej coś zauważyć - wykorzystane inne wykresy
## violin
analysis %>%
  na.omit() %>%
  ggplot(aes(x = Pclass, y = Age)) + 
  geom_violin()+
  labs(title  = str_wrap("Pclass",80), 
       subtitle = "by Age")

analysis %>%
  na.omit() %>%
  ggplot(aes(x = Pclass, y = Age)) + 
  geom_violin()+
  facet_grid(~Family) +
  labs(title  = str_wrap("Pclass",80), 
       subtitle = "by Age")

analysis %>%
  na.omit() %>%
  ggplot(aes(x = Family, y = Age)) + 
  geom_violin()+
  facet_grid(~ Family) +
  labs(title  = str_wrap("Family",80), 
       subtitle = "by Age")

analysis %>%
  na.omit() %>%
  ggplot(aes(x = Family, y = Age)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  facet_grid(~ Embarked) +
  labs(title  = str_wrap("Family",80), 
       subtitle = "by Age")

analysis %>%
  na.omit() %>%
  ggplot(aes(x = Embarked, y = Age)) + 
  geom_violin()+
  facet_grid(~ Family) +
  labs(title  = str_wrap("Family",80), 
       subtitle = "by city")








