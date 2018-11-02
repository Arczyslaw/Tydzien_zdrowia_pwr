#Autor: Arkadiusz Poziemski
#Do użytku na potrzeby projektu 'Tydzień zdrowia'
library(dplyr)
library(ggplot2)

Tydzien_zdrowia <- Tydzien_Zdrowia_Odpowiedzi_
index <- c(1,2,4, 7:13,15,16, 18:24, 26:27, 31:33) #te zmienne chcemy mieć jako faktory
Tydzien_zdrowia <- Tydzien_zdrowia[ ,-1]
for (k in index){
  Tydzien_zdrowia[ ,k] <- as.factor(unlist(Tydzien_zdrowia[ ,k]))
}

#Zmieniam nazwy dla wygody
names(Tydzien_zdrowia) <- c('Plec', 'Wydzial', 'Rok', 'Zaangazowanie', 'Wzrost','Wiek', 'Stres',
                           'Praca','Na_uczelni', 'Mieszkanie', 'Dojazd', 'Nauka', 'Nauka_godz', 'Srednia', 
                           'Utrzymanie', 'Sposob_bycia', 'Woda', 'Kawa', 'Energetyki','Palenie', 
                           'Uzaleznienia','Alkohol', 'Miejsce_jedzenia','Jedzenie', 'Aktywnosc', 
                           'Regularnosc_aktywnosci', 'Okulary','Spanie', 'Zadowolenie', 'Zdrowie', 
                           'Suplementy', 'Alergia', 'Czas_Kontuzji')
summary(Tydzien_zdrowia)
Tydzien_zdrowia$Srednia[Tydzien_zdrowia$Srednia==15]<- NA

#Porządkujemy faktory, dal większej przejrzystości wykresów
Tydzien_zdrowia$Zaangazowanie <- ordered(Tydzien_zdrowia$Zaangazowanie, levels=c('Nie', 'Sporadycznie', 'Tak'))
Tydzien_zdrowia$Stres<-ordered(Tydzien_zdrowia$Stres, levels=c('Zdecydowanie nie', 'Raczej nie', 'Raczej tak', 'Zdecydowanie tak'))
Tydzien_zdrowia$Na_uczelni <- ordered(Tydzien_zdrowia$Na_uczelni, levels=c('0-10', '10-20', '20-30', '30-40', '40+'))
Tydzien_zdrowia$Nauka <- ordered(Tydzien_zdrowia$Nauka, c('Nie', 'Tak'))
Tydzien_zdrowia$Nauka_godz <- ordered(Tydzien_zdrowia$Nauka_godz, c('0-5', '5-10', '10-15','15-20', '20+'))
Tydzien_zdrowia$Kawa <- ordered(Tydzien_zdrowia$Kawa, c('0-1', '2-3', '4-5'))
Tydzien_zdrowia$Energetyki <- ordered(Tydzien_zdrowia$Energetyki, c('0-1', '2-3', '4-5'))
Tydzien_zdrowia$Palenie <- ordered(Tydzien_zdrowia$Palenie, c('Nie palę', 'Okazyjnie', 'Kilka razy w tygodniu','Kilka razy dziennie'))
Tydzien_zdrowia$Alkohol <- ordered(Tydzien_zdrowia$Alkohol, c('Nie piję alkoholu', 'Kilka razy w roku', 'Kilka razy w miesiącu', 'Kilka razy w tygodniu', 'Codziennie')) 
Tydzien_zdrowia$Regularnosc_aktywnosci <- ordered(Tydzien_zdrowia$Regularnosc_aktywnosci, c('Nie', 'Tak'))
Tydzien_zdrowia$Okulary <- ordered(Tydzien_zdrowia$Okulary, c('Nie', 'Tak, przy określonych czynnościach (np. do czytania)', 'Tak, przy każdej czynności'))
Tydzien_zdrowia$Suplementy <- ordered(Tydzien_zdrowia$Suplementy, c('Nie', 'Tak'))
Tydzien_zdrowia$Alergia <- ordered(Tydzien_zdrowia$Alergia, c('Nie', 'Tak'))
Tydzien_zdrowia$Czas_Kontuzji <- ordered(Tydzien_zdrowia$Czas_Kontuzji, c('Przed 10. rokiem życia', '10-15 lat', '16-19 lat','20-25 lat', 'Po 25. roku życia'))

#################Wykresy kołowe dla jednej zmiennej###############
#Funkcja generująca wykres
wykres_kolowy <- function(cecha){
  pie <- ggplot(Tydzien_zdrowia, aes(x=factor(1), fill=cecha))+
    geom_bar(width = 1, color='black')+
    coord_polar(theta = "y")
  pie
}
ggplot(Tydzien_zdrowia, aes(x= factor(1), fill=as.factor(Zdrowie)))+
  geom_bar(width = 1, color='black')+
  coord_polar(theta = "y")
wykres_kolowy(factor(Tydzien_zdrowia$Rok))

factory <- Tydzien_zdrowia[sapply(Tydzien_zdrowia, is.factor)]
wykresiki <- lapply(factory, wykres_kolowy)
# wykresiki zawierają listę wykresów
wykresiki[24]
#Interesujące według mnie
interesujące1 <- c(3,4,8, 10, 11, 15, 16, 17, 19, 20, 21)

################### Wykresy zaleznosci od zdrowia ##############
Tydzien_zdrowia_stres <- Tydzien_zdrowia_zdrowie %>%
  mutate(Stres_ogólny=ifelse(Stres %in% c('Zdecydowanie nie', 'Raczej nie'), 'Niezestresowany', 'Zestresowany'))
#Wywalam zdrowie 1 bo ma za mało odpowiedzi
Tydzien_zdrowia_zdrowie <- Tydzien_zdrowia %>%
  filter(Zdrowie!=1)
#Wykresy zdrowie a stres
ggplot(Tydzien_zdrowia_zdrowie,aes(x=Zdrowie, fill=Stres))+
  geom_bar(position='fill')

ggplot(Tydzien_zdrowia_zdrowie,aes(x=Zdrowie, fill=Stres))+
  geom_bar(position='fill')+
  facet_wrap(~Plec)
ggplot(Tydzien_zdrowia_zdrowie,aes(x=Zdrowie, fill=Stres))+
  geom_bar(position='fill')+
  facet_wrap(~Plec)

ggplot(Tydzien_zdrowia_stres, aes(x=Zdrowie, fill=Stres_ogólny))+
  geom_bar(position='dodge')
ggplot(Tydzien_zdrowia_stres, aes(x=Zdrowie, fill=Stres_ogólny))+
  geom_bar(position='fill')

#Zdrowie a alkohol
ggplot(Tydzien_zdrowia_zdrowie, aes(x=Zdrowie, fill=Alkohol))+
  geom_bar(position='fill')

#Zdrowie a aktywność
ggplot(Tydzien_zdrowia_zdrowie, aes(x=Zdrowie, fill=Regularnosc_aktywnosci))+
  geom_bar(position='fill')

ggplot(Tydzien_zdrowia_zdrowie, aes(x=Zdrowie, fill=as.factor(Zadowolenie)))+
  geom_bar(position = 'fill')

Srednia_sen <- Tydzien_zdrowia_zdrowie %>%
  group_by(Zdrowie) %>%
  summarise(srednia_sen=mean(Spanie))
ggplot()+
  geom_boxplot(data=Tydzien_zdrowia_zdrowie, aes(x=as.factor(Zdrowie), y=Spanie))+
  geom_point(data=Srednia_sen, aes(x=as.factor(Zdrowie), y=srednia_sen), color='red')

Srednia_aktywnosc <- Tydzien_zdrowia_zdrowie %>%
  group_by(Zdrowie) %>%
  summarise(srednia_aktywnosc=mean(Aktywnosc))
#Aktywnosc w zaleznosci od zdrowia; boxplot + srednia
ggplot()+
  geom_boxplot(data=Tydzien_zdrowia_zdrowie, aes(x=as.factor(Zdrowie), y=Aktywnosc))+
  geom_point(data=Srednia_aktywnosc, aes(x=as.factor(Zdrowie), y=srednia_aktywnosc), color='red')

######################Inne wykresy#############


#Filtruje wydzialy bo w większosci jest bardzo mało ocen
wydzialy_aktywne <- Tydzien_zdrowia %>%
  group_by(Wydzial) %>%
  summarise(liczba=n())%>%
  filter(liczba>7) %>%
  select(Wydzial)
wydzialy_aktywne <- as.numeric(unlist(wydzialy_aktywne))
dane_wydzialy_aktywne <- Tydzien_zdrowia %>%
  filter(as.numeric(Wydzial) %in% wydzialy_aktywne)

ggplot(data = dane_wydzialy_aktywne, aes(x=Wydzial, fill=Stres))+
  geom_bar(position = 'fill')
ggplot(data = dane_wydzialy_aktywne, aes(x=Wydzial, fill=Zaangazowanie))+
  geom_bar(position = 'fill')

ggplot(Tydzien_zdrowia, aes(x=Nauka_godz, y=Srednia, color=Zaangazowanie))+
  geom_point()

ggplot(Tydzien_zdrowia, aes(x=Rok, fill=Stres))+
  geom_bar(position = 'fill')

Zadowoleni <- Tydzien_zdrowia %>%
  group_by(Zadowolenie) %>%
  summarise(liczba=n()) %>%
  filter(liczba>10) %>%
  select(Zadowolenie)
  
Zadowoleni_tabela <- Tydzien_zdrowia %>%
  filter(Zadowolenie %in% unlist(Zadowoleni))

ggplot(Zadowoleni_tabela, aes(x=Zadowolenie, fill=Zaangazowanie))+
  geom_bar(position = 'fill')

#Rok a alkohol
Tydzien_zdrowia_zdrowie %>%
  group_by(Plec, Rok)%>%
  summarise(liczba=n())
#Bardzo mało ankiet dla męzczyzn z 3. i 5. roku
ggplot(Tydzien_zdrowia, aes(x=Rok, fill=Alkohol))+
  geom_bar(position='dodge')+
  facet_wrap(~Plec)

ggplot(Tydzien_zdrowia, aes(x=Rok, fill=Alkohol))+
  geom_bar(position='fill')+
  facet_wrap(~Plec)







