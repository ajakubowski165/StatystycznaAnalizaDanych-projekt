#___________________ BIBLIOTEKI
library(readxl)
library(lubridate)
library(plotly)
library(graphics)

#___________________ USTAWIENIE WORKING DIRECTORY
setwd('D:/nauka/SEMESTR 4/Statystyczna analiza danych/Projekt')


#___________________ IMPORT ORAZ PRZYGOTOWANIE DANYCH 

#Wczytanie danych do zmiennej dane_start
dane_start <- (read_excel("tabl12_bezrobotni_zarejestrowani_oferty_pracy.xlsx"))

#Zmiana wczytanych danych na ramkê danych
dane_start <- as.data.frame(dane_start)

#Przyciêcie ramki danych do ³atwiejszej analizy
dane <- dane_start[-c(1:5),c(1,2,4,20,22,26,30)]

#Zmiana nazw kolumn
colnames(dane) <- c("Rok/Miesiac",
                    "Liczba_bezrobotnych_ogolem",
                    "Liczba_bezrobotnych_kobiet",
                    "Stopa_bezrobocia",
                    "Bezrobotni_nowo_zarejestrowani",
                    "Bezrobotni_wyrejestrowani",
                    "Liczba_zgloszonych_ofert_pracy")

#zmiana typu danych w kolumnach
dane$Liczba_bezrobotnych_ogolem <- as.numeric(dane$Liczba_bezrobotnych_ogolem)
dane$Liczba_bezrobotnych_kobiet <- as.numeric(dane$Liczba_bezrobotnych_kobiet)
dane$Stopa_bezrobocia <- as.numeric(dane$Stopa_bezrobocia)
dane$Bezrobotni_nowo_zarejestrowani <- as.numeric(dane$Bezrobotni_nowo_zarejestrowani)
dane$Bezrobotni_wyrejestrowani <- as.numeric(dane$Bezrobotni_wyrejestrowani)
dane$Liczba_zgloszonych_ofert_pracy <- as.numeric(dane$Liczba_zgloszonych_ofert_pracy)

#przemno¿enie razy 1000
dane$Liczba_bezrobotnych_ogolem <- dane$Liczba_bezrobotnych_ogolem * 1000
dane$Liczba_bezrobotnych_kobiet <- dane$Liczba_bezrobotnych_kobiet * 1000
dane$Bezrobotni_nowo_zarejestrowani <- dane$Bezrobotni_nowo_zarejestrowani * 1000
dane$Bezrobotni_wyrejestrowani <- dane$Bezrobotni_wyrejestrowani * 1000
dane$Liczba_zgloszonych_ofert_pracy <- dane$Liczba_zgloszonych_ofert_pracy * 1000



#__________________ OBLICZANIE PARAMETRÓW

#1 - œrednia
(srBO <- mean(dane$Liczba_bezrobotnych_ogolem))
(srBK <- mean(dane$Liczba_bezrobotnych_kobiet))
(srSB <- mean(dane$Stopa_bezrobocia))
(srBNZ <- mean(dane$Bezrobotni_nowo_zarejestrowani))
(srBW <- mean(dane$Bezrobotni_wyrejestrowani))
(srOP <- mean(dane$Liczba_zgloszonych_ofert_pracy))

#2 - maksymalne wartoœci
(maxBO <- max(dane$Liczba_bezrobotnych_ogolem))
(maxBK <- max(dane$Liczba_bezrobotnych_kobiet))
(maxSB <- max(dane$Stopa_bezrobocia))
(maxBNZ <- max(dane$Bezrobotni_nowo_zarejestrowani))
(maxBW <- max(dane$Bezrobotni_wyrejestrowani))
(maxOP <- max(dane$Liczba_zgloszonych_ofert_pracy))

#3 - minimalne wartoœci
(minBO <- min(dane$Liczba_bezrobotnych_ogolem))
(minBK <- min(dane$Liczba_bezrobotnych_kobiet))
(minSB <- min(dane$Stopa_bezrobocia))
(minBNZ <- min(dane$Bezrobotni_nowo_zarejestrowani))
(minBW <- min(dane$Bezrobotni_wyrejestrowani))
(minOP <- min(dane$Liczba_zgloszonych_ofert_pracy))

#4 - rozstêp
(rozBO <- maxBO - minBO)
(rozBK <- maxBK - minBK)
(rozSB <- maxSB - minSB)
(rozBNZ <- maxBNZ - minBNZ)
(rozBW <- maxBW - minBW)
(rozOP <- maxOP - minOP)

#5 - odchylenie standardowe
(sdBO <- sd(dane$Liczba_bezrobotnych_ogolem))
(sdBK <- sd(dane$Liczba_bezrobotnych_kobiet))
(sdSB <- sd(dane$Stopa_bezrobocia))
(sdBNZ <- sd(dane$Bezrobotni_nowo_zarejestrowani))
(sdBW <- sd(dane$Bezrobotni_wyrejestrowani))
(sdOP <- sd(dane$Liczba_zgloszonych_ofert_pracy))

#6 - wariancja
(warBO <- var(dane$Liczba_bezrobotnych_ogolem))
(warBK <- var(dane$Liczba_bezrobotnych_kobiet))
(warSB <- var(dane$Stopa_bezrobocia))
(warBNZ <- var(dane$Bezrobotni_nowo_zarejestrowani))
(warBW <- var(dane$Bezrobotni_wyrejestrowani))
(warOP <- var(dane$Liczba_zgloszonych_ofert_pracy))

#7 - mediana
(medianBO <- median(dane$Liczba_bezrobotnych_ogolem))
(medianBK <- median(dane$Liczba_bezrobotnych_kobiet))
(medianSB <- median(dane$Stopa_bezrobocia))
(medianBNZ <- median(dane$Bezrobotni_nowo_zarejestrowani))
(medianBW <- median(dane$Bezrobotni_wyrejestrowani))
(medianOP <- median(dane$Liczba_zgloszonych_ofert_pracy))

#8 - kwantyle
(quantileBO <- quantile(dane$Liczba_bezrobotnych_ogolem))
(quantileBK <- quantile(dane$Liczba_bezrobotnych_kobiet))
(quantileSB <- quantile(dane$Stopa_bezrobocia))
(quantileBNZ <- quantile(dane$Bezrobotni_nowo_zarejestrowani))
(quantileBW <- quantile(dane$Bezrobotni_wyrejestrowani))
(quantileOP <- quantile(dane$Liczba_zgloszonych_ofert_pracy))

#9 - rozstêp miêdzykwartylowy
(iqrBO <- IQR(dane$Liczba_bezrobotnych_ogolem))
(iqrBK <- IQR(dane$Liczba_bezrobotnych_kobiet))
(iqrSB <- IQR(dane$Stopa_bezrobocia))
(iqrBNZ <- IQR(dane$Bezrobotni_nowo_zarejestrowani))
(iqrBW <- IQR(dane$Bezrobotni_wyrejestrowani))
(iqrOP <- IQR(dane$Liczba_zgloszonych_ofert_pracy))

#10 - wspó³czynnik zmiennoœci
(VBO <- sdBO/srBO)
(VBK <- sdBK/srBK)
(VSB <- sdSB/srSB)
(VBNZ <- sdBNZ/srBNZ)
(VBW <- sdBW/srBW)
(VOP <- sdOP/srOP)


#__________________WIZUALIZACJA


### HISTOGRAMY ###

#Histogram ogólnej liczby bezrobotnych
hist(as.ts(dane$Liczba_bezrobotnych_ogolem), 
     main = "Bezrobotni ogó³em", 
     xlab = "Iloœæ osób bezrobotnych", 
     ylab = "Liczebnoœæ", 
     ylim = c(1,50), 
     xlim = c(minBO,maxBO),
     col = "yellow",
     las = 2,
     cex.axis = 0.6,
     xaxt = "n")
axis(side = 1, 
     at = c(800000, 1000000, 1200000, 1400000, 1600000, 1800000, 2000000, 2200000), 
     labels = c("800000", "1000000", "1200000", "1400000", "1600000", "1800000", "2000000", "2200000"), 
     las = 2,
     cex.axis = 0.6)


#Histogram liczby bezrobotnych nowo zarejestrowanych
hist(as.ts(dane$Bezrobotni_nowo_zarejestrowani), 
     main = "Nowo zarejestrowani bezrobotni", 
     xlab = "Iloœæ nowo zarejestrowanych bezrobotnych", 
     ylab = "Liczebnoœæ", 
     ylim = c(1,30), 
     xlim = c(minBNZ,maxBNZ),
     col = "pink",
     cex.axis = 0.6,
     las = 2,
     xaxt = "n")
axis(side = 1, 
     at = c(100000, 120000, 140000, 160000, 180000, 200000, 220000, 240000, 260000, 280000, 300000), 
     labels = c("100000","120000","140000","160000","180000","200000","220000","240000","260000","280000","300000"), 
     las = 2,
     cex.axis = 0.6)

#Histogram liczby bezrobotnych wyrejestrowanych
hist(as.ts(dane$Bezrobotni_wyrejestrowani), 
     main = "Bezrobotni wyrejestrowani", 
     xlab = "Iloœæ bezrobotnych wyrejestrowanych", 
     ylab = "Liczebnoœæ", 
     ylim = c(1,50), 
     xlim = c(minBW,maxBW),
     col = "lightgreen")

#Histogram ofert pracy
hist(as.ts(dane$Liczba_zgloszonych_ofert_pracy), 
     main = "Zg³oszone oferty pracy", 
     xlab = "Iloœæ zg³oszonych ofert pracy w ci¹gu miesi¹ca", 
     ylab = "Liczebnoœæ", 
     ylim = c(1,50), 
     xlim = c(minOP,maxOP),
     col = "lightblue")



### WYKRES PUDE£KOWY ###

tsBNZ <- ts(data = dane$Bezrobotni_nowo_zarejestrowani, 
            start = decimal_date(ymd("2010-01-01")), 
            frequency = 12)
boxplot(tsBNZ~ cycle(tsBNZ),
        col = "yellowgreen",          
        main = "Wykres pude³kowy dla nowo zarejestrowanych bezrobotnych",
        xlab = "Miesiace",
        ylab = "Ilosc osob")


tsBW <- ts(data = dane$Bezrobotni_wyrejestrowani, 
           start = decimal_date(ymd("2010-01-01")), 
           frequency = 12)
boxplot(tsBW~ cycle(tsBW),
        col = "purple",          
        main = "Wykres pude³kowy dla wyrejestrowanych bezrobotnych",
        xlab = "Miesiace",
        ylab = "Ilosc osob")



### WYKRES LINIOWY ###

# Tworzenie szeregów czasowych
tsBO <- ts(data = dane$Liczba_bezrobotnych_ogolem, start = decimal_date(ymd("2010-01-01")), frequency = 12)
tsOP <- ts(data = dane$Liczba_zgloszonych_ofert_pracy, start = decimal_date(ymd("2010-01-01")), frequency = 12)

# Obliczanie œrednich wartoœci dla ka¿dego miesi¹ca
sr_miesieczna_BO <- tapply(tsBO, cycle(tsBO), mean)
sr_miesieczna_OP <- tapply(tsOP, cycle(tsOP), mean)

plot(sr_miesieczna_BO, type="l", col="darkgreen",
     main="Œrednia iloœæ osób bezrobotnych",
     xlab="Miesi¹ce", ylab="Iloœæ osób")
points(sr_miesieczna_BO, col=c("blue","red"), pch=18, cex=2)

plot(sr_miesieczna_OP, type="l", col="blue",
     main="Œrednia iloœæ ofert pracy",
     xlab="Miesi¹ce", ylab="Iloœæ ofert")
points(sr_miesieczna_OP, col=c("purple","green"), pch=20, cex=2)


### WYKRES KO£OWY ###

suma_ogolnie <- sum(dane$Liczba_bezrobotnych_ogolem)
suma_kobiet <- sum(dane$Liczba_bezrobotnych_kobiet)
suma_mezczyzn <- suma_ogolnie - suma_kobiet


# Obliczenie procentowego udzia³u kobiet i mê¿czyzn wœród bezrobotnych
procent_kobiet <- (suma_kobiet / suma_ogolnie) * 100
procent_mezczyzn <- (suma_mezczyzn / suma_ogolnie) * 100

# Tworzenie obiektu dataframe z wartoœciami procentowymi
dane_procentowe <- data.frame(
  p³eæ = c("Kobiety", "Mê¿czyŸni"),
  procent = c(procent_kobiet, procent_mezczyzn)
)

# Tworzenie wykresu ko³owego 3D
wykres <- plot_ly(dane_procentowe, labels = ~p³eæ, values = ~procent, type = "pie", 
                  hole = 0.3, hoverinfo = "label+percent",
                  marker = list(colors = c("#FF69B4", "#1E90FF")))

# Dodanie tytu³u wykresu i opisu osi Y
wykres <- layout(wykres, title = "Procentowy udzia³ kobiet i mê¿czyzn wœród bezrobotnych")

# Wyœwietlenie wykresu
wykres


#___________________HIPOTEZY

#hipoteza pierwsza
H1 <- t.test(x=dane$Stopa_bezrobocia, 
             mu = 10, 
             alternative="less")
H1

#odrzucamy hipoteze zerowa na rzecz hipotezy alternatywnej


#hipoteza druga
H2 <- var.test(dane$Liczba_zgloszonych_ofert_pracy, 
               dane$Bezrobotni_wyrejestrowani)
H2
