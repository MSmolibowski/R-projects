#BIBLIOTEKI
{
    library(dplyr)
    library(ggpubr)
    library(car)
    library(dunn.test)
    library(FSA)
    library(Hmisc)
    library(multcompView)
    library(datasets)
    library(rcompanion)
    library(ggplot2)
    library(gridExtra)
    library(psych)
    library(gtsummary)
    library(lattice)
    library(car)
    library(PMCMRplus)
    library(xlsx)
    library(readxl)
    library(MultNonParam)
    library(agricolae)
    library(coin)
    library(survival)
    
    
}
#WCZYTANIE DANYCH i porbanie poczatkowych informacj



{       #-------------------Start programu-------------------------------------------
decyzja <- readline(prompt = "Czy chcesz przeprowadzic analize wpisz: 'TAK' lub 'NIE' : ")

    gg = 0 #numer zapisanego pliku
    
while(decyzja != 'NIE')
{ #------------------------------START programu-------------------------------------------------------
gg<- gg + 1
dane <- read.csv2(file.choose(), sep = ';')
dane$grupa<- as.factor(dane$grupa)

save_data_name <- readline(prompt="Podaj nazwe pliku do zapisu: ")

graph_name <- paste(gg, save_data_name, 'graph', sep="-")
graph_save_name <- paste(graph_name, '.pdf', sep = '') 

save_result_data_name <- paste(gg, 'res', save_data_name, sep="-")   #nazwa pliku wyjsciowego
save_result_data_name <- paste(save_result_data_name, ".xlsx", sep = '')

{# START OBLICZEN
{
{
    columns <- colnames(dane) #wektor z nazwami kolumn
    numb_col <- length(columns) #liczba kolumn
    numb_row <- nrow(dane)  # liczba wierszy
    gr <- unique(dane$grupa)  #wektor z grupami
    
    group_size <- c(1:length(gr))
    
    koniec_gr <- c(1:length(gr))
    pocz_gr <- c(1:length(gr))
    numb_gr <- length(gr)
    
    
}

##########Pobranie poczarkow i koncow grup oraz ich wielkosci######################

{
for(i in 1:length(gr))
{
    group_size[i] <- 0
    koniec_gr[i] <- 0
    pocz_gr[i] <-0
}

for(i in 1:length(gr))
{
    
    for(j in 1:numb_row)
    {
        if(gr[i] == dane[j,1])
        {
            group_size[i] <- group_size[i] + 1
        }
    }
    
}

for(i in 1:numb_gr)   #petla do zapisania konca grupy i poczatku !!! POPRAWIONA
{
    if(i == 1)
    {       
        pocz_gr[i] <- 1
        koniec_gr[i] <- group_size[i]
        
    }
    else
    {
        pocz_gr[i] <- koniec_gr[i-1] + 1
        koniec_gr[i] <- pocz_gr[i] + group_size[i]-1      
        
    }
    
}
}
######## Charakterystyki opisowe (liczenie dla 1 kolumny - masa)

{#Start
for(k in 1:numb_gr)
{
    
    p <- group_by(dane[pocz_gr[k]:koniec_gr[k],1:numb_col]) %>%
        summarise(
            Srednia = format(round(mean(dane[pocz_gr[k]:koniec_gr[k],2], na.rm = TRUE), 2), nsmall = 2),
            Sr_wazona = format(round(weighted.mean(dane[pocz_gr[k]:koniec_gr[k],2], na.rm = TRUE), 2), nsmall = 2),
            
            Odch_St = format(round(sd(dane[pocz_gr[k]:koniec_gr[k],2], na.rm = TRUE), 2), nsmall = 2),
            Mediana = format(round(median(dane[pocz_gr[k]:koniec_gr[k],2], na.rm = TRUE), 2), nsmall = 2),
            MAX = format(round(max(dane[pocz_gr[k]:koniec_gr[k],2], na.rm = TRUE), 2), nsmall = 2),
            MIN = format(round(min(dane[pocz_gr[k]:koniec_gr[k],2], na.rm = TRUE), 2), nsmall = 2),
            Wariancja = format(round(var(dane[pocz_gr[k]:koniec_gr[k],2], na.rm = TRUE), 2), nsmall = 2),
        )
    
    if(k == 1){p2 <- p}
    if(k == 2)
    {
        p3<-rbind(p2,p)
    }
    if(k > 2)
    {
        p3<-rbind(p3,p)
    }
}

#       Wpisanie danych z p3 do nowej tabeli i dodanie nazw zwiazkow
charakterystyki_opisowe <- p3
charakterystyki_opisowe$Zwiazek <- gr

#       Testy normalnosci i homogenicznosci     


# TEST NORMALNOSCI

for(k in 1:numb_gr)
{
    s <- group_by(dane[pocz_gr[k]:koniec_gr[k],1:numb_col]) %>%
        summarise(
            statistic = round(shapiro.test(dane[pocz_gr[k]:koniec_gr[k],2])$statistic,3),
            p.value = round(shapiro.test(dane[pocz_gr[k]:koniec_gr[k],2])$p.value ,3)
        )
    
    if(k == 1){s2 <- s}
    if(k == 2)
    {
        s3<-rbind(s2,s)
    }
    if(k > 2)
    {
        s3<-rbind(s3,s)
    }
    
}
s3$Zwiazek <- gr
wilk_table <- s3

#--------------------Test homogenicznosci--------------------


homogenity_test <- leveneTest(masa ~ grupa, data = dane)



# ----------- Sprawdzenie jaki test wybrac Anova czy Kruskall Wallies???
           

wilk_pvale <- wilk_table$p.value    #wyciagniecie p_value
wilk_yes <- 1 #bazowo brak roznic miedzy danymi


levene_pvalue <- homogenity_test$`Pr(>F)`   #wyciagniecie p_value
homogenity_yes <- 0


for(r in 1:length(wilk_pvale))
{
    if(wilk_pvale[r] < 0.05)
    {
        wilk_yes <- 0
        print("Wilk - Brak normalnosci rozkladu")
        print(wilk_pvale)

        break
    }
}
if(wilk_yes == 1)
{
    print("Wilk - Rozklad normalny")
    print(wilk_pvale)
    
}

if(levene_pvalue[1] > 0.05)
{
    homogenity_yes <- 1
    print("Leven - Homogenicznosc danych")
    print(levene_pvalue[1])
}
else if(levene_pvalue[1] < 0.05)
{
    homogenity_yes <- 0
    print("Leven - Brak homogenicznosci")
    print(levene_pvalue[1])
}


}#KONIEC
    

#-------------------------WYBOR TESTU STATYSTYCZNEGO DLA WIELU GRUP!!!--------------------------
{#start
c <- ""

if(wilk_yes == 0)   #Brak rozkladu normalnego ------> KruskallWallis
{
    
    print("Wilk-> NIE")
    KW_test <- kruskal.test(masa ~ grupa, data = dane)
    wybrany_test <- "KruskallWallies"
    print("KW test - ")
    
    
    if(KW_test$p.value > 0.05)
    {
        print("KW no post-hoc Wilk-> NIE")
    }
    else if(KW_test$p.value < 0.05)
    {
        DT_posthoc <- dunnTest(dane$masa, dane$grupa)
        print("KW Dunn test->Wilk  NIE")
        
    }
    
}
else if(wilk_yes == 1)
{   
    print("Wilk-> TAK")
    
    if(homogenity_yes == 0) #homogenicznosc niezachowana ----> KW test
    {
        KW_test <- kruskal.test(masa ~ grupa, data = dane)
        wybrany_test <- "KruskallWallies"
        print("KW test Wilk-> TAK; Leven ->NIE")
        
        if(KW_test$p.value > 0.05)
        {
            test_posthoc <- "NIE"
            print("KW no post-hoc Wilk-> TAK; Leven ->NIE")
            
        }
        else if(KW_test$p.value < 0.05)
        {
            DT_posthoc <- dunnTest(dane$masa, dane$grupa)
            print("KW Dunn test Wilk-> TAK, Leven ->NIE")
            
        }
        
    }
    if(homogenity_yes == 1)    #homogeniocznosc zachowana zrob test ANOVA
    {
        
        
        A_test <- summary(aov(masa ~ grupa, data = dane))[[1]] #ladny wynik
        A_test_pval <- summary(aov(masa ~ grupa, data = dane))[[1]][["Pr(>F)"]][[1]]
        to_tukey <- aov(masa ~ grupa, data = dane) #ladny wynik
        
        wybrany_test <- "ANOVA"
        print("Anova test -> Wilk-> TAK' Leven -> TAK")
        
        
        if(A_test_pval > 0.05)  #Brak roznic miedzy grupami
        {
            print("Anova test -> Wilk-> TAK' Leven -> TAK")
            
        }
        else if(A_test_pval < 0.05) #sa roznice miedzy grupami
        {
            TT_posthoc <- TukeyHSD(to_tukey)
            print("Anova test -> Wilk-> TAK' Leven -> TAK")
            
        }

        
    }
}
}
#----------------------------------------------------------------------------
######------------------------Wyciagniecie liter KW !!!---------------------------------------------------
{   #START
if(wybrany_test == "KruskallWallies")
{
    temp <- DT_posthoc$res #wyciagniecie samych danych
    Comparision_Padj  <- temp[-c(2,3)]  #wyciagniecie nazw i wartosci pvalue
    
    
    cld <- cldList(P.adj ~ Comparison,
                   data = Comparision_Padj,
                   threshold  = 0.05)       #wyciagniecie liter dla testu KW
    
    #cld <- as.data.frame.list(cld)
}
if(wybrany_test == "ANOVA")
{
    cld <- multcompLetters4(to_tukey, TT_posthoc)
}


#-------------------------------WYKRES-----------------------------------------------------


    graph_data <- group_by(dane, grupa) %>%
    summarise(mean=mean(masa), quant = quantile(masa, probs = 0.75)) %>%
    arrange(desc(mean))

#-----------------sortowanie grup w Tk---------------------------- PRZYJRZEC SIE TUTAJ (czy potrzebne te cld as.data.frame)?


if(wybrany_test == 'KruskallWallies')              #Przy tescie KruskalaWaliesa potrzebne sortowanie grup
{
    cld <- as.data.frame.list(cld)  #tutaj ma byc cld a nie jak dla Anovy cld$grupa czy costam
    
    new_graph_data <- graph_data[order(graph_data$grupa),]
    new_graph_data$cld <- cld$Letter
    
}
if(wybrany_test == 'ANOVA')     #Przy Anovie-Tukey niepotrzeba sortowania liter  grup
{
    new_graph_data <- graph_data
    cld <- as.data.frame.list(cld$grupa)
    new_graph_data$cld <- cld$Letters
}
#Zapis wykres start
    # 2. Create the plot
    
    wykres <- ggplot(dane, aes(grupa, masa) )+
        geom_boxplot(aes(fill = grupa)) +
        labs(x = 'Badany zwiazek', y = 'Masa roslin (g)')+
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        geom_text(data = new_graph_data, aes(x = grupa, y = quant, label = cld), size = 2.5, vjust=-1, hjust =-0.1, color = "darkblue")+
        ggtitle(graph_name)
    
    
    pdf(graph_save_name)
    print(wykres)
    dev.off()
    
    print(wykres)
    
    # 3. Close the file
#zapis wykres koniec
}#KONIEC
    
}#koniec klamry wykonujacej
#----------------------------------SAVE DATA-----------------------------------------
}#KONIEC OBLICZEN


write.xlsx(dane, file = save_result_data_name, sheetName = "Loaded data", append = FALSE)
write.xlsx(charakterystyki_opisowe, file = save_result_data_name, sheetName = "Opisowe", append = TRUE) #opisowe
write.xlsx(wilk_table, file = save_result_data_name, sheetName = "Szafirowilk", append = TRUE) #wynik testu szafirowilka
write.xlsx(homogenity_test, file = save_result_data_name, sheetName = "Homogenicznosc", append = TRUE) #wynik testu homogenicznosci
write.xlsx(cld, file = save_result_data_name, sheetName = "Litery", append = TRUE) #Litery

# ------- dane do zapisu, plik w jakim zapisuje, nazwa arkusza, dopisz do istniejacego pliku = tak/nie)

if(wybrany_test == "KruskallWallies")
{   
    kw_export <- as.data.frame.list(KW_test)
    dt_export <- DT_posthoc$res
    wynik_dunn_test <- DT_posthoc$res
    
    
    write.xlsx(kw_export, file = save_result_data_name, sheetName = "KruskallWallies", append = TRUE) #wynik testu KW
   # write.xlsx(dt_export, file = save_result_data_name, sheetName = "DunnTest", append = TRUE) #wynik testu Dunna
    write.xlsx(wynik_dunn_test, file = save_result_data_name, sheetName = "DunnTest - porownanie grup", append = TRUE) #wynik testu Dunna
    

}
if(wybrany_test =="ANOVA")
{
    
    #kw_export <- as.data.frame.list(A_test_pval)
    TT <- as.data.frame(TT_posthoc$grupa)
    #wynik_Tukey_test <- TT_posthoc 
    
    write.xlsx(A_test, file = save_result_data_name, sheetName = "ANOVA", append = TRUE) #wynik testu ANOVA
  #  write.xlsx(TT, file = save_result_data_name, sheetName = "TukeyTest", append = TRUE) #wynik testu Tukey
    write.xlsx(TT, file = save_result_data_name, sheetName = "TukeyTest - porownanie grup", append = TRUE) #wynik testu Tukey
    
}


print("Koniec czesci analitycznej")
cat("Zapisano plik: ", save_result_data_name, " oraz graf: ", graph_save_name,'\n')
decyzja <- readline(prompt = "Czy chcesz przeprowadzic analize wpisz: 'TAK' lub 'NIE' : ")


}        #koniec czesci analityczej i zapisu

    print("Koniec dzialania programu")
    print("Do widzenia")
    
}   #koniec calego programu


 
  