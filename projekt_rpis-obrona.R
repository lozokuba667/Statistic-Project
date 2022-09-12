library(Hmisc)
library(dplyr)
library(data.table)
library(purrr)
library(rlang)
library(ggpubr)
library(ggplot2)
library(car)
library(dunn.test)
library(FSA)
#TWÓJ FOLDER DO KTÓREGO ZAPISYWANE BED¥ WYKRESY\OBRAZY\ETC


cat("\n")
cat("Podaj sciezke do folderu gdzie beda zapisywane wykresy (np D:\\Me\\FOLDER_TEST): ")
user_prop <- readLines(con = "stdin", n=1)
folder_name_path <- user_prop

outlier_boxplots_file <- paste(folder_name_path,"\\","outliers.pdf",sep="")
shapiro_density_file <-  paste(folder_name_path,"\\","density.pdf",sep="")
corelation_file <-  paste(folder_name_path,"\\","corelation.pdf",sep="")
summary_file <-  paste(folder_name_path,"\\","summary.txt",sep="")

# test_plot_part <- paste(folder_name_path,"\\","plots.pdf",sep="")
# folder_name_path <- "D:\\Me\\FOLDER_TEST"



options(scipen = 999)

#Funkcje(aby kod lepiej wygl¹da³)

#----------------BASIC FUNCTIONS START----------------------------
check_if_proceed <- function(){
  cat("Czy chcesz kontynuowac? (Y - tak, N i kazdy inny znak - nie):")
  ans <- readLines(con = "stdin", n=1)
  if(ans == 'Y'){return(TRUE)}
  else{return(FALSE)}
}

check_if_proceed_overSteps <- function(){
  if(check_if_proceed()){
    cat("Zrozumiale przechodze do dzialania!\n")
  } else{stop("Zrozumiale, koncze dzialanie programu!",call. = FALSE)}
}

check_args_isFileCsv <- function(user_args){
  if(!grepl(".csv",args[1], fixed=TRUE)){
    stop("Podany plik nie jest plikiem csv!", call. = FALSE)
  } else {cat("Przechodze do dzialania!\n")}
}

check_args <- function(user_args){
  if(length(args) == 0){
    stop("Powinien zostac podany przynajmniej jeden argument", call. = FALSE)
  } else if(length(args) > 1) {
    cat("Program przyjmnie pierwszy argument jako plik do dzialania\n")
    if(check_if_proceed()){
      check_args_isFileCsv(user_args)
    } else {stop("Rozumiem, ponow komende i sprobuj jeszcze raz\n",call. = FALSE)}
  } else{check_args_isFileCsv(user_args)}
}



#----------------BASIC FUNCTIONS END---------------------------------


#----------------1ST SUBSECTIONS START----------------------------------
find_outlier_val_graphicRepresentation <- function(data,col_num,out){
  column_name <- colnames(data[col_num])
  # name_of_plot <- paste("Outlier values for",column_name)
  # path_to_plot <- paste(folder_name_path,"\\",name_of_plot,".jpg", sep = "")
  # jpeg(file = path_to_plot, width = 600, height = 400)
  boxplot(data[,col_num] ~ data[,1],
          ylab = column_name,
          main = paste("Boxplot of",column_name,"(outlier values check)")
  )
  # mtext(paste("Outliers in whole column: ",paste(out, collapse = ", ")))
  # dev.off()
}

find_outlier_val <- function(data,col_num){
  splited_data <- split(data,f=data[1])
  general_outlier <- boxplot.stats(data[,col_num])$out
  for(splited in splited_data){
    outlier <- boxplot.stats(splited[,col_num])$out
    outlier_pos <- which(splited[,col_num] %in% c(outlier))
    cat("Wartosci odstajace (grupa:",splited[1,1],"):",outlier,"//// Polozenie:",outlier_pos,"\n")
  }
  find_outlier_val_graphicRepresentation(data,col_num,general_outlier)
}

get_means_groups <- function(data,col_num){
  splited_data <- split(data,data[1])
  mean_container <- list()
  for(elem in splited_data){
    mean_of_group <- mean(elem[,col_num], na.rm = TRUE)
    mean_container[elem[1,1]] <- mean_of_group
  }
  return(mean_container)
}


handle_impute <- function(data,column,mean_c){
  na_placement <- which(is.na(column))
  for(index in na_placement){
    group_of_intrest <- data[index,1]
    column[index] <- mean_c[[group_of_intrest]]
  }
  return(column)
}

handle_single_column <- function(data,column,col_num){
  cat("Puste krotki:",sum(is.na(column)),"\n")
  if(sum(is.na(column)) == 0){
    return(column)
  } else {
    cat("*****************************************************************************************************\n")
    cat("UWAGA!!! PROGRAM ZAMIENI PUSTE KROTKI NA SREDNIA WARTOSC W ODNIESIENIU DO DANEJ GRUPY I KOLUMNY\n")
    mean_container <- get_means_groups(data,col_num)
    # EWENTUALNA WIADOMOŒÆ
    if(check_if_proceed()){
      cat("*****************************************************************************************************\n")
      column <- handle_impute(data,column,mean_container)
      return(column)
    } else {
      cat("*****************************************************************************************************\n")
      stop("Zrozumiale. Prosze przeanalizuj swoja tabele i sprobuj jeszcze raz po wprowadzeniu modyfikacji",call. = FALSE)
    }
  }
}
#----------------1ST SUBSECTIONS END----------------------------------------

get_shapiro_plot <- function(user_data,col_name){
  # shapiro_plots_directory <- "Density_plots"
  # directory_overall <- paste(folder_name_path,"\\", shapiro_plots_directory,sep="")
  # dir.create(path=directory_overall)
  name_of_file = paste("Density_",col_name,".png",sep="")
  complete_path <- paste(folder_name_path,"\\",name_of_file,sep="")
  
  ggp <- parse_expr(colnames(user_data[1]))
  rp <- parse_expr(col_name)
  
  # jpeg(file = complete_path, width = 600, height = 400)
  # ggdensity(user_data, x= col_name,
  #           color = colnames(user_data[1]),
  #           fill = colnames(user_data[1]),
  #           palette = c("pink","blue","purple"),
  #           ylab = "gestosc",
  #           xlab = col_name
  #           ) + facet_wrap(~ eval(ggp), scales = "free")
  
  # ggexport(sp, filename = complete_path)
  
  x<-ggplot(user_data, aes(x=eval(rp), fill=eval(ggp))) +
    geom_density(alpha = 0.4) +
    labs(title = paste("Density plot for", col_name,sep = " "))+
    xlab(col_name)
  
  print(x)
}


perform_shapiro_test <-function(data, name){
  name_to_var <- parse_expr(name)
  pvalueShapiroTest <- group_by(data, data[1]) %>%
    summarise(
      statistic = shapiro.test(eval(name_to_var))$statistic,
      p.value = shapiro.test(eval(name_to_var))$p.value,
    )
  # cat("Oto Test Shapiro dla parametru ",name,"\n")
  # print(pvalueShapiroTest)
  summary_of_param <- format(pvalueShapiroTest$p.value, scientific = FALSE, digits = 3)
  summary_of_param <- as.numeric(summary_of_param)
  get_shapiro_plot(data,name)
  return(summary_of_param)
}


perform_levene_test <- function(group_name, col_name, u_data){
  g_name <- parse_expr(group_name)
  v_name <- parse_expr(col_name)
  # cat("Ponizej przedstawiony jest test Levene dla parametru:",col_name)
  # lt <- leveneTest(eval(v_name)~eval(g_name), data = u_data)
  # print(lt)
  p_val_lT <- leveneTest(eval(v_name)~eval(g_name), data = u_data)$"Pr(>F)"[1]
  return(p_val_lT)
}

evaluate_shapiro <- function(sv){
  return(ifelse(all(sv>0.05),TRUE,FALSE))
}

evaluate_levene <- function(lv){
  return(ifelse(lv>0.05,TRUE,FALSE))
}


get_test_identityOBRONA <- function(p_value){
  if(p_value > 0.05){
    return("ns")
  } else if(p_value <= 0.05 & p_value > 0.01){
    return("*")
  } else if(p_value <= 0.01 & p_value > 0.001){
    return("**")
  } else if(p_value <= 0.001 & p_value > 0.0001){
    return("***")
  } else {
    return("****")
  }
}




show_proper_test_messege <- function(st, lt, name_of_column){
  cat("Shapro p-values:",st[[name_of_column]],"\n")
  cat("Levene p-value:",lt[[name_of_column]],"\n")
  cat("\n")
  cat("WERDYKT: ")
  
  if(evaluate_shapiro(st[[name_of_column]])){
    cat("Zgodny z rozkladem normalnym \\\ ")
    if(evaluate_levene(lt[[name_of_column]])){
      cat("Homogeniczny\n")
    } else{cat("NIE homogeniczny\n")}
  } else{cat("NIEZGODNY z rozkladem normalnym\n")}
  
}

get_information_for_further_testing <-function(st, lt, name_of_column){
  shapiro_pass <- evaluate_shapiro(st[[name_of_column]])
  levene_pass <- evaluate_levene(lt[[name_of_column]])
  return(c(shapiro_pass,levene_pass))
}

perform_tStudentTest <- function(data,col_name){
  grp_param <- parse_expr(colnames(data[1]))
  researched_param <- parse_expr(col_name)
  p_val <- t.test(eval(researched_param) ~ eval(grp_param), data = data , var.equal = TRUE)$p.value
  cat("P-value",p_val,"\n")
  print(t.test(eval(researched_param) ~ eval(grp_param), data = data , var.equal = TRUE))
  cat("\n")
  cat("WERDYKT: ")
  if(p_val < 0.05){
    cat("Wystepuja roznice miedzy grupami\n")
  } else{
    cat("Brak roznic miedzy grupami\n")
  }
}

perform_WelchTest <- function(data,col_name){
  grp_param <- parse_expr(colnames(data[1]))
  researched_param <- parse_expr(col_name)
  p_val <- t.test(eval(researched_param) ~ eval(grp_param), data = data , var.equal = FALSE)$p.value
  cat("P-value:",p_val)
  print(t.test(eval(researched_param) ~ eval(grp_param), data = data , var.equal = FALSE))
  cat("\n")
  cat("WERDYKT: ")
  if(p_val < 0.05){
    cat("Wystepuja roznice miedzy grupami\n")
  } else{
    cat("Brak roznic miedzy grupami\n")
  }
}

perform_WilcxonTest <- function(data,col_name){
  grp_param <- parse_expr(colnames(data[1]))
  researched_param <- parse_expr(col_name)
  p_val <- wilcox.test(eval(researched_param)~eval(grp_param), data = data)$p.value
  cat("P-value:",p_val)
  print(wilcox.test(eval(researched_param)~eval(grp_param), data = data))
  cat("\n")
  cat("WERDYKT: ")
  if(p_val < 0.05){
    cat("Wystepuja roznice miedzy grupami\n")
  } else{
    cat("Brak roznic miedzy grupami\n")
  }
}

perform_ANOVATest <- function(data,col_name){
  grp_param <- parse_expr(colnames(data[1]))
  researched_param <- parse_expr(col_name)
  p_val <- summary(aov(eval(researched_param) ~ eval(grp_param), data = data))[[1]][["Pr(>F)"]][[1]]
  cat("P-value:",p_val,"\n")
  cat("\n")
  print(summary(aov(eval(researched_param) ~ eval(grp_param), data = data)))
  # p_val <- summary(aov(eval(researched_param) ~ eval(grp_param), data = data))[[1]][["Pr(>F)"]][[1]]
  # cat("P-value:",p_val,"\n")
  cat("\n")
  cat("WERDYKT: ")
  if(p_val < 0.05){
    cat("Wystepuja roznice miedzy grupami\n")
    cat("\n")
    cat("TEST TUKEYA (post hoc aby sprawdzic pomiedzy konkretnie ktorymi grupami wystepuja roznice)\n")
    cat("\n")
    print(TukeyHSD(aov(eval(researched_param) ~ eval(grp_param), data = data)))
    cat("\n")
  } else{
    cat("Brak roznic miedzy grupami\n")
  }
}

perform_Kruskal_WallisTest <- function(data,col_name,col_num){
  grp_param <- parse_expr(colnames(data[1]))
  researched_param <- parse_expr(col_name)
  # kruskal.test(eval(researched_param)~eval(grp_param), data = data)
  p_val <- kruskal.test(eval(researched_param)~eval(grp_param), data = data)$p.value
  cat("P-value:",p_val," ISTOTNOSC (OBRONA!!!):",get_test_identityOBRONA(p_val),"\n")
  cat("\n")
  print(kruskal.test(eval(researched_param)~eval(grp_param), data = data))
  cat("\n")
  cat("WERDYKT: ")
  if(p_val < 0.05){
    cat("Wystepuja roznice miedzy grupami\n")
    cat("\n")
    cat("TEST DUNNA (post hoc aby sprawdzic pomiedzy konkretnie ktorymi grupami wystepuja roznice)\n")
    cat("\n")
    dunn <- dunnTest(as.numeric(data[,col_num]), data[,1])
    print(dunn, dunn.test.results = TRUE)
    cat("\n")
  } else{
    cat("Brak roznic miedzy grupami\n")
  }
}


choose_test_messageTwoGroups <- function(stats,u_data,col_num,col_name){
  cat("Wybrany test: ")
  if(stats[1] == TRUE){
    if(stats[2]==TRUE){
      cat("test T.STUDENTA!\n")
      perform_tStudentTest(u_data,col_name)
    } else {
      cat("test WELCHA!\n")
      perform_WelchTest(u_data,col_name)
      cat("\n")
    }
  } else {
    cat("test WILCOXONA!\n")
    perform_WilcxonTest(u_data,col_name)
  }
}


choose_test_messageMoreThanTwoGroups <- function(stats,u_data,col_num,col_name){
  cat("Wybrany test: ")
  if(stats[1] == TRUE){
    if(stats[2]==TRUE){
      cat("test ANOVA!\n")
      perform_ANOVATest(u_data,col_name)
    } else {
      cat("test KRUSKALA - WALLISA!\n")
      perform_Kruskal_WallisTest(u_data,col_name,col_num)
    }
  } else {
    cat("test KRUSKALA - WALLISA!\n")
    perform_Kruskal_WallisTest(u_data,col_name,col_num)
  }
}



perform_tests <-function(number,stats,u_data,col_num,col_name){
  if(number == 2){
    choose_test_messageTwoGroups(stats,u_data,col_num,col_name)
  } else{
    choose_test_messageMoreThanTwoGroups(stats,u_data,col_num,col_name)
  }
}


check_corelationDrection <- function(est){
  cat("Kierunek: ")
  if(est > 0){
    cat("Korelacja dodatnia (gdy X roœnie to Y te¿ roœnie)\n")
  } else if (est == 0){
    cat("Brak jasnej korelacji (gdy X roœnie to Y czasem roœnie, czasem maleje)\n")
  } else {
    cat("Korelacja ujemna (gdy X roœnie to Y maleje)\n")
  }
}

check_corelationPower <- function(est){
  cat("Sila: ")
  if(-1 < est & est < -0.7){cat("Bardzo silna korelacja ujemna\n")}
  else if(-0.7 < est & est < -0.5){cat("Silna korelacja ujemna\n")}
  else if(-0.5 < est & est < -0.3){cat("Korelacja ujemna o srednim natezeniu\n")}
  else if(-0.3 < est & est < -0.2){cat("Slaba korelacja ujemna\n")}
  else if(-0.2 < est & est < 0.2){cat("Brak sily korelacji\n")}
  else if(0.2 < est & est < 0.3){cat("Slaba korelacja dodatnia\n")}
  else if(0.3 < est & est < 0.5){cat("Korelacja dodatnia o srednim natezeniu\n")}
  else if(0.5 < est & est < 0.7){cat("Silna korelacja dodatnia\n")}
  else{cat("Bardzo silna korelacja dodatnia\n")}
}



check_corelation <- function(from,to){
  corelation_test <- cor.test(from, to, method = "pearson")
  # cat("P-value:",corelation_test$p.value,"\n")
  cat("Korelacja: ")
  if(corelation_test$p.value > 0.05){
    cat("Brak\n")
  } else {
    cat("Istnieje!\n")
    cat("Wspó³czynnik korelacji:",corelation_test$estimate,"\n")
    check_corelationDrection(corelation_test$estimate)
    check_corelationPower(corelation_test$estimate)
  }
  
}

create_corelation_plot <- function(from_n, to_n, cur_grp){
  y<-ggscatter(cur_grp, x = from_n, y = to_n,
               add = "reg.line", conf.int = TRUE,
               cor.coef = TRUE, cor.method = "pearson",
               color = colnames(cur_grp[1]), fill = colnames(cur_grp[1]),
               ylab = from_n,
               xlab = to_n)
  print(y)
}

get_numeric_namesOBRONA <- function(my_data){
  container <- c()
  for(i in 1:ncol(my_data)){
    if(is.numeric(my_data[,i])){
      container <- append(container,colnames(my_data[i]))
    }
  }
  return(container)
}


evaluate_singleShapiroOBRONA <- function(item){
  if(item < 0.05){
    return("NIEZGODNY")
  } else {
    return("ZGODNY")
  }
}

create_Shapiro_raportOBRONA <- function(shapiro_ls,group_names,numeric_names){
  cat("\n")
  cat("OTO PELNY RAPORT PODSUMOWUJACY ZGODNOSC Z ROZKLADEM NORMALNYM (OBRONA)!!!:\n")
  cat("\n")
  # print(shapiro_ls)
  # print(group_names)
  # print(numeric_names)
  for(name in numeric_names){
    cat("PARAMETR:",name,"\n")
    cat("------------------------------------------------------------------------------\n")
    for(j in 1:length(group_names)){
      cat("Grupa:",group_names[j],"\\\\")
      cat(group_names[j])
      cat("P-val:",shapiro_ls[[name]][j],"\\\\")
      cat("Rezultat:",evaluate_singleShapiroOBRONA(shapiro_ls[[name]][j]),"\n")
      cat("\n")
    }
    cat("--------------------------------------------------------------------------------\n")
    cat("\n")
  }
}



show_welcoming_message <-function(){
  cat("\n")
  cat("-----------------PROGRAM DO ANALIZY STATYSTYCZNEJ--------------------------------\n")
  cat("Witaj uzytkowniku!\n")
  cat("Ten program sluzy do przeprowadzenia pelnowymiarowej analizy statystycznej dla danych ilosciowych i operacji z nia powiazanych:\n")
  cat("Glowne komponenty programu to:\n")
  cat("#1. Przygotowanie danych wejscowych przez usuniecie brakow danych / Informacje o wartosciach odstajacych\n")
  cat("#2. Przygotowanie charakterystyki dla badanych grup (w formie tabelarycznej)\n")
  cat("#3. Wykonanie analizy porownawczej pomiedzy grupami\n")
  cat("#4. Wykonanie analizy korelacji\n")
  cat("\n")
  cat("Jesli nie rozumiesz powyzszych pojec, to spokojnie - program jest przygotowany w sposob przystepny dla nowych uzytkownikow :)\n")
  cat("PAMIÊTAJ: Do ka¿dego z w.w podpunktów s¹ do³¹czane wykresy w folderze który poda³eœ zanim przeczyta³eœ t¹ wiadomoœæ. Odpowiednio pliki nazywaj¹ siê: outliers.pdf, summary.txt, density.pdf, corelation.pdf\n")
  cat("\n")
}

show_explaining_1 <- function(){
  cat("\n")
  cat("***************************************** SLOWO WSTEPU #1 *********************************************\n")
  cat("\n")
  cat("#1 PRZYGOTOWANIE DANYCH WEJSCIOWYCH\n")
  cat("\n")
  cat("Ten komponent programu zajmuje sie przygotowaniem twoich danych do dalszej analizy\n")
  cat("Glownym jego zadaniem jest sprawdzenie twoich danych pod katem pustych wartosci (NA) oraz wartosci odstajacych\n")
  cat("DLACZEGO PUSTE WARTOSCI SA ZLE? -- Poniewaz ich obecnosc uniemozliwia przeprowadzenie analizy statystycznej\n")
  cat("DLACZEGO INFORMUJEMY O ODSTAJACYCH WARTOSCIACH? --- Poniewaz dane odstajace od normy, sa w stanie skutecznie namieszac w analizie statystycznej\n")
  cat("Skoro sprawy teoretyczne mamy juz za soba, to przejdzmy do wyjasnienia formatu! (w jaki sposob program wyswietla dane)\n")
  cat("\n")
  cat("INFORMACJE DLA KOLUMNY: x -------> Jaka obecnie kolumne analizuje program\n")
  cat("------------------------------------------------------------------------------\n")
  cat("Puste krotki: x ------> Ile pustych wartosci znajduje sie w danej kolumnie\n")
  cat("Wartosci odstajace (grupa x): x \\\\ Polozenie: x ----------> Jakie wartosci konkretnie odstaja od normy w ramach danej grupy i gdzie sie znajduja (numer to dany wiersz w tabeli)\n")
  cat("------------------------------------------------------------------------------------\n")
  cat("\n")
  cat("W przypadku gdy program znajdzie puste wartosci, to zatrzyma swoje dzialanie. To czas dla ciebie na podjecie akcji\n")
  cat("Domyslnie program przeliczy srednia artymetyczna w ramach danej grupy i badanej kolumny. Jesli nie chcesz sie meczyc z zamienianem samodzielnie wartosci lub odpowiada ci taka zmiana --> postepuj zgodnie z komunikatem\n")
  cat("Jesli jednak nie chcesz z jakiegos powodu tego robic w sposob proponowany - to zgodnie z komunikatem odmow. Program wtedy zatrzyma swoje dzialanie definitywnie, a ty bedziesz miec czas na poprawienie wartosci manualnie\n")
  cat("\n")
  cat("W przypadku wartosci odstajacych, program dodatowo zapisuje wykresy boxplot w podanym folderze. Jesli chcesz zrozumiec na czym polega boxplot to zajrzyj do manuala!\n")
  cat("\n")
  cat("***********************************************************************************************************************\n")
}


show_explaining_2 <- function(){
  cat("\n")
  cat("***************************************** SLOWO WSTEPU #2 *********************************************\n")
  cat("\n")
  cat("#2 CHARAKTERYSTYKA GRUP\n")
  cat("\n")
  cat("Ten komponent programu zajmuje sie przygotowaniem charakterystyk w formie tabelarycznej\n")
  cat("Taka charakterystyka sluzy glownie tobie jako informacja zwrotna, jak skonstruowane sa wprowadzone dane\n")
  cat("PO CO TO? ---> Glownie po to bys mogl na spokojnie jeszcze raz przeanalizowac dane w szerszym ujeciu\n")
  cat("Pora wyjasnic format!\n")
  cat("\n")
  cat("$jakas_grupa -----> Grupa wyciagnieta z danych\n")
  cat("\n")
  cat("    Badany parametr(kolumna)   \n")
  cat("\n")
  cat("Min. x -> minimum -> Minimalna wartosc wyciagnieta z wycinka grupa/kolumna\n")
  cat("1st Qu. --> 1st Quartile --> Pierwszy kwartyl ---> Najprosciej wyjasnic to na przykladzie. Jesli masz zbior danych (1,2,3,4,5,6,7,8) (ergo: poukladany rosnaco) to pierszym kwartylem bedzie liczba 2. Innymi slowami wartosc bedaca na pozycji pierwszej cwiartki\n")
  cat("Med. --> Mediana ---> Wartosc srodkowa --> Wartosc bedaca w polowie zbioru --> Zgodnie z przykladem wyzej byloby to (4+5)/2 = 4.5\n")
  cat("Mean. --> Srednia ---> Srednia arytmetyczna danego wycinka\n")
  cat("3rd Qu. --> 3rd Quartile --> Trzeci kwartyl ---> Wartosc bedaca w 3/4 zbioru. Analogicznie do przykladu z mediana i 1sz Qu. tu 3rd Quartile byloby 6\n")
  cat("Max. --> Maximum --> Maksymalna wartosc z wycinka\n")
  cat("\n")
  cat("Dodatkowo nalezy wspomniec, ze komponent przedstawi tez analize dla danych nieliczbowych. W tym wypadku jest to jednak tylko informacja o klasie i typie danych. Nieistotne dla dzialania programu\n")
  cat("\n")
  cat("***********************************************************************************************************************\n")
  
}



show_explaining_3 <- function(){
  cat("\n")
  cat("***************************************** SLOWO WSTEPU #3 *********************************************\n")
  cat("\n")
  cat("#3 ANALIZA POROWNAWCZA GRUP (ZGODNOSC Z ROZKLADEM NORMALNYM I JEDNORODNOSC FUNKCJI)\n")
  cat("\n")
  cat("Ten komponent programu zajmuje sie analiza prownawcza pomiedzy grupami\n")
  cat("To jest pierwsza czesc komponentu ktora okresla czy dany parametr w ramach grup jest zgodny z rozkladem normalnym, oraz czy jest jednorodny\n")
  cat("PO CO TO? --> Poniewaz od okreslenia zgodnosci i jednorodnosci zalezy to jakie testy zostana dobrane do wlasciwej analizy porownawczej\n")
  cat("W przypadku obu testow liczy sie przede wszystkim uzyskane p-value (taki parametr)\n")
  cat("Tak samo w obu przypadkach wartosc p-value musi byc wieksza niz 0.05. To swiadczy o wystepowaniu zgodnosci/jednoorodnosci\n")
  cat("\n")
  cat("TEST SHAPIRO zwraca 3 wartosci p_value --- po jednej dla kazdej grupy --- w tym wypadku KAZDA wartosc p-value musi spelniac warunek >0.05\n")
  cat("TEST LEVENE zwraca 1 wartosc p-value --- tutaj nie potrzeba wiekszej filozofii\n")
  cat("Pora wyjasnic format!\n")
  cat("\n")
  cat("WYNIKI TESTU SHAPIRO/LEVENE DLA PARAMETRU: x\n")
  cat("--------------------------------------------------------------\n")
  cat("Shapiro p-values: x, ... ----> Wartosci p_values na podstawie testu Shapiro\n")
  cat("Levene p-value: x ------------> Wartosc p_value z testu Levene\n")
  cat("\n")
  cat("WERDYKT: text ---------------> Wypisanie slownie wyniku testu\n")
  cat("---------------------------------------------------\n")
  cat("\n")
  cat("**************************************************************************************************************\n")
}


show_explaining_3.5 <- function(){
  cat("\n")
  cat("***************************************** KONTYNUACJA #3 *********************************************\n")
  cat("\n")
  cat("#3 ANALIZA POROWNAWCZA GRUP (WLASCIWE TESTY)\n")
  cat("\n")
  cat("Po sprawdzeniu jednorodnosci i homogenicznosci dalsza czesc komponentu dobierze odpowiedni test w celu analizy porownawczej\n")
  cat("Testy sa dobierane na podstawie:\n")
  cat("1. Liczby badanych grup\n")
  cat("2. Zgodnosc z rozkladem normalnym\n")
  cat("3. Homogenicznosc funkcji\n")
  cat("\n")
  cat("Wybor odpowiedniego testu odbywa sie automatycznie przez program\n")
  cat("Regula oceniania tego czy grupy sa ze soba zgodne jest podobna do pracy przy Shapiro|Levene\n")
  cat("Opiera sie na tym czy p-value jest wieksze mniejsze od 0.05!\n")
  cat("W przypadku gdy istnieja roznice, a grup badanych jest wiecej niz 2 --- W takim wypadku przeprowadza sie post_hoc\n")
  cat("Post hoc ma na celu wykazac pomiedzy konkretnie jakimi grupami wystepuja roznice, oraz jak bardzo sa istotne\n")
  cat("Pora omowic format!\n")
  cat("\n")
  cat("PARAMETER x\n")
  cat("----------------------------------\n")
  cat("Wybrany test: test xxxx ---------> jaki test zostal dobrany przez program\n")
  cat("P-value z testu: x ------------> Otrzymane p-value z testu\n")
  cat("\n")
  cat("Werdykt: text --------> Czy miedzy grupami sa/nie sa istotne roznice statystyczne\n")
  cat("*NOTKA*\n")
  cat("----------------------------------------------\n")
  cat("\n")
  cat("*NOTKA* ---> W przypadku negatywnego wyniku zostanie przeprowadzony post_hoc, Wynik danego post hoc jest zaprezentowany w formie blokowej. Dokladniejsze wyjasnienie w dolaczonym manualu\n")
  cat("\n")
  cat("**************************************************************************************************************\n")
  cat("\n")
}



show_explaining_4 <- function(){
  cat("\n")
  cat("***************************************** SLOWO WSTEPU #4 *********************************************\n")
  cat("\n")
  cat("#4 ANALIZA KORELACJI\n")
  cat("\n")
  cat("Ten komponent programu bada tzw korelacje pomiedzy poszczegolnymi parametrami w obrebie danych grup.\n")
  cat("Korelacja najprosciej - jest to zaleznosc przyczynowo-skutkowa pomiedzy poszczegolnymi parametrami\n")
  cat("W celu zbadania tych zaleznosci jest zastosowany test korelacji metoda Peasona\n")
  cat("Z takiego testu otrzymujemy zarowno wspolczynnik korelacji jak i p-value\n")
  cat("Jesli p-value jest mniejsze niz 0.05 to znaczy ze korelacja istnieje\n")
  cat("W tym przypadku program wyswietli wyspolczynnik korelacji i tekstowo okresli sile i kierunek\n")
  cat("Pora wyjasnic format\n")
  cat("\n")
  cat("DANE DLA GRUPY: x\n")
  cat("*******************************************************************************************************\n")
  cat("KORELACJA x - y\n")
  cat("-----------------------------------------------\n")
  cat("Korelacja - istnieje/nie istnieje\n")
  cat("Wspolczynnik korelacji - x\n")
  cat("Kierunek:\n")
  cat("Sila:\n")
  cat("-------------------------------------------------------\n")
  cat("*******************************************************************************************************\n")
  cat("\n")
  cat("*****************************************************************************************************\n")
  cat("\n")
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------





# "Main"
# 1
args <- commandArgs(trailingOnly = TRUE)
check_args(args)
user_data <- read.csv2(file = args[1], header = TRUE)
dir.create(path = folder_name_path,showWarnings = FALSE)

show_welcoming_message()
check_if_proceed_overSteps()

show_explaining_1()
check_if_proceed_overSteps()

pdf(file = outlier_boxplots_file)
for(i in 1:ncol(user_data)){
  if(is.numeric(user_data[,i])){
    cat("INFORMACJE DLA KOLUMNY:",colnames(user_data[i]),"\n")
    cat("----------------------------------------------------------\n")
    user_data[,i] <- handle_single_column(user_data,user_data[,i],i)
    find_outlier_val(user_data,i)
    cat("-----------------------------------------------------------\n")
  }
  cat("\n")
}
dev.off()

cat("\n")
cat("!!!!!!!!!KONIEC PRACY KOMPONENTU!!!!!!!!!!!!\n")
cat("\n")
check_if_proceed_overSteps()

# 2
cat("\n")
show_explaining_2()
cat("\n")
check_if_proceed_overSteps()
cat("\n")

sum <- user_data %>%
  split(user_data[,1]) %>%
  map(summary)

print(sum)
write.table(sum, file = summary_file, sep = "\t")

cat("\n")
cat("!!!!!!!!!KONIEC PRACY KOMPONENTU!!!!!!!!!!!!\n")
cat("\n")
check_if_proceed_overSteps()

#3

cat("\n")
show_explaining_3()
cat("\n")
check_if_proceed_overSteps()

group_names <- unique(user_data[[1]])
shapiro_stats <- list()
levene_stats <- list()
evaluation_ls<- list()

cat("\n")

pdf(shapiro_density_file)
for(j in 1:ncol(user_data)){
  if(is.numeric(user_data[,j])){
    name_of_col <- colnames(user_data[j])
    cat("WYNIKI TESTU SHAPIRO|LEVENE DLA PARAMETRU:",name_of_col,"\n")
    cat("------------------------------------------------------------------------------------------\n")
    shapiro_stats[[name_of_col]] <- perform_shapiro_test(user_data,name_of_col)
    levene_stats[[name_of_col]] <- perform_levene_test(colnames(user_data[1]),name_of_col,user_data)
    show_proper_test_messege(shapiro_stats,levene_stats,name_of_col)
    evaluation_ls[[name_of_col]] <- get_information_for_further_testing(shapiro_stats,levene_stats,name_of_col)
  }
  cat("-------------------------------------------------------------------------------------------------\n")
  cat("\n")
  cat("\n")
}
dev.off()

# print(shapiro_stats)
# cat(shapiro_stats[['hsCRP']][1])
# cat(group_names[1])

num_namesOBRONA <- get_numeric_namesOBRONA(user_data)
create_Shapiro_raportOBRONA(shapiro_stats,group_names,num_namesOBRONA)


cat("\n")
cat("!!!!!!!!!KONIEC PRACY KOMPONENTU!!!!!!!!!!!!\n")
cat("\n")
check_if_proceed_overSteps()

cat("\n")
show_explaining_3.5()
cat("\n")
check_if_proceed_overSteps()
cat("\n")


for(x in 1:ncol(user_data)){
  if(is.numeric(user_data[,x])){
    name_of_col <- colnames(user_data[x])
    cat("ANALIZA DLA PARAMETRU:",name_of_col,"\n")
    cat("--------------------------------------------------------------------------------------\n")
    perform_tests(length(group_names),evaluation_ls[[name_of_col]],user_data,x,name_of_col)
    cat("--------------------------------------------------------------------------------------\n")
  }
  cat("\n")
  cat("\n")
}



cat("\n")
cat("!!!!!!!!!KONIEC PRACY KOMPONENTU!!!!!!!!!!!!\n")
cat("\n")
check_if_proceed_overSteps()

cat("\n")
show_explaining_4()
cat("\n")
check_if_proceed_overSteps()
cat("\n")


pdf(corelation_file)
splitted_data <- split(user_data, user_data[1])
for(item in splitted_data){
  cat("\n")
  cat("DANE DLA GRUPY",item[1,1],"\n")
  cat("****************************************************************\n")
  for(i in 1:(ncol(item)-1)){
    if(is.numeric(item[,i])){
      for(j in (i+1):ncol(item)){
        if(is.numeric(item[,j])){
          cat("PARA",colnames(item[i]), "---",colnames(item[j]),"\n")
          cat("------------------------------------------------------------\n")
          check_corelation(item[,i],item[,j])
          create_corelation_plot(colnames(item[i]), colnames(item[j]), item)
          cat("-------------------------------------------------------------\n")
        }
        cat("\n")
      }
    }
  }
  cat("****************************************************************\n")
  cat("\n")
}
dev.off()












