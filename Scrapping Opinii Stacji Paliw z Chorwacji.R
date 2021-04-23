library(tidyverse)
library(rvest)
library(glue)
library(stringr)
library(lubridate)
library(knitr)
library(kableExtra)
library(RSelenium)
library(scales)
library(xlsx)


########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#####
###  Author: Tomasz Szczurowski
###  Date: 08/04/2021
######
####            Kod dziala w nastepujacy sposob: zebrac linki do miejsc danego typu w Google Maps np. stacje benzynowe, a nastepnie pobierane dane obiektow
####
####            Czesc 1: Zbieranie linków do miejsc danego typu
####            Petla zbiera linki do konkretnych miejsc z Google maps tzn. 'wyniki do podstron w ramach zapytania 'stacje paliw w poblizu Warszawa'.
####            Nastepnie zapuszczam ponownie funkcje wyzej wchodzac w inne geotrafie zamiast 'Warszawa' np. Radom, Inoroclaw i 100 innych najwiekszych miast w Polsce
####
####            Czesc 2: Pobieranie danych do konkretnych miejsc
####            Petla, ktora wchodzi w kazdy z linkow i pobiera kluczowe atrybuty miesjsca: Nazwa, # opinii, liczba gwiazdek, adres, WWW, tel. godziny otwarcia
###
## 
### Oglnie kod napisany w oparciu o to szkolenie - mozna tu znalezc materialy odnosnie jak pobrac Dockera
### https://blog.prokulski.science/2018/06/05/webscrapping-w-r/
###
### ### ### ### ### 
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################


####### Czesc 1 - Wchodze na Google Maps i zbieram linki do stacji benzynowych

### Odpalam Dockera
docker_pid <- system("docker run -d -p 4445:4444 --shm-size=2g selenium/standalone-chrome", intern = TRUE)
print(docker_pid)
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
Sys.sleep(5) # Daje maszynie 5 sekund na restart

### Otwiera przegladarke
remDr$open()

#### Wchodze pierwszy raz na strone, aby kliknac 'I accept' i nastepnie mozna korzystac z Google Maps

### Wchodzi na konkretna strone internetowa
remDr$navigate("https://www.google.pl/maps/search/stacje+paliw+w+pobli%C5%BCu+Szczecin/@51.9182925,19.5063461,7z")
Sys.sleep(5) # 5 sekund

# Clicks on I agree button in Google
webElem <- remDr$findElement(using = 'xpath', value = "//button")
webElem$clickElement()



#### Wyswietlam ekran po poczekaniu 5 sekund, zeby zobaczyc, czy strona sie odpalila
Sys.sleep(5)     
remDr$setWindowSize(1920, 1080, "current")
remDr$screenshot(display = TRUE)



##### Regiony, ktore chce scrappowac
Miasta <- c("Bjelovar-Bilogora","Brod-Posavina", "Dubrovnik-Neretva","Zagreb City","Istria","Karlovac","Koprivnica-KriÅ¾evci","Krapina-Zagorje","Lika-Senj",
            "Medimurje","Osijek-Baranja","Pozega-Slavonia","Kvarner","Sisak-Moslovina","Split-Dalmatia","Sibenik-Knin","Varazdin","Virovitica-Podravina",
            "Vukovar-Srijem","Zadar","Zagreb County")

### Kod ma wade - nie potrfi rozwinac listy z Google z 10 na 20 obiektów przez co pobieram tylko 10 wynikow z 20 na podstronie

# Miasto <- c("Bjelovar-Bilogora")

PobierzMiejsca <- function(Miasto) {

  #### Warto podmienic w URL wspolrzedne i kraj jesli inne, niz Chorwacja
  URL_ZLokalizacjaMiasta <- paste(c("https://www.google.pl/maps/search/stacje+paliw+w+poblizu+",Miasto,",+Chorwacja/@44.883722,14.3667112,7.88z"), sep="",collapse = '')
  
  remDr$navigate(URL_ZLokalizacjaMiasta)
  Sys.sleep(5) # 5 sekund
  
  
  #### Czyszcze zmienne i rozpoczynam pobieranie kontenu strony i rozparsowuje
  
  webElem <- remDr$findElement(using = 'xpath', value = "//*[@id='n7lv7yjyC35__section-pagination-button-next']")
  
  KodStrony <- rm()
  
  html_source <- remDr$getPageSource()
  
  KodStrony <- read_html(html_source[[1]])
  
  
  # szukamy wszystkich linków do stacji w Google
  linki_tmp <- KodStrony %>% 
    html_node(xpath = '//*[@id="pane"]') %>%
    html_nodes(xpath = "//div[@class='widget-pane widget-pane-visible']/div[@tabindex='-1']/div[@class='widget-pane-content-holder']/
                div[@class='section-layout section-layout-root']/div/div/div") %>%
    html_nodes("a") %>% ### Dotad kod zwraca note w HTML
    # wartosc parametru href w znaczniku a
    html_attr("href")
  
  
  linki <- vector()
  linki <- c(linki, linki_tmp)
  
  ##### Petla zbierajaca Wyniki z podstron

  
  tryCatch( {   
    for (i in 1:20){
      
  
        ### Klikam wejdz na kolejna strone (strzalka w prawym dolnym rogu)
        webElem <- remDr$findElement(using = 'xpath', value = "//*[@id='n7lv7yjyC35__section-pagination-button-next']")
        webElem$clickElement()
        Sys.sleep(2) # 2 sekundy przerwy, zanim zaladuja sie wyniki
        
        #### Wyswietlam ekran po poczekaniu 2 sekund, zeby zobaczyc, czy strona sie odpalila
        remDr$screenshot(display = TRUE)
        
        #### Czyszcze zmienne i rozpoczynam pobieranie zawartosci strony, który rozparsowuje
        
        KodStrony <- rm()
        
        html_source <- remDr$getPageSource()
        
        KodStrony <- read_html(html_source[[1]])
        
        # szukamy wszystkich linków do stacje w Google
        linki_tmp <- KodStrony %>% 
          html_node(xpath = '//*[@id="pane"]') %>%
          html_nodes(xpath = "//div[@class='widget-pane widget-pane-visible']/div[@tabindex='-1']/div[@class='widget-pane-content-holder']/
                div[@class='section-layout section-layout-root']/div/div/div") %>%
          html_nodes("a") %>%
          # wartosc parametru href w znaczniku a
          html_attr("href")
        
        #### Przerywa petle jesli nowe wyniki sa takie same jak ostatnie
        if(tail(linki, n=1) ==  tail(linki_tmp, n=1)){
          break
        }
        linki <- c(linki, linki_tmp)
        print( paste0( c(Miasto,": Obecnie scrappuje ",i + 1 ," strone wynikow"),collapse = '' ) )
        
      }
    }
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n") })
  
  
  
  linki <- unique(linki)
  
  ##### Komunikat: Scrappujac miejsca zebralem xxx linkow.
  
  cat(Miasto,":udalo sie zebrac informacje o",length(linki), "miejscach\n")
  
  return(linki)
}

##### Zapuszczam petle zbierajaca obiekty np. 'stacje benzynowe' w innych miastach


linki <- PobierzMiejsca(Miasta[1])

for (i in 2:length(Miasta)){
  linki <- c(linki,PobierzMiejsca(Miasta[i]))
  
}

linki <- unique(linki)

# #### Zapisanie linkow gdybym potrzebowal
# write.table(linki, file = "Linki.csv", sep = "|", row.names = FALSE)

DanePunktuNaMapieFinal <- tibble(Nazwa = Nazwa, KategoriaObiektu = KategoriaObiektu, Adres = Adres, GodzinyOtwarcia = GodzinyOtwarcia, Strona_Internetowa = Strona_Internetowa,
                            NumerTelefonu = NumerTelefonu, Rating = Rating, NoOfOppinions = NoOfOppinions,urlInput = LocationGoogleUrl, urlOutput = unlist(remDr$getCurrentUrl()))


####### Czesc 2 - Wchodze na kazdy z obiektów na Google Maps i pobieram szczególowe dane



for ( i in 1:length(linki)){ ## 

  
  LocationGoogleUrl <-  linki[i]
  
  ### Wchodzi na strone obiektu z vectora linki
  
  remDr$navigate(LocationGoogleUrl)
  Sys.sleep(3) # Czekam 3 sekundy zanim zaladuje strone
  
  
  remDr$screenshot(display = TRUE)
  
  
  tryCatch({
    KodStrony <- rm()
    
    html_source <- remDr$getPageSource()
    
    KodStrony <- read_html(html_source[[1]])
    
    GlowneInformacje <- KodStrony %>%
      html_node(xpath = "//div[@id='pane']/div[@class='widget-pane widget-pane-visible']/div[@tabindex='-1']/
              div[@class='widget-pane-content-holder']/div[@class='section-layout section-layout-root']/
              div[@class='section-hero-header-title']/div[1]/div[1]")
    
    Nazwa <- GlowneInformacje %>% html_node(xpath = "//div[@jsinstance='*0']/h1[@class='section-hero-header-title-title gm2-headline-5']") %>% html_text() %>%  trimws()
    Rating <-  GlowneInformacje %>% html_node(xpath = "//div[2]/div/div[1]/div[@role='button']") %>% html_text() %>%  trimws()
    NoOfOppinions <- GlowneInformacje %>% html_node(xpath = "//div[2]/div/div[1]/span[1]") %>% html_text() %>%  trimws()
    KategoriaObiektu <- GlowneInformacje %>% html_node(xpath = "//div[@class='section-hero-header-title-description-container']/
                                        div[@class='section-rating']/div[@class='gm2-body-2']/span") %>% html_text() %>%  trimws()
    Adres <- KodStrony %>%
      html_node(xpath = "//div[@id='pane']/div[@class='widget-pane widget-pane-visible']/div[@tabindex='-1']/
              div[@class='widget-pane-content-holder']/div[@class='section-layout section-layout-root']/div[@class='section-layout']/div/button") %>% html_text() %>%  trimws()

    GodzinyOtwarcia <- KodStrony %>%
      html_node(xpath = "//div[@id='pane']/div[@class='widget-pane widget-pane-visible']/div[@tabindex='-1']/
              div[@class='widget-pane-content-holder']/div[@class='section-layout section-layout-root']/div[@class='section-layout']/
              div[@class='cX2WmPgCkHi__root gm2-body-2 cX2WmPgCkHi__dense']") %>% html_text() %>%  trimws()
    
    
    #### Czasem Strona i numer telefonu sa zle wskazane - trzeba to recznie naprawic w Excelu 
    
    Strona_Internetowa <- KodStrony %>%
      html_node(xpath = "//div[@id='pane']/div[@class='widget-pane widget-pane-visible']/div[@tabindex='-1']/
              div[@class='widget-pane-content-holder']/div[@class='section-layout section-layout-root']/div[@class='section-layout']/div[3]/button") %>% html_text() %>%  trimws()
    
    NumerTelefonu <- KodStrony %>%
      html_node(xpath = "//div[@id='pane']/div[@class='widget-pane widget-pane-visible']/div[@tabindex='-1']/
              div[@class='widget-pane-content-holder']/div[@class='section-layout section-layout-root']/div[@class='section-layout']/div[4]/button") %>% html_text() %>%  trimws()

    DanePunktuNaMapie <- tibble(Nazwa = Nazwa, KategoriaObiektu = KategoriaObiektu, Adres = Adres, GodzinyOtwarcia = GodzinyOtwarcia, Strona_Internetowa = Strona_Internetowa,
                                NumerTelefonu = NumerTelefonu, Rating = Rating, NoOfOppinions = NoOfOppinions,urlInput = LocationGoogleUrl, urlOutput = unlist(remDr$getCurrentUrl()))
    
    Sys.sleep(1)
    if ( exists("DanePunktuNaMapieFinal")){
    DanePunktuNaMapieFinal <- rbind(DanePunktuNaMapieFinal,DanePunktuNaMapie)      
    } else{
    DanePunktuNaMapieFinal <- DanePunktuNaMapie     
    }

    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  print(paste0( c(label_percent()(i/length(linki))," :obecnie scrappuje ",i ," link sposrod ", length(linki)),collapse='') )
  
}


### Formatting data and reordering columns
DanePunktuNaMapieFinal <- unique(DanePunktuNaMapieFinal)
DanePunktuNaMapieFinal$Rating <- gsub(",", ".",DanePunktuNaMapieFinal$Rating)
DanePunktuNaMapieFinal$Rating <- as.numeric(DanePunktuNaMapieFinal$Rating)

# write.table(DanePunktuNaMapieFinal, file = "Stacje Paliw Chorwacja.csv", sep = "|", row.names = FALSE)
write.xlsx(as.data.frame(DanePunktuNaMapieFinal), file = "Stacje Paliw Chorwacja.xlsx", row.names = FALSE)

