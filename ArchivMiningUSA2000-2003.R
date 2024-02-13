#Projekt: Archiv Scraping Bush Administration.

#Quelle: Renewal in Iraq, https://georgewbush-whitehouse.archives.gov/infocus/iraq/archive.html
#Autor: Tim Broszio

#Modul: Forschungsdesign und Projektdurchführungen in den 'Internationalen Beziehungen'
#Forschungsmodul Teil I/ II
#Dozent: Prof. Dr. Stefan A. Schirm
#Sommersemester 2020/ Wintersemester 2021

#Let's get started!----

getwd()

#Die Working Directory muss individuell ausgewaehlt werden.
# Entscheided selbst welche Ordner euch passend erscheint 
# und waehlt via setwd("Pfad") selbst einen Dateipfad aus

setwd("C://RUB//Master GTG//Data Camp R//Forschungsprojekt Irak Mining Analyse//Archiv Scraping USA//")

# Load Packages muessen schon installiert sein,
# wenn dies nicht der Fall ist muessen saemtliche Pakete aus der PacmanFunktion mit: 
# install.packages("")
# installiert werden
#Setzen der Systemsprache auf English (vorteilhaft bei der Recherche von Fehlermeldungen)

Sys.setenv(language="en")

# Alten Workspace bereinigen:
rm(list = ls())

# Notwendige Pakete laden----

pac <- "pacman" %in% rownames(installed.packages())
if(!pac) install.packages("pacman")
library(pacman)
rm(pac)
p_load("robotstxt", "rvest", "tidyverse", "tidyr", "tibble", "knitr")

# web scrapping erlaubt?----
#Hier wird auf die Terms of Service zugegriffen, ob ein maschineller Zugriff erlaubt ist!

paths_allowed(
  paths = c("https://georgewbush-whitehouse.archives.gov/infocus/iraq/archive.html")
)

################## Scraping Bush Administration Iraq Archive ##############################
#Auswahl der URL 
usa_bush_iraq <- read_html("https://georgewbush-whitehouse.archives.gov/infocus/iraq/archive.html")

#Titel der Seite
usa_bush_iraq %>%
  html_node("h1") %>%
  html_text()


# Aufbau der Seite in html
usa_bush_iraq %>%html_text()

###### Titel und URl der DOkumente ansteuern /auswaehlen #######

#Titel der Dokumente
titles <-  usa_bush_iraq %>%
  html_nodes("#left-col a") %>%
  html_text()

#Urls der Dokumente
urls <- usa_bush_iraq %>%
  html_nodes("#left-col a") %>%
  html_attr("href") 

#Dataframe aus den Titeln und den URLs erstellen!

usa_bush_arch <-  data.frame(Link=urls, Dokumententitel = titles, stringsAsFactors = FALSE)


###### Erstellung von Values: URLS und Titles separat nach Jahren - Dataframes----

# Urls und Title fuer 2001

urls01 <-  urls[527:525]

titles01 <- titles[527:525]


url_title01 <-  data.frame(Links = urls01, Dokumententitel = titles01, stringsAsFactors = FALSE)

# Urls und Title fuer 2002

urls02 <- urls[523:481]

titles02 <-  titles[523:481]

url_title02 <-  data.frame(Links = urls02, Dokumententitel = titles02, stringsAsFactors = FALSE)

# Urls und Title fuer 2003 bis Kriegsende Rede vom 01.05.2001
# hier werden einige Dokument ausgeschlossen, 
# da es sich nicht um Dokumente der Bush Administration handelt

urls03 <- urls[479:474]

titles03 <- titles[479:474]


url_title03 <-  data.frame(Links = urls03, Dokumententitel = titles03, stringsAsFactors = FALSE)

urls031 <- urls[472:468]

titles031 <- titles[472:468]


url_title031 <- data.frame(Links = urls031, Dokumententitel = titles031, stringsAsFactors = FALSE)


urls032 <- urls[466:451]

titles032 <- titles[466:451]


url_title032 <- data.frame(Links = urls032, Dokumententitel = titles032, stringsAsFactors = FALSE)


urls033 <- urls[449:441]

titles033 <- titles[449:441]


url_title033 <- data.frame(Links = urls033, Dokumententitel = titles033, stringsAsFactors = FALSE)


#Nun kann entweder fuer jeden Df 0102 oder 03 die content url ausgewaehlt und angehaengt werden 
#Getrennte Version, je df aus 01 02 03
#Hier muss darauf geachtet werden, dass nicht alle Beitraege des Archivs direkt von 
#der Bush Administration stammen und somit rausgenommen werden muessen. 

#Arbeit bleibt uns also nicht erspart ;)



#Stamm Url anheften - komplette Url erzeugen----
#die Dokumente liegen in sog. "nested links vor"


#01

content_urls01 = urls01 %>%
  paste("https://georgewbush-whitehouse.archives.gov", ., sep="")


#anheften an df01

url_title01["Content URL"] <- content_urls01

#02
content_urls02 = urls02 %>%
  paste("https://georgewbush-whitehouse.archives.gov", ., sep="")
 

url_title02["Content URL"] <- content_urls02

#03

content_urls03 = urls03 %>%
  paste("https://georgewbush-whitehouse.archives.gov", ., sep="")
 

url_title03["Content URL"] <- content_urls03


content_urls031 = urls031 %>%
  paste("https://georgewbush-whitehouse.archives.gov", ., sep="")
 

url_title031["Content URL"] <- content_urls031


content_urls032 = urls032 %>%
  paste("https://georgewbush-whitehouse.archives.gov", ., sep="")
 

url_title032["Content URL"] <- content_urls032


content_urls033 = urls033 %>%
  paste("https://georgewbush-whitehouse.archives.gov", ., sep="")
 

url_title033["Content URL"] <- content_urls033



# Theoretisch kann jetzt wieder gemerged werden oder
# die Get Text Funktion wird einzeln durchgefuehrt und anschliessend manuell an die Dfs geheftet

#Separate Durchfuehrung

#Erstelle Funktion get_documents 01-03, wobei jeweils vorgegeben wird, das url angefahren wird, 
#dort der entsprechende node und der Text ausgewählt wird und als ein String ausgegeben wird
# wir erhalten also für jede URL den Text der Dokumente!

get_documents01 = function(url) {
  page = read_html(url)
  documents01 = page %>% html_nodes("p+ p") %>% html_text() %>% paste(collapse =",")
  return(documents01)
}

alldocuments01 = sapply(content_urls01, FUN = get_documents01)

url_title01["Text"] <- alldocuments01

#02

get_documents02 = function(url) {
  page = read_html(url)
  documents02 = page %>% html_nodes("p+ p") %>% html_text() %>% paste(collapse =",")
  return(documents02)
}

alldocuments02 = sapply(content_urls02, FUN = get_documents02)

url_title02["Text"] <- alldocuments02

#03

get_documents03 = function(url) {
  page = read_html(url)
  documents03 = page %>% html_nodes("p+ p") %>% html_text() %>% paste(collapse =",")
  return(documents03)
}

alldocuments03 = sapply(content_urls03, FUN = get_documents03)

url_title03["Text"] <- alldocuments03


get_documents031 = function(url) {
  page = read_html(url)
  documents031 = page %>% html_nodes("p+ p") %>% html_text() %>% paste(collapse =",")
  return(documents031)
}

alldocuments031 = sapply(content_urls031, FUN = get_documents031)

url_title031["Text"] <- alldocuments031

get_documents032 = function(url) {
  page = read_html(url)
  documents032 = page %>% html_nodes("p+ p") %>% html_text() %>% paste(collapse =",")
  return(documents032)
}

alldocuments032 = sapply(content_urls032, FUN = get_documents032)

url_title032["Text"] <- alldocuments032

get_documents033 = function(url) {
  page = read_html(url)
  documents033 = page %>% html_nodes("p+ p") %>% html_text() %>% paste(collapse =",")
  return(documents033)
}

alldocuments033 = sapply(content_urls033, FUN = get_documents033)

url_title033["Text"] <- alldocuments033



############### mergen der separaten DFs----

#Dataframe aus dem kompletten URLs, Titeln und Texten erzeugen!

#daher rbind "stack the rows"


url_title_content0102 <- rbind(url_title01, url_title02)
url_title_content010203 <- rbind(url_title_content0102, url_title03)
url_title_content010203 <- rbind(url_title_content010203, url_title031)
url_title_content010203 <- rbind(url_title_content010203, url_title032)
url_title_content010203 <- rbind(url_title_content010203, url_title033)

# Entfernung der Spalte 'Links' & Content URL
url_title_content010203 <- subset(url_title_content010203, select = -Links)

title_content010203 <- url_title_content010203[ , -2]


# Speichern des DF zur Bearbeitung mit weiteren Scripts

# Auf Working Directory achten, die Datei landet in dem Anfangs ausgewaehlten Ordner:

# Befehl setwd(). getwd() zeigt aktuelle Directory an!
# In der Working Directory werden alle Outputs gespeichert.

save(title_content010203, file = "Bush Dokuments bis Kriegsende2.Rda")

# Dieser Dataframe besteht aus allen relevanten Dokumenten und Dokumententiteln der Bush Administration
# und bildet dieGrundlage der folgenden Analyse (Script 'Quantitative Analyse Textlicher Daten')


