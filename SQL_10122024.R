


  
  
  
  library(tidyverse)
  library(RSQLite)
  library(DBI)
  
  # SQL
  
  # Connexion
  con <- dbConnect(RSQLite::SQLite(), dbname="C:/Users/cepe-s4-09/Desktop/ravary_20112024/10-12-2024/star.db")
  
  # Liste des tables
  dbListTables(con)
  
  # Liste des variables
  dbListFields(con, "Etat")
  dbListFields(con, "Topologie")
  
  # premières lignes
  
  test_Etat <- head(dbReadTable(con, "Etat"), 3)
  test_Topo <- head(dbReadTable(con, "Topologie"), 3)
  
  # Fonction dplyr
  etat <- tbl(con, "Etat")

  # Montrer la requête SQL  
  etat %>%
    select(id) %>%
    mutate(id_double = as.double(id)) %>%
    show_query()
  
  tab_query <- dbGetQuery(con, "SELECT * FROM Topologie")
  
  t_Etat <- tbl(con, "Etat")
  t_Topologie <- tbl(con, "Topologie")

  Topologie2 <- t_Topologie %>%
    select(nom, id=id_proche_1)

  # Ajout 
  # id_proche_1 vient de Topologie, id vient d'Etat 
  topologie_db <- Topologie2 %>%
    left_join(t_Topologie, by = "id")
  
  #   

  topologie_db2 <- collect(topologie_db)
  
  topologie <- topologie_db %>%
    select(nom, id_proche_1, latitude, longitude) %>%
    mutate(distance=sqr((latitude.x - latitude.y)^2 - (longitude.x - longitude.y)^2))
  
  dbDisconnect(con)
  
  
  
  # Playlists 
  
  # Connexion
  con <- dbConnect(RSQLite::SQLite(), dbname="C:/Users/cepe-s4-09/Desktop/ravary_20112024/10-12-2024/chinook.db")
  
  # Liste des tables
  dbListTables(con)
  
  # Liste des variables
  dbListFields(con, "Playlist")
  dbListFields(con, "Track")
  dbListFields(con, "PlaylistTrack")
  
  # premières lignes
  
  head_Playlist <- head(dbReadTable(con, "Playlist"), 20)
  head_Track <- head(dbReadTable(con, "Track"), 20)
  head_PlaylistTrack <- head(dbReadTable(con, "PlaylistTrack"), 20)

  
  tbl_PlaylistTrack <-  tbl(con, "PlaylistTrack") 
  tbl_Playlist <-  tbl(con, "Playlist") 
  tbl_Track <-  tbl(con, "Track") 
  
  nb_pistes <- tbl_PlaylistTrack %>%
    group_by(PlaylistId) %>%
    summarise(nombre_pistes = n())

  # Playlists 1 et 8
  
  
  
  
  # JSON et API
  
  # Iris de Fisher
  
  library(jsonlite)

  mon_JSON <- toJSON(list(name="Test", valeur=42))  

  typeof(mon_JSON)  

  test_dans_R <- fromJSON(mon_JSON)  

  iris_JSON <- toJSON(iris, pretty=TRUE)  

  iris_JSON2 <- toJSON(iris, dataframe="columns")  
  
  print(iris_JSON2)
  
  stream_out(iris, con=file("mon_iris.json", open="w"))
  stream_in(file("mon_iris.json", open="r"))
  
  # Stars Wars API
  url_next <- "https://swapi.dev/api/planets/?format=json"
  
  planets <- NULL
  while(!is.null(url_next)) {
    obj <- fromJSON(url_next)
    planets <- rbind(planets, obj[["results"]])
    url_next <- obj[["next"]]
  }
  
  nb_planetes <- summarise(planets, n_planet = n())
  
  stream_out(nb_planetes, con=file("planets.json", open="w"))
  
  # Bonsaïs
  
  
  
  library(rvest)
  
  url_bonsais <- "https://umizenbonsai.com/shop/bonsai/coniferes/"
  
  page_conif <- read_html(url_bonsais)

  bonsai_nodes <- html_nodes(page_conif, "li.entry")
  
  for (k in seq_along(bonsai_nodes)) {
    bonsai_node <- bonsai_nodes[k]
    
    nom <- bonsai_node %>%
      html_node("li.title") %>%
      html_text()
    
    prix <- bonsai_node %>% 
      html_node("span.woocommerce-Price-amount") %>%
      html_text()
  
    lien <- bonsai_node %>%
      html_node("a.woocommerce-LoopProduct-link") %>%
      html_attrs("href")
      
    bonsais <- bonsais %>% bind_rows(
      tibble(nom=nom, prix=prix)
    )
  }
  
  bonsais
  
  # prix <- html_nodes(page_conif, "span.price")
  
  # prix <- html_text(nodes)
  
  # html_attrs(nodes)
  
 
  # nom du bonsaï : a href
  # lien : div class

  
  
  # Cate Blanchett
  
  
  
  url_wikipedia <- "https://fr.wikipedia.org/" 
  
  url_blanchett <- "wiki/Cate_Blanchett" 
  
  data_html <- paste0(url_wikipedia, url_blanchett) %>% 
    read_html() 
  
  film_selector <- "#mw-content-text div ul:nth-of-type(3) li i a" 
  
  film_nodes <- data_html %>% 
    html_nodes(film_selector) %>% 
    html_attrs() 
  
  films <- tibble() 
  
  # next arrête l'étape de la boucle et passe à la boucle suivante
  # break interrompt la boucle
  
  for(k in seq_along(film_nodes)) { 
    
    film_node <- film_nodes[[k]]  
  
    if("class" %in% names(film_node)) next # Absence de page dédiée 
    
    if(film_node["title"] == "Galadriel") next # Mauvais lien 
    
    films <- rbind(films, list(titre=film_node["title"], url=film_node["href"]))
  }
  
  
  url_larmes_d_un_homme <- "https://fr.wikipedia.org/wiki/Les_Larmes_d%27un_homme"
  
  data_html_film <- read_html(url_larmes_d_un_homme)
  
  
  larmes_selector <- html_nodes(data_html_film, "a.external.text")
  
  for (i in seq_along(larmes_selector)){
    larmes_selecto <- larmes_selector[[i]]  
    if(! "IMDb" %in% names(larmes_selecto)) next # Abs
  }

    
  
  