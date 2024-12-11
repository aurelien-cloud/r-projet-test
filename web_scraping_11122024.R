


  
  
  
  library(tidyverse)
  library(RSQLite)
  library(DBI)
  library(jsonlite)
  library(rvest)
  library(stringr)
  
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
  
  
  # url_larmes_d_un_homme <- "https://fr.wikipedia.org/wiki/Les_Larmes_d%27un_homme"
  
  # data_html_film <- read_html(url_larmes_d_un_homme)
  
  
  # larmes_selector <- html_nodes(data_html_film, "a.external.text")
  
  # for (i in seq_along(larmes_selector)){
  #    larmes_selecto <- larmes_selector[[i]]  
  #    if(! "IMDb" %in% names(larmes_selecto)) next # Abs
  # }
  
  

  #  film_imdb_url <- film_imdb_url %>%
  #   str_extract("utl_prefix=(.*)", group=1)
  
  #  film_imdb_id <- film_imdb_url %>%
  #   str_extract("id=(.*)", group=1)
  
  # url_imdb_casting <- paste0("https://www.imdb.com/title/", film_imdb_id, "/fulcredits/")
  
  
  
  
  
  
  
  get_url_imdb <- function(url_film) {
    external_selector <- "a[class='externaltext']"
    
    data_html <- paste0(url_wikipedia, url_film) %>% 
      read_html() 
    
    external_nodes <- data_html %>% 
      html_nodes(external_selector) 
    
    url <- NULL 
    
    for (external_node in external_nodes) { 
      if(external_node %>% html_text() == "IMDb") { 
        external_attrs <- external_node %>% 
          html_attrs() 
        url <- external_attrs["href"] 
        break 
      } 
    }
    
    return(str_extract(url, "url_prefix=(.*)$",group=1))
  }
  
  films <- films %>%
    rowwise() %>%
    mutate(url_imdb=get_url_imdb(url))
  
   head(films, 2)
 
   film_imdb_id <- film_imdb_url %>% str_extract("id=(.*)", group=1)
   url_imdb_casting <- paste0("https://www.imdb.com/title/", film_imdb_id, "/fullcredits/" )
      
    
  
  # MongoDB
   
  library(mongolite)

  m <- mongo("planets")   
  if(m$count() > 0) {m$drop()}
   
  m$import(file("C:/Users/cepe-s4-09/Desktop/ravary_20112024/planets.json"))
  m$count()  

  m$find(query='{"rotation_period": "25"}') %>% head(4)

  m$find() # tout sauf _id
  m$find(fields='{"name": true}') # _id et name
  m$find(fields='{"_id": false, "name": true}')  # seulement name
  
  m$find(query='{"rotation_period": "25"}',
         fields='{"_id": false, 
                  "name": true, 
                  "rotation_period": true, 
                  "orbital_period": true, 
                  "diameter": true}',
         sort='{"diameter": -1}') %>% 
    head(50)

  # problème : le tri ne fonctionne pas comme prévu car nous avons des caractères
  
  df_planets <- stream_in(file("C:/Users/cepe-s4-09/Desktop/ravary_20112024/planets.json"))

  df_planets2 <- df_planets %>%
    mutate(diameter = as.double(diameter)) %>%
    select(-films, -gravity, -residents, -created, -edited) %>%
    mutate()
  
  climate2 = str_split(df_planets2$climate, ", ")
  terrain2 = str_split(df_planets2$terrain, ", ")
  
  df_planets3 <- df_planets2 %>%
    filter(rotation_period == 25) %>%
    arrange(desc(diameter)) %>%
    select(name, rotation_period, orbital_period, diameter) 
  
  head(df_planets3, 10)
  # nom qui commence par T
  
  m$find(query='{"name": {"$regex": "^T"}}',
         fields='{"name": true}') %>%
    head(50)

  # diamètre strictement supérieur à 1000 et où se trouvent des montagnes
  
  df_planets$climate <- strsplit(df_planets$climate,",") 
  df_planets$terrain <- strsplit(df_planets$terrain,",")
  
  m$find(query='{"$and": [
                          {"diameter": {"$gt": "10000"}},
                          {"terrain":{"$in":["mountains"]}}]}',
         fields='{"name": true,
                  "diameter": true}') %>%
    head(50)
  
  m$find(query='{"name":"unknown"}')
  
  m$remove(query='{"name":"unknown"}')
  
  m$find(query='{"name":"unknown"}')

    
  m$aggregate('[{"$group":{"_id":null, "count":{ "$sum":1 }}}]')
  m$count()
  
  m$aggregate('[ 
    { "$match": { 
              "terrain": {"$in": ["glaciers"]} 
              } 
              }, { 
              "$group": { 
              "_id": null, 
              "diameter": {"$avg": "$diameter"}, 
              "population": {"$sum": "$population"} 
              } 
              } 
              ]')
  
  
  
  # pokemon
  
  url_pokemon <- "https://scrapeme.live/product-category/pokemon/"
  
  # récupération des données de la page web
  page_pokemon <- read_html(url_pokemon)
  
  pokemon_nodes <- html_nodes(page_pokemon, "li.product")

  
  # nom, prix et poids
  
  mes_pokemons <- tibble()

  for (k in seq_along(pokemon_nodes)) {
    pokemon_node <- pokemon_nodes[k]
    
    nom <- pokemon_node %>%
      html_node("h2") %>%
      html_text()
    
    prix <- pokemon_node %>%
      html_node("span.woocommerce-Price-amount") %>%
      html_text
    
    mes_pokemons <- mes_pokemons %>% bind_rows(
      tibble(nom=nom,
             prix=prix))
  }

  poids <- pokemon_node %>%
    html_node("li.href") %>%
    html_text
  
   html_table

   external_nodes <- html_nodes(page_pokemon, "a[class='woocommerce-LoopProduct-link woocommerce-loop-product__link']")

   url <- NULL 
   
   for (external_node in external_nodes) { 
     external_attrs <- html_attrs(external_node) 
     url <- external_attrs["href"] 
     break 
   } 
   
   
   get_url_pokemon <- function(url_pokemon)
   { external_selector <- "a[class='externaltext']"
     data_html <- paste0(url_wikipedia, url_film) %>% read_html() 
     external_nodes <- data_html %>% html_nodes(external_selector) 
      url<-NULL 
      for(external_nodein external_nodes){ 
        if(external_node %>% html_text()=="IMDb"){ 
          external_attrs <- external_node %>% html_attrs() 
          url<-external_attrs["href"] 
          break } } 
 return(str_extract(url,"url_prefix=(.*)$",group=1)) 
      }
   
    
   
   # version corrigée
   
   url_pokemon_base <- "https://scrapeme.live/product-category/pokemon/page/"

   page_id <- 1 
   pokemons <- tibble()
   
   
   
   # api pokemon
   
   
   url_pokeapi_base <- "https://pokeapi.co/api/v2/pokemon/"
   
   # l'url est la suivante : https://pokeapi.co/api/v2/pokemon/pikachu
   

   
   for (pokemon_nom in pull(mes_pokemons2, nom)) {
     
     pokemon_data <- fromJSON(paste0(url_pokeapi_base, tolower(pokemon_nom)))
     
     capacites <- pokemon_data$abilities %>% 
       pull(ability) %>% 
       pull(name) %>% 
       unique() 
     
     stats <- pokemon_data$stats %>% 
       unnest(stat) 
     
     hp <- stats %>% 
       filter(name=="hp") %>% 
       pull(base_stat) 
     
     attack<-stats %>%
       filter(name =="attack") %>%
       pull(base_stat) 
     
     defense<-stats %>%
       filter(name=="defense") %>%
       pull(base_stat) 
     
     speed<-stats %>%
       filter(name=="speed") %>%
       pull(base_stat) 
     
     types <- pokemon_data$types %>% 
       pull(type) %>% 
       pull(name) %>% 
       unique() 
     
     api_pokemons <- api_pokemons %>% 
       rbind(tibble(nom=pokemon_nom, 
                    capacites=list(capacites), 
                    hp=hp, 
                    attack=attack, 
                    defense=defense, 
                    speed=speed, 
                    types=list(types)))}

   
   
   pokemons <- pokemons %>% 
     left_join(api_pokemons, by="nom") 
   
   pokemons      
   
   
   mes_pokemons2 <- mes_pokemons %>%
     mutate(nom = tolower(nom))
   
   url <- "https://pokeapi.co/api/v2/pokemon/pikachu"
   
   pokemon <- fromJSON("https://pokeapi.co/api/v2/pokemon/pikachu")
   
   capacites <- pokemon$abilities %>% 
     pull(ability) %>% 
     pull(name) %>% 
     unique()
   
   stats <- pokemon$stats %>% 
     unnest(stat) 
   
   hp <- stats %>% 
     filter(name=="hp") %>% 
     pull(base_stat) 
   
   attack <- stats %>%
     filter(name =="attack") %>%
     pull(base_stat) 
   
   defense <- stats %>%
     filter(name == "defense") %>%
     pull(base_stat) 
   
   speed <- stats %>%
     filter(name == "speed") %>%
     pull(base_stat) 
   
   types <- pokemon$types %>% 
     pull(type) %>% 
     pull(name) %>% 
     unique() 
   
   
   
   
   for (pokemon_nom in pull(mes_pokemons2, nom)) {
     
     pokemon_data <- fromJSON(paste0("https://pokeapi.co/api/v2/pokemon/", pokemon_nom))
     
     capacites <- pokemon_data$abilities %>% 
       pull(ability) %>% 
       pull(name) %>% 
       unique() 
     
     stats <- pokemon_data$stats %>% 
       unnest(stat) 
     
     hp <- stats %>% 
       filter(name=="hp") %>% 
       pull(base_stat) 
     
     attack<-stats %>%
       filter(name =="attack") %>%
       pull(base_stat) 
     
     defense<-stats %>%
       filter(name=="defense") %>%
       pull(base_stat) 
     
     speed<-stats %>%
       filter(name=="speed") %>%
       pull(base_stat) 
     
     types <- pokemon_data$types %>% 
       pull(type) %>% 
       pull(name) %>% 
       unique() 
     
     api_pokemons <- api_pokemons %>% 
       rbind(tibble(nom=pokemon_nom, 
                    capacites=list(capacites), 
                    hp=hp, 
                    attack=attack, 
                    defense=defense, 
                    speed=speed, 
                    types=list(types)))
     }

   api_pokemons <- tibble()
   
   pokemons <- pokemon_data %>% 
     left_join(api_pokemons, by="nom")   
   
   str(api_pokemons)
   
   
   # mongoDB
   
   m <- mongo("pokemons")
   
   if (m$count() > 0) m$drop() # Insertiondesdonnées m$insert(pokemons)
   
   