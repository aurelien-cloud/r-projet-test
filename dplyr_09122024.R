

library(tidyverse)

# iris


iris <- iris

iris2 <- iris %>%
  select(Petal.Width, Species)

iris3 <- iris %>%
  filter(as.factor(Species) %in% c("versicolor", "virginica"))

iris4 <- iris %>%
  group_by(Species) %>%
  summarise(nombre = n())

iris5 <- iris %>%
  group_by(Species) %>%
  summarise(moyenne = mean(Petal.Width))


iris6 <- iris %>%
  mutate(Sum.Width = sum(Petal.Width, Sepal.Width)) 

iris7 <- iris %>%
  mutate(Species = as.character(Species)) %>%
  group_by(Species) %>%
  summarise(moyenne_espece = mean(Sepal.Length),
            variance_espece = var(Sepal.Length))


# Houston flights

library(hflights) 

hflights <- as_tibble(hflights)

hflights2 <- hflights %>%
  select(ends_with("Time"))

hflights3 <- hflights %>%
  select(matches("d.st") | starts_with("Taxi"))

hflights4 <- hflights %>%
  mutate(ActualGroundTime = ActualElapsedTime - AirTime,
         AverageSpeed = Distance / AirTime) %>%
  arrange(desc(AverageSpeed))

hflights5 <- hflights %>%
  filter(Dest == "JFK")

hflights6 <- hflights5 %>%
  summarise(nombre_vols_JFK = n()) %>%
  print()


resume_hflights1 <- hflights %>%
  mutate(nombre_vols = n()) %>%
  slice(1) %>%
  select(nombre_vols)

resume_hflights2 <- hflights %>%
  filter(Cancelled == 1) %>%
  mutate(nombre_vols_annules = n()) %>%
  slice(1) %>%
  select(nombre_vols_annules)

resume_hflights3 <- hflights %>%
  mutate(nombre_distinctes = n_distinct(UniqueCarrier)) %>%
  slice(1) %>%
  select(nombre_distinctes)

resume_hflights <- bind_cols(resume_hflights1, resume_hflights2, resume_hflights3) %>%
  print()


table(hflights$UniqueCarrier)

resume2_1 <- hflights
  
  
# Une clé primaire est toujours unique
  
  
  # données sur le tennis
  
  rg <- read_csv("C:/Users/cepe-s4-09/Desktop/ravary_20112024/rolandgarros2013.csv")
  
  rg2 <- rg %>%
    filter(Round == 7)
  
  print(select(rg2, Player1, Player2))

  # 
  
  rg3 <- rg %>%
    mutate(ACE = ACE.1 + ACE.2) %>%
    summarise(nb_aces = mean(ACE))
  
  
  rg4 <- rg %>%
    mutate(ACE = ACE.1 + ACE.2) %>%
    group_by(Round) %>%
    summarise(nb_aces = mean(ACE))

  
  rg_joueurs <- distinct(bind_rows(select(rg, Joueur=Player1), select(rg, Joueur=Player2)))
  
  rg_bis <- rg %>%
    mutate(joueur_gagnant = if_else(Round == 1, Player1, Player2))

  nb_victoires <- as_tibble(table(rg_bis$joueur_gagnant))
  
  rg_joueurs2 <- rg_joueurs %>%
    full_join(rg_bis)
  
  
  
  
  oa <- read_csv("C:/Users/cepe-s4-09/Desktop/ravary_20112024/openaustralie2013.csv")
  