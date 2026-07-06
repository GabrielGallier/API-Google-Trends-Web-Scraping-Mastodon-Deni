# ==============================================================================
########################### PROJET "DENI" – SCRIPT R ###########################
#
# PARTIE 1 : API Google Trends
# PARTIE 2 : Scraping Mastodon 
#
# Objectif général :
#
# - Suivre la diffusion du terme "déni" et d'expressions associées
#   ("déni de démocratie" et "déni de justice". "Déni de réalité" n'a pas eu
#   d'informations intéressantes et n'a pas été choisi à cause de la pauvreté
#   des résultats).
#
# - "Déni macron" a été demandé pour voir si des occurences précises sont
#   apparues.
#
# - Articuler ces dynamiques à une réflexion sociologique sur l'évolution
#   des sensibilités, des normes et des relations, et en particulier sur le
#   "déni pervers" comme forme de violence morale et stratégie de domination.
#    
# =============================================================================


################################################################################
######################## PARTIE 1 — API GOOGLE TRENDS ##########################
################################################################################

# --- 1. Chargement des packages ---

install.packages("gtrendsR")
install.packages(c("dplyr", "tidyr", "ggplot2", "lubridate", "corrplot", "tibble", 
                   "giscoR", "sf", "wordcloud"))

library(gtrendsR)  # accès à l'API Google Trends
library(dplyr)     
library(tidyr)     
library(ggplot2)   # graphiques
library(lubridate) # dates
library(corrplot)
library(tibble)
library(giscoR)
library(sf)
library(wordcloud)


# --- 2. Paramètres et rappels méthodologiques ---
# Google Trends : points de rigueur méthodologique

# 1) La variable "hits" est un indice normalisé entre 0 et 100.
# 
# - Un indice normalisé est une mesure transformée sur une même échelle 
#   (souvent entre 0 et 1, ici 0 à 100 ou autour d’une moyenne donnée) afin de 
#   rendre comparables entre eux des indicateurs qui n’ont pas les mêmes
#   unités ou les mêmes amplitudes.
#
# - 100 = moment où le terme est le plus recherché dans la période et la zone
#   géographique choisies.
#
# - Les autres valeurs sont proportionnelles à ce maximum.
#
#  -> Ce n'est donc pas un volume absolu de requêtes, leur volume brut diffère.


# 2) Normalisation par temps et par espace :
#
# - Chaque point est divisé par le nombre total de requêtes effectuées dans la 
#   même zone et la même période, puis mis à l'échelle 0–100.
#
# - Cela permet de comparer des termes au sein d'une même requête, mais rend
#   difficile la lecture en termes de "quantité" brute de recherches.

# 3) Comparabilité entre termes :
# 
# - Quand on met plusieurs mots-clés dans une seule requête, ils sont mis sur
#   une échelle commune.
#    
# - Mais si on lance des requêtes séparées pour chaque mot, les échelles ne
#   sont pas directement comparables.
#
#  -> D'où l'intérêt de regrouper "déni", "déni de démocratie", etc. dans une
#     même requête.

# 4) Valeurs "<1" et arrondis :
# 
# - Google Trends renvoie parfois "<1" pour des valeurs très faibles
#   (avant arrondi).
#
# - On peut les recoder en 0.5 puis convertir en numérique, en gardant à
#   l’esprit que cela reste une approximation.

# 5) Séries susceptibles de changer légèrement dans le temps :
#
# - Google peut recalibrer les séries historiques (changement de
#   l’échantillonnage, mise à jour des totalisations).
#
# - Donc une même requête relancée plusieurs mois plus tard peut donner des
#   "hits" légèrement différents.

# 6) Limites :
# 
# - On observe une attention (recherche) et pas l’usage effectif du mot dans
#   les textes.
#
# - On n’identifie pas les contextes d’énonciation, d’où l’intérêt de
#   compléter par le scraping de posts ensuite.


# --- 3. Définition des mots-clés, de la zone et de la période ---

keywords <- c(
  "déni",
  "déni de démocratie",
  "déni de justice",
  "déni macron"
)

# Zone d'étude : France
geo <- "FR"

# Période : de 2004-01-01 à aujourd'hui (on peut ajuster la date de fin)
timewin <- "2004-01-01 2025-12-07"

# N.B. : L'API de Google Trends commence en 2004, pas avant !

# --- 4. Requête à l’API Google Trends ---

gt_raw <- gtrends(
  keyword = keywords,
  geo     = geo,
  time    = timewin,
  gprop   = "web"  # recherche web générale
)

# L’objet gt_raw contient plusieurs data.frames (ou tableaux).
# Celui qui nous intéresse ici : gt_raw$interest_over_time
str(gt_raw$interest_over_time)


# --- 5. Mise en forme des données temporelles ----

gt_time <- gt_raw$interest_over_time %>%
  as_tibble() %>%
  mutate(
    date    = as.Date(date),
    keyword = factor(
      keyword,
      levels = keywords,
      labels = c("déni", "déni de démocratie", "déni de justice", "déni macron")
    ),
    # convertir toutes les valeurs "<x"
    hits_clean = ifelse(grepl("^<", hits), 0.5, hits),
    hits_num = as.numeric(hits_clean)
  )

head(gt_time)

# --- Séries dérivées ---

# 6a. Série normalisée par le maximum propre à chaque mot-clé
gt_rel <- gt_time %>%
  group_by(keyword) %>%
  mutate(
    max_hits  = max(hits_num, na.rm = TRUE),
    hits_rel  = ifelse(max_hits > 0, hits_num / max_hits, NA_real_)
  ) %>%
  ungroup()

# 6b. Moyenne annuelle de l'indice par mot-clé
gt_year_mean <- gt_time %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(keyword, year) %>%
  summarise(
    hits_mean = mean(hits_num, na.rm = TRUE),
    .groups   = "drop"
  )


# --- 7. Graphiques ---

# Evolution temporelle depuis 2004

p_time <- ggplot(gt_time,
                 aes(x = date, y = hits_num, colour = keyword)) +
  geom_line() +
  labs(
    title  = "Recherches Google en France - « déni » et expressions associées (2004-aujourd'hui)",
    x      = "Date",
    y      = "Indice Google Trends (0–100, recodé)",
    colour = "Mot-clé"
  ) +
  theme_minimal()

print(p_time)


# Evolution temporelle lissée depuis 2004

ggplot(gt_time, aes(x = date, y = hits_num, colour = keyword)) +
  geom_point(alpha = 0.2, size = 0.7) +
  geom_smooth(se = FALSE, linewidth = 0.9) +
  labs(
    x = "Date",
    y = "Intensité de recherche",
    colour = "Mot-clé",
    title = "Tendance lissée des recherches Google"
  ) +
  theme_minimal(base_size = 12)


# Évolution différenciée des recherches selon le mot-clé

ggplot(gt_time, aes(x = date, y = hits_num)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ keyword, ncol = 2) +
  labs(
    x = "Année",
    y = "Intensité de recherche (indice 0–100)",
    title = "Évolution des recherches par expression",
    subtitle = "Chaque panneau correspond à un mot-clé"
  ) +
  scale_x_date(
    limits      = c(as.Date("2004-01-01"), as.Date("2026-01-01")),
    breaks      = seq(as.Date("2004-01-01"), as.Date("2026-01-01"), by = "2 years"),
    date_labels = "%Y"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text  = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Graphique pour connaître les pics de chaque expression !

ggplot(gt_rel, aes(date, hits_rel, colour = keyword)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ keyword, ncol = 2) +
  labs(
    x = "Année",
    y = "Intensité relative (0–1)",
    title = "Dynamique relative des recherches par expression",
    subtitle = "Chaque série est normalisée par son maximum propre"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Intensité moyenne annuelle des recherches du "déni"

gt_year_mean_deni <- gt_year_mean %>%
  filter(keyword == "déni")

ggplot(gt_year_mean_deni, aes(x = factor(year), y = hits_mean)) +
  geom_col(fill = "salmon") +
  labs(
    x = "Année",
    y = "Indice moyen",
    title = "Intensité moyenne annuelle des recherches pour « déni »"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Requêtes associées au déni

# 1. Extraction brute 

rel_q_raw <- gt_raw$related_queries %>%
  as_tibble()

# 2. Mise en forme : 

rel_q <- rel_q_raw %>%
  transmute(
    keyword,              
    query = value,        
    kind  = category      
  ) %>%
  filter(!is.na(query), query != "") %>%
  arrange(keyword, kind, query)

# 3. Tableau par mot-clé et catégorie

tab_par_keyword <- rel_q %>% 
  group_by(keyword, kind) %>% 
  summarise(
    `Requêtes associées` = paste(query, collapse = ", "),
    .groups = "drop"
  )


# 4. Tableau des expressions les plus fréquentes 

# Ici : "nombre de mentions" = nombre de fois où une requête apparaît
# comme 'related query' (tous mots-clés et catégories confondus).

tab_pop_queries <- rel_q %>%
  count(query, name = "n_mentions", sort = TRUE)

View(tab_pop_queries)


# Intérêt par région

reg <- gt_raw$interest_by_region %>%
  as_tibble()

glimpse(reg)

# On nettoie les hits et labels

reg_clean <- reg %>%
  mutate(
    keyword = factor(
      keyword,
      levels = keywords,
      labels = c("déni",
                 "déni de démocratie",
                 "déni de justice",
                 "déni macron"
                 )
    ),
    hits_clean = ifelse(grepl("^<", hits), 0.5, hits),
    hits_num   = as.numeric(hits_clean)
  )


# Graphique par région

reg_deni <- reg_clean %>%
  filter(keyword == "déni") %>%
  arrange(hits_num)

ggplot(reg_deni, aes(x = reorder(location, hits_num), y = hits_num)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Région",
    y = "Indice Google Trends (0–100)",
    title = "Intensité des recherches pour « déni » par région en France"
  ) +
  theme_minimal(base_size = 12)


# BONUS : Carte du déni en France de 2004 à aujourd'hui

reg_deni <- reg %>%
  mutate(
    hits_clean = ifelse(grepl("^<", hits), 0.5, hits),
    hits_num   = as.numeric(hits_clean)
  ) %>%
  filter(keyword == keywords[1]) %>%  # keywords[1] = "déni"
  mutate(
    region_fr = case_when(
      location == "Alsace"                     ~ "Alsace",
      location == "Aquitaine"                  ~ "Aquitaine",
      location == "Auvergne"                   ~ "Auvergne",
      location == "Brittany"                   ~ "Bretagne",
      location == "Burgundy"                   ~ "Bourgogne",
      location == "Centre-Val de Loire"        ~ "Centre",
      location == "Champagne-Ardenne"          ~ "Champagne-Ardenne",
      location == "Corsica"                    ~ "Corse",
      location == "Franche-Comté"              ~ "Franche-Comté",
      location == "Île-de-France"              ~ "Île de France",
      location == "Languedoc-Roussillon"       ~ "Languedoc-Roussillon",
      location == "Limousin"                   ~ "Limousin",
      location == "Lorraine"                   ~ "Lorraine",
      location == "Lower Normandy"             ~ "Basse-Normandie",
      location == "Midi-Pyrénées"              ~ "Midi-Pyrénées",
      location == "Nord-Pas-de-Calais"         ~ "Nord - Pas-de-Calais",
      location == "Pays de la Loire"           ~ "Pays de la Loire",
      location == "Picardy"                    ~ "Picardie",
      location == "Poitou-Charentes"           ~ "Poitou-Charentes",
      location == "Provence-Alpes-Côte d'Azur" ~ "Provence-Alpes-Côte d'Azur",
      location == "Rhone-Alpes"                ~ "Rhône-Alpes",
      location == "Upper Normandy"             ~ "Haute-Normandie",
      TRUE                                     ~ location
    )
  ) %>%
  select(region_fr, hits_num)

# 2. Fond de carte : anciennes régions (NUTS 2, année ~2013) 

fr_regions_old <- gisco_get_nuts(
  country    = "FR",
  nuts_level = 2,
  year       = 2013,
  resolution = "3"
) %>%
  # on enlève les DOM-TOM pour rester sur la métropole
  filter(!NUTS_NAME %in% c("Guadeloupe", "Martinique", "Guyane",
                           "La Réunion", "Mayotte"))

# 3. Jointure carte + Google Trends 

map_data_old <- fr_regions_old %>%
  left_join(reg_deni, by = c("NUTS_NAME" = "region_fr"))

# 4. Carte finale 

ggplot(map_data_old) +
  geom_sf(aes(fill = hits_num), colour = "white", linewidth = 0.2) +
  coord_sf(xlim = c(-5.5, 10), ylim = c(41, 51.5), expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(
    title   = "Intensité des recherches Google pour « déni » par région",
    fill    = "Indice\n(0–100)",
    caption = "Source : Google Trends, France (anciennes régions)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )


# Interprétation :
# 
# - Comparer les niveaux moyens et les pics ("max_hits") entre "déni" et les
#   trois expressions.
#
# - Relier les pics à des événements politico-médiatiques (crises, controverses)
#   et à la thèse d’une montée du "déni" comme forme de gestion de la dissonance
#   entre savoirs scientifiques et intérêts dominants.
#   
# - Garder en tête la nature relative et normalisée de "hits_num".

# Aller plus loin ? :
#
# - Plusieurs pistes de recherches sur l'API de Google Trends, sinon d'autres,
#   peuvent nous permettre d'aller plus loin.
# 
#   1. le déni ne se dit pas uniquement “déni”, il se formule sous plusieurs formes.
#
#      - "refus de reconnaître" ; "négation" ; "minimisation" ; "déformation" ;
#        "doute" ; "scepticisme" ; "déni climatique" ; "faker news" ;
#        "désinformation" ; "narratif" ; "vérité alternative", etc.
#
#      - Alors, on pourrait créer un index multi-lexical du déni pour voir si 
#        "déni" est un terme "politique", "moral", "psychologique", etc., et 
#        voir quels registres discursifs montent ou descendent.
#

################################################################################
######################### PARTIE 2 — SCRAPING MASTODON #########################
################################################################################

# Idée : se "débrouiller" avec les ressources publiques, sans passer par l’API 
# officielle.
#
# Stratégie méthodologique :
#
# - On utilise les flux RSS associés aux hashtags Mastodon.
#   Exemple : https://mastodon.social/tags/introduction.rss
# 
# - Cela nous donne un XML structuré (posts récents avec ce hashtag), très
#   facile à parser avec xml2 / rvest.
#    
# - On récupère :
#      
#  - la date de publication ;
#  - le texte (description) du post
# 
# - Ensuite, on cherche à l’intérieur de ces textes nos expressions :
#   "déni", "déni de démocratie", "déni de justice", "déni de réalité".
#
# Remarque :
# 
# - On reste sur de petits volumes (quelques centaines de posts), on ne bombarde
#   pas le serveur.
# 
# - Pour une collecte massive, il faudrait l’API + accord explicit (ou une
#   autre méthode moins conventionnelle).


# -------------------------------------------------------------------------
# 1. Packages nécessaires
# -------------------------------------------------------------------------
# A installer une fois dans la console si besoin :
install.packages(c("xml2", "rvest", "stringr", "purrr", "tibble",
                   "lubridate", "dplyr", "tidyr", "ggplot2", "anytime","tidytext",
                   "stopwords", "ggraph", "igraph"))

library(xml2)
library(rvest)
library(stringr)
library(purrr)
library(tibble)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(anytime)   
library(tidytext)
library(stopwords)
library(ggraph)
library(igraph)


# -------------------------------------------------------------------------
# 2. Fonction pour scraper un flux RSS de hashtag Mastodon
# -------------------------------------------------------------------------
#
# NB : on utilise un hashtag sans accent, par ex. "deni".
#      On filtre ensuite dans le texte pour les formes complètes.

get_mastodon_hashtag_rss <- function(tag,
                                     instance = "mastodon.social",
                                     max_items = Inf) {
  # 1) Construire l'URL du flux RSS du hashtag
  rss_url <- paste0("https://",
                    instance,
                    "/tags/",
                    URLencode(tag, reserved = TRUE),
                    ".rss")
  
  message("Lecture du flux RSS : ", rss_url)
  
  # 2) Lire le XML
  feed <- read_xml(rss_url)
  
  # 3) Récupérer tous les items (chaque item = un post)
  items <- xml_find_all(feed, "//item")
  
  if (!is.infinite(max_items)) {
    items <- items[seq_len(min(length(items), max_items))]
  }
  
  # 4) Extraire titre, date brute, description (texte HTML)
  titles <- items %>%
    xml_find_first("title") %>%
    xml_text()
  
  pubdates_raw <- items %>%
    xml_find_first("pubDate") %>%
    xml_text()
  
  # Parsing robuste des dates avec anytime
  # → si un format ne passe pas, on obtient NA, mais PAS d'erreur.
  pubdates <- suppressWarnings(
    anytime(pubdates_raw, tz = "UTC")
  )
  
  descr_html <- items %>%
    xml_find_first("description") %>%
    xml_text()
  
  # 5) Nettoyer la description du HTML (balises)
  descr_text <- descr_html %>%
    gsub("<.*?>", " ", .) %>%   # enlever balises HTML
    gsub("\\s+", " ", .) %>%    # compacter les espaces
    str_trim()                  # supprimer espaces début/fin
  
  # 6) On renvoie un tibble
  tibble(
    instance      = instance,
    hashtag       = tag,
    title         = titles,
    pubdate_utc   = pubdates,      # POSIXct (ou NA si non parsé)
    content_text  = descr_text,
    pubdate_raw   = pubdates_raw   # on garde la chaîne brute au cas où
  )
}

# -------------------------------------------------------------------------
# 3. Scraping du hashtag "deni" sur mastodon.social
# -------------------------------------------------------------------------

hashtags <- c("deni")

deni_posts <- map_dfr(
  hashtags,
  ~ get_mastodon_hashtag_rss(
    tag      = .x,
    instance = "mastodon.social",
    max_items = 500   # limite raisonnable
  )
)

# Vérifications rapides
nrow(deni_posts)
head(deni_posts$pubdate_raw)
head(deni_posts$pubdate_utc)
head(deni_posts$content_text)

# Si toutes les dates sont NA (rare mais possible),
# on aura des séries temporelles impossibles à tracer.
# Mais le code ne plantera pas.

# -------------------------------------------------------------------------
# 4. Création de variables pour les différentes formes de "déni"
# -------------------------------------------------------------------------

deni_posts <- deni_posts %>%
  mutate(
    # Tout en minuscules pour faciliter la recherche
    content_low = tolower(content_text),
    
    # Indicateur présence "déni" (avec ou sans accent)
    has_deni_simple =
      str_detect(content_low, "déni") |
      str_detect(content_low, "deni"),
    
    # Indicateurs pour les syntagmes
    has_deni_democratie =
      str_detect(content_low, "déni de démocratie") |
      str_detect(content_low, "deni de democratie"),
    
    has_deni_justice =
      str_detect(content_low, "déni de justice") |
      str_detect(content_low, "deni de justice"),
    
    has_deni_macron =
      str_detect(content_low, "déni macron") |
      str_detect(content_low, "deni macron")
  )

# Vérification rapide
deni_posts %>%
  select(pubdate_utc, content_text,
         has_deni_simple,
         has_deni_democratie,
         has_deni_justice,
         has_deni_macron) %>%
  head()

View(deni_posts)


# -------------------------------------------------------------------------
# 5. Variables temporelles (date, mois) – seulement si pubdate_utc non NA
# -------------------------------------------------------------------------

deni_posts <- deni_posts %>%
  mutate(
    pubdate_paris = with_tz(pubdate_utc, tzone = "Europe/Paris"),
    date          = as.Date(pubdate_paris),
    month         = floor_date(date, unit = "month")
  )


# -------------------------------------------------------------------------
# 6. Agrégation mensuelle
# -------------------------------------------------------------------------

deni_month <- deni_posts %>%
  filter(!is.na(month)) %>%   # on enlève les lignes sans date
  group_by(month) %>%
  summarise(
    n_posts        = n(),
    n_deni_simple  = sum(has_deni_simple,    na.rm = TRUE),
    n_deni_demo    = sum(has_deni_democratie, na.rm = TRUE),
    n_deni_justice = sum(has_deni_justice,   na.rm = TRUE),
    n_deni_macron  = sum(has_deni_macron,    na.rm = TRUE),
    .groups        = "drop"
  )

deni_month

View(deni_month)


###############################################################################
# A. Sélection de 10–15 posts illustratifs
###############################################################################

# On crée une variable "expression" qui indique de quel type de déni il s’agit
deni_labeled <- deni_posts %>%
  mutate(
    expression = case_when(
      has_deni_democratie ~ "déni de démocratie",
      has_deni_justice    ~ "déni de justice",
      has_deni_macron     ~ "déni macron",
      has_deni_simple     ~ "déni (toutes formes)",
      TRUE                ~ NA_character_
    )
  ) %>%
  filter(!is.na(expression))

# 10 à 15 posts illustratifs : ici on prend les plus récents
posts_illustratifs <- deni_labeled %>%
  arrange(desc(pubdate_paris)) %>%  # du plus récent au plus ancien
  slice_head(n = 15) %>%            # tu peux mettre 10 si tu préfères
  select(pubdate_paris, expression, content_text)

# Affichage dans la console
posts_illustratifs

View(posts_illustratifs)


# -------------------------------------------------------------------------
# 7. Mise en forme longue pour les graphiques
# -------------------------------------------------------------------------

deni_month_long <- deni_month %>%
  mutate(month = as.Date(month)) %>%
  filter(!is.na(month)) %>%
  select(
    month,
    n_deni_simple,
    n_deni_demo,
    n_deni_justice,
    n_deni_macron
  ) %>%
  pivot_longer(
    cols      = -month,
    names_to  = "expression",
    values_to = "n"
  ) %>%
  mutate(
    expression = recode(
      expression,
      n_deni_simple  = "déni (toutes formes)",
      n_deni_demo    = "« déni de démocratie »",
      n_deni_justice = "« déni de justice »",
      n_deni_macron  = "« déni macron »"
    )
  ) %>%
  filter(!is.na(n))

deni_month_long

View(deni_month_long)

# -------------------------------------------------------------------------
# 8. Graphiques
# -------------------------------------------------------------------------

# Volume global par mois (tous types de déni confondus)

# Graphique optimisé : volume mensuel de posts #deni sur Mastodon

p_masto_month <- deni_month %>%
  filter(!is.na(month)) %>% 
  ggplot(aes(x = month, y = n_posts)) +
  geom_col(fill = "grey40", width = 25) +
  geom_text(aes(label = n_posts),
            vjust = -0.3, size = 4) +
  scale_x_date(
    date_labels = "%b\n%Y",   
    date_breaks = "1 month"
  ) +
  expand_limits(y = max(deni_month$n_posts, na.rm = TRUE) + 1) +
  labs(
    x = "Mois",
    y = "Nombre de posts (#deni)",
    title = "Volume mensuel de posts Mastodon contenant le hashtag #deni",
    subtitle = "Instance mastodon.social – données issues du flux RSS du hashtag"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )

# Affichage
p_masto_month

# Agrégation sur l'ensemble de la période
deni_totaux <- deni_month_long %>%
  group_by(expression) %>%
  summarise(
    n_total = sum(n, na.rm = TRUE),
    .groups = "drop"
  )

# Répartition des posts par type de déni (sur l’ensemble de la période)

ggplot(deni_totaux, aes(x = n_total, y = reorder(expression, n_total))) +
  geom_point(size = 4, colour = "#444444") +
  geom_text(aes(label = n_total), nudge_x = 0.5, size = 4) +
  labs(
    x = "Nombre de posts",
    y = "",
    title = "Présence des différentes formes de « déni » dans les posts Mastodon",
    subtitle = "Seule la forme générique « déni » apparaît dans les données"
  ) +
  theme_minimal(base_size = 12)

# Limite !


# Top 10 des mots autour de "déni (toutes formes)"

# Stopwords FR
sw_fr <- stopwords("fr")

# Stopwords "techniques" / bruit
stop_extra <- c(
  "https", "http", "www", "com", "net", "fr",
  "amp", "nbsp", "t", "co",
  "trailblazers", "blazers", "portlandtrailblazers",
  "pdh", "deni", "c'est", "d'"
)


# Co-occurences déni

mots_freq_clean <- deni_posts %>%
  transmute(text = tolower(content_text)) %>%      
  unnest_tokens(word, text, token = "words") %>%   
  filter(
    !word %in% sw_fr,              
    !word %in% stop_extra,         
    !str_detect(word, "^https?"),  
    !str_detect(word, "[0-9]"),    
    nchar(word) > 2,               
    str_detect(word, "[[:alpha:]]")
  ) %>%
  count(word, sort = TRUE)         

head(mots_freq_clean, 20)

mots_top10_no_deni <- mots_freq_clean %>%
  filter(!word %in% c("dén", "deni", "déni")) %>%
  slice_max(n, n = 10)

ggplot(mots_top10_no_deni, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "#284B63") +
  coord_flip() +
  labs(
    x = "Mot",
    y = "Fréquence",
    title = "Co-occurrences les plus fréquentes autour de « déni »",
    subtitle = "Stopwords + bruit retirés – mastodon.social (juil.–nov. 2025)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 12)
  )


# BONUS : Bigrammes

# 1. Tokenisation en bigrammes
bigrams_deni <- deni_posts %>%
  transmute(text = tolower(content_text)) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  # séparer les deux mots
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  # filtrer les stopwords et le bruit
  filter(
    !word1 %in% sw_fr,
    !word2 %in% sw_fr,
    !word1 %in% stop_extra,
    !word2 %in% stop_extra,
    !str_detect(word1, "^https?"),
    !str_detect(word2, "^https?"),
    !str_detect(word1, "[0-9]"),
    !str_detect(word2, "[0-9]"),
    nchar(word1) > 2,
    nchar(word2) > 2
  ) %>%
  # on garde seulement les bigrammes où l’un des deux mots est déni/deni
  filter(
    word1 %in% c("déni", "deni") | word2 %in% c("déni", "deni")
  ) %>%
  # on reconstitue le bigramme propre
  mutate(bigram = paste(word1, word2)) %>%
  count(bigram, sort = TRUE)

# 2. Top 10 bigrammes
bigrams_top10 <- bigrams_deni %>%
  slice_max(n, n = 10) %>%
  arrange(n)

# 3. Graphique 
ggplot(bigrams_top10,
       aes(x = n, y = reorder(bigram, n))) +
  geom_col(fill = "#284B63") +
  labs(
    title    = "Bigrammes les plus fréquents autour de « déni »",
    subtitle = "Hashtag #deni sur mastodon.social (juil.–nov. 2025)",
    x        = "Fréquence",
    y        = "Bigramme"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.text.y   = element_text(size = 11)
  )

# BONUS : Réseaux de bigrammes

# C'est une alternative qui pourra nous servir dans le cadre ou nous obtenons
# plusieurs données.

# 1) Bigrammes à partir des posts
bigrams_deni <- deni_posts %>%
  transmute(text = tolower(content_text)) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("w1", "w2"), sep = " ") %>%
  filter(!is.na(w1), !is.na(w2))

# 2) On ne garde que les bigrammes contenant déni / deni
bigrams_deni <- bigrams_deni %>%
  filter(w1 %in% c("déni", "deni") | w2 %in% c("déni", "deni"))

# 3) Centre « déni » -> autre mot
bigrams_edges <- bigrams_deni %>%
  mutate(
    source = "déni",
    target = if_else(w1 %in% c("déni", "deni"), w2, w1)
  )

# 4) Nettoyage des cibles
bigrams_edges_clean <- bigrams_edges %>%
  filter(
    !target %in% sw_fr,
    !target %in% stop_extra,
    !str_detect(target, "^https?"),
    !str_detect(target, "[0-9]"),
    nchar(target) > 2,
    str_detect(target, "[[:alpha:]]")
  )

# 5) Comptage des co-occurrences (tu l'as déjà)
edges_counts <- bigrams_edges_clean %>%
  count(source, target, sort = TRUE)

# 6) Seuil pour garder un graphe lisible
edges_top <- edges_counts %>%
  filter(n >= 2)
if (nrow(edges_top) == 0) {
  edges_top <- edges_counts %>% slice_max(n, n = 10)
}

# 7) Graphe igraph
#    On renomme n -> weight pour plus de clarté
g_deni <- graph_from_data_frame(
  edges_top %>% rename(weight = n),
  directed = FALSE
)

# Ajout du degré comme attribut de sommet
V(g_deni)$degree <- igraph::degree(g_deni)

# 8) ggraph
set.seed(123)

ggraph(g_deni, layout = "fr") +
  geom_edge_link(aes(width = weight),
                 colour = "grey60", alpha = 0.7,
                 show.legend = FALSE) +
  geom_node_point(aes(size = degree),
                  colour = "black", fill = "white",
                  shape = 21, stroke = 0.5,
                  show.legend = FALSE) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = 4,
                 family = "sans") +
  scale_edge_width(range = c(0.4, 2.2)) +
  scale_size(range = c(3, 8)) +
  labs(
    title    = "Réseau de co-occurrences autour de « déni »",
    subtitle = "Bigrammes issus des posts avec le hashtag #deni sur mastodon.social (juil.–nov. 2025)",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11)
  )


# --- 10. Remarques / limites du scraping ---
#
# 1) Ce qu’on scrape :
#    - Un flux RSS public d’un hashtag (#deni) sur UNE instance donnée
#      (ici mastodon.social).
#    - On obtient donc un échantillon de posts publics, récents,
#      contenant explicitement ce hashtag.
#
# 2) Ce qu’on ne voit pas :
#    - Les posts sans ce hashtag mais contenant le mot "déni".
#    - Les autres instances du fediverse (on est sur un morceau
#      particulier de l’espace Mastodon).
#    - Les contenus supprimés, les comptes privés, etc.
#
# 3) Positionnement méthodologique :
#    - Google Trends : indicateur macro d’attention (recherches) pour
#      "déni" et syntagmes associés.
#    - Scraping Mastodon : éclairage micro/interactionnel sur des
#      usages concrets, en contexte, de ces expressions.
#    - Ensemble, les deux approches permettent d’articuler les
#      transformations des structures sociales (élites, champs
#      politiques, configuration médiatique) et des structures
#      psychiques (formes de défense, déni pervers, violence morale).
#
# 4) Pour aller plus loin :
#    - Ajouter d’autres hashtags (#denidedemocratie, etc.) si tu
#      en identifies, et combiner les flux.
#    - Faire une petite analyse lexicale (tokens) des posts qui
#      contiennent "déni de démocratie" vs "déni de justice", etc.
#    - Rétro-interpréter ces matériaux à partir du texte théorique
#      que tu as donné (différence entre déni "classique" et
#      déni pervers, rapports de classe, insécurité ontologique).
# ==============================================================================
