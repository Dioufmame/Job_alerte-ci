#!/usr/bin/env Rscript
# ==========================================================================
# Agent d'alerte emploi - Offres DATA en Côte d'Ivoire
# Scrape plusieurs sites d'emploi ivoiriens, filtre sur des mots-clés data,
# et envoie une notification Telegram pour chaque nouvelle offre détectée.
# ==========================================================================

suppressPackageStartupMessages({
  library(rvest)
  library(httr)
  library(jsonlite)
  library(stringr)
  library(dplyr)
})

# --------------------------------------------------------------------------
# 1. CONFIGURATION
# --------------------------------------------------------------------------

# Le token et le chat_id sont lus depuis les variables d'environnement
# (définies comme secrets GitHub Actions, voir README.md)
TELEGRAM_BOT_TOKEN <- Sys.getenv("TELEGRAM_BOT_TOKEN")
TELEGRAM_CHAT_ID   <- Sys.getenv("TELEGRAM_CHAT_ID")

if (TELEGRAM_BOT_TOKEN == "" || TELEGRAM_CHAT_ID == "") {
  stop("TELEGRAM_BOT_TOKEN ou TELEGRAM_CHAT_ID manquant. Vérifie les secrets GitHub Actions ou tes variables d'environnement locales.")
}

# Fichier qui garde la mémoire des offres déjà notifiées (évite les doublons)
SEEN_FILE <- "data/seen_urls.json"

# Mots-clés qui définissent une offre potentiellement "data" pertinente.
# Volontairement large : ce premier filtre sert juste à réduire le nombre
# de pages à ouvrir en détail. C'est le score de correspondance avec le CV
# (plus bas) qui fait le vrai tri, donc on préfère ratisser large ici plutôt
# que de rater une offre "Pilotage de la performance" ou "Analyste contrôle
# de gestion" qui n'a pas le mot "data" dans son titre mais qui en est une.
KEYWORDS <- c(
  "data", "donn[ée]es?", "big data", "data ?scientist", "data ?analyst",
  "data ?miner", "data ?engineer", "\\bsql\\b", "power ?bi",
  "business ?intelligence", "d[ée]cisionnel", "reporting", "dashboard",
  "tableau de bord", "statisticien", "statistiqu", "\\bR\\b", "python",
  "shiny", "\\betl\\b",
  # intitulés liés au pilotage / à la performance / au contrôle de gestion,
  # qui recouvrent souvent des postes data même sans le mot "data"
  "pilotage", "performance", "contr[ôo]le de gestion", "kpi", "indicateurs?",
  "analyste", "business ?analyst", "charg[ée] d'?[ée]tudes?",
  "monitoring", "[ée]valuation", "\\bmis\\b", "chef de projet data",
  "informatique d[ée]cisionnel", "gestion de la performance"
)
KEYWORD_REGEX <- paste(KEYWORDS, collapse = "|")

# --- Profil CV : compétences extraites de ton CV (Mame Abdoulaye Diouf) ---
# Chaque regex est associée à un libellé lisible affiché dans les messages.
# Modifie cette liste si ton profil évolue (nouvel outil, nouvelle
# spécialité, etc.)
CV_SKILLS <- c(
  "\\bR\\b", "python", "\\bsql\\b", "power ?bi", "postgres(ql)?", "oracle",
  "starburst", "talend", "\\bspss\\b", "\\betl\\b", "data ?mining",
  "data ?analyst", "data ?scientist", "data ?miner", "data ?engineer",
  "data ?management", "machine learning", "mod[èe]le pr[ée]dictif",
  "statistiqu", "\\bkpi\\b", "tableau de bord", "dashboard", "reporting",
  "shiny", "business intelligence", "d[ée]cisionnel",
  "gouvernance des donn[ée]es", "qualit[ée] des donn[ée]es", "big data",
  "pipeline", "automatisation", "ing[ée]nierie des donn[ée]es",
  "pilotage", "performance", "contr[ôo]le de gestion", "indicateurs?"
)
SKILL_LABELS <- c(
  "R", "Python", "SQL", "Power BI", "PostgreSQL", "Oracle", "Starburst",
  "Talend", "SPSS", "ETL", "Data Mining", "Data Analyst", "Data Scientist",
  "Data Miner", "Data Engineer", "Data Management", "Machine Learning",
  "Modèle prédictif", "Statistiques", "KPI", "Tableau de bord", "Dashboard",
  "Reporting", "Shiny", "Business Intelligence", "Décisionnel",
  "Gouvernance des données", "Qualité des données", "Big Data", "Pipeline",
  "Automatisation", "Ingénierie des données",
  "Pilotage", "Performance", "Contrôle de gestion", "Indicateurs"
)

# Nombre minimum de compétences en commun entre le CV et l'offre pour
# qu'elle soit jugée pertinente et notifiée (ajustable via variable d'env)
MATCH_THRESHOLD <- as.numeric(Sys.getenv("MATCH_THRESHOLD", "3"))

# Liste des pages à scraper : nom du site + URL de recherche/liste
# NOTE: si un site change sa structure HTML, ajuste juste ces URLs,
# la logique d'extraction ci-dessous est générique (elle regarde tous les
# liens <a> de la page et filtre sur le texte du lien).
SOURCES <- list(
  list(name = "Emploi.ci",  url = "https://www.emploi.ci/recrutement-big-data"),
  list(name = "Emploi.ci",  url = "https://www.emploi.ci/recrutement-data-analyst"),
  list(name = "Novojob",    url = "https://www.novojob.com/cote-d-ivoire/offres-d-emploi/business-data-analyst"),
  list(name = "Novojob",    url = "https://www.novojob.com/cote-d-ivoire/offres-d-emploi/data-scientist")
)

# --------------------------------------------------------------------------
# 2. FONCTIONS
# --------------------------------------------------------------------------

# Rend une URL absolue si elle est relative (ex: "/offre/123" -> "https://site.com/offre/123")
make_absolute_url <- function(href, base_url) {
  if (is.na(href)) return(NA_character_)
  if (str_detect(href, "^https?://")) return(href)
  parsed <- httr::parse_url(base_url)
  root <- paste0(parsed$scheme, "://", parsed$hostname)
  if (str_detect(href, "^/")) return(paste0(root, href))
  paste0(root, "/", href)
}

# Scrape une page: retourne un data.frame(site, titre, url)
scrape_source <- function(source) {
  message(sprintf("Scraping %s ...", source$url))
  offers <- tryCatch({
    page <- read_html(source$url, timeout(20))
    links <- page %>% html_elements("a")

    titles <- links %>% html_text2()
    hrefs  <- links %>% html_attr("href")

    df <- data.frame(
      site  = source$name,
      titre = str_squish(titles),
      href  = hrefs,
      stringsAsFactors = FALSE
    )

    df <- df %>%
      filter(!is.na(href), nchar(titre) > 5) %>%
      filter(str_detect(str_to_lower(titre), str_to_lower(KEYWORD_REGEX))) %>%
      mutate(url = vapply(href, make_absolute_url, character(1), base_url = source$url)) %>%
      select(site, titre, url) %>%
      distinct()

    df
  }, error = function(e) {
    message(sprintf("  Erreur sur %s: %s", source$url, conditionMessage(e)))
    data.frame(site = character(0), titre = character(0), url = character(0))
  })
  offers
}

# Récupère le texte complet de la page de détail d'une offre (pour un
# matching plus fin que le simple titre)
fetch_offer_text <- function(url) {
  tryCatch({
    page <- read_html(url, timeout(20))
    body_node <- page %>% html_element("body")
    if (is.na(body_node)) return("")
    body_node %>% html_text2()
  }, error = function(e) {
    message(sprintf("  Impossible de charger le détail de %s : %s", url, conditionMessage(e)))
    ""
  })
}

# Calcule le score de correspondance entre un texte d'offre et le profil CV
# Retourne une liste avec le score (nombre de compétences en commun) et le
# détail des compétences trouvées
compute_match <- function(text) {
  text_lower <- str_to_lower(text)
  hits <- vapply(CV_SKILLS, function(p) str_detect(text_lower, p), logical(1))
  list(score = sum(hits), matched = SKILL_LABELS[hits])
}

# Charge la liste des URLs déjà notifiées
load_seen <- function() {
  if (!file.exists(SEEN_FILE)) return(character(0))
  tryCatch(unlist(fromJSON(SEEN_FILE)), error = function(e) character(0))
}

# Sauvegarde la liste des URLs notifiées (garde les 1000 dernières pour ne pas grossir indéfiniment)
save_seen <- function(seen_urls) {
  seen_urls <- tail(unique(seen_urls), 1000)
  dir.create(dirname(SEEN_FILE), showWarnings = FALSE, recursive = TRUE)
  write(toJSON(seen_urls, auto_unbox = FALSE), SEEN_FILE)
}

# Envoie un message Telegram
send_telegram <- function(text) {
  api_url <- sprintf("https://api.telegram.org/bot%s/sendMessage", TELEGRAM_BOT_TOKEN)
  res <- POST(
    api_url,
    body = list(
      chat_id = TELEGRAM_CHAT_ID,
      text = text,
      parse_mode = "HTML",
      disable_web_page_preview = FALSE
    ),
    encode = "form"
  )
  if (status_code(res) != 200) {
    message("Erreur envoi Telegram: ", content(res, "text", encoding = "UTF-8"))
  }
  invisible(status_code(res) == 200)
}

# --------------------------------------------------------------------------
# 3. EXECUTION
# --------------------------------------------------------------------------

all_offers <- bind_rows(lapply(SOURCES, scrape_source))

if (nrow(all_offers) == 0) {
  message("Aucune offre trouvée sur ce passage (ou sites indisponibles).")
  quit(save = "no", status = 0)
}

seen_urls <- load_seen()
new_offers <- all_offers %>% filter(!url %in% seen_urls)

message(sprintf("Offres trouvées: %d | Nouvelles: %d", nrow(all_offers), nrow(new_offers)))

if (nrow(new_offers) > 0) {
  n_sent <- 0
  for (i in seq_len(nrow(new_offers))) {
    offer <- new_offers[i, ]

    detail_text <- fetch_offer_text(offer$url)
    full_text   <- paste(offer$titre, detail_text)
    match       <- compute_match(full_text)

    if (match$score >= MATCH_THRESHOLD) {
      matched_str <- paste(match$matched, collapse = ", ")
      msg <- sprintf(
        "🔔 <b>Offre DATA compatible avec ton profil</b>\n\n📌 %s\n🏷️ Source : %s\n✅ Score : %d compétences en commun (%s)\n🔗 %s",
        offer$titre, offer$site, match$score, matched_str, offer$url
      )
      send_telegram(msg)
      n_sent <- n_sent + 1
    } else {
      message(sprintf("Offre écartée (score %d < seuil %d) : %s", match$score, MATCH_THRESHOLD, offer$titre))
    }
    Sys.sleep(1) # évite de spammer les serveurs et l'API Telegram
  }
  message(sprintf("%d offre(s) envoyée(s) sur %d candidate(s).", n_sent, nrow(new_offers)))
  # On marque TOUTES les offres candidates comme vues (matchées ou non),
  # pour ne pas re-vérifier indéfiniment les mêmes annonces à chaque run.
  save_seen(c(seen_urls, new_offers$url))
} else {
  message("Rien de nouveau à vérifier.")
}

message("Terminé.")
