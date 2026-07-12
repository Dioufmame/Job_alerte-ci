#!/usr/bin/env Rscript
# ==========================================================================
# Agent d'alerte emploi - Offres DATA en Côte d'Ivoire
# Scrape plusieurs sites d'emploi ivoiriens, filtre sur des mots-clés data,
# et envoie une notification Telegram pour chaque nouvelle offre détectée.
# ==========================================================================

suppressPackageStartupMessages({
  library(rvest)
  library(xml2)
  library(httr)
  library(jsonlite)
  library(stringr)
  library(dplyr)
})

# User-Agent "navigateur" pour éviter d'être bloqué par les protections
# anti-bot de certains sites (Emploi.ci notamment refuse les requêtes sans
# en-tête de navigateur reconnu)
BROWSER_UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"

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
# NOTE: si le secret GitHub MATCH_THRESHOLD existe mais est vide, la
# variable d'environnement est quand même définie (à ""), donc la valeur
# par défaut de Sys.getenv() n'est PAS utilisée. On sécurise avec un
# contrôle explicite pour éviter un MATCH_THRESHOLD = NA qui ferait
# planter toute la boucle de scoring plus bas.
MATCH_THRESHOLD <- suppressWarnings(as.numeric(Sys.getenv("MATCH_THRESHOLD", "3")))
if (is.na(MATCH_THRESHOLD)) MATCH_THRESHOLD <- 3

# Liste des pages à scraper : nom du site + URL de recherche/liste
# NOTE: si un site change sa structure HTML, ajuste juste ces URLs,
# la logique d'extraction ci-dessous est générique (elle regarde tous les
# liens <a> de la page et filtre sur le texte du lien).
SOURCES <- list(
  list(name = "Emploi.ci",  url = "https://www.emploi.ci/recrutement-big-data"),
  list(name = "Emploi.ci",  url = "https://www.emploi.ci/recrutement-data-analyst"),
  list(name = "Novojob",    url = "https://www.novojob.com/cote-d-ivoire/offres-d-emploi/business-data-analyst"),
  list(name = "Novojob",    url = "https://www.novojob.com/cote-d-ivoire/offres-d-emploi/data-scientist"),

  # --- Nouveaux sites d'emploi ---
  list(name = "EmploiRapide", url = "https://emploirapide.net/search?q=data"),
  list(name = "Educarriere",  url = "https://emploi.educarriere.ci/nos-offres"),

  # --- Secteur bancaire (agrégateurs, couvrent la plupart des banques) ---
  list(name = "Emploi.ci - Banque", url = "https://www.emploi.ci/emploi-banque"),
  list(name = "Novojob - Banque",   url = "https://www.novojob.com/cote-d-ivoire/offres-d-emploi/offres-par-secteur/125-banque-assurance-finance"),

  # --- Sites carrières directs de quelques banques (pages statiques) ---
  list(name = "SIB",                url = "https://sib.ci/recrutement/"),
  list(name = "NSIA Banque",        url = "https://www.nsiabanque.ci/notre-marque-employeur/nos-offres-demploi/"),
  list(name = "BNI",                url = "https://www.bni.ci/recrutement/"),
  list(name = "Afriland First Bank", url = "https://afrilandfirstbankci.com/careers/"),
  list(name = "BGFIBank",           url = "https://groupebgfibank.com/emploi-et-carriere/trouver-mon-futur-emploi/"),
  list(name = "BICICI",             url = "https://www.bicici.com/nous-connaitre/recrutement/offres-demploi/"),
  list(name = "BOA Côte d'Ivoire",  url = "https://boacoteivoire.com/institutionnels/travailler-chez-boa/"),
  list(name = "Orabank",            url = "https://www.orabank.net/fr/groupe/carriere/candidature-spontanee"),

  # --- Grandes entreprises (pages "entreprise" Novojob, plus fiables que
  # de deviner le site propre de chaque société) ---
  list(name = "Orange CI (Novojob)",   url = "https://www.novojob.com/entreprise/orange-ci/offres-d-emploi"),
  list(name = "Unilever CI (Novojob)", url = "https://www.novojob.com/cote-d-ivoire/entreprise/unilever-4/offres-d-emploi"),
  list(name = "Nestlé CI (Novojob)",   url = "https://www.novojob.com/cote-d-ivoire/entreprise/nestle-2/offres-d-emploi"),
  list(name = "SOLIBRA (Novojob)",     url = "https://www.novojob.com/cote-d-ivoire/entreprise/solibra-societe-de-limonaderies-et-de-brasseries-d-afrique/offres-d-emploi"),
  list(name = "BICICI (Novojob)",      url = "https://www.novojob.com/cote-d-ivoire/entreprise/bicici/offres-d-emploi"),
  list(name = "SODECI",                url = "https://sodeci.mycv.tech/"),

  # --- Page générale (toutes offres, toutes entreprises confondues) ---
  # Filtrée ensuite par KEYWORD_REGEX puis par le score CV, donc pas de
  # risque de recevoir des offres hors-sujet.
  # NOTE: Emploi.ci bloque les requêtes automatisées (HTTP 403), probablement
  # une protection anti-bot qui bloque les IP de serveurs cloud comme
  # GitHub Actions. On garde la source de coeur de metier ci-dessus au cas
  # où le blocage soit levé, et on s'appuie sur Novojob pour la couverture
  # générale en attendant.
  list(name = "Emploi.ci - Toutes offres", url = "https://www.emploi.ci/recherche-jobs-cote-ivoire"),
  list(name = "Novojob - Toutes offres",   url = "https://www.novojob.com/cote-d-ivoire/offres-d-emploi")
)

# --------------------------------------------------------------------------
# 2. FONCTIONS
# --------------------------------------------------------------------------

# Lit une page HTML à partir d'une réponse httr, avec repli automatique si
# l'encodage n'est pas de l'UTF-8 valide (certains sites, comme bni.ci,
# utilisent encore du Latin-1 / Windows-1252 sans le déclarer correctement)
safe_read_html <- function(resp) {
  raw <- httr::content(resp, as = "raw")
  tryCatch(
    xml2::read_html(raw),
    error = function(e) {
      txt <- httr::content(resp, as = "text", encoding = "ISO-8859-1")
      xml2::read_html(txt)
    }
  )
}

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
    resp <- httr::GET(
      source$url,
      httr::add_headers(
        "User-Agent" = BROWSER_UA,
        "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language" = "fr-FR,fr;q=0.9,en;q=0.8"
      ),
      httr::timeout(40)
    )
    if (httr::status_code(resp) >= 400) {
      stop(sprintf("HTTP %s", httr::status_code(resp)))
    }

    page  <- safe_read_html(resp)
    links <- page %>% html_elements("a")

    if (length(links) == 0) {
      return(data.frame(site = character(0), titre = character(0), url = character(0)))
    }

    # Extraction noeud par noeud (plus lent mais robuste : évite les erreurs
    # de type "STRING_ELT() ne peut être appliqué qu'à un vecteur de
    # caractères" qui surviennent quand html_text2()/html_attr() renvoient
    # un résultat inattendu sur certains liens malformés)
    titles <- vapply(links, function(nd) {
      t <- tryCatch(html_text2(nd), error = function(e) NA_character_)
      if (is.null(t) || length(t) == 0) NA_character_ else as.character(t)[1]
    }, character(1))

    hrefs <- vapply(links, function(nd) {
      h <- tryCatch(html_attr(nd, "href"), error = function(e) NA_character_)
      if (is.null(h) || length(h) == 0) NA_character_ else as.character(h)[1]
    }, character(1))

    df <- data.frame(
      site  = source$name,
      titre = str_squish(titles),
      href  = hrefs,
      stringsAsFactors = FALSE
    )

    df <- df %>%
      filter(!is.na(href), !is.na(titre), nchar(titre) > 5) %>%
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
    resp <- httr::GET(
      url,
      httr::add_headers(
        "User-Agent" = BROWSER_UA,
        "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language" = "fr-FR,fr;q=0.9,en;q=0.8"
      ),
      httr::timeout(40)
    )
    if (httr::status_code(resp) >= 400) return("")
    page <- safe_read_html(resp)
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
  if (is.null(text) || length(text) == 0 || is.na(text)) text <- ""
  text_lower <- str_to_lower(text)
  # isTRUE() protège contre les NA (ex: texte mal encodé) qui feraient
  # planter le if() plus bas avec "valeur manquante là où VRAI/FAUX
  # était nécessaire"
  hits <- vapply(CV_SKILLS, function(p) isTRUE(str_detect(text_lower, p)), logical(1))
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

    # Envoi automatique si le mot "data" ou "données" apparaît explicitement
    # dans le titre ou la description de l'offre, même si le score de
    # correspondance CV est en dessous du seuil habituel.
    has_data_keyword <- isTRUE(str_detect(str_to_lower(full_text), "\\bdata\\b|donn[ée]es?"))

    if (match$score >= MATCH_THRESHOLD || has_data_keyword) {
      matched_str <- paste(match$matched, collapse = ", ")
      if (has_data_keyword && match$score < MATCH_THRESHOLD) {
        raison <- sprintf("🔎 Mot-clé \"data\"/\"données\" détecté dans l'annonce (score CV : %d/%.0f, sous le seuil mais envoyé quand même)", match$score, MATCH_THRESHOLD)
      } else {
        raison <- sprintf("✅ Score : %d compétences en commun (%s)", match$score, matched_str)
      }
      msg <- sprintf(
        "🔔 <b>Offre DATA compatible avec ton profil</b>\n\n📌 %s\n🏷️ Source : %s\n%s\n🔗 %s",
        offer$titre, offer$site, raison, offer$url
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
