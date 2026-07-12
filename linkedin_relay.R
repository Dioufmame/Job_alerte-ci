#!/usr/bin/env Rscript
# ==========================================================================
# Relais LinkedIn -> Telegram
# Se connecte à la boîte Gmail par IMAP, cherche les emails d'alerte emploi
# de LinkedIn non lus, extrait les offres (titre + lien), et les envoie sur
# Telegram. Ne fait AUCUN scraping de LinkedIn : on lit uniquement les
# emails que LinkedIn envoie légitimement suite à une alerte créée par
# l'utilisateur sur linkedin.com.
# ==========================================================================

suppressPackageStartupMessages({
  library(mRpostman)
  library(httr)
  library(stringr)
  library(xml2)
  library(rvest)
  library(jsonlite)
  library(dplyr)
})

# --------------------------------------------------------------------------
# 1. CONFIGURATION
# --------------------------------------------------------------------------

GMAIL_ADDRESS      <- Sys.getenv("GMAIL_ADDRESS")
GMAIL_APP_PASSWORD <- Sys.getenv("GMAIL_APP_PASSWORD")
TELEGRAM_BOT_TOKEN <- Sys.getenv("TELEGRAM_BOT_TOKEN")
TELEGRAM_CHAT_ID   <- Sys.getenv("TELEGRAM_CHAT_ID")

if (GMAIL_ADDRESS == "" || GMAIL_APP_PASSWORD == "" ||
    TELEGRAM_BOT_TOKEN == "" || TELEGRAM_CHAT_ID == "") {
  stop("Une variable d'environnement est manquante (GMAIL_ADDRESS, GMAIL_APP_PASSWORD, TELEGRAM_BOT_TOKEN, TELEGRAM_CHAT_ID). Vérifie les secrets GitHub Actions.")
}

# Fichier qui garde la mémoire des emails déjà relayés (sécurité en plus du
# flag IMAP "\Seen", au cas où le marquage échouerait sur un run)
SEEN_FILE <- "data/seen_linkedin_emails.json"

# Fichier qui garde la mémoire des OFFRES déjà envoyées sur Telegram (par
# lien). Nécessaire en plus de SEEN_FILE, car LinkedIn republie parfois la
# même offre dans plusieurs emails différents (jours différents) : sans
# cette mémoire, la même offre pourrait être envoyée plusieurs fois.
SEEN_JOBS_FILE <- "data/seen_linkedin_job_urls.json"

# --------------------------------------------------------------------------
# 2. FONCTIONS
# --------------------------------------------------------------------------

load_seen_jobs <- function() {
  if (!file.exists(SEEN_JOBS_FILE)) return(character(0))
  tryCatch(unlist(fromJSON(SEEN_JOBS_FILE)), error = function(e) character(0))
}

save_seen_jobs <- function(seen_job_urls) {
  seen_job_urls <- tail(unique(seen_job_urls), 1000)
  dir.create(dirname(SEEN_JOBS_FILE), showWarnings = FALSE, recursive = TRUE)
  write(toJSON(seen_job_urls, auto_unbox = FALSE), SEEN_JOBS_FILE)
}

load_seen <- function() {
  if (!file.exists(SEEN_FILE)) return(character(0))
  tryCatch(unlist(fromJSON(SEEN_FILE)), error = function(e) character(0))
}

save_seen <- function(seen_ids) {
  seen_ids <- tail(unique(seen_ids), 500)
  dir.create(dirname(SEEN_FILE), showWarnings = FALSE, recursive = TRUE)
  write(toJSON(seen_ids, auto_unbox = FALSE), SEEN_FILE)
}

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

# Décode un fragment encodé en quoted-printable (format email courant pour
# le HTML), ex: "D=C3=A9velopper" -> "Développer"
# NOTE: les caractères accentués UTF-8 sont codés sur PLUSIEURS octets
# consécutifs (ex: "é" = les 2 octets =C3=A9). Il faut les regrouper avant
# de les convertir, sinon la conversion octet-par-octet produit des
# séquences invalides et fait planter gsub().
decode_quoted_printable <- function(txt) {
  txt <- gsub("=\r?\n", "", txt)  # sauts de ligne "mous" (soft line breaks)
  pattern <- "(=[0-9A-Fa-f]{2})+"  # un ou plusieurs octets consécutifs
  m <- gregexpr(pattern, txt, perl = TRUE)
  regmatches(txt, m) <- lapply(regmatches(txt, m), function(runs) {
    vapply(runs, function(run) {
      hex_codes <- regmatches(run, gregexpr("[0-9A-Fa-f]{2}", run, perl = TRUE))[[1]]
      bytes <- as.raw(strtoi(hex_codes, base = 16L))
      tryCatch({
        s <- rawToChar(bytes)
        Encoding(s) <- "UTF-8"
        s
      }, error = function(e) "")
    }, character(1))
  })
  txt
}

# Extrait la partie HTML d'un email brut multipart et la décode
extract_html_part <- function(raw_body) {
  # Cherche le "boundary" du multipart
  boundary_match <- str_match(raw_body, 'boundary="?([^"\r\n;]+)"?')
  if (is.na(boundary_match[1, 2])) {
    # Pas de multipart détecté : on suppose que le corps entier est le HTML
    return(raw_body)
  }
  boundary <- boundary_match[1, 2]
  parts <- str_split(raw_body, fixed(paste0("--", boundary)))[[1]]

  html_part <- NULL
  for (part in parts) {
    if (str_detect(part, regex("Content-Type:\\s*text/html", ignore_case = TRUE))) {
      html_part <- part
      break
    }
  }
  if (is.null(html_part)) return(NA_character_)

  # Sépare les en-têtes MIME du contenu (séparés par une ligne vide)
  split_pos <- str_locate(html_part, "\r?\n\r?\n")
  if (is.na(split_pos[1, 1])) return(NA_character_)
  headers <- substr(html_part, 1, split_pos[1, 1])
  content <- substr(html_part, split_pos[1, 2] + 1, nchar(html_part))

  if (str_detect(headers, regex("quoted-printable", ignore_case = TRUE))) {
    content <- decode_quoted_printable(content)
  } else if (str_detect(headers, regex("base64", ignore_case = TRUE))) {
    content <- tryCatch(
      rawToChar(base64enc::base64decode(str_remove_all(content, "[\r\n]"))),
      error = function(e) content
    )
  }
  content
}

# Villes/mentions qui indiquent une offre basée en Côte d'Ivoire
CI_LOCATION_REGEX <- "c[oô]te d.?ivoire|abidjan|bouak[ée]|yamoussoukro|san.?pedro|korhogo|daloa|man\\b|ivory coast"

# Nombre maximum de jours d'ancienneté toléré pour une offre (au-delà, on
# l'ignore même si l'email lui-même est récent)
MAX_OFFER_AGE_DAYS <- 5

# Extrait l'ancienneté d'une offre à partir du texte LinkedIn du type
# "il y a 3 jours" / "il y a 2 semaines" / "il y a 1 mois" (FR) ou
# "3 days ago" / "2 weeks ago" / "1 month ago" (EN, au cas où le compte
# LinkedIn serait en anglais). Retourne NA si aucun motif n'est trouvé
# (dans ce cas, l'offre est conservée par prudence, voir README).
extract_offer_age_days <- function(context) {
  ctx <- str_to_lower(context)

  if (str_detect(ctx, "il y a moins d|less than a|just now|à l'instant")) return(0)

  m <- str_match(ctx, "il y a\\s+(\\d+)\\s*(minute|min|heure|h\\b|jour|j\\b|semaine|sem|mois)")
  if (is.na(m[1, 1])) {
    m <- str_match(ctx, "(\\d+)\\s*(minute|hour|h\\b|day|d\\b|week|wk|month|mo\\b)s?\\s+ago")
  }
  if (is.na(m[1, 1])) return(NA_real_)

  n    <- as.numeric(m[1, 2])
  unit <- m[1, 3]
  switch(unit,
    "minute" = n / 1440, "min" = n / 1440,
    "heure" = n / 24, "hour" = n / 24, "h" = n / 24,
    "jour" = n, "j" = n, "day" = n, "d" = n,
    "semaine" = n * 7, "sem" = n * 7, "week" = n * 7, "wk" = n * 7,
    "mois" = n * 30, "month" = n * 30, "mo" = n * 30,
    NA_real_
  )
}

# Extrait les offres (titre + lien) d'un HTML d'email LinkedIn, en ne
# gardant que celles situées en Côte d'Ivoire ET publiées il y a moins de
# MAX_OFFER_AGE_DAYS jours (recherche dans le texte qui entoure le lien de
# l'offre, puisque LinkedIn affiche généralement la localisation et
# l'ancienneté juste à côté du titre du poste dans ses emails).
extract_linkedin_jobs <- function(html_content) {
  tryCatch({
    page  <- xml2::read_html(html_content)
    links <- page %>% html_elements("a")

    hrefs  <- links %>% html_attr("href")
    titles <- links %>% html_text2()

    # Contexte élargi : on remonte jusqu'à 3 niveaux d'ancêtres HTML autour
    # de chaque lien pour y chercher la localisation et l'ancienneté
    context_texts <- vapply(links, function(nd) {
      ctx <- nd
      for (i in 1:3) {
        parent <- tryCatch(xml2::xml_parent(ctx), error = function(e) NULL)
        if (is.null(parent) || length(parent) == 0) break
        ctx <- parent
      }
      tryCatch(html_text2(ctx), error = function(e) "")
    }, character(1))

    df <- data.frame(
      titre    = str_squish(titles),
      href     = hrefs,
      contexte = context_texts,
      stringsAsFactors = FALSE
    )

    df$age_jours <- vapply(df$contexte, extract_offer_age_days, numeric(1))

    # Info seulement (ne bloque plus l'envoi) : le texte "il y a X jours"
    # n'est pas fiable à 100% dans les emails LinkedIn selon leur format
    # exact. Le vrai filtre d'ancienneté se fait maintenant sur la date de
    # réception de l'email lui-même (voir plus bas dans le script), qui est
    # une donnée structurée fiable plutôt qu'un texte à deviner.
    for (i in seq_len(nrow(df))) {
      if (!is.na(df$age_jours[i])) {
        message(sprintf("  [INFO] \"%s\" — ancienneté détectée dans le texte : %.1f jour(s)", df$titre[i], df$age_jours[i]))
      }
    }

    df <- df %>%
      filter(!is.na(href), str_detect(href, "linkedin\\.com/.*jobs/(view|comm)")) %>%
      filter(nchar(titre) > 3) %>%
      distinct(href, .keep_all = TRUE) %>%
      filter(str_detect(str_to_lower(contexte), regex(CI_LOCATION_REGEX, ignore_case = TRUE)))

    df %>% select(titre, href)
  }, error = function(e) {
    data.frame(titre = character(0), href = character(0))
  })
}

# --------------------------------------------------------------------------
# 3. EXECUTION
# --------------------------------------------------------------------------

con <- configure_imap(
  url      = "imaps://imap.gmail.com",
  username = GMAIL_ADDRESS,
  password = GMAIL_APP_PASSWORD
)

con$select_folder(name = "INBOX")

# Date à partir de laquelle on considère les emails (5 jours en arrière).
# On construit la date au format attendu par IMAP ("JJ-Mmm-AAAA", avec
# l'abréviation du mois toujours en anglais, indépendamment de la langue du
# système), pour éviter tout souci de locale sur le serveur GitHub Actions.
five_days_ago <- Sys.Date() - 5
MONTH_ABBR_EN <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
date_since <- sprintf(
  "%02d-%s-%d",
  as.integer(format(five_days_ago, "%d")),
  MONTH_ABBR_EN[as.integer(format(five_days_ago, "%m"))],
  as.integer(format(five_days_ago, "%Y"))
)

# Cherche les emails non lus, envoyés spécifiquement par le système
# d'alertes emploi de LinkedIn (et non toutes ses notifications), reçus au
# cours des 5 derniers jours seulement (les offres plus anciennes ne sont
# plus pertinentes)
ids <- tryCatch(
  con$search(
    AND(
      OR(
        string(expr = "jobalerts-noreply@linkedin.com", where = "FROM"),
        string(expr = "jobs-noreply@linkedin.com", where = "FROM")
      ),
      flag(name = "UNSEEN"),
      since(date_char = date_since)
    )
  ),
  error = function(e) {
    message("Erreur lors de la recherche IMAP: ", conditionMessage(e))
    integer(0)
  }
)

if (length(ids) == 0) {
  message("Aucun nouvel email LinkedIn non lu.")
  quit(save = "no", status = 0)
}

# Limite de sécurité : on ne traite qu'un nombre raisonnable d'emails par
# passage, pour ne pas noyer Telegram de messages d'un coup si beaucoup
# d'emails non lus se sont accumulés (ex: premier lancement). Le reste sera
# traité automatiquement aux passages suivants (toutes les 3h), puisqu'on
# retraite les plus anciens en premier.
MAX_PER_RUN <- 15
if (length(ids) > MAX_PER_RUN) {
  message(sprintf("%d emails non lus trouvés, traitement des %d plus anciens ce passage-ci (le reste suivra aux prochains passages).", length(ids), MAX_PER_RUN))
  ids <- head(ids, MAX_PER_RUN)
}

message(sprintf("%d email(s) LinkedIn à traiter ce passage.", length(ids)))

seen_ids <- load_seen()
seen_job_urls <- load_seen_jobs()
n_sent <- 0
n_skipped_dup <- 0

# Normalise une URL LinkedIn pour la comparaison (enlève les paramètres de
# tracking après le "?", qui changent à chaque email pour la même offre et
# empêcheraient sinon de la reconnaître comme déjà envoyée)
normalize_job_url <- function(url) {
  str_split(url, "\\?", n = 2)[[1]][1]
}

# Lit la date de réception EXACTE de l'email depuis son en-tête (champ
# "Date:"), et retourne son ancienneté en jours. Plus précis que le filtre
# IMAP "since()" qui ne raisonne qu'au jour près (pas à l'heure près).
get_email_age_days <- function(id) {
  header <- tryCatch(con$fetch_header(id, use_uid = TRUE), error = function(e) NA_character_)
  if (is.na(header)) return(NA_real_)

  date_line <- str_extract(header, "(?im)^Date:\\s*.+$")
  if (is.na(date_line)) return(NA_real_)
  date_str <- str_trim(str_remove(date_line, "(?i)^Date:\\s*"))

  # Les dates email suivent le format RFC 5322, ex: "Wed, 09 Jul 2026 14:32:10 +0000"
  parsed <- tryCatch(
    as.POSIXct(date_str, format = "%a, %d %b %Y %H:%M:%S %z", tz = "UTC"),
    error = function(e) NA
  )
  if (is.na(parsed)) return(NA_real_)

  as.numeric(difftime(Sys.time(), parsed, units = "days"))
}

for (id in ids) {
  msg_key <- as.character(id)

  # Vérification précise de la date de réception : on ignore complètement
  # l'email s'il a plus de MAX_OFFER_AGE_DAYS jours, même si le filtre IMAP
  # since() (moins précis, au jour près) l'a laissé passer.
  email_age <- get_email_age_days(id)
  if (!is.na(email_age) && email_age > MAX_OFFER_AGE_DAYS) {
    message(sprintf("  Email #%s ignoré : reçu il y a %.1f jours (> %d jours)", id, email_age, MAX_OFFER_AGE_DAYS))
    tryCatch(con$add_flags(id, flags = "Seen", use_uid = TRUE), error = function(e) NULL)
    seen_ids <- c(seen_ids, msg_key)
    next
  }

  raw_body <- tryCatch(con$fetch_body(id, use_uid = TRUE), error = function(e) NA_character_)
  if (is.na(raw_body)) {
    message(sprintf("  Impossible de lire l'email #%s", id))
    next
  }

  html_content <- extract_html_part(raw_body)
  if (is.na(html_content)) {
    message(sprintf("  Pas de contenu HTML exploitable dans l'email #%s", id))
  } else {
    jobs <- extract_linkedin_jobs(html_content)
    jobs_avant_dedup <- nrow(jobs)
    jobs$href_normalise <- vapply(jobs$href, normalize_job_url, character(1))
    jobs <- jobs %>% filter(!href_normalise %in% seen_job_urls)
    n_skipped_dup <- n_skipped_dup + (jobs_avant_dedup - nrow(jobs))

    if (nrow(jobs) > 0) {
      for (i in seq_len(nrow(jobs))) {
        job <- jobs[i, ]
        msg <- sprintf(
          "💼 <b>Offre LinkedIn (via alerte email)</b>\n\n📌 %s\n🔗 %s",
          job$titre, job$href
        )
        send_telegram(msg)
        n_sent <- n_sent + 1
        seen_job_urls <- c(seen_job_urls, job$href_normalise)
        Sys.sleep(1)
      }
    } else {
      message(sprintf("  Aucune offre extraite de l'email #%s (structure HTML peut-être différente)", id))
    }
  }

  # Marque l'email comme lu pour ne pas le retraiter la prochaine fois
  tryCatch(con$add_flags(id, flags = "Seen", use_uid = TRUE), error = function(e) {
    message("  Impossible de marquer l'email comme lu: ", conditionMessage(e))
  })
  seen_ids <- c(seen_ids, msg_key)
}

save_seen(seen_ids)
save_seen_jobs(seen_job_urls)
message(sprintf("Terminé. %d offre(s) envoyée(s) sur Telegram, %d doublon(s) ignoré(s).", n_sent, n_skipped_dup))
