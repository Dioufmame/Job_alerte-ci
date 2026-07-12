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

# --------------------------------------------------------------------------
# 2. FONCTIONS
# --------------------------------------------------------------------------

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
decode_quoted_printable <- function(txt) {
  txt <- gsub("=\r?\n", "", txt)  # sauts de ligne "mous" (soft line breaks)
  matches <- gregexpr("=[0-9A-Fa-f]{2}", txt)
  decoded <- txt
  m <- regmatches(txt, matches)[[1]]
  if (length(m) > 0) {
    for (code in unique(m)) {
      byte_val <- strtoi(substring(code, 2), base = 16L)
      decoded <- gsub(code, rawToChar(as.raw(byte_val)), decoded, fixed = TRUE)
    }
  }
  decoded
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

# Extrait les offres (titre + lien) d'un HTML d'email LinkedIn
extract_linkedin_jobs <- function(html_content) {
  tryCatch({
    page  <- xml2::read_html(html_content)
    links <- page %>% html_elements("a")

    hrefs  <- links %>% html_attr("href")
    titles <- links %>% html_text2()

    df <- data.frame(titre = str_squish(titles), href = hrefs, stringsAsFactors = FALSE)
    df <- df %>%
      filter(!is.na(href), str_detect(href, "linkedin\\.com/.*jobs/(view|comm)")) %>%
      filter(nchar(titre) > 3) %>%
      distinct(href, .keep_all = TRUE)
    df
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

# Cherche les emails non lus envoyés par LinkedIn (alertes emploi)
ids <- tryCatch(
  con$search(
    AND(
      string(expr = "linkedin.com", where = "FROM"),
      flag(name = "UNSEEN")
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

message(sprintf("%d email(s) LinkedIn non lu(s) trouvé(s).", length(ids)))

seen_ids <- load_seen()
n_sent <- 0

for (id in ids) {
  msg_key <- as.character(id)

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
    if (nrow(jobs) > 0) {
      for (i in seq_len(nrow(jobs))) {
        job <- jobs[i, ]
        msg <- sprintf(
          "💼 <b>Offre LinkedIn (via alerte email)</b>\n\n📌 %s\n🔗 %s",
          job$titre, job$href
        )
        send_telegram(msg)
        n_sent <- n_sent + 1
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
message(sprintf("Terminé. %d offre(s) envoyée(s) sur Telegram.", n_sent))
