name: Relais LinkedIn vers Telegram

on:
  schedule:
    - cron: '15 */3 * * *'   # Décalé de 15 min par rapport à l'autre workflow
  workflow_dispatch: {}

permissions:
  contents: write

jobs:
  relay-linkedin:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout repo
        uses: actions/checkout@v4

      - name: Setup R
        uses: r-lib/actions/setup-r@v2
        with:
          r-version: '4.4.0'

      - name: Cache packages R
        uses: actions/cache@v4
        with:
          path: ${{ env.R_LIBS_USER }}
          key: r-pkgs-linkedin-${{ hashFiles('linkedin_relay.R') }}

      - name: Installer les dépendances système
        run: |
          sudo apt-get update
          sudo apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev

      - name: Installer les packages R
        run: |
          Rscript -e 'install.packages(c("mRpostman","httr","stringr","xml2","rvest","jsonlite","dplyr","base64enc"), repos="https://cloud.r-project.org")'

      - name: Lancer le relais LinkedIn
        env:
          GMAIL_ADDRESS: ${{ secrets.GMAIL_ADDRESS }}
          GMAIL_APP_PASSWORD: ${{ secrets.GMAIL_APP_PASSWORD }}
          TELEGRAM_BOT_TOKEN: ${{ secrets.TELEGRAM_BOT_TOKEN }}
          TELEGRAM_CHAT_ID: ${{ secrets.TELEGRAM_CHAT_ID }}
        run: Rscript linkedin_relay.R

      - name: Sauvegarder la mémoire des emails et offres relayés
        run: |
          git config user.name "job-alert-bot"
          git config user.email "job-alert-bot@users.noreply.github.com"
          git add data/seen_linkedin_emails.json data/seen_linkedin_job_urls.json
          git diff --quiet --cached || git commit -m "Mise à jour de la mémoire du relais LinkedIn [skip ci]"
          git push
