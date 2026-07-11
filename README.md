# Agent d'alerte — Offres data en Côte d'Ivoire

Ce projet scrape automatiquement des sites d'emploi ivoiriens (Emploi.ci,
Novojob) toutes les 3 heures, filtre les offres liées à la data, et
t'envoie une notification Telegram sur ton téléphone pour chaque nouvelle
offre.

Coût : **0 FCFA** (GitHub Actions gratuit + bot Telegram gratuit).

## Étape 1 — Créer ton bot Telegram (5 minutes)

1. Ouvre Telegram, cherche le contact **@BotFather**.
2. Envoie-lui `/newbot`.
3. Donne un nom à ton bot (ex: "Alerte Data CI") puis un identifiant
   unique se terminant par `bot` (ex: `alerte_data_ci_bot`).
4. BotFather te donne un **token** du type
   `123456789:AAExxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx`. Garde-le précieusement,
   c'est ton `TELEGRAM_BOT_TOKEN`.

## Étape 2 — Récupérer ton chat_id

1. Dans Telegram, cherche ton bot par son nom et clique sur **Démarrer / Start**.
   Envoie-lui n'importe quel message (ex: "salut").
2. Depuis un navigateur, ouvre cette URL en remplaçant `<TOKEN>` par ton token :
   `https://api.telegram.org/bot<TOKEN>/getUpdates`
3. Tu verras une réponse JSON contenant `"chat":{"id":123456789, ...}`.
   Ce nombre est ton `TELEGRAM_CHAT_ID`.

## Étape 3 — Créer le dépôt GitHub

1. Crée un nouveau dépôt **privé** sur GitHub (ex: `job-alert-ci`).
2. Mets-y les fichiers de ce dossier (`scrape_jobs.R`, `data/seen_urls.json`,
   `.github/workflows/job-alert.yml`, ce `README.md`).

Depuis ton terminal, dans le dossier du projet :

```bash
git init
git add .
git commit -m "Initial commit - agent alerte data CI"
git branch -M main
git remote add origin https://github.com/<ton-user>/job-alert-ci.git
git push -u origin main
```

## Étape 4 — Ajouter les secrets

Dans ton dépôt GitHub : **Settings → Secrets and variables → Actions → New
repository secret**. Ajoute :

- `TELEGRAM_BOT_TOKEN` → le token de l'étape 1
- `TELEGRAM_CHAT_ID` → l'identifiant de l'étape 2

## Étape 5 — Activer et tester

1. Va dans l'onglet **Actions** de ton dépôt.
2. Le workflow "Alerte offres data CI" doit apparaître. Clique dessus puis
   **Run workflow** pour le lancer manuellement une première fois.
3. Si tout est bien configuré, tu dois recevoir un message Telegram pour
   chaque offre data détectée sur les sites listés.
4. Ensuite, le workflow tournera automatiquement toutes les 3 heures, sans
   rien faire de plus.

## Couverture des banques de Côte d'Ivoire

**Sites carrières directs ajoutés** : SIB, NSIA Banque, BNI, Afriland First Bank, BGFIBank, BICICI, BOA Côte d'Ivoire, Orabank.

**Banques couvertes indirectement** via les agrégateurs Emploi.ci et Novojob (catégorie "Banque") dès qu'elles y publient une offre : Ecobank, Société Générale CI, UBA, Standard Chartered, Citibank, et la plupart des autres établissements de ta liste.

**Non ajoutées individuellement** : certaines banques (Ecobank notamment) utilisent un système de recrutement basé sur JavaScript (Oracle Cloud, Workday, etc.) que le scraping simple ne peut pas lire directement — leurs offres seront quand même captées si elles les republient sur Emploi.ci ou Novojob, ce qu'elles font généralement. Pour les banques plus petites de ta liste (BBG-CI, BDA, BDU-CI, BHCI, BMS, BRM-CI, BSIC-CI, CBI-CI, CNCE, DBCI, FIDELIS, GTBANK-CI, MANSA BANK, OAC, SAFCA, SCBCI, SGBCI, STABIC BANK, UBA, Citibank, Orange Bank, Versus Bank), je n'ai pas trouvé ou vérifié de page carrières fiable — donne-moi une URL précise pour n'importe laquelle et je l'ajoute.

## Sources actuellement surveillées

- **Emploi.ci** et **Novojob** : recherches ciblées "data"
- **Emploi.ci - Banque** et **Novojob - Banque** : agrégateurs qui couvrent les offres de la plupart des banques de Côte d'Ivoire (SIB, NSIA, Ecobank, Société Générale CI, BOA, Orabank, etc.) publiées sur ces plateformes
- **SIB, NSIA Banque, BNI** : sites carrières directs de ces banques
- **Emploi.ci - Toutes offres** : page générale, filtrée ensuite par mots-clés et par le score CV

**Ajouter une entreprise ou une banque en particulier** : donne-moi l'URL de sa page carrières/recrutement, et je l'ajoute à `SOURCES` dans `scrape_jobs.R`. Attention : certains sites d'entreprise utilisent des technologies web modernes (React, Angular) qui chargent le contenu par JavaScript — le scraping simple utilisé ici ne peut pas les lire. On le saura après un premier test (le site apparaîtra en erreur ou renverra 0 offre dans les logs), et on pourra soit l'ajuster, soit s'appuyer davantage sur les agrégateurs (Emploi.ci, Novojob) qui republient déjà la plupart de ces offres.

## Matching avec ton CV

Le script ne se contente pas de filtrer sur des mots-clés génériques : pour
chaque offre candidate, il va chercher la page de détail complète et compare
son contenu à la liste de compétences extraites de ton CV
(`CV_SKILLS` / `SKILL_LABELS` dans `scrape_jobs.R` : R, Python, SQL, Power BI,
ETL, Data Mining, Machine Learning, etc.).

- Une offre n'est envoyée sur Telegram **que si elle partage au moins
  `MATCH_THRESHOLD` compétences avec ton CV** (3 par défaut).
- Le message Telegram affiche le score et la liste des compétences en
  commun, pour que tu voies immédiatement pourquoi l'offre matche.
- Les offres en dessous du seuil sont quand même marquées comme "vues"
  (pour ne pas être re-analysées à chaque run), mais ne te sont pas
  envoyées.

**Pour ajuster la sensibilité** : modifie `MATCH_THRESHOLD` dans le script,
ou définis la variable d'environnement / secret GitHub `MATCH_THRESHOLD`
(ex: `2` pour être moins strict et recevoir plus d'offres, `5` pour être
plus sélectif).

**Pour mettre à jour ton profil** (nouvelle compétence acquise, nouvel
outil) : ajoute une entrée dans `CV_SKILLS` (le motif de recherche) et la
même position dans `SKILL_LABELS` (le libellé affiché).

## Personnalisation

- **Ajouter des sources** : dans `scrape_jobs.R`, ajoute une entrée dans la
  liste `SOURCES` avec le nom du site et l'URL de la page de résultats.
- **Ajouter/retirer des mots-clés** : modifie le vecteur `KEYWORDS` en haut
  du script.
- **Changer la fréquence** : modifie la ligne `cron: '0 */3 * * *'` dans
  `.github/workflows/job-alert.yml` (ex: `0 8,14,20 * * *` pour 3 fois par
  jour à heures fixes).

## Limites à connaître

- Si un site change sa mise en page HTML, l'extraction peut cesser de
  fonctionner — dans ce cas, il faut ajuster le sélecteur dans
  `scrape_source()`.
- Le script scrape des pages publiques (pas d'authentification). Il ne
  couvre pas LinkedIn, dont le scraping automatisé viole les conditions
  d'utilisation — pour LinkedIn, utilise plutôt leurs alertes email
  natives (recherche → "Créer une alerte").
- GitHub Actions gratuit offre 2000 minutes/mois pour les dépôts privés,
  largement suffisant pour ce cas d'usage (chaque run dure ~1-2 minutes,
  soit environ 240 minutes/mois pour un run toutes les 3h).
