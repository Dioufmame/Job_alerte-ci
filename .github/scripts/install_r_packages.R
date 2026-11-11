# Installe les packages R en binaires précompilés (Posit Package Manager) :
# pas de compilation, pas d'apt-get, donc rapide et insensible aux lenteurs
# des miroirs Ubuntu. Usage : Rscript .github/scripts/install_r_packages.R pkg1 pkg2 ...
pkgs <- commandArgs(trailingOnly = TRUE)
lib <- Sys.getenv("R_LIBS_USER")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))

codename <- tryCatch(
  sub("^VERSION_CODENAME=", "", grep("^VERSION_CODENAME=", readLines("/etc/os-release"), value = TRUE)),
  error = function(e) "noble"
)
options(
  repos = c(
    P3M  = sprintf("https://packagemanager.posit.co/cran/__linux__/%s/latest", codename),
    CRAN = "https://cloud.r-project.org"
  ),
  HTTPUserAgent = sprintf(
    "R/%s R (%s)", getRversion(),
    paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
  ),
  timeout = 300
)

# Un package en cache mais qui ne se charge plus (bibliothèque système
# disparue après une mise à jour du runner) est réinstallé.
loads <- function(p) {
  ok <- tryCatch({ suppressPackageStartupMessages(requireNamespace(p, lib.loc = .libPaths(), quietly = TRUE)) },
                 error = function(e) FALSE)
  isTRUE(ok)
}
missing <- pkgs[!vapply(pkgs, loads, logical(1))]
if (length(missing)) {
  message("Installation : ", paste(missing, collapse = ", "))
  install.packages(missing, lib = lib, dependencies = c("Depends", "Imports", "LinkingTo"))
}

# Vérification finale dans un processus R neuf : on échoue ici, avec un
# message clair, plutôt qu'au milieu du script d'alerte.
for (p in pkgs) {
  res <- system2("Rscript", c("-e", shQuote(sprintf("suppressPackageStartupMessages(library(%s))", p))),
                 stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(res, "status"))) {
    stop("Le package ", p, " ne se charge pas :\n", paste(res, collapse = "\n"))
  }
}
message("Packages R prêts : ", paste(pkgs, collapse = ", "))
