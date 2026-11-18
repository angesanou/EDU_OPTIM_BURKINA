# ==========================================================================
# demarrer.R - Lance EDU-OPTIM BURKINA sur n'importe quel poste
# ==========================================================================
# Windows : double-cliquer sur LANCER_EDU_OPTIM.bat (qui appelle ce fichier).
# RStudio / Mac / Linux : ouvrir EDU_OPTIM_BURKINA.Rproj puis
#   source("demarrer.R")
# Au premier lancement, les paquets manquants sont installes depuis le CRAN
# (connexion Internet requise une seule fois), puis les donnees sont
# preparees (environ 1 minute). Les lancements suivants sont plus rapides.
# ==========================================================================

# --- Se placer dans le dossier de l'application ----------------------------
local({
  ici <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL)
  if (is.null(ici)) {
    a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(a)) ici <- dirname(normalizePath(sub("^--file=", "", a[1])))
  }
  if (!is.null(ici)) setwd(ici)
})
if (!file.exists("global.R")) stop("Lancer demarrer.R depuis le dossier EDU_OPTIM_BURKINA.")

# --- Version de R ------------------------------------------------------------
if (getRversion() < "4.2.0")
  stop("EDU-OPTIM demande R 4.2 ou plus recent (version installee : ", getRversion(),
       "). Telecharger R : https://cran.r-project.org")

# --- Paquets -----------------------------------------------------------------
PAQUETS <- c("shiny", "bslib", "plotly", "leaflet", "DT", "htmltools", "readxl", "openxlsx",
             "data.table", "yaml", "digest", "commonmark", "sf", "spdep", "plm", "lmtest",
             "sandwich", "car", "forecast", "lpSolve", "ggplot2", "jsonlite")
# Bibliotheque d'appoint : copies de paquets refuses par Windows (voir plus bas)
APPOINT <- file.path(getwd(), "bibliotheque_appoint")
if (dir.exists(APPOINT)) .libPaths(c(APPOINT, .libPaths()))

installe <- function(p) nzchar(system.file(package = p))
erreur_chargement <- function(p) tryCatch({ suppressPackageStartupMessages(loadNamespace(p)); NULL },
                                          error = function(e) conditionMessage(e))
est_blocage <- function(msg) grepl("strat.gie de contr|LoadLibrary|0xc0e90002|application control", msg, ignore.case = TRUE)

# 1. Installer seulement les paquets reellement absents
manquants <- PAQUETS[!vapply(PAQUETS, installe, logical(1))]
if (length(manquants)) {
  message("Premier lancement : installation de ", length(manquants), " paquet(s) : ",
          paste(manquants, collapse = ", "))
  lib <- .libPaths()[1]
  if (identical(lib, APPOINT) || file.access(lib, 2) != 0) {
    lib <- Sys.getenv("R_LIBS_USER")
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
    .libPaths(unique(c(if (dir.exists(APPOINT)) APPOINT, lib, .libPaths())))
  }
  type <- if (.Platform$OS.type == "windows" || Sys.info()[["sysname"]] == "Darwin") "binary" else getOption("pkgType")
  utils::install.packages(manquants, lib = lib, repos = "https://cloud.r-project.org", type = type)
  encore <- manquants[!vapply(manquants, installe, logical(1))]
  if (length(encore)) stop("Installation impossible pour : ", paste(encore, collapse = ", "),
                           ". Verifier la connexion Internet puis relancer.")
}

# 2. Verifier que chaque paquet se charge.
# Sous Windows 11, la protection « Smart App Control » peut refuser la bibliotheque
# compilee (.dll) d'un paquet R. On cherche alors une autre copie de ce paquet deja
# presente sur le poste (installee avec une autre version de R) et on la place en
# tete des bibliotheques, dans le dossier bibliotheque_appoint.
# Windows affiche alors une fenetre « Image incorrecte » : cliquer sur OK, le
# lancement continue.
autres_copies <- function(pk) {
  cands <- c(Sys.glob(file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", "*", pk)),
             Sys.glob(file.path(Sys.getenv("USERPROFILE"), "Documents", "R", "win-library", "*", pk)),
             Sys.glob(file.path(Sys.getenv("ProgramFiles"), "R", "R-*", "library", pk)))
  cands <- unique(normalizePath(cands, winslash = "/", mustWork = FALSE))
  cands[file.exists(file.path(cands, "DESCRIPTION"))]
}
essayees <- character(0)
for (tour in 1:20) {
  # On s'arrete au premier echec : chaque refus de Windows ouvre une fenetre a fermer
  ko <- NULL
  for (p in PAQUETS) { m <- erreur_chargement(p); if (!is.null(m)) { ko <- setNames(list(m), p); break } }
  if (is.null(ko)) break
  bloques <- Filter(est_blocage, ko)
  if (!length(bloques)) stop("Chargement impossible du paquet ", names(ko), " : ", ko[[1]])
  # Paquet dont la .dll est refusee (cite dans le message d'erreur)
  pk_dll <- unique(sub("^.*[/\\\\]([^/\\\\]+)[/\\\\]libs[/\\\\].*$", "\\1", unlist(bloques)))
  pk_dll <- pk_dll[vapply(pk_dll, installe, logical(1))]
  remplace <- FALSE
  for (pk in pk_dll) {
    actuel <- normalizePath(system.file(package = pk), winslash = "/", mustWork = FALSE)
    essayees <- unique(c(essayees, actuel))   # la copie refusee n'est plus proposee
    cands <- setdiff(autres_copies(pk), c(essayees, normalizePath(file.path(APPOINT, pk), winslash = "/", mustWork = FALSE)))
    if (!length(cands)) next
    dir.create(APPOINT, showWarnings = FALSE)
    unlink(file.path(APPOINT, pk), recursive = TRUE)
    file.copy(cands[1], APPOINT, recursive = TRUE)
    essayees <- c(essayees, cands[1]); remplace <- TRUE
    message("Windows refuse le paquet ", pk, " : utilisation de la copie ", cands[1])
  }
  if (!remplace) stop("Windows (protection « Smart App Control ») refuse le paquet ",
                      paste(pk_dll, collapse = ", "), " et aucune autre copie n'est disponible sur ce poste. ",
                      "Lancez l'application sur un autre ordinateur, ou faites autoriser ce fichier par ",
                      "l'administrateur du poste. Detail : ", unlist(bloques)[1])
  .libPaths(unique(c(APPOINT, .libPaths())))
}

# --- Lancement ---------------------------------------------------------------
message("Demarrage d'EDU-OPTIM BURKINA (la premiere preparation des donnees prend environ 1 minute)...")
message("L'application s'ouvre dans le navigateur. Pour l'arreter : fermer cette fenetre (ou Echap dans RStudio).")
# Options (facultatives) : EDU_OPTIM_PORT=4330 fixe le port ;
# EDU_OPTIM_SANS_NAVIGATEUR=1 n'ouvre pas le navigateur (tests, serveur).
port <- suppressWarnings(as.integer(Sys.getenv("EDU_OPTIM_PORT")))
shiny::runApp(".", launch.browser = !identical(Sys.getenv("EDU_OPTIM_SANS_NAVIGATEUR"), "1"),
              port = if (is.na(port)) getOption("shiny.port") else port)
