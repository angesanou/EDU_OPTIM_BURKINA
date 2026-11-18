# ==========================================================================
# 01_utils.R — Fonctions utilitaires génériques
# ==========================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

#' Division protégée : renvoie NA si le dénominateur est nul ou manquant.
diviser <- function(a, b) ifelse(is.na(a) | is.na(b) | b == 0, NA_real_, a / b)

#' Somme qui renvoie NA seulement si TOUTES les valeurs sont manquantes.
somme <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

#' Moyenne pondérée tolérante aux NA.
moy_ponderee <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

#' Normalise un libellé pour les jointures (sans accents, majuscules).
normaliser_nom <- function(x) {
  x <- iconv(as.character(x), from = "UTF-8", to = "ASCII//TRANSLIT")
  toupper(gsub("[^A-Za-z0-9]", "", x))
}

#' Format numérique français : 12 450 ; 2,8 M ; 45,3 %.
fmt_nombre <- function(x, decimales = 0) {
  ifelse(is.na(x) | !is.finite(x), "–",
         formatC(round(x, decimales), format = "f", digits = decimales,
                 big.mark = " ", decimal.mark = ","))
}
fmt_compact <- function(x) {
  vapply(x, function(v) {
    if (is.na(v) || !is.finite(v)) return("–")
    a <- abs(v)
    if (a >= 1e9) return(paste0(fmt_nombre(v / 1e9, 1), " Md"))
    if (a >= 1e6) return(paste0(fmt_nombre(v / 1e6, 1), " M"))
    if (a >= 1e4) return(paste0(fmt_nombre(v / 1e3, 0), " k"))
    fmt_nombre(v, 0)
  }, character(1), USE.NAMES = FALSE)
}
fmt_pct <- function(x, decimales = 1) ifelse(is.na(x) | !is.finite(x), "–", paste0(fmt_nombre(100 * x, decimales), " %"))
fmt_fcfa <- function(x) ifelse(is.na(x), "–", paste0(fmt_compact(x), " FCFA"))

#' Année scolaire « 2024-2025 » à partir de l'année de début.
annee_scolaire <- function(debut) paste0(debut, "-", debut + 1)

#' Vérifie qu'une table contient des colonnes ; erreur lisible sinon.
verifier_colonnes <- function(df, colonnes, nom_table = "table") {
  manq <- setdiff(colonnes, names(df))
  if (length(manq))
    stop(sprintf("%s : colonnes manquantes : %s", nom_table, paste(manq, collapse = ", ")), call. = FALSE)
  invisible(TRUE)
}

#' Coefficient de Gini (inégalité de répartition) — 0 = égalité parfaite.
gini <- function(x, w = NULL) {
  ok <- !is.na(x)
  x <- x[ok]
  w <- if (is.null(w)) rep(1, length(x)) else w[ok]
  if (length(x) < 2 || sum(x * w) == 0) return(NA_real_)
  o <- order(x); x <- x[o]; w <- w[o]
  p <- cumsum(w) / sum(w); l <- cumsum(x * w) / sum(x * w)
  1 - sum((p - c(0, head(p, -1))) * (l + c(0, head(l, -1))))
}

#' Journal des traitements (historique affiché dans le module Données).
JOURNAL <- new.env()
JOURNAL$entrees <- data.frame(horodatage = character(), etape = character(),
                              detail = character(), stringsAsFactors = FALSE)
journaliser <- function(etape, detail) {
  JOURNAL$entrees <- rbind(JOURNAL$entrees, data.frame(
    horodatage = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    etape = etape, detail = detail, stringsAsFactors = FALSE))
  invisible(NULL)
}
