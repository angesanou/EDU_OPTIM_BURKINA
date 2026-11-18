# ==========================================================================
# 11_controle_qualite.R — Contrôles de cohérence et de qualité des données
# ==========================================================================
# Chaque contrôle renvoie un tableau (contrôle, anomalies, statut, détail).
# Ils s'appliquent indifféremment aux données synthétiques ou réelles.

#' Profil d'une table : observations, variables, taux de manquants.
profil_table <- function(df, nom, niveau, periode = NULL) {
  num <- vapply(df, is.numeric, logical(1))
  data.frame(
    jeu = nom, niveau_geographique = niveau,
    observations = nrow(df), variables = ncol(df),
    periode = periode %||% "–",
    taux_manquants = if (any(num)) mean(is.na(as.matrix(df[, num, drop = FALSE]))) else NA_real_,
    stringsAsFactors = FALSE)
}

#' Taux de manquants par variable.
manquants_par_variable <- function(df, variables = intersect(VARIABLES_SAISIE, names(df))) {
  data.frame(variable = variables,
             manquants = vapply(variables, function(v) sum(is.na(df[[v]])), numeric(1)),
             taux = vapply(variables, function(v) mean(is.na(df[[v]])), numeric(1)),
             row.names = NULL, stringsAsFactors = FALSE)
}

#' Batterie de contrôles sur le panel province.
controler_panel <- function(panel, geo) {
  res <- list()
  ajout <- function(controle, n, detail, gravite = "Erreur") {
    res[[length(res) + 1]] <<- data.frame(
      controle = controle, anomalies = n,
      statut = if (n == 0) "Conforme" else gravite, detail = detail, stringsAsFactors = FALSE)
  }
  cle <- paste(panel$cycle, panel$code_geo, panel$annee_debut)
  ajout("Doublons de clé (cycle × province × année)", sum(duplicated(cle)), "Chaque combinaison doit être unique.")
  ajout("Identifiants géographiques inconnus de DIM_GEO", sum(!panel$code_geo %in% geo$code_geo),
        "code_geo absent du référentiel.")
  if ("id_ligne" %in% names(panel)) {
    attendu <- paste(PREFIXE_CYCLE[as.character(panel$cycle)], panel$code_geo, panel$annee_debut, sep = "_")
    ajout("Identifiant de ligne cohérent avec les clés", sum(panel$id_ligne != attendu, na.rm = TRUE),
          "id_ligne = PRE|PRI|PPR|SEC _ code_geo _ année.", "Avertissement")
  }
  ecart <- abs(panel$eff_g + panel$eff_f - panel$eff_t)
  ajout("Effectifs : garçons + filles = total", sum(ecart > 0.5, na.rm = TRUE), "Tolérance 0,5 élève.")
  ecart_s <- abs(rowSums(panel[, c("eff_public_t", "eff_prive_t", "eff_communautaire_t")]) - panel$eff_t)
  ajout("Effectifs : Σ statuts = total", sum(ecart_s > 0.5, na.rm = TRUE), "Public + privé + communautaire.", "Avertissement")
  ecart_m <- abs(panel$eff_urbain_t + panel$eff_rural_t - panel$eff_t)
  ajout("Effectifs : urbain + rural = total", sum(ecart_m > 0.5, na.rm = TRUE), "", "Avertissement")
  ajout("Salles en bon état ≤ salles totales", sum(panel$nb_salles_bon_etat > panel$nb_salles_t, na.rm = TRUE), "")
  ajout("Établissements équipés ≤ établissements",
        sum(panel$nb_etab_eau > panel$nb_etab_t | panel$nb_etab_electricite > panel$nb_etab_t |
              panel$nb_etab_latrines > panel$nb_etab_t, na.rm = TRUE), "Eau, électricité, latrines.")
  ajout("Enseignants qualifiés ≤ enseignants", sum(panel$nb_ens_qualifies > panel$nb_ens_t, na.rm = TRUE), "")
  ajout("Valeurs négatives", sum(as.matrix(panel[, intersect(VARIABLES_ADDITIVES, names(panel))]) < 0, na.rm = TRUE), "")
  r <- diviser(panel$eff_t, panel$nb_salles_t)
  ajout("Ratio élèves/salle extrême (> 200)", sum(r > 200, na.rm = TRUE),
        "Valeur plausible mais à vérifier (double flux, erreur de saisie).", "Avertissement")
  tbs_calc <- 100 * diviser(panel$eff_t, panel$pop_age_scolaire)
  ajout("TBS publié ≠ TBS recalculé (> 2 points)", sum(abs(panel$tbs_t - tbs_calc) > 2, na.rm = TRUE),
        "Écart entre le taux saisi et effectif / population.", "Avertissement")
  o <- order(panel$cycle, panel$code_geo, panel$annee_debut)
  p <- panel[o, ]
  var_ann <- ave(p$eff_t, p$cycle, p$code_geo, FUN = function(z) c(NA, diff(z) / head(z, -1)))
  ajout("Rupture de série (variation annuelle des effectifs > 40 %)", sum(abs(var_ann) > 0.4, na.rm = TRUE),
        "Fermetures, reclassements ou erreurs.", "Avertissement")
  ajout("Lignes sans effectif (qualité MANQUANT)", sum(is.na(panel$eff_t)),
        "Combinaisons cycle × province × année non renseignées.", "Avertissement")
  do.call(rbind, res)
}

#' Contrôles des établissements (si disponibles).
controler_etablissements <- function(etab) {
  if (is.null(etab)) return(NULL)
  dbl <- duplicated(etab[, c("id_etab", "annee_debut")])
  out <- data.frame(
    controle = c("Doublons établissement × année", "Coordonnées manquantes",
                 "Salles en bon état ≤ salles", "Effectif négatif"),
    anomalies = c(sum(dbl), sum(is.na(etab$lon) | is.na(etab$lat)),
                  sum(etab$nb_salles_bon_etat > etab$nb_salles_t, na.rm = TRUE), sum(etab$eff_t < 0, na.rm = TRUE)),
    detail = c("Retirés automatiquement (première occurrence conservée).", "", "", ""),
    stringsAsFactors = FALSE)
  out$statut <- ifelse(out$anomalies == 0, "Conforme", "Avertissement")
  out
}

#' Nettoyage minimal et traçable : retire les doublons d'établissements.
nettoyer_etablissements <- function(etab) {
  if (is.null(etab)) return(NULL)
  dbl <- duplicated(etab[, c("id_etab", "annee_debut")])
  if (any(dbl)) journaliser("Nettoyage", sprintf("%d doublon(s) établissement × année retiré(s)", sum(dbl)))
  etab[!dbl, , drop = FALSE]
}
