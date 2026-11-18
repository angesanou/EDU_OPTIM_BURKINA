# ==========================================================================
# 27_simulations.R — Laboratoire de scénarios
# ==========================================================================

SCENARIOS <- list(
  reference = list(nom = "Référence", budget = 100e9, mult_demo = 1, lambda = 0.5, alpha = 0,
                   reallocation = FALSE, description = "Budget annuel moyen, croissance tendancielle, équité modérée."),
  contraint = list(nom = "Budget contraint", budget = 50e9, mult_demo = 1, lambda = 0.5, alpha = 0,
                   reallocation = FALSE, description = "Budget divisé par deux."),
  renforce = list(nom = "Investissement renforcé", budget = 250e9, mult_demo = 1, lambda = 0.5, alpha = 0.2,
                  reallocation = FALSE, description = "Effort exceptionnel ; 20 % minimum des besoins des zones critiques."),
  equite = list(nom = "Équité territoriale", budget = 100e9, mult_demo = 1, lambda = 3, alpha = 0.3,
                reallocation = FALSE, description = "Forte priorité à la couverture minimale de chaque province."),
  pression = list(nom = "Pression démographique", budget = 100e9, mult_demo = 1.6, lambda = 0.5, alpha = 0,
                  reallocation = FALSE, description = "Croissance des effectifs 60 % plus rapide que la tendance."),
  reallocation = list(nom = "Réallocation des enseignants", budget = 100e9, mult_demo = 1, lambda = 0.5, alpha = 0,
                      reallocation = TRUE, description = "Redéploiement des enseignants excédentaires avant investissement."))

#' Projette le panel (année de base) à l'horizon, à stock constant.
projeter_situation <- function(ind_prov, annee_base, horizon, mult_demo = 1) {
  base <- ind_prov[ind_prov$annee_debut == annee_base, ]
  passe <- ind_prov[ind_prov$annee_debut == annee_base - 4, ]
  k <- match(paste(base$code_geo, base$cycle), paste(passe$code_geo, passe$cycle))
  g <- (base$eff_t / passe$eff_t[k])^(1 / 4) - 1
  g[!is.finite(g)] <- 0
  g <- pmin(pmax(g, -0.05), 0.12) * mult_demo
  f <- (1 + g)^(horizon - annee_base)
  for (v in c("eff_t", "eff_g", "eff_f", "eff_urbain_t", "eff_rural_t", "eff_public_t", "eff_prive_t",
              "eff_communautaire_t", "nouv_cp1_t", "eff_cm2_t", "redoublants_cm2_t"))
    base[[v]] <- round(base[[v]] * f)
  fp <- (1 + 0.028 * mult_demo)^(horizon - annee_base)
  for (v in c("pop_age_scolaire", "pop_age_entree", "pop_age_fin", "pop_totale")) base[[v]] <- round(base[[v]] * fp)
  base$annee_debut <- horizon
  base$croissance_projetee <- g
  base
}

#' Redéploiement des enseignants excédentaires (problème de transport).
reallouer_enseignants <- function(sit, normes, distances = NULL, part_mobile = 0.5) {
  out <- sit; transferts <- NULL
  for (cy in unique(as.character(sit$cycle))) {
    k <- which(as.character(sit$cycle) == cy)
    besoin <- ceiling(sit$eff_t[k] / norme(normes, "RATIO_ELEVES_ENSEIGNANT", cy))
    surplus <- floor(pmax(0, sit$nb_ens_t[k] - besoin) * part_mobile)
    manque <- pmax(0, besoin - sit$nb_ens_t[k])
    surplus[is.na(surplus)] <- 0; manque[is.na(manque)] <- 0
    o <- which(surplus > 0); dd <- which(manque > 0)
    if (!length(o) || !length(dd)) next
    S <- sum(surplus[o]); Dm <- sum(manque[dd])
    dem <- floor(manque[dd] * min(1, S / Dm))
    if (sum(dem) == 0) next
    codes <- sit$code_geo[k]
    cm <- if (!is.null(distances) && all(c(codes[o], codes[dd]) %in% rownames(distances)))
      distances[codes[o], codes[dd], drop = FALSE] else matrix(1, length(o), length(dd))
    tr <- lpSolve::lp.transport(unname(cm), "min", rep("<=", length(o)), surplus[o], rep(">=", length(dd)), dem)
    if (tr$status != 0) next
    sol <- tr$solution
    out$nb_ens_t[k[o]] <- out$nb_ens_t[k[o]] - rowSums(sol)
    out$nb_ens_t[k[dd]] <- out$nb_ens_t[k[dd]] + colSums(sol)
    w <- which(sol > 0, arr.ind = TRUE)
    if (nrow(w)) transferts <- rbind(transferts, data.frame(
      cycle = cy, origine = sit$nom[k[o]][w[, 1]], destination = sit$nom[k[dd]][w[, 2]], enseignants = sol[w],
      distance_km = if (!is.null(distances)) unname(cm[w]) else NA_real_))
  }
  list(situation = calculer_indicateurs(out, normes), transferts = transferts)
}

#' Exécute un scénario complet et renvoie ses résultats synthétiques.
executer_scenario <- function(sc, ind_prov, imde_bornes, normes, couts, annee_base, horizon,
                              distances = NULL, cycles = names(CYCLES)) {
  sit <- projeter_situation(ind_prov[as.character(ind_prov$cycle) %in% cycles, ], annee_base, horizon, sc$mult_demo)
  sit <- calculer_indicateurs(sit, normes)
  transferts <- NULL
  if (isTRUE(sc$reallocation)) {
    r <- reallouer_enseignants(sit, normes, distances); sit <- r$situation; transferts <- r$transferts
  }
  avant <- calculer_imde(sit, imde_bornes)
  avant$poids <- 0.5 + ifelse(is.na(avant$imde), 50, avant$imde) / 100
  dispo <- c(salles = sc$dispo_salles %||% Inf, enseignants = sc$dispo_ens %||% Inf)
  opt <- optimiser_allocation(avant, couts, sc$budget, dispo = dispo, lambda = sc$lambda, alpha = sc$alpha)
  apres <- appliquer_allocation(sit, opt$allocation, normes, imde_bornes)
  w <- avant$eff_t
  al <- opt$allocation
  n_res <- function(r) if (is.null(al)) 0 else sum(al$alloue[al$ressource == r])
  resume <- data.frame(
    scenario = sc$nom, budget = sc$budget, cout_total = opt$cout_total %||% 0,
    imde_avant = moy_ponderee(avant$imde, w), imde_apres = moy_ponderee(apres$imde, w),
    gini_imde_apres = gini(apres$imde),
    unites_critiques_avant = sum(avant$classe_imde %in% c("Critique", "Sévère")),
    unites_critiques_apres = sum(apres$classe_imde %in% c("Critique", "Sévère")),
    salles = n_res("salles"),
    enseignants = n_res("enseignants") + (if (!is.null(transferts)) sum(transferts$enseignants) else 0),
    eleves_beneficiaires_salles = if (is.null(al)) 0 else sum(al$alloue[al$ressource == "salles"] *
      norme(normes, "TAILLE_CLASSE_MAX", al$cycle[al$ressource == "salles"])),
    deficit_salles_restant = sum(apres$deficit_salles, na.rm = TRUE),
    deficit_ens_restant = sum(apres$deficit_enseignants, na.rm = TRUE),
    couverture_min = opt$couverture_min %||% NA_real_, stringsAsFactors = FALSE)
  list(resume = resume, avant = avant, apres = apres, optimisation = opt, transferts = transferts)
}

#' Matrice des distances (km) entre centroïdes de provinces.
distances_provinces <- function(provinces_sf) {
  if (is.null(provinces_sf)) return(NULL)
  ce <- sf::st_centroid(sf::st_transform(sf::st_geometry(provinces_sf), 32630))
  m <- matrix(as.numeric(sf::st_distance(ce)), nrow = length(ce)) / 1000
  dimnames(m) <- list(provinces_sf$code_geo, provinces_sf$code_geo)
  m
}
