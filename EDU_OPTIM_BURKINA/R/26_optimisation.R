# ==========================================================================
# 26_optimisation.R — Allocation optimale des ressources (programmation linéaire)
# ==========================================================================
# MODÈLE (unités u = province × cycle, ressources r) :
#   Variables   x[u,r] ≥ 0  quantité de ressource r allouée à l'unité u
#               z ∈ [0,1]   taux de couverture minimal garanti à chaque province
#   Objectif    max Σ_u Σ_r  w_u · v_r · x[u,r] / D_r   +   λ · z · Σ_r v_r
#               w_u : poids de priorité ; v_r : importance de la ressource ;
#               D_r : déficit total de r (normalise des unités hétérogènes).
#   Contraintes Σ c_r · x[u,r] ≤ B                               (budget)
#               Σ_u x[u,r] ≤ A_r                                 (disponibilité)
#               x[u,r] ≤ d[u,r]                                  (pas au-delà du besoin)
#               Σ_{u∈p,r} c_r x[u,r] ≥ z · C_p   ∀ province p     (équité territoriale)
#               x[u,r] ≥ α · d[u,r] pour les unités « Critique »  (besoin minimal)
#               Σ_u x[u,r] ≥ ζ · D_r   ∀ r   (+ μ · ζ · Σ_r v_r dans l'objectif :
#               équilibre entre ressources, couverture minimale commune ζ)
# Résolution : lpSolve. Mode « relaxation + arrondi » (rapide ; entier garanti
# par arrondi inférieur puis complétion gloutonne du budget restant) ou mode
# « PLNE exacte » (branch-and-bound) pour les problèmes de petite taille.

RESSOURCES_OPTIM <- c(salles = "deficit_salles", enseignants = "deficit_enseignants",
                      tables_bancs = "deficit_tables_bancs", latrines = "deficit_latrines",
                      eau = "deficit_eau", electricite = "deficit_electricite", cantines = "deficit_cantines")

#' Optimise l'allocation.
#' @param unites data.frame : code_geo, cycle, poids (w_u), classe_imde, colonnes deficit_*.
#' @param couts  table des coûts (ressource, cout_unitaire, libelle).
#' @param budget budget total (FCFA). @param dispo vecteur nommé A_r (Inf = illimité).
#' @param importance vecteur nommé v_r. @param lambda poids de l'équité. @param alpha plancher.
optimiser_allocation <- function(unites, couts, budget, ressources = names(RESSOURCES_OPTIM),
                                 dispo = NULL, importance = NULL, lambda = 0.5, alpha = 0,
                                 mu = 1, mode = c("relaxation", "exacte")) {
  mode <- match.arg(mode)
  if (!is.numeric(budget) || is.na(budget) || budget < 0) stop("Budget invalide.")
  cu <- setNames(couts$cout_unitaire, couts$ressource)[ressources]
  if (is.null(importance)) importance <- setNames(rep(1, length(ressources)), ressources)
  dispo_tout <- setNames(rep(Inf, length(ressources)), ressources)
  if (!is.null(dispo)) for (r in intersect(names(dispo), ressources)) if (!is.na(dispo[[r]])) dispo_tout[[r]] <- dispo[[r]]
  dispo <- dispo_tout
  # Table des variables (u, r) avec déficit > 0
  var <- do.call(rbind, lapply(ressources, function(r) {
    d <- unites[[RESSOURCES_OPTIM[[r]]]]
    if (is.null(d)) return(NULL)
    k <- which(!is.na(d) & d > 0)
    if (!length(k)) return(NULL)
    data.frame(i = k, code_geo = unites$code_geo[k], cycle = as.character(unites$cycle[k]), ressource = r,
               deficit = d[k], cout = cu[[r]], w = unites$poids[k],
               critique = unites$classe_imde[k] %in% "Critique", stringsAsFactors = FALSE)
  }))
  if (is.null(var) || !nrow(var)) return(list(statut = "Aucun déficit à combler pour cette sélection.", allocation = NULL))
  var$w[is.na(var$w)] <- 1
  D <- tapply(var$deficit, var$ressource, sum)
  var$coef <- var$w * importance[var$ressource] / D[var$ressource]
  nv <- nrow(var); iz <- nv + 1; izeta <- nv + 2
  provs <- unique(var$code_geo)
  Cp <- tapply(var$cout * var$deficit, var$code_geo, sum)
  # Plancher : réduit automatiquement si le budget ne le permet pas
  alpha_eff <- alpha
  cout_plancher <- sum(floor(alpha * var$deficit[var$critique]) * var$cout[var$critique])
  if (alpha > 0 && cout_plancher > budget) alpha_eff <- alpha * budget / cout_plancher * 0.98
  # Contraintes en format creux (ligne, colonne, valeur)
  trip <- list(); dirs <- character(); rhs <- numeric(); ligne <- 0
  add <- function(cols, vals, dir, b) {
    ligne <<- ligne + 1
    trip[[ligne]] <<- cbind(ligne, cols, vals); dirs[ligne] <<- dir; rhs[ligne] <<- b
  }
  add(seq_len(nv), var$cout, "<=", budget)
  for (r in unique(var$ressource)) if (is.finite(dispo[[r]])) {
    k <- which(var$ressource == r); add(k, rep(1, length(k)), "<=", dispo[[r]])
  }
  for (j in seq_len(nv)) add(j, 1, "<=", var$deficit[j])
  if (alpha_eff > 0) for (j in which(var$critique)) {
    b <- floor(alpha_eff * var$deficit[j]); if (b > 0) add(j, 1, ">=", b)
  }
  if (lambda > 0) {
    for (p in provs) { k <- which(var$code_geo == p); add(c(k, iz), c(var$cout[k], -Cp[[p]]), ">=", 0) }
    add(iz, 1, "<=", 1)
  }
  # Équilibre entre ressources : taux de couverture minimal commun ζ (max-min).
  # Sans lui, l'optimum « remplit » d'abord les ressources bon marché
  # (tables-bancs, cantines) et délaisse les salles, coûteuses.
  if (mu > 0) {
    for (r in unique(var$ressource)) { k <- which(var$ressource == r); add(c(k, izeta), c(rep(1, length(k)), -D[[r]]), ">=", 0) }
    add(izeta, 1, "<=", 1)
  }
  dense <- do.call(rbind, trip)
  obj <- c(var$coef, if (lambda > 0) lambda * sum(importance) else 0, if (mu > 0) mu * sum(importance) else 0)
  message_mode <- NULL
  int_vec <- NULL
  if (mode == "exacte") {
    # lpSolve n'a pas de limite de temps : le branch-and-bound peut exploser
    # au-delà de quelques dizaines de variables entières -> seuil prudent.
    if (nv > 40) { mode <- "relaxation"; message_mode <- "Problème trop grand pour la PLNE exacte (> 40 variables) : relaxation + arrondi utilisée." }
    else int_vec <- seq_len(nv)
  }
  sol <- lpSolve::lp("max", obj, dense.const = dense, const.dir = dirs, const.rhs = rhs,
                     int.vec = int_vec, compute.sens = 0)
  if (sol$status != 0) return(list(statut = "Problème infaisable : réduisez le plancher α, augmentez le budget ou les disponibilités.",
                                   allocation = NULL))
  x <- sol$solution[seq_len(nv)]
  if (is.null(int_vec)) {
    # Arrondi inférieur puis complétion gloutonne (meilleur rapport valeur/coût)
    x <- floor(x + 1e-6)
    reste <- budget - sum(x * var$cout)
    utilise <- tapply(x, factor(var$ressource, levels = ressources), sum); utilise[is.na(utilise)] <- 0
    ordre <- order(var$coef / var$cout, decreasing = TRUE)
    for (j in ordre) {
      if (reste < min(var$cout)) break
      r <- var$ressource[j]
      marge <- if (is.finite(dispo[[r]])) dispo[[r]] - utilise[[r]] else Inf
      k <- min(var$deficit[j] - x[j], floor(reste / var$cout[j]), marge)
      if (k > 0) { x[j] <- x[j] + k; reste <- reste - k * var$cout[j]; utilise[[r]] <- utilise[[r]] + k }
    }
  }
  var$alloue <- x
  var$cout_alloue <- x * var$cout
  var$couverture <- diviser(x, var$deficit)
  par_ressource <- do.call(rbind, lapply(split(var, var$ressource), function(v) data.frame(
    ressource = v$ressource[1], deficit_initial = sum(v$deficit), alloue = sum(v$alloue),
    deficit_restant = sum(v$deficit - v$alloue), couverture = sum(v$alloue) / sum(v$deficit),
    cout = sum(v$cout_alloue))))
  par_ressource$libelle <- couts$libelle[match(par_ressource$ressource, couts$ressource)]
  cov_prov <- tapply(var$cout_alloue, var$code_geo, sum) / Cp
  list(statut = paste(c("Solution optimale trouvée.", message_mode,
                        if (alpha_eff < alpha) sprintf("Plancher ramené à %.0f %% (budget insuffisant).", 100 * alpha_eff)),
                      collapse = " "),
       mode = mode, allocation = var, par_ressource = par_ressource,
       cout_total = sum(var$cout_alloue), budget = budget,
       couverture_min = min(cov_prov), couverture_provinces = cov_prov,
       valeur_objectif = sum(var$coef * var$alloue))
}

#' Applique une allocation aux stocks et recalcule indicateurs et IMDE.
appliquer_allocation <- function(ind, alloc, normes, bornes) {
  if (is.null(alloc)) return(calculer_imde(ind, bornes))
  a <- alloc[alloc$alloue > 0, ]
  cle_i <- paste(ind$code_geo, as.character(ind$cycle))
  ajoute <- function(r) {
    if (!any(a$ressource == r)) return(rep(0, nrow(ind)))
    s <- tapply(a$alloue[a$ressource == r], paste(a$code_geo, a$cycle)[a$ressource == r], sum)
    v <- as.numeric(s[cle_i]); v[is.na(v)] <- 0; v
  }
  plus <- function(x, y) ifelse(is.na(x), ifelse(y > 0, y, NA), x + y)
  sal <- ajoute("salles")
  ind$nb_salles_t <- plus(ind$nb_salles_t, sal); ind$nb_salles_bon_etat <- plus(ind$nb_salles_bon_etat, sal)
  ens <- ajoute("enseignants")
  ind$nb_ens_t <- plus(ind$nb_ens_t, ens); ind$nb_ens_qualifies <- plus(ind$nb_ens_qualifies, ens)
  ind$nb_tables_bancs <- plus(ind$nb_tables_bancs, ajoute("tables_bancs"))
  ind$nb_latrines <- plus(ind$nb_latrines, ajoute("latrines"))
  ind$nb_etab_eau <- plus(ind$nb_etab_eau, ajoute("eau"))
  ind$nb_etab_electricite <- plus(ind$nb_etab_electricite, ajoute("electricite"))
  ind$nb_etab_cantine <- plus(ind$nb_etab_cantine, ajoute("cantines"))
  calculer_imde(calculer_indicateurs(ind, normes), bornes)
}
