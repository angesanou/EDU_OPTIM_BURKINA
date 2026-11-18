# ==========================================================================
# MODULE 1 — ACCUEIL
# ==========================================================================

mod_accueil_ui <- function(id) {
  ns <- NS(id)
  tuiles <- lapply(PAGES$id[-1], function(p) {
    i <- which(PAGES$id == p)
    desc <- c(tableau_bord = "Indicateurs nationaux, régions, tendances, urbain/rural.",
              donnees = "Jeux de données, qualité, doublons, dictionnaire.",
              diagnostic = "Déficits par région, province, commune, établissement.",
              imde = "Indice multidimensionnel, sensibilité et robustesse.",
              cartographie = "Cartes, densité, accessibilité et regroupements géographiques.",
              econometrie = "Panel à effets fixes, tests, bootstrap.",
              previsions = "Projections des effectifs et des besoins, backtesting.",
              priorisation = "Classement multicritère TOPSIS des territoires.",
              optimisation = "Allocation optimale sous contrainte budgétaire.",
              simulations = "Comparaison de scénarios de politique.",
              rapports = "PDF, Excel, CSV, fiches territoriales.",
              documentation = "Formules, méthodes, hypothèses et limites.",
              utilisateurs = "Profils, périmètres et comptes.")[[p]]
    div(class = "carte tuile", `data-page` = p, role = "button", tabindex = "0",
        div(class = "ic", icon(PAGES$icone[i])), div(h4(PAGES$court[i]), p(desc)))
  })
  tagList(
    uiOutput(ns("heros")),
    section("Indicateurs clés", "Année et cycle choisis dans l'en-tête"),
    uiOutput(ns("kpis")),
    layout_columns(
      col_widths = c(7, 5),
      carte("Alertes principales", uiOutput(ns("alertes")), sous_titre = "détectées automatiquement", plein_ecran = FALSE),
      carte("Objectifs stratégiques", plein_ecran = FALSE,
            alerte("info", tags$b("Équité territoriale"), " : réduire les écarts entre territoires les mieux et les moins dotés.", icone = "scale-balanced"),
            alerte("info", tags$b("Conditions d'apprentissage"), " : salles en bon état, enseignants qualifiés, eau, latrines, électricité.", icone = "school"),
            alerte("info", tags$b("Anticipation"), " : prévoir les besoins liés à la croissance démographique et aux déplacements.", icone = "arrow-trend-up"),
            alerte("info", tags$b("Efficience budgétaire"), " : maximiser l'impact de chaque franc investi.", icone = "coins"),
            alerte("info", tags$b("Transparence"), " : méthodes documentées, résultats reproductibles.", icone = "magnifying-glass"))),
    section("Accès rapide aux modules"),
    do.call(layout_columns, c(tuiles, list(col_widths = breakpoints(sm = 6, lg = 4, xl = 3)))),
    section("État du système éducatif"),
    carte("Synthèse automatique", uiOutput(ns("resume")), plein_ecran = FALSE),
    uiOutput(ns("mention"))
  )
}

mod_accueil_server <- function(id, app) moduleServer(id, function(input, output, session) {

  nat <- reactive({
    n <- app$D()$ind$national; f <- app$filtres()
    n[as.character(n$cycle) == f$cycle & n$annee_debut == f$annee, ]
  })
  nat_prec <- reactive({
    n <- app$D()$ind$national; f <- app$filtres()
    n[as.character(n$cycle) == f$cycle & n$annee_debut == f$annee - 1, ]
  })
  im <- reactive({
    x <- app$D()$imde[[app$filtres()$cycle]]
    x[x$annee_debut == app$filtres()$annee, ]
  })

  output$heros <- renderUI({
    D <- app$D(); m <- D$d$meta
    n_etab <- if (!is.null(D$ind$etablissements)) length(unique(D$ind$etablissements$id_etab)) else
      sum(nat()$nb_etab_t, na.rm = TRUE)
    div(class = "heros",
        div(class = "surtitre", "BURKINA FASO"),
        h2("Mieux répartir les ressources éducatives là où les besoins sont les plus forts"),
        p("EDU-OPTIM relie le diagnostic territorial, un indice multidimensionnel de déficit, les prévisions de besoins,",
          "la priorisation multicritère et l'optimisation budgétaire dans un même outil, du préscolaire au secondaire."),
        div(class = "meta",
            div(tags$b(fmt_nombre(n_etab)), "établissements suivis"),
            div(tags$b(paste(min(ANNEES), "–", max(ANNEES) + 1)), "période couverte"),
            div(tags$b(format(as.Date(m$date_fichier), "%d/%m/%Y")), "dernière mise à jour des données"),
            div(tags$b(if (EST_SYNTHETIQUE) "Synthétiques" else "Collecte"), "nature des données"),
            div(tags$b(libelle_perimetre(app$utilisateur(), DONNEES$d$geo)), "périmètre")))
  })

  output$kpis <- renderUI({
    n <- nat(); p <- nat_prec(); x <- im()
    exiger(nrow(n) == 1, "Aucune donnée pour cette année et ce cycle.")
    imde_m <- moy_ponderee(x$imde, x$eff_t)
    layout_columns(
      col_widths = breakpoints(sm = 6, xl = 3),
      kpi("Élèves scolarisés", fmt_compact(n$eff_t), icone = "children",
          evol = if (nrow(p)) diviser(n$eff_t, p$eff_t) - 1, sous_texte = "par rapport à l'année précédente"),
      kpi(if (is.na(n$taux_fonctionnalite)) "Établissements recensés" else "Établissements fonctionnels",
          fmt_nombre(if (is.na(n$nb_etab_fonctionnels)) n$nb_etab_t else n$nb_etab_fonctionnels), icone = "school", ton = "vert",
          sous_texte = if (is.na(n$taux_fonctionnalite)) "fermetures non renseignées cette année (relevé ESU : 2021)"
                       else sprintf("%s fermé(s), %s fonctionnels", fmt_nombre(n$nb_etab_fermes), fmt_pct(n$taux_fonctionnalite, 0)),
          jauge = n$taux_fonctionnalite, couleur_jauge = COULEURS$vert),
      kpi("IMDE moyen", fmt_nombre(imde_m, 1), "/ 100", icone = "layer-group", ton = "orange",
          sous_texte = "pondéré par les effectifs", jauge = imde_m / 100, couleur_jauge = COULEURS$orange),
      kpi("Provinces en situation critique", sum(x$classe_imde == "Critique", na.rm = TRUE), icone = "triangle-exclamation",
          ton = "rouge", sous_texte = sprintf("et %d en situation sévère, sur %d", sum(x$classe_imde == "Sévère", na.rm = TRUE), nrow(x))))
  })

  output$alertes <- renderUI({
    n <- nat(); p <- nat_prec(); x <- im(); f <- app$filtres(); D <- app$D()
    req(nrow(n) == 1)
    al <- list()
    crit <- x[x$classe_imde %in% "Critique", ]
    if (nrow(crit)) al[[length(al) + 1]] <- alerte("critique", tags$b(sprintf("%d province(s) en déficit critique : ", nrow(crit))),
                                                   paste(head(crit$nom[order(-crit$imde)], 5), collapse = ", "),
                                                   if (nrow(crit) > 5) sprintf(" et %d autre(s)", nrow(crit) - 5))
    if (isTRUE(n$nb_etab_fermes > 0))
      al[[length(al) + 1]] <- alerte("critique", tags$b(sprintf("%s établissements fermés", fmt_nombre(n$nb_etab_fermes))),
                                     sprintf(" (%s du réseau), soit %s élèves touchés.", fmt_pct(1 - n$taux_fonctionnalite),
                                             fmt_nombre(n$nb_eleves_affectes)))
    tc <- norme(D$d$normes, "TAILLE_CLASSE_MAX", if (f$cycle == "TOUS") "PRIMAIRE" else f$cycle)
    if (isTRUE(n$ratio_eleves_salle_utilisable > tc))
      al[[length(al) + 1]] <- alerte("avert", tags$b(sprintf("%s élèves par salle utilisable", fmt_nombre(n$ratio_eleves_salle_utilisable, 1))),
                                     sprintf(" au-delà de la norme de %s ; déficit estimé : %s salles.", tc, fmt_nombre(n$deficit_salles)))
    if (nrow(p) && isTRUE(n$deficit_enseignants > p$deficit_enseignants))
      al[[length(al) + 1]] <- alerte("avert", tags$b("Le déficit en enseignants s'aggrave : "),
                                     sprintf("%s contre %s l'année précédente.", fmt_nombre(n$deficit_enseignants), fmt_nombre(p$deficit_enseignants)))
    q <- DONNEES$controles$panel
    nq <- sum(q$statut != "Conforme")
    if (nq) al[[length(al) + 1]] <- alerte("info", tags$b(sprintf("%d contrôle(s) de qualité à examiner", nq)),
                                           " dans le module Données et sources.")
    if (nrow(p) && isTRUE(n$taux_eau > p$taux_eau))
      al[[length(al) + 1]] <- alerte("ok", tags$b("Progrès : "), sprintf("couverture en eau de %s (+%s points).",
                                     fmt_pct(n$taux_eau), fmt_nombre(100 * (n$taux_eau - p$taux_eau), 1)))
    if (!length(al)) al <- list(alerte("ok", "Aucune alerte pour cette sélection."))
    tagList(al)
  })

  output$resume <- renderUI({
    n <- nat(); x <- im(); f <- app$filtres(); req(nrow(n) == 1)
    pire <- x[which.max(x$imde), ]; mieux <- x[which.min(x$imde), ]
    reg <- app$D()$ind$region
    reg <- reg[as.character(reg$cycle) == f$cycle & reg$annee_debut == f$annee, ]
    div(style = "font-size:14px;line-height:1.7;color:#22344D",
        p(HTML(paste0(sprintf("En <b>%s</b>, pour le cycle <b>%s</b>, le système compte <b>%s élèves</b>%s%s",
                       annee_scolaire(f$annee), tolower(libelle_cycle(f$cycle)), fmt_nombre(n$eff_t),
                       if (is.na(n$nb_etab_t)) "" else sprintf(" dans <b>%s établissements</b>", fmt_nombre(n$nb_etab_t)),
                       if (is.na(n$nb_etab_fermes)) "" else sprintf(" (dont %s fermés)", fmt_nombre(n$nb_etab_fermes))),
                       if (is.na(n$nb_ens_t) && is.na(n$nb_salles_t)) ". Les enseignants et les salles de classe ne sont pas publiés par province pour ce cycle."
                       else sprintf(", encadrés par <b>%s enseignants</b> dans <b>%s salles de classe</b>.", fmt_nombre(n$nb_ens_t), fmt_nombre(n$nb_salles_t))))),
        if (!is.na(n$ratio_eleves_ens) || !is.na(n$ratio_eleves_salle_utilisable)) p(HTML(sprintf("On compte en moyenne <b>%s élèves par enseignant</b> et <b>%s élèves par salle utilisable</b>. %s des établissements fonctionnels disposent d'un point d'eau, %s de l'électricité et %s de latrines.",
                       fmt_nombre(n$ratio_eleves_ens, 1), fmt_nombre(n$ratio_eleves_salle_utilisable, 1),
                       fmt_pct(n$taux_eau, 0), fmt_pct(n$taux_electricite, 0), fmt_pct(n$taux_latrines, 0)))),
        if (nrow(x)) p(HTML(sprintf("Les écarts territoriaux sont marqués : l'IMDE va de <b>%s</b> (%s) à <b>%s</b> (%s). L'indice de Gini de l'IMDE entre provinces vaut %s.",
                                    fmt_nombre(mieux$imde, 1), mieux$nom, fmt_nombre(pire$imde, 1), pire$nom, fmt_nombre(gini(x$imde), 2)))),
        if (sum(!is.na(reg$ratio_eleves_salle_utilisable)) > 1) p(HTML(sprintf("Région la plus en difficulté sur le ratio élèves/salle : <b>%s</b> (%s) ; la mieux dotée : <b>%s</b> (%s).",
                                          reg$nom[which.max(reg$ratio_eleves_salle_utilisable)], fmt_nombre(max(reg$ratio_eleves_salle_utilisable, na.rm = TRUE), 1),
                                          reg$nom[which.min(reg$ratio_eleves_salle_utilisable)], fmt_nombre(min(reg$ratio_eleves_salle_utilisable, na.rm = TRUE), 1)))))
  })

  output$mention <- renderUI(mention_donnees(app$D()$d$meta))
})
