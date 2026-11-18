# ==========================================================================
# MODULE 4 — DIAGNOSTIC ÉDUCATIF
# ==========================================================================

INDICATEURS_DIAG <- c(
  setNames(CATALOGUE_INDICATEURS$code, CATALOGUE_INDICATEURS$libelle),
  "Déficit en salles (unités)" = "deficit_salles", "Déficit en enseignants (unités)" = "deficit_enseignants",
  "Déficit en tables-bancs (unités)" = "deficit_tables_bancs", "Déficit en latrines (cabines)" = "deficit_latrines",
  "Établissements sans eau" = "deficit_eau", "Établissements sans électricité" = "deficit_electricite",
  "Établissements sans cantine" = "deficit_cantines")
INDIC_POURCENT <- c("part_salles_mauvais", "taux_deficit_salles", "part_ens_non_qualifies", "part_sans_eau",
                    "part_sans_electricite", "part_sans_cantine", "taux_fonctionnalite", "part_eleves_affectes")

mod_diagnostic_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(class = "page-intro", "Analyse des déficits par niveau territorial. Les déficits sont calculés au niveau le plus fin puis additionnés : un excédent dans un territoire ne compense pas le manque d'un autre."),
    div(class = "panneau-param mb-3",
        layout_columns(
          col_widths = breakpoints(sm = 12, md = c(3, 4, 5)),
          radioButtons(ns("niveau"), "Niveau d'analyse", c("Région", "Province", "Commune", "Établissement"),
                       selected = "Province", inline = TRUE),
          selectizeInput(ns("regions"), "Filtrer les régions", choices = NULL, multiple = TRUE,
                         options = list(placeholder = "Toutes les régions")),
          selectInput(ns("indicateur"), "Indicateur à comparer", INDICATEURS_DIAG, "ratio_eleves_salle_utilisable", selectize = FALSE))),
    uiOutput(ns("kpis")),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      carte("Territoires les plus vulnérables", plotlyOutput(ns("g_top"), height = 420), sous_titre = "15 situations les plus défavorables"),
      carte("Pression sur les salles et les enseignants", plotlyOutput(ns("g_nuage"), height = 420),
            sous_titre = "taille = élèves ; couleur = classe IMDE",
            pied = "Les lignes pointillées indiquent les normes. En haut à droite : double pression (salles et enseignants).")),
    carte("Tableau de diagnostic", DTOutput(ns("t_diag")), sous_titre = "recherche, tri et export"),
    uiOutput(ns("mention"))
  )
}

mod_diagnostic_server <- function(id, app) moduleServer(id, function(input, output, session) {

  observe({
    r <- unique(app$D()$ind$province$code_region)
    updateSelectizeInput(session, "regions", choices = setNames(r, REGIONS[r]), server = FALSE)
  })

  donnees <- reactive({
    D <- app$D(); f <- app$filtres(); niv <- input$niveau
    x <- switch(niv,
      "Région" = { r <- D$ind$region; r[as.character(r$cycle) == f$cycle & r$annee_debut == f$annee, ] },
      "Province" = { p <- D$imde[[f$cycle]]; p[p$annee_debut == f$annee, ] },
      "Commune" = {
        exiger(!is.null(D$imde_communes), "Niveau communal indisponible : aucun fichier d'établissements pour cette source.")
        c <- D$imde_communes[[f$cycle]]; c[c$annee_debut == f$annee, ] },
      "Établissement" = {
        e <- D$ind$etablissements
        exiger(!is.null(e), "Niveau établissement indisponible : aucun fichier d'établissements pour cette source.")
        e <- e[e$annee_debut == f$annee & (f$cycle == "TOUS" | as.character(e$cycle) == f$cycle), ]
        e$nom <- paste0(e$id_etab, " (", e$nom_commune, ")"); e$code_geo <- e$id_etab
        e })
    if (length(input$regions)) {
      col <- if (niv == "Région") "code_geo" else "code_region"
      x <- x[x[[col]] %in% input$regions, ]
    }
    if (!"classe_imde" %in% names(x)) x$classe_imde <- NA
    x
  })

  output$kpis <- renderUI({
    x <- donnees(); req(nrow(x) > 0)
    # Somme, ou mention explicite quand la variable n'est pas publiée par province
    s <- function(v) if (!v %in% names(x) || all(is.na(x[[v]]))) "Non publié" else fmt_nombre(sum(x[[v]], na.rm = TRUE))
    layout_columns(
      col_widths = breakpoints(sm = 6, lg = 4, xl = 3),
      kpi("Salles manquantes", s("deficit_salles"), icone = "door-open", ton = "rouge"),
      kpi("Enseignants manquants", s("deficit_enseignants"), icone = "chalkboard-user", ton = "rouge"),
      kpi("Établissements sans eau", s("deficit_eau"), icone = "droplet", ton = "petrole"),
      kpi("Établissements sans électricité", s("deficit_electricite"), icone = "bolt", ton = "violet"),
      kpi("Établissements sans cantine", s("deficit_cantines"), icone = "utensils", ton = "violet"),
      kpi("Tables-bancs manquants", s("deficit_tables_bancs"), icone = "chair", ton = "orange"),
      kpi("Cabines de latrines manquantes", s("deficit_latrines"), icone = "restroom", ton = "orange"))
  })

  output$g_top <- renderPlotly({
    x <- donnees(); v <- input$indicateur; req(v %in% names(x))
    sens <- if (v %in% CATALOGUE_INDICATEURS$code) CATALOGUE_INDICATEURS$sens[CATALOGUE_INDICATEURS$code == v] else 1
    x <- x[!is.na(x[[v]]), ]
    exiger(nrow(x) > 0, "Indicateur non renseigné pour cette sélection.")
    x <- head(x[order(-sens * x[[v]]), ], 15)
    pct <- v %in% INDIC_POURCENT
    val <- if (pct) 100 * x[[v]] else x[[v]]
    x$nom <- make.unique(as.character(x$nom))
    plot_ly(x, x = val, y = ~factor(nom, levels = rev(nom)), type = "bar", orientation = "h",
            marker = list(color = colorRampPalette(c(PALETTE_DEFICIT[5], PALETTE_DEFICIT[3]))(nrow(x))),
            hovertemplate = paste0("%{y}<br>%{x:,.1f}", if (pct) " %" else "", "<extra></extra>")) |>
      plotly_edu(titre_x = names(INDICATEURS_DIAG)[INDICATEURS_DIAG == v], legende = FALSE)
  })

  output$g_nuage <- renderPlotly({
    x <- donnees(); f <- app$filtres(); D <- app$D()
    x <- x[!is.na(x$ratio_eleves_salle_utilisable) & !is.na(x$ratio_eleves_ens) & is.finite(x$ratio_eleves_salle_utilisable), ]
    exiger(nrow(x) > 0, "Données insuffisantes.")
    if (nrow(x) > 4000) x <- x[sample(nrow(x), 4000), ]
    cyc <- if (f$cycle == "TOUS") "PRIMAIRE" else f$cycle
    tc <- norme(D$d$normes, "TAILLE_CLASSE_MAX", cyc); re <- norme(D$d$normes, "RATIO_ELEVES_ENSEIGNANT", cyc)
    cl <- as.character(x$classe_imde); cl[is.na(cl)] <- "Non classé"
    pal <- c(setNames(PALETTE_DEFICIT, CLASSES_IMDE), "Non classé" = COULEURS$petrole)
    plot_ly(x, x = ~ratio_eleves_salle_utilisable, y = ~ratio_eleves_ens, type = "scatter", mode = "markers",
            color = cl, colors = pal, size = ~eff_t, sizes = c(6, 40),
            marker = list(opacity = 0.75, line = list(color = "#fff", width = 0.6)),
            text = ~nom, hovertemplate = "%{text}<br>Élèves/salle : %{x:.1f}<br>Élèves/ens. : %{y:.1f}<extra></extra>") |>
      layout(shapes = list(
        list(type = "line", x0 = tc, x1 = tc, y0 = 0, y1 = 1, yref = "paper", line = list(dash = "dot", color = "#8A98AB")),
        list(type = "line", y0 = re, y1 = re, x0 = 0, x1 = 1, xref = "paper", line = list(dash = "dot", color = "#8A98AB")))) |>
      plotly_edu(titre_x = "Élèves par salle utilisable", titre_y = "Élèves par enseignant")
  })

  output$t_diag <- renderDT({
    x <- donnees(); req(nrow(x) > 0)
    out <- data.frame(Territoire = x$nom, `Élèves` = x$eff_t,
                      `Élèves par salle utilisable` = round(x$ratio_eleves_salle_utilisable, 1), `Élèves par enseignant` = round(x$ratio_eleves_ens, 1),
                      `Élèves par table-banc` = round(x$ratio_eleves_table_banc, 1),
                      `Salles manquantes` = x$deficit_salles, `Enseignants manquants` = x$deficit_enseignants,
                      `Tables-bancs manquants` = x$deficit_tables_bancs, `Cabines de latrines manquantes` = x$deficit_latrines,
                      `Sans eau` = x$deficit_eau, `Sans électricité` = x$deficit_electricite, `Sans cantine` = x$deficit_cantines,
                      check.names = FALSE)
    if ("imde" %in% names(x) && any(!is.na(x$imde))) {
      out$IMDE <- round(x$imde, 1); out$Classe <- badge_classe(x$classe_imde)
    }
    tableau(out, page = 15, escape = FALSE, ordre = list(list(if ("IMDE" %in% names(out)) ncol(out) - 2 else 5, "desc")))
  })

  output$mention <- renderUI(mention_donnees(app$D()$d$meta))
})
