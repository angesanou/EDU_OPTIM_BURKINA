# ==========================================================================
# MODULE 10 — OPTIMISATION DES RESSOURCES
# ==========================================================================

mod_optimisation_ui <- function(id) {
  ns <- NS(id)
  cu <- COUTS_DEFAUT
  tagList(
    p(class = "page-intro", "Moteur de recherche opérationnelle : il propose l'allocation des ressources qui réduit le plus le déficit pondéré par la priorité des territoires, sous contrainte de budget, de disponibilité et d'équité. Unité de décision : province × cycle."),
    # Réglages : bande horizontale en haut de page (comme les autres modules)
    div(class = "panneau-param mb-3",
        layout_columns(
          col_widths = breakpoints(sm = 12, md = 6, xl = c(3, 3, 3, 3)),
          div(tags$h6("Budget"),
              numericInput(ns("budget"), "Budget disponible (milliards de FCFA)", 100, min = 0, step = 5),
              radioButtons(ns("mode"), "Résolution", c("Relaxation et arrondi (rapide)" = "relaxation", "Programme entier exact (40 variables au plus)" = "exacte")),
              actionButton(ns("optimiser"), "Relancer l'optimisation", icon = icon("bolt"), class = "btn-primary w-100"),
              p(class = "eq-legende", style = "margin:6px 0 0", "Les résultats se mettent à jour automatiquement à chaque changement de réglage.")),
          div(tags$h6("Ressources à allouer"),
              checkboxGroupInput(ns("ressources"), NULL, setNames(cu$ressource, cu$libelle), selected = cu$ressource)),
          div(tags$h6("Cycles et arbitrages"),
              checkboxGroupInput(ns("cycles"), "Cycles concernés", setNames(names(CYCLES), CYCLES), selected = "PRIMAIRE", inline = TRUE),
              sliderInput(ns("lambda"), "Poids de l'équité territoriale (λ)", 0, 5, 0.5, step = 0.25, width = "100%"),
              sliderInput(ns("mu"), "Équilibre entre ressources (μ)", 0, 5, 1, step = 0.25, width = "100%"),
              sliderInput(ns("alpha"), "Besoin minimal couvert dans les zones critiques (α)", 0, 0.6, 0, step = 0.05, width = "100%")),
          div(tags$h6("Coûts unitaires et disponibilités"),
              tags$details(tags$summary(style = "font-size:14px;font-weight:650;cursor:pointer;margin-bottom:6px", "Afficher et modifier"),
                           lapply(seq_len(nrow(cu)), function(i) div(style = "display:flex;gap:8px",
                             numericInput(ns(paste0("c_", cu$ressource[i])), paste(cu$libelle[i], "(FCFA)"), cu$cout_unitaire[i], min = 0, width = "60%"),
                             numericInput(ns(paste0("d_", cu$ressource[i])), "Quantité max.", NA, min = 0, width = "40%")))),
              p(class = "eq-legende", "Coûts des salles et des forages documentés ; autres valeurs indicatives, à confirmer.")))),
    # Résultats sur toute la largeur
    uiOutput(ns("statut")),
    uiOutput(ns("kpis")),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      carte("Déficit couvert par ressource", plotlyOutput(ns("g_ressources"), height = 360),
            sous_titre = "unités manquantes et unités financées"),
      carte("Carte des investissements", leafletOutput(ns("carte"), height = 360), sous_titre = "milliards de FCFA par province")),
    carte("Allocation par province", plotlyOutput(ns("g_provinces"), height = 400),
          sous_titre = "les 25 provinces qui reçoivent le plus, coût par ressource"),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(5, 7)),
      carte("IMDE avant et après intervention", plotlyOutput(ns("g_imde"), height = 420),
            pied = "Sous la diagonale : amélioration. Recalcul de l'IMDE avec les mêmes bornes."),
      carte("Allocation détaillée", DTOutput(ns("t_alloc")))),
    carte("Modèle, hypothèses et limites", plein_ecran = FALSE,
          eq("max &nbsp;&sum;<sub><i>u</i></sub> &sum;<sub><i>r</i></sub> <i>w<sub>u</sub></i> <i>v<sub>r</sub></i> <i>x<sub>ur</sub></i> / <i>D<sub>r</sub></i> + &lambda; <i>z</i> &sum;<sub><i>r</i></sub> <i>v<sub>r</sub></i>",
             "Fonction objectif : part des déficits comblés, pondérée par la gravité de chaque province (poids égal à 0,5 + IMDE / 100) et par la valeur de chaque ressource."),
          div(class = "eq-contraintes",
              tags$table(
                tags$tr(tags$td(HTML("&sum; <i>c<sub>r</sub></i> <i>x<sub>ur</sub></i> &le; <i>B</i>")), tags$td("budget disponible")),
                tags$tr(tags$td(HTML("&sum;<sub><i>u</i></sub> <i>x<sub>ur</sub></i> &le; <i>A<sub>r</sub></i>")), tags$td("disponibilité de la ressource (enseignants recrutables, par exemple)")),
                tags$tr(tags$td(HTML("0 &le; <i>x<sub>ur</sub></i> &le; <i>d<sub>ur</sub></i>")), tags$td("pas d'allocation au-delà du besoin")),
                tags$tr(tags$td(HTML("&sum;<sub><i>u</i>&isin;<i>p</i>, <i>r</i></sub> <i>c<sub>r</sub></i> <i>x<sub>ur</sub></i> &ge; <i>z</i> <i>C<sub>p</sub></i>")), tags$td("couverture minimale de chaque province")),
                tags$tr(tags$td(HTML("<i>x<sub>ur</sub></i> &ge; &alpha; <i>d<sub>ur</sub></i>")), tags$td("besoin minimal des provinces en situation critique")),
                tags$tr(tags$td(HTML("&sum;<sub><i>u</i></sub> <i>x<sub>ur</sub></i> &ge; &zeta; <i>D<sub>r</sub></i>")), tags$td("couverture minimale commune à toutes les ressources")))),
          tags$ul(style = "font-size:14px;line-height:1.7;margin-top:10px",
                  tags$li("Le poids de gravité (0,5 + IMDE / 100) donne plus de valeur aux provinces les plus déficitaires ; le déficit total de chaque ressource sert à comparer des ressources de nature différente (salles, forages, kits solaires)."),
                  tags$li("Les gains sont mesurés sur les déficits calculés avec les normes : ils ne mesurent pas directement les apprentissages."),
                  tags$li("Coûts unitaires : salles et forages documentés, autres postes à confirmer ; ni entretien ni économies d'échelle ; un enseignant est compté pour une année de salaire."),
                  tags$li("Relaxation + arrondi : solution entière réalisable, à quelques unités de l'optimum entier ; la PLNE exacte est réservée aux petits problèmes."),
                  tags$li("Le modèle ne tient pas compte de la capacité d'exécution locale ni de la sécurité des chantiers."))),
    uiOutput(ns("mention"))
  )
}

mod_optimisation_server <- function(id, app) moduleServer(id, function(input, output, session) {
  cu <- COUTS_DEFAUT

  # Cadrage sur les données : sélection par défaut des seuls cycles et ressources
  # dont le déficit est calculable (stocks publiés par province).
  observe({
    p <- app$D()$ind$province
    cols <- intersect(RESSOURCES_OPTIM, names(p))
    cyc_ok <- names(CYCLES)[vapply(names(CYCLES), function(cy)
      any(!is.na(unlist(p[as.character(p$cycle) == cy, cols, drop = FALSE]))), logical(1))]
    res_ok <- names(RESSOURCES_OPTIM)[vapply(RESSOURCES_OPTIM, function(v) v %in% names(p) && any(!is.na(p[[v]])), logical(1))]
    if (length(cyc_ok)) updateCheckboxGroupInput(session, "cycles", selected = cyc_ok)
    if (length(res_ok)) updateCheckboxGroupInput(session, "ressources", selected = res_ok)
  })

  # Réglages lus ensemble, avec un délai d'une seconde : le calcul suit chaque
  # changement (budget, ressources, curseurs, coûts, année) sans être relancé à
  # chaque frappe. Le bouton force un nouveau calcul.
  reglages <- debounce(reactive(list(
    budget = input$budget, ressources = input$ressources, cycles = input$cycles,
    lambda = input$lambda, mu = input$mu, alpha = input$alpha, mode = input$mode,
    couts = lapply(cu$ressource, function(r) input[[paste0("c_", r)]]),
    dispo = lapply(cu$ressource, function(r) input[[paste0("d_", r)]]),
    annee = app$filtres()$annee, bouton = input$optimiser)), 1000)

  res <- reactive({
    g <- reglages(); D <- app$D()
    exiger(length(g$ressources) > 0, "Cochez au moins une ressource à allouer.")
    exiger(length(g$cycles) > 0, "Cochez au moins un cycle.")
    exiger(is.numeric(g$budget) && !is.na(g$budget) && g$budget >= 0, "Le budget doit être un nombre positif.")
    couts <- cu
    couts$cout_unitaire <- vapply(seq_len(nrow(cu)), function(i) g$couts[[i]] %||% cu$cout_unitaire[i], numeric(1))
    exiger(!any(is.na(couts$cout_unitaire) | couts$cout_unitaire <= 0), "Les coûts unitaires doivent être positifs.")
    dispo <- vapply(g$ressources, function(r) { v <- g$dispo[[match(r, cu$ressource)]]; if (is.null(v) || is.na(v)) Inf else v }, numeric(1))
    u <- do.call(rbind, lapply(g$cycles, function(cy) { x <- D$imde[[cy]]; x[x$annee_debut == g$annee, ] }))
    exiger(!is.null(u) && nrow(u) > 0, "Aucune donnée pour l'année choisie.")
    u$poids <- 0.5 + ifelse(is.na(u$imde), 0.5, u$imde / 100)
    cols <- intersect(RESSOURCES_OPTIM[g$ressources], names(u))
    sans <- g$cycles[vapply(g$cycles, function(cy)
      all(is.na(unlist(u[as.character(u$cycle) == cy, cols, drop = FALSE]))), logical(1))]
    if (length(sans)) showNotification(paste0("Aucun déficit calculable pour : ", paste(CYCLES[sans], collapse = ", "),
      " (stocks par province non publiés) : ces cycles ne reçoivent aucune allocation."), type = "warning", duration = 6, id = session$ns("sans_deficit"))
    withProgress(message = "Résolution du programme linéaire…", value = 0.5, {
      o <- optimiser_allocation(u, couts, g$budget * 1e9, g$ressources, dispo, lambda = g$lambda,
                                alpha = g$alpha, mu = g$mu, mode = g$mode)
      if (is.null(o$allocation)) {
        list(opt = o)
      } else {
        apres <- do.call(rbind, lapply(g$cycles, function(cy) {
          x <- u[as.character(u$cycle) == cy, ]
          appliquer_allocation(x, o$allocation, D$d$normes, DONNEES$bornes[[cy]])
        }))
        o$allocation$nom <- u$nom[match(o$allocation$code_geo, u$code_geo)]
        r <- list(opt = o, avant = u, apres = apres, couts = couts)
        app$resultats$optimisation <- r
        r
      }
    })
  })

  output$statut <- renderUI({
    o <- res()$opt
    alerte(if (is.null(o$allocation)) "critique" else "ok", tags$b("Statut : "), o$statut,
           if (!is.null(o$allocation)) sprintf(" Mode : %s.", o$mode))
  })

  output$kpis <- renderUI({
    r <- res(); req(!is.null(r$opt$allocation)); o <- r$opt
    w <- r$avant$eff_t
    layout_columns(
      col_widths = breakpoints(sm = 6, xl = 3),
      kpi("Coût total alloué", fmt_fcfa(o$cout_total), icone = "coins", sous_texte = sprintf("%s du budget", fmt_pct(o$cout_total / max(o$budget, 1), 0)),
          jauge = o$cout_total / max(o$budget, 1)),
      kpi("IMDE moyen après", fmt_nombre(moy_ponderee(r$apres$imde, w), 1), icone = "arrow-trend-down", ton = "vert",
          evol = moy_ponderee(r$apres$imde, w) / moy_ponderee(r$avant$imde, w) - 1, evol_bon_si_hausse = FALSE,
          sous_texte = sprintf("avant : %s", fmt_nombre(moy_ponderee(r$avant$imde, w), 1))),
      kpi("Unités critiques ou sévères", sprintf("%d → %d", sum(r$avant$classe_imde %in% c("Critique", "Sévère")),
                                                sum(r$apres$classe_imde %in% c("Critique", "Sévère"))), icone = "circle-exclamation", ton = "rouge"),
      kpi("Couverture minimale garantie", fmt_pct(o$couverture_min, 1), icone = "scale-balanced", ton = "violet",
          sous_texte = "de la province la moins servie"))
  })

  output$g_ressources <- renderPlotly({
    r <- res(); req(!is.null(r$opt$allocation)); x <- r$opt$par_ressource
    plot_ly(x, x = ~libelle, y = ~deficit_initial, type = "bar", name = "Déficit initial", marker = list(color = "#E4E9F0")) |>
      add_trace(y = ~alloue, name = "Alloué", marker = list(color = COULEURS$petrole),
                text = ~fmt_pct(couverture, 0), textposition = "outside") |>
      layout(barmode = "overlay", yaxis = list(type = "log")) |> plotly_edu(titre_y = "Unités (échelle log)")
  })

  output$g_provinces <- renderPlotly({
    r <- res(); req(!is.null(r$opt$allocation)); a <- r$opt$allocation
    tot <- tapply(a$cout_alloue, a$nom, sum); ordre <- names(sort(tot, decreasing = TRUE))[1:min(25, length(tot))]
    a <- a[a$nom %in% ordre, ]
    p <- plot_ly()
    for (i in seq_along(unique(a$ressource))) {
      rr <- unique(a$ressource)[i]; s <- aggregate(cout_alloue ~ nom, a[a$ressource == rr, ], sum)
      p <- add_trace(p, x = s$nom, y = s$cout_alloue / 1e9, type = "bar", name = cu$libelle[cu$ressource == rr],
                     marker = list(color = PALETTE_SERIES[i]))
    }
    p |> layout(barmode = "stack", xaxis = list(categoryorder = "array", categoryarray = ordre)) |>
      plotly_edu(titre_y = "Milliards de FCFA", marge_b = 60)
  })

  output$g_imde <- renderPlotly({
    r <- res(); req(!is.null(r$apres))
    k <- match(paste(r$avant$code_geo, r$avant$cycle), paste(r$apres$code_geo, r$apres$cycle))
    d <- data.frame(nom = paste0(r$avant$nom, " (", CYCLES[as.character(r$avant$cycle)], ")"), avant = r$avant$imde, apres = r$apres$imde[k], eff = r$avant$eff_t)
    plot_ly(d, x = ~avant, y = ~apres, type = "scatter", mode = "markers", text = ~nom, size = ~eff, sizes = c(5, 30),
            marker = list(color = COULEURS$petrole, opacity = 0.6, line = list(color = "#fff", width = 0.5)),
            hovertemplate = "%{text}<br>avant %{x:.1f} → après %{y:.1f}<extra></extra>") |>
      add_lines(x = c(0, 100), y = c(0, 100), line = list(color = "#8A98AB", dash = "dot"), inherit = FALSE, showlegend = FALSE) |>
      plotly_edu(titre_x = "IMDE avant", titre_y = "IMDE après", legende = FALSE)
  })

  output$carte <- renderLeaflet({
    r <- res(); req(!is.null(r$opt$allocation)); g <- app$D()$d$geometries$provinces
    exiger(!is.null(g), "Aucune géométrie.")
    a <- r$opt$allocation; s <- tapply(a$cout_alloue, a$code_geo, sum) / 1e9
    g$investissement <- as.numeric(s[g$code_geo]); g$investissement[is.na(g$investissement)] <- 0
    pal <- colorNumeric(c("#F4F6F9", "#9FD9D2", "#1FB5A8", "#0E6E7E", "#0B1A2E"), domain = g$investissement)
    bb <- sf::st_bbox(g)
    carte_base(CONFIG$application$fond_carte_en_ligne) |>
      addPolygons(data = g, fillColor = ~pal(investissement), fillOpacity = 0.9, color = "#fff", weight = 1,
                  label = sprintf("%s : %s Md FCFA", g$nom, fmt_nombre(g$investissement, 2))) |>
      addLegend("bottomright", pal = pal, values = g$investissement, title = "Md FCFA") |>
      fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]])
  })

  output$t_alloc <- renderDT({
    r <- res(); req(!is.null(r$opt$allocation)); a <- r$opt$allocation; a <- a[a$alloue > 0, ]
    tableau(data.frame(Province = a$nom, Cycle = unname(CYCLES[a$cycle]), Ressource = cu$libelle[match(a$ressource, cu$ressource)],
                       `Déficit` = a$deficit, `Alloué` = a$alloue, Couverture = fmt_pct(a$couverture, 0),
                       `Coût (M FCFA)` = round(a$cout_alloue / 1e6, 1), check.names = FALSE), page = 15, ordre = list(list(6, "desc")))
  })

  output$mention <- renderUI(mention_donnees(app$D()$d$meta))
})
