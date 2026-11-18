# ==========================================================================
# MODULE 9 — PRIORISATION TERRITORIALE (TOPSIS)
# ==========================================================================

mod_priorisation_ui <- function(id) {
  ns <- NS(id)
  cr <- CRITERES_PRIORISATION
  tagList(
    p(class = "page-intro", "Classement multicritère des provinces par la méthode TOPSIS : chaque province est située entre une situation « idéalement prioritaire » et une situation « la moins prioritaire ». Ajustez les poids pour refléter les arbitrages politiques ; l'analyse de sensibilité montre la stabilité du classement."),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(4, 8)),
      div(class = "panneau-param",
          tags$h6("Poids des critères"),
          lapply(seq_len(nrow(cr)), function(i)
            sliderInput(ns(paste0("w_", cr$code[i])), cr$libelle[i], 0, 0.5, cr$poids[i], step = 0.05)),
          actionLink(ns("reinit"), "Rétablir les poids par défaut", icon = icon("rotate-left")),
          uiOutput(ns("somme"))),
      div(
        uiOutput(ns("kpis")),
        carte("Score de priorité et contributions des critères", plotlyOutput(ns("g_top"), height = 520),
              sous_titre = "20 premières provinces",
              pied = "Contribution = part de chaque critère dans l'éloignement à la situation la moins prioritaire."))),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      carte("Carte des priorités", leafletOutput(ns("carte"), height = 430)),
      carte("Classement régional", plotlyOutput(ns("g_regions"), height = 430), sous_titre = "score moyen pondéré par la population scolaire")),
    carte("Classement national et justification", DTOutput(ns("t_classement"))),
    carte("Sensibilité aux pondérations (±30 %, 300 tirages)",
          actionButton(ns("lancer_sens"), "Lancer l'analyse de sensibilité", icon = icon("shuffle"), class = "btn-light btn-sm mb-2"),
          plotlyOutput(ns("g_sens"), height = 460)),
    carte("Critères et justification", plein_ecran = FALSE, DTOutput(ns("t_criteres")),
          note_methodo("Score TOPSIS : C = d⁻ / (d⁺ + d⁻), où d⁺ et d⁻ sont les distances euclidiennes (après normalisation vectorielle et pondération) à la situation la plus prioritaire et à la moins prioritaire. Valeurs manquantes remplacées par la médiane du critère.")),
    uiOutput(ns("mention"))
  )
}

mod_priorisation_server <- function(id, app) moduleServer(id, function(input, output, session) {
  cr <- CRITERES_PRIORISATION

  observeEvent(input$reinit, for (i in seq_len(nrow(cr))) updateSliderInput(session, paste0("w_", cr$code[i]), value = cr$poids[i]))
  poids <- reactive({
    w <- vapply(cr$code, function(k) input[[paste0("w_", k)]] %||% 0, numeric(1))
    if (sum(w) == 0) w[] <- 1
    w / sum(w)
  })
  output$somme <- renderUI(tags$small(style = "color:#8A98AB", "Les poids sont renormalisés pour totaliser 100 %."))

  mc <- reactive({
    D <- app$D(); f <- app$filtres()
    p <- panel_cycle(D, f$cycle); im <- D$imde[[f$cycle]]; im <- im[im$annee_debut == f$annee, ]
    if (nrow(im) && all(is.na(im$imde)))
      showNotification(paste("IMDE non calculable pour ce cycle : le critère « déficit » est neutralisé ;",
                             "le classement repose sur la population, la vulnérabilité, l'accessibilité,",
                             "la pression démographique et l'équité."), type = "warning", duration = 10)
    exiger(nrow(im) >= 2, "Au moins deux provinces sont nécessaires pour un classement.")
    matrice_criteres(p, im, annee = f$annee)
  })
  pri <- reactive({
    r <- prioriser(mc(), poids())
    app$resultats$priorisation <- r
    r
  })

  output$kpis <- renderUI({
    r <- pri()
    layout_columns(
      col_widths = breakpoints(sm = 6, xl = 4),
      kpi("Priorité maximale", sum(r$niveau_priorite == "Priorité maximale"), icone = "flag", ton = "rouge",
          sous_texte = paste(head(r$nom, 3), collapse = ", ")),
      kpi("Province n° 1", r$nom[1], icone = "ranking-star", ton = "orange", sous_texte = sprintf("score %s", fmt_nombre(r$score[1], 3))),
      kpi("Critère le plus discriminant", names(sort(table(r$critere_principal), decreasing = TRUE))[1], icone = "filter", ton = "violet",
          sous_texte = "le plus souvent déterminant"))
  })

  output$g_top <- renderPlotly({
    r <- head(pri(), 20)
    ctb <- attr(pri(), "contributions"); ctb <- ctb[match(r$code_geo, ctb$code_geo), ]
    p <- plot_ly()
    for (j in seq_len(nrow(cr))) {
      p <- add_trace(p, y = factor(r$nom, levels = rev(r$nom)), x = r$score * ctb[[cr$code[j]]], type = "bar", orientation = "h",
                     name = cr$libelle[j], marker = list(color = PALETTE_SERIES[j]),
                     hovertemplate = paste0("%{y}<br>", cr$libelle[j], " : %{x:.3f}<extra></extra>"))
    }
    p |> layout(barmode = "stack") |> plotly_edu(titre_x = "Score TOPSIS (0-1)")
  })

  output$carte <- renderLeaflet({
    g <- app$D()$d$geometries$provinces
    exiger(!is.null(g), "Aucune géométrie disponible.")
    g <- joindre_geo(g, pri()[, c("code_geo", "score", "rang_national", "niveau_priorite", "critere_principal")])
    pal <- colorNumeric(c("#F4F6F9", "#F3E3A6", "#F0A35E", "#D1495B"), domain = c(0, 1))
    bb <- sf::st_bbox(g)
    carte_base(CONFIG$application$fond_carte_en_ligne) |>
      addPolygons(data = g, fillColor = ~pal(score), fillOpacity = 0.88, color = "#fff", weight = 1,
                  label = lapply(sprintf("<b>%s</b><br>Rang %s, score %s<br>%s<br>Critère principal : %s", g$nom, g$rang_national,
                                         fmt_nombre(g$score, 3), g$niveau_priorite, g$critere_principal), HTML),
                  highlightOptions = highlightOptions(weight = 2.5, color = COULEURS$nuit)) |>
      addLegend("bottomright", pal = pal, values = c(0, 1), title = "Score de priorité") |>
      fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]])
  })

  output$g_regions <- renderPlotly({
    r <- pri()
    s <- tapply(r$score * r$population, r$code_region, sum) / tapply(r$population, r$code_region, sum)
    x <- data.frame(nom = REGIONS[names(s)], score = as.numeric(s)); x <- x[order(x$score), ]
    plot_ly(x, x = ~score, y = ~factor(nom, levels = nom), type = "bar", orientation = "h",
            marker = list(color = colorRampPalette(c("#F3E3A6", "#D1495B"))(nrow(x))),
            hovertemplate = "%{y}<br>score moyen %{x:.3f}<extra></extra>") |>
      plotly_edu(titre_x = "Score moyen", legende = FALSE)
  })

  output$t_classement <- renderDT({
    r <- pri()
    tableau(data.frame(`Rang national` = r$rang_national, Province = r$nom, `Région` = REGIONS[r$code_region],
                       `Rang régional` = r$rang_regional, Score = round(r$score, 3), Niveau = r$niveau_priorite,
                       IMDE = round(r$deficit, 1), `Pop. scolaire` = fmt_nombre(r$population),
                       `Vulnérabilité` = round(r$vulnerabilite, 3), `Établissements pour 100 km²` = round(r$accessibilite, 2),
                       `Croissance démographique (%/an)` = round(r$pression, 2), `Coût par élève (FCFA)` = fmt_nombre(r$cout),
                       `Critère principal` = r$critere_principal, check.names = FALSE), page = 15)
  })

  sens <- eventReactive(input$lancer_sens, {
    withProgress(message = "Sensibilité des priorités…", value = 0.4, sensibilite_priorites(mc(), poids(), n = 300))
  })
  output$g_sens <- renderPlotly({
    s <- sens(); s <- head(s[order(s$rang), ], 25)
    plot_ly(s, y = ~factor(nom, levels = rev(nom))) |>
      add_segments(x = ~rang_min, xend = ~rang_max, yend = ~factor(nom, levels = rev(nom)),
                   line = list(color = "#F3D2A6", width = 6), name = "Rang min – max", hoverinfo = "none") |>
      add_markers(x = ~rang, marker = list(color = COULEURS$nuit, size = 8), name = "Rang actuel",
                  text = ~sprintf("%s<br>rang %d (min %d, max %d)<br>P(top 10) = %s", nom, rang, rang_min, rang_max, fmt_pct(prob_top10, 0)),
                  hoverinfo = "text") |>
      plotly_edu(titre_x = "Rang (1 = priorité la plus forte)")
  })

  output$t_criteres <- renderDT({
    w <- poids()
    tableau(data.frame(`Critère` = cr$libelle, Sens = ifelse(cr$sens > 0, "À maximiser", "À minimiser"),
                       `Poids actuel` = fmt_pct(w, 0), Justification = cr$explication, check.names = FALSE), boutons = FALSE)
  })

  output$mention <- renderUI(mention_donnees(app$D()$d$meta))
})
