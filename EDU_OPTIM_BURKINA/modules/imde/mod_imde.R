# ==========================================================================
# MODULE 5 — INDICE MULTIDIMENSIONNEL DE DÉFICIT ÉDUCATIF (IMDE)
# ==========================================================================

mod_imde_ui <- function(id) {
  ns <- NS(id)
  dims <- names(DIMENSIONS_IMDE)
  tagList(
    p(class = "page-intro", "L'IMDE résume ", length(unlist(DIMENSIONS_IMDE)), " indicateurs en ", length(DIMENSIONS_IMDE), " dimensions sur une échelle de 0 (aucun déficit) à 100 (déficit maximal). Les bornes de normalisation sont fixes sur toute la période : l'indice est comparable d'une année à l'autre. Modifiez la méthode pour tester la robustesse des classements."),
    div(class = "panneau-param mb-3",
        layout_columns(
          col_widths = breakpoints(sm = 12, md = c(2, 3, 2, 2, 3)),
          radioButtons(ns("niveau"), "Niveau", c("Province", "Commune"), inline = TRUE),
          selectInput(ns("normalisation"), "Normalisation",
                      c("Min-max à bornes fixes (principale)" = "minmax", "Z-score" = "zscore", "Rangs" = "rang"), selectize = FALSE),
          selectInput(ns("agregation"), "Agrégation",
                      c("Arithmétique (principale)" = "arithmetique", "Géométrique" = "geometrique"), selectize = FALSE),
          selectInput(ns("poids"), "Pondération",
                      c("Poids égaux (principale)" = "egal", "ACP" = "acp", "Entropie" = "entropie", "Personnalisée" = "perso"), selectize = FALSE),
          uiOutput(ns("poids_affiches"))),
        conditionalPanel(sprintf("input['%s'] == 'perso'", ns("poids")),
                         do.call(layout_columns, c(lapply(seq_along(dims), function(i)
                           sliderInput(ns(paste0("w", i)), dims[i], 0, 1, 0.2, step = 0.05)), list(col_widths = breakpoints(sm = 12, md = c(3, 2, 2, 3, 2))))))),
    uiOutput(ns("kpis")),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(7, 5)),
      carte("Carte de l'IMDE", leafletOutput(ns("carte"), height = 470),
            pied = libelle_limites()),
      carte("Classement", plotlyOutput(ns("g_classement"), height = 470), sous_titre = "25 territoires les plus déficitaires")),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(7, 5)),
      carte("Profil par dimension", plotlyOutput(ns("g_heatmap"), height = 460), sous_titre = "score 0-100 par dimension"),
      carte("Répartition par classe dans le temps", plotlyOutput(ns("g_classes"), height = 460))),
    carte("Évolution de l'IMDE",
          selectizeInput(ns("suivi"), NULL, choices = NULL, multiple = TRUE, width = "100%",
                         options = list(placeholder = "Choisir des territoires (5 plus déficitaires par défaut)", maxItems = 8)),
          plotlyOutput(ns("g_evolution"), height = 320)),
    section("Sensibilité et robustesse", "Le classement dépend-il des choix méthodologiques ?"),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(7, 5)),
      carte("Sensibilité aux poids (500 tirages aléatoires de Dirichlet)",
            actionButton(ns("lancer_sens"), "Lancer l'analyse", icon = icon("shuffle"), class = "btn-light btn-sm mb-2"),
            plotlyOutput(ns("g_sensibilite"), height = 430),
            pied = "Barre = intervalle de rang 5 %–95 % ; point = rang avec la méthode principale. Une barre courte = classement robuste."),
      carte("Robustesse : 6 variantes méthodologiques", plotlyOutput(ns("g_robustesse"), height = 300),
            uiOutput(ns("robustesse_txt")), sous_titre = "corrélations de Spearman entre classements")),
    carte("Tableau complet", DTOutput(ns("t_imde"))),
    carte("Méthode", plein_ecran = FALSE, uiOutput(ns("methode"))),
    uiOutput(ns("mention"))
  )
}

mod_imde_server <- function(id, app) moduleServer(id, function(input, output, session) {
  dims <- names(DIMENSIONS_IMDE)

  poids <- reactive({
    if (input$poids != "perso") return(input$poids)
    w <- vapply(seq_along(dims), function(i) input[[paste0("w", i)]] %||% 0.2, numeric(1))
    if (sum(w) == 0) w <- rep(1, length(w))
    setNames(w, dims)
  })
  defaut <- reactive(input$normalisation == "minmax" && input$agregation == "arithmetique" && identical(poids(), "egal"))

  # Panel complet (toutes années) avec IMDE selon la méthode choisie
  imde_tout <- reactive({
    D <- app$D(); cy <- app$filtres()$cycle
    if (input$niveau == "Province") {
      exiger(any(!is.na(D$imde[[cy]]$imde)), MESSAGE_SANS_IMDE)
      if (defaut()) return(D$imde[[cy]])
      base <- panel_cycle(D, cy); b <- DONNEES$bornes[[cy]]
    } else {
      exiger(!is.null(D$imde_communes), "Niveau communal indisponible pour cette source de données.")
      if (defaut()) return(D$imde_communes[[cy]])
      base <- D$ind$commune[as.character(D$ind$commune$cycle) == cy, ]
      tout <- DONNEES$ind$commune; b <- bornes_imde(tout[as.character(tout$cycle) == cy, ])
    }
    x <- calculer_imde(base, b, input$normalisation, input$agregation, poids())
    x$rang <- rang_imde(x)
    x
  })
  imde_annee <- reactive({ x <- imde_tout(); x[x$annee_debut == app$filtres()$annee, ] })

  output$poids_affiches <- renderUI({
    w <- attr(imde_tout(), "poids")
    if (is.null(w)) w <- setNames(rep(0.2, 5), dims)
    div(tags$label(class = "control-label", "Poids effectifs"),
        div(style = "font-size:14px;color:#5A6B80;line-height:1.5",
            HTML(paste(sprintf("%s : <b>%s</b>", substr(names(w), 1, 18), fmt_pct(w, 0)), collapse = "<br>"))))
  })

  observe({
    x <- imde_annee(); x <- x[order(-x$imde), ]
    updateSelectizeInput(session, "suivi", choices = setNames(x$code_geo, x$nom), selected = head(x$code_geo, 5), server = TRUE)
  })

  output$kpis <- renderUI({
    x <- imde_annee(); exiger(nrow(x) > 0, "Aucune donnée.")
    moy_dim <- vapply(dims, function(d) moy_ponderee(x[[paste0("dim_", d)]], x$eff_t), numeric(1))
    layout_columns(
      col_widths = breakpoints(sm = 6, xl = 3),
      kpi("IMDE moyen", fmt_nombre(moy_ponderee(x$imde, x$eff_t), 1), "/ 100", icone = "layer-group", ton = "orange",
          sous_texte = "pondéré par les effectifs", jauge = moy_ponderee(x$imde, x$eff_t) / 100, couleur_jauge = COULEURS$orange),
      kpi("Territoires critiques", sum(x$classe_imde == "Critique", na.rm = TRUE), icone = "circle-exclamation", ton = "rouge",
          sous_texte = sprintf("IMDE ≥ %s ; %d sévères", SEUILS_IMDE[5], sum(x$classe_imde == "Sévère", na.rm = TRUE))),
      kpi("Dimension la plus déficitaire", names(which.max(moy_dim)), icone = "magnifying-glass-chart", ton = "violet",
          sous_texte = sprintf("score moyen %s / 100", fmt_nombre(max(moy_dim), 1))),
      kpi("Inégalité entre territoires", fmt_nombre(gini(x$imde), 3), icone = "scale-unbalanced", ton = "petrole",
          sous_texte = "indice de Gini de l'IMDE"))
  })

  output$carte <- renderLeaflet({
    D <- app$D(); x <- imde_annee()
    couche <- if (input$niveau == "Province") D$d$geometries$provinces else D$d$geometries$communes
    exiger(!is.null(couche), "Aucune géométrie disponible : ajoutez un fichier de limites dans config.yml.")
    g <- joindre_geo(couche, x[, c("code_geo", "imde", "classe_imde", "rang", "eff_t", paste0("dim_", dims))])
    pal <- pal_imde()
    detail <- do.call(paste, c(lapply(dims, function(d) sprintf("%s : %s", d, fmt_nombre(g[[paste0("dim_", d)]], 0))), sep = "<br>"))
    lab <- sprintf("<b>%s</b><br>IMDE : %s (%s)<br>Rang : %s<br>%s", g$nom, fmt_nombre(g$imde, 1), g$classe_imde, g$rang, detail)
    carte_base(CONFIG$application$fond_carte_en_ligne) |>
      addPolygons(data = g, fillColor = ~pal(imde), fillOpacity = 0.85, color = "#FFFFFF", weight = 1,
                  label = lapply(lab, HTML), highlightOptions = highlightOptions(weight = 2.5, color = COULEURS$nuit, bringToFront = TRUE)) |>
      addLegend("bottomright", pal = pal, values = c(0, 100), title = "IMDE", opacity = 0.9,
                labFormat = function(type, cuts, p) paste0(CLASSES_IMDE, " (", head(cuts, -1), "–", cuts[-1], ")")) |>
      fitBounds(lng1 = sf::st_bbox(g)[[1]], lat1 = sf::st_bbox(g)[[2]], lng2 = sf::st_bbox(g)[[3]], lat2 = sf::st_bbox(g)[[4]])
  })

  output$g_classement <- renderPlotly({
    x <- imde_annee(); x <- head(x[order(-x$imde), ], 25)
    x$nom <- make.unique(as.character(x$nom))
    plot_ly(x, x = ~imde, y = ~factor(nom, levels = rev(nom)), type = "bar", orientation = "h",
            marker = list(color = setNames(PALETTE_DEFICIT, CLASSES_IMDE)[as.character(x$classe_imde)]),
            hovertemplate = "%{y}<br>IMDE %{x:.1f}<extra></extra>") |>
      plotly_edu(titre_x = "IMDE (0-100)", legende = FALSE) |> layout(xaxis = list(range = c(0, 100)))
  })

  output$g_heatmap <- renderPlotly({
    x <- imde_annee(); x <- head(x[order(-x$imde), ], 30)
    m <- as.matrix(x[, paste0("dim_", dims)]); colnames(m) <- dims
    plot_ly(z = m, x = dims, y = make.unique(as.character(x$nom)), type = "heatmap",
            colorscale = list(c(0, "#2E9E6B"), c(0.35, "#F3E3A6"), c(0.65, "#F0A35E"), c(1, "#D1495B")), zmin = 0, zmax = 100,
            hovertemplate = "%{y}<br>%{x} : %{z:.0f}<extra></extra>", colorbar = list(title = "", thickness = 10)) |>
      plotly_edu(legende = FALSE) |> layout(yaxis = list(autorange = "reversed"))
  })

  output$g_classes <- renderPlotly({
    x <- imde_tout(); x <- x[!is.na(x$classe_imde), ]
    t <- as.data.frame(table(annee = x$annee_debut, classe = x$classe_imde))
    p <- plot_ly()
    for (cl in CLASSES_IMDE) {
      s <- t[t$classe == cl, ]
      p <- add_trace(p, x = annee_scolaire(as.integer(as.character(s$annee))), y = s$Freq, type = "bar", name = cl,
                     marker = list(color = setNames(PALETTE_DEFICIT, CLASSES_IMDE)[[cl]]))
    }
    p |> layout(barmode = "stack") |> plotly_edu(titre_y = "Nombre de territoires")
  })

  output$g_evolution <- renderPlotly({
    x <- imde_tout(); sel <- input$suivi; req(length(sel) > 0)
    p <- plot_ly()
    for (i in seq_along(sel)) {
      s <- x[x$code_geo == sel[i], ]; s <- s[order(s$annee_debut), ]
      p <- add_trace(p, x = annee_scolaire(s$annee_debut), y = s$imde, type = "scatter", mode = "lines+markers",
                     name = s$nom[1], line = list(width = 2.4, color = PALETTE_SERIES[(i - 1) %% 8 + 1], shape = "spline"))
    }
    p |> layout(shapes = lapply(SEUILS_IMDE[2:5], function(s) list(type = "line", y0 = s, y1 = s, x0 = 0, x1 = 1, xref = "paper",
                                                                   line = list(color = "#E4E9F0", dash = "dot")))) |>
      plotly_edu(titre_y = "IMDE")
  })

  sens <- eventReactive(input$lancer_sens, {
    withProgress(message = "Analyse de sensibilité…", value = 0.3, {
      sensibilite_imde(imde_annee()[!is.na(imde_annee()$imde), ], n = 500)
    })
  })
  output$g_sensibilite <- renderPlotly({
    s <- sens(); s <- head(s[order(s$rang_principal), ], 30)
    s$nom <- make.unique(as.character(s$nom))
    plot_ly(s, y = ~factor(nom, levels = rev(nom))) |>
      add_segments(x = ~rang_p05, xend = ~rang_p95, yend = ~factor(nom, levels = rev(nom)),
                   line = list(color = "#9FC9CF", width = 6), name = "Intervalle 5–95 %", hoverinfo = "none") |>
      add_markers(x = ~rang_principal, marker = list(color = COULEURS$nuit, size = 8), name = "Rang principal",
                  text = ~sprintf("%s<br>Rang %d (5 %% : %d ; 95 %% : %d)<br>P(quartile critique) = %s", nom, rang_principal,
                                  round(rang_p05), round(rang_p95), fmt_pct(prob_quartile_critique, 0)), hoverinfo = "text") |>
      plotly_edu(titre_x = "Rang (1 = plus déficitaire)")
  })

  rob <- reactive({
    x <- imde_annee(); x <- x[!is.na(x$imde), ]
    b <- if (input$niveau == "Province") DONNEES$bornes[[app$filtres()$cycle]] else bornes_imde(x)
    robustesse_imde(x, b)
  })
  output$g_robustesse <- renderPlotly({
    r <- rob()$spearman
    plot_ly(z = r, x = colnames(r), y = rownames(r), type = "heatmap", zmin = 0.5, zmax = 1,
            colorscale = list(c(0, "#F0A35E"), c(0.5, "#E3F1F3"), c(1, "#0E6E7E")),
            text = round(r, 2), texttemplate = "%{text}", hovertemplate = "%{y} / %{x} : %{z:.3f}<extra></extra>",
            colorbar = list(thickness = 10)) |> plotly_edu(legende = FALSE, marge_b = 80)
  })
  output$robustesse_txt <- renderUI({
    r <- rob(); m <- min(r$spearman[upper.tri(r$spearman)], na.rm = TRUE)
    alerte(if (m > 0.9) "ok" else if (m > 0.75) "avert" else "critique",
           sprintf("Corrélation de rang minimale entre variantes : %s. Déplacement médian de rang : %s place(s), maximal : %s.",
                   fmt_nombre(m, 3), fmt_nombre(stats::median(r$ecart_max), 0), max(r$ecart_max)))
  })

  output$t_imde <- renderDT({
    x <- imde_annee(); x <- x[order(-x$imde), ]
    out <- data.frame(Rang = x$rang, Territoire = x$nom, IMDE = round(x$imde, 1), Classe = badge_classe(x$classe_imde),
                      check.names = FALSE)
    for (d in dims) out[[d]] <- round(x[[paste0("dim_", d)]], 1)
    out$`Dimensions renseignées` <- x$nb_dimensions
    tableau(out, page = 15, escape = FALSE)
  })

  output$methode <- renderUI({
    w <- attr(imde_tout(), "poids") %||% setNames(rep(0.2, 5), dims)
    tagList(
      eq("<i>s<sub>i</sub></i> = (<i>x<sub>i</sub></i> &minus; min) / (max &minus; min)",
         "Normalisation à bornes fixes (percentiles 2 % et 98 % de toutes les provinces et années). Pour un indicateur favorable (TBS, fonctionnalité), on utilise (max − x) / (max − min)."),
      eq("<i>D<sub>d</sub></i> = moyenne des scores <i>s<sub>i</sub></i> disponibles de la dimension <i>d</i>"),
      eq("IMDE = 100 &times; &sum;<sub><i>d</i></sub> <i>w<sub>d</sub></i> <i>D<sub>d</sub></i> / &sum;<sub><i>d</i></sub> <i>w<sub>d</sub></i>",
         "Variante géométrique, non compensatoire : produit des scores de dimension, chacun élevé à son poids."),
      p(class = "eq-legende", "Classes à seuils fixes : favorable (moins de 25), modéré (25 à 37,5), préoccupant (37,5 à 50), sévère (50 à 62,5), critique (62,5 et plus)."),
      tags$ul(style = "font-size:14px;margin-top:10px",
              lapply(dims, function(d) tags$li(tags$b(d), sprintf(" (poids %s) : ", fmt_pct(w[[d]], 0)),
                                              paste(CATALOGUE_INDICATEURS$libelle[match(DIMENSIONS_IMDE[[d]], CATALOGUE_INDICATEURS$code)], collapse = " ; ")))),
      note_methodo("Les poids égaux suivent la convention de l'IDH et de l'IPM (aucune dimension n'est jugée prioritaire a priori). L'indice n'est calculé que si au moins 3 dimensions sur 5 sont renseignées."))
  })

  output$mention <- renderUI(mention_donnees(app$D()$d$meta))
})
