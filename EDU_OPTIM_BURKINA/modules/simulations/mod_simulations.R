# ==========================================================================
# MODULE 11 — SIMULATIONS ET SCÉNARIOS
# ==========================================================================

mod_simulations_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(class = "page-intro", "Laboratoire de scénarios : chaque scénario projette la situation à l'horizon choisi (croissance des effectifs, stocks constants), peut redéployer les enseignants excédentaires, puis alloue le budget avec le moteur d'optimisation. Les résultats sont comparés côte à côte."),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(4, 8)),
      div(class = "panneau-param parametres-collants",
          div(class = "action-haut", actionButton(ns("lancer"), "Lancer la simulation", icon = icon("play"), class = "btn-primary w-100")),
          tags$h6("Scénarios prédéfinis"),
          checkboxGroupInput(ns("scenarios"), NULL, setNames(names(SCENARIOS), vapply(SCENARIOS, `[[`, "", "nom")),
                             selected = names(SCENARIOS)),
          sliderInput(ns("horizon"), "Horizon de simulation", ANNEE_MAX + 1, ANNEE_MAX + 8, ANNEE_MAX + 3, sep = ""),
          tags$hr(),
          tags$h6("Scénario personnalisé"),
          checkboxInput(ns("perso"), "Ajouter un scénario personnalisé", FALSE),
          conditionalPanel(sprintf("input['%s']", ns("perso")),
            numericInput(ns("p_budget"), "Budget (Md FCFA)", 120, min = 0, step = 10),
            numericInput(ns("p_salles"), "Salles à construire au maximum", NA, min = 0),
            numericInput(ns("p_ens"), "Enseignants à recruter au maximum", NA, min = 0),
            sliderInput(ns("p_demo"), "Croissance démographique (× tendance)", 0.5, 2, 1, step = 0.1),
            sliderInput(ns("p_lambda"), "Priorité à l'équité (λ)", 0, 5, 1, step = 0.25),
            checkboxInput(ns("p_realloc"), "Réallouer d'abord les enseignants existants", FALSE))),
      div(
        uiOutput(ns("info")),
        carte("Comparaison des scénarios", DTOutput(ns("t_resume"))),
        layout_columns(
          col_widths = breakpoints(sm = 12, md = c(6, 6)),
          carte("IMDE moyen avant / après", plotlyOutput(ns("g_imde"), height = 320)),
          carte("Coût et gain d'IMDE", plotlyOutput(ns("g_frontiere"), height = 320),
                pied = "Gain = baisse de l'IMDE moyen pondéré par les élèves.")))),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      carte("Ressources mobilisées", plotlyOutput(ns("g_ressources"), height = 340)),
      carte("Déficits restants à l'horizon", plotlyOutput(ns("g_restants"), height = 340))),
    carte("Redéploiement des enseignants (scénario de réallocation)", DTOutput(ns("t_transferts")),
          pied = "Problème de transport : minimise la distance totale entre provinces excédentaires (50 % de l'excédent mobilisable) et déficitaires."),
    uiOutput(ns("mention"))
  )
}

mod_simulations_server <- function(id, app) moduleServer(id, function(input, output, session) {

  sims <- eventReactive(input$lancer, {
    D <- app$D()
    sc <- SCENARIOS[input$scenarios]
    if (isTRUE(input$perso)) sc$personnalise <- list(
      nom = "Personnalisé", budget = (input$p_budget %||% 0) * 1e9, mult_demo = input$p_demo, lambda = input$p_lambda,
      alpha = 0, reallocation = isTRUE(input$p_realloc), dispo_salles = input$p_salles, dispo_ens = input$p_ens)
    exiger(length(sc) > 0, "Choisissez au moins un scénario.")
    b_tous <- do.call(rbind, DONNEES$bornes[names(CYCLES)])
    res <- list()
    withProgress(message = "Simulation des scénarios…", value = 0, {
      for (k in names(sc)) {
        incProgress(1 / length(sc), detail = sc[[k]]$nom)
        res[[k]] <- tryCatch(executer_scenario(sc[[k]], D$ind$province, b_tous, D$d$normes, COUTS_DEFAUT, ANNEE_MAX,
                                               input$horizon, DONNEES$distances),
                             error = function(e) { showNotification(paste(sc[[k]]$nom, ":", conditionMessage(e)), type = "error"); NULL })
      }
    })
    res <- Filter(Negate(is.null), res)
    app$resultats$simulations <- res
    res
  })

  resume <- reactive({ s <- sims(); exiger(length(s) > 0, "Aucun résultat."); do.call(rbind, lapply(s, `[[`, "resume")) })

  output$info <- renderUI({
    if (!isTruthy(input$lancer)) alerte("info", "Choisissez les scénarios et l'horizon, puis cliquez sur ", tags$b("Lancer la simulation"),
                                        ". Comptez quelques secondes par scénario.")
  })

  output$t_resume <- renderDT({
    r <- resume()
    tableau(data.frame(`Scénario` = r$scenario, Budget = fmt_fcfa(r$budget), `Coût engagé` = fmt_fcfa(r$cout_total),
                       `IMDE avant` = round(r$imde_avant, 1), `IMDE après` = round(r$imde_apres, 1),
                       `Gain` = round(r$imde_avant - r$imde_apres, 1), `Unités critiques/sévères après` = r$unites_critiques_apres,
                       Salles = fmt_nombre(r$salles), Enseignants = fmt_nombre(r$enseignants),
                       `Élèves bénéficiaires (salles)` = fmt_nombre(r$eleves_beneficiaires_salles),
                       `Gini IMDE après` = round(r$gini_imde_apres, 3), `Couverture min.` = fmt_pct(r$couverture_min, 0),
                       check.names = FALSE), boutons = TRUE, page = 10)
  })

  output$g_imde <- renderPlotly({
    r <- resume()
    plot_ly(r, x = ~scenario, y = ~imde_avant, type = "bar", name = "Avant (projeté)", marker = list(color = "#E4E9F0")) |>
      add_trace(y = ~imde_apres, name = "Après allocation", marker = list(color = COULEURS$petrole)) |>
      layout(barmode = "group") |> plotly_edu(titre_y = "IMDE moyen", marge_b = 40)
  })

  output$g_frontiere <- renderPlotly({
    r <- resume()
    plot_ly(r, x = ~cout_total / 1e9, y = ~imde_avant - imde_apres, type = "scatter", mode = "markers+text", text = ~scenario,
            textposition = "top center", marker = list(size = 16, color = PALETTE_SERIES[seq_len(nrow(r))], line = list(color = "#fff", width = 2)),
            hovertemplate = "%{text}<br>%{x:,.1f} Md FCFA<br>gain %{y:.2f} point(s)<extra></extra>") |>
      plotly_edu(titre_x = "Coût engagé (Md FCFA)", titre_y = "Gain d'IMDE (points)", legende = FALSE)
  })

  output$g_ressources <- renderPlotly({
    r <- resume()
    plot_ly(r, x = ~scenario, y = ~salles, type = "bar", name = "Salles", marker = list(color = COULEURS$petrole)) |>
      add_trace(y = ~enseignants, name = "Enseignants (recrutés + redéployés)", marker = list(color = COULEURS$orange)) |>
      layout(barmode = "group") |> plotly_edu(titre_y = "Unités", marge_b = 40)
  })

  output$g_restants <- renderPlotly({
    r <- resume()
    plot_ly(r, x = ~scenario, y = ~deficit_salles_restant, type = "bar", name = "Salles manquantes", marker = list(color = COULEURS$rouge)) |>
      add_trace(y = ~deficit_ens_restant, name = "Enseignants manquants", marker = list(color = "#F0A35E")) |>
      layout(barmode = "group") |> plotly_edu(titre_y = "Unités", marge_b = 40)
  })

  output$t_transferts <- renderDT({
    s <- sims(); t <- do.call(rbind, lapply(s, `[[`, "transferts"))
    exiger(!is.null(t) && nrow(t) > 0, "Aucun redéploiement : sélectionnez le scénario « Réallocation des enseignants ».")
    tableau(data.frame(Cycle = unname(CYCLES[t$cycle]), Origine = t$origine, Destination = t$destination,
                       Enseignants = t$enseignants, `Distance (km)` = round(t$distance_km), check.names = FALSE), page = 10)
  })

  output$mention <- renderUI(mention_donnees(app$D()$d$meta))
})
