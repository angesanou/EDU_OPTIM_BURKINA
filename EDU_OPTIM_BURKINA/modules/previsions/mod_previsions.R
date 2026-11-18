# ==========================================================================
# MODULE 8 — PRÉVISIONS ET PROJECTIONS
# ==========================================================================

mod_previsions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(class = "page-intro", "Projection des effectifs puis des besoins en salles, enseignants, tables-bancs et latrines à stock constant. Six méthodes sont comparées par backtesting (apprentissage jusqu'en 2021-2022, test sur les trois dernières années)."),
    div(class = "panneau-param mb-3",
        layout_columns(
          col_widths = breakpoints(sm = 12, md = c(4, 3, 3, 2)),
          selectizeInput(ns("territoire"), "Territoire", choices = NULL),
          selectInput(ns("methode"), "Méthode principale", setNames(names(METHODES_PREVISION), METHODES_PREVISION), "holt", selectize = FALSE),
          sliderInput(ns("horizon"), "Horizon", min = ANNEE_MAX + 1, max = ANNEE_MAX + 11, value = CONFIG$periode$horizon_projection %||% 2030, sep = ""),
          sliderInput(ns("mult"), "Croissance (× tendance)", 0.5, 2, 1, step = 0.1))),
    div(actionButton(ns("calculer"), "Calculer les besoins de toutes les provinces", icon = icon("calculator"), class = "btn-primary mb-3")),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(8, 4)),
      carte("Effectifs observés et projetés", plotlyOutput(ns("g_prev"), height = 400),
            pied = "Bandes : intervalles de prévision à 80 % et 95 % de la méthode principale. Lignes fines : autres méthodes."),
      carte("Backtesting de la série", DTOutput(ns("t_back")),
            pied = "MAPE = erreur absolue moyenne en %. Couverture = part des observations dans l'intervalle à 95 %.")),
    section("Besoins à l'horizon", "toutes les provinces du cycle, méthode principale"),
    uiOutput(ns("kpis_besoins")),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      carte("Besoins additionnels par région", plotlyOutput(ns("g_besoins"), height = 380)),
      carte("Performance moyenne des méthodes (toutes provinces)", DTOutput(ns("t_back_panel")))),
    carte("Besoins détaillés par province", DTOutput(ns("t_besoins"))),
    carte("Hypothèses", plein_ecran = FALSE, tags$ul(style = "font-size:14px;line-height:1.7",
      tags$li("Les stocks (salles en bon état, enseignants, tables-bancs, latrines) sont maintenus au niveau de la dernière année observée."),
      tags$li("Besoin = ⌈effectif projeté / norme⌉ − stock actuel, borné à 0 ; normes officielles de la feuille NORMES."),
      tags$li("Méthode démographique : population d'âge scolaire projetée au taux de croissance des 5 dernières années × TBS prolongé linéairement (plafonné à 125 % du maximum observé)."),
      tags$li("Le multiplicateur de croissance amplifie l'écart entre la projection et la dernière valeur observée (scénarios de pression démographique)."),
      tags$li("Les séries courtes (10 points) rendent les intervalles larges : interpréter les ordres de grandeur, pas les décimales."))),
    uiOutput(ns("mention"))
  )
}

mod_previsions_server <- function(id, app) moduleServer(id, function(input, output, session) {

  observe({
    D <- app$D(); p <- D$ind$province
    prov <- unique(p[, c("code_geo", "nom", "code_region")])
    ch <- c(setNames("NAT", if (is.null(provinces_autorisees(app$utilisateur(), DONNEES$d$geo))) "Burkina Faso (national)" else "Ensemble du périmètre"),
            setNames(paste0("R:", unique(prov$code_region)), paste("Région :", REGIONS[unique(prov$code_region)])),
            setNames(paste0("P:", prov$code_geo), paste("Province :", prov$nom)))
    updateSelectizeInput(session, "territoire", choices = ch, selected = ch[1], server = TRUE)
  })

  serie <- reactive({
    D <- app$D(); f <- app$filtres(); t <- input$territoire; req(t)
    x <- if (t == "NAT") D$ind$national else if (startsWith(t, "R:")) D$ind$region[D$ind$region$code_geo == sub("R:", "", t), ] else
      panel_cycle(D, f$cycle)[panel_cycle(D, f$cycle)$code_geo == sub("P:", "", t), ]
    x <- x[as.character(x$cycle) == f$cycle, ]
    x[order(x$annee_debut), ]
  })

  prev <- reactive({
    s <- serie(); exiger(nrow(s) >= 5, "Série trop courte.")
    h <- input$horizon - max(s$annee_debut)
    l <- lapply(names(METHODES_PREVISION), function(m) {
      r <- prevoir_serie(s$eff_t, s$annee_debut, h, m, s$pop_age_scolaire, input$mult); r$methode <- m; r })
    do.call(rbind, l)
  })

  output$g_prev <- renderPlotly({
    s <- serie(); pr <- prev(); m <- input$methode
    pm <- pr[pr$methode == m, ]
    lien <- data.frame(annee = max(s$annee_debut), prevision = s$eff_t[nrow(s)])
    p <- plot_ly() |>
      add_ribbons(x = pm$annee, ymin = pm$bas95, ymax = pm$haut95, fillcolor = "rgba(14,110,126,0.10)", line = list(width = 0), name = "IC 95 %") |>
      add_ribbons(x = pm$annee, ymin = pm$bas80, ymax = pm$haut80, fillcolor = "rgba(14,110,126,0.20)", line = list(width = 0), name = "IC 80 %")
    for (mm in setdiff(names(METHODES_PREVISION), m)) {
      q <- rbind(lien, pr[pr$methode == mm, c("annee", "prevision")])
      p <- add_lines(p, x = q$annee, y = q$prevision, name = METHODES_PREVISION[[mm]], line = list(width = 1.2, dash = "dot"), opacity = 0.7)
    }
    q <- rbind(lien, pm[, c("annee", "prevision")])
    p |>
      add_lines(x = q$annee, y = q$prevision, name = paste0("Projection : ", METHODES_PREVISION[[m]]), line = list(color = COULEURS$petrole, width = 3)) |>
      add_trace(x = s$annee_debut, y = s$eff_t, type = "scatter", mode = "lines+markers", name = "Observé",
                line = list(color = COULEURS$nuit, width = 3), marker = list(size = 7, color = COULEURS$nuit)) |>
      plotly_edu(titre_y = "Élèves", titre_x = "Année de début de l'année scolaire")
  })

  output$t_back <- renderDT({
    s <- serie(); b <- backtester(s$eff_t, s$annee_debut, s$pop_age_scolaire)
    exiger(!is.null(b), "Série trop courte pour un backtesting.")
    b <- b[order(b$mape), ]
    tableau(data.frame(`Méthode` = b$libelle, `MAPE %` = round(b$mape, 2), RMSE = fmt_nombre(b$rmse),
                       `Couv. 95 %` = fmt_pct(b$couverture95, 0), check.names = FALSE), boutons = FALSE)
  })

  besoins <- eventReactive(input$calculer, {
    D <- app$D(); f <- app$filtres()
    cycles <- if (f$cycle == "TOUS") names(CYCLES) else f$cycle
    withProgress(message = "Projection des provinces…", value = 0, {
      l <- lapply(seq_along(cycles), function(i) {
        incProgress(1 / length(cycles), detail = CYCLES[[cycles[i]]])
        p <- D$ind$province[as.character(D$ind$province$cycle) == cycles[i], ]
        pj <- projeter_panel(p, input$horizon, input$methode, input$mult)
        bf <- besoins_futurs(pj[pj$annee == input$horizon, ], p[p$annee_debut == max(p$annee_debut), ], D$d$normes)
        bf$nom <- p$nom[match(bf$code_geo, p$code_geo)]; bf$code_region <- p$code_region[match(bf$code_geo, p$code_geo)]
        list(besoins = bf, back = backtester_panel(p, methodes = names(METHODES_PREVISION)))
      })
      list(besoins = do.call(rbind, lapply(l, `[[`, "besoins")), back = do.call(rbind, lapply(l, `[[`, "back")))
    })
  })

  output$kpis_besoins <- renderUI({
    b <- besoins()$besoins
    layout_columns(
      col_widths = breakpoints(sm = 6, xl = 3),
      kpi(sprintf("Élèves attendus en %s", input$horizon), fmt_compact(sum(b$prevision)), icone = "children",
          evol = sum(b$prevision) / sum(b$eff_actuel) - 1, sous_texte = "par rapport à aujourd'hui"),
      kpi("Salles à construire", fmt_nombre(somme(b$besoin_salles)), icone = "door-open", ton = "orange",
          sous_texte = "cycles dont le stock par province est publié"),
      kpi("Enseignants à recruter", fmt_nombre(somme(b$besoin_enseignants)), icone = "chalkboard-user", ton = "violet",
          sous_texte = "cycles dont le stock par province est publié"),
      kpi("Coût indicatif", fmt_fcfa(somme(b$cout_estime)), icone = "coins", ton = "rouge", sous_texte = "salles, enseignants (1 an), tables-bancs, latrines"))
  })

  output$g_besoins <- renderPlotly({
    b <- besoins()$besoins
    exiger(any(!is.na(b$besoin_salles)), paste("Besoins en salles et enseignants non calculables pour ce cycle :",
      "stocks provinciaux non publiés (seule la projection des effectifs est disponible)."))
    r <- aggregate(cbind(besoin_salles, besoin_enseignants) ~ code_region, b, sum)
    r$nom <- REGIONS[r$code_region]; r <- r[order(r$besoin_salles), ]
    plot_ly(r, y = ~factor(nom, levels = nom), x = ~besoin_salles, type = "bar", orientation = "h", name = "Salles",
            marker = list(color = COULEURS$petrole)) |>
      add_trace(x = ~besoin_enseignants, name = "Enseignants", marker = list(color = COULEURS$orange)) |>
      layout(barmode = "group") |> plotly_edu(titre_x = "Unités supplémentaires nécessaires")
  })

  output$t_back_panel <- renderDT({
    b <- besoins()$back
    b <- aggregate(cbind(mape, couverture95) ~ libelle, b, mean); b <- b[order(b$mape), ]
    tableau(data.frame(`Méthode` = b$libelle, `MAPE moyen %` = round(b$mape, 2), `Couv. 95 %` = fmt_pct(b$couverture95, 0),
                       check.names = FALSE), boutons = FALSE)
  })

  output$t_besoins <- renderDT({
    b <- besoins()$besoins
    tableau(data.frame(Province = b$nom, Cycle = unname(CYCLES[b$cycle]), `Élèves actuels` = b$eff_actuel,
                       `Élèves projetés` = round(b$prevision), `IC 95 %` = sprintf("%s – %s", fmt_nombre(b$bas95), fmt_nombre(b$haut95)),
                       Salles = b$besoin_salles, Enseignants = b$besoin_enseignants, `Tables-bancs` = b$besoin_tables_bancs,
                       Latrines = b$besoin_latrines, `Coût (M FCFA)` = round(b$cout_estime / 1e6), check.names = FALSE),
            page = 15, ordre = list(list(5, "desc")))
  })

  output$mention <- renderUI(mention_donnees(app$D()$d$meta))
})
