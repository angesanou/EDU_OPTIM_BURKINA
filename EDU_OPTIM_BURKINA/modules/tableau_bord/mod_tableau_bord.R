# ==========================================================================
# MODULE 2 — TABLEAU DE BORD GÉNÉRAL
# ==========================================================================
# Tous les chiffres sont CALCULÉS à partir de la base (aucune saisie en dur).

INDICATEURS_TB <- c(
  "Élèves par salle utilisable" = "ratio_eleves_salle_utilisable", "Élèves par enseignant" = "ratio_eleves_ens",
  "Taux brut de scolarisation (%)" = "tbs", "Couverture en eau" = "taux_eau",
  "Couverture en électricité" = "taux_electricite", "Couverture en latrines" = "taux_latrines",
  "Part de salles en mauvais état" = "part_salles_mauvais", "Taux de fonctionnalité" = "taux_fonctionnalite",
  "Déficit en salles" = "deficit_salles", "Déficit en enseignants" = "deficit_enseignants", "Élèves" = "eff_t")
EST_TAUX <- c("taux_eau", "taux_electricite", "taux_latrines", "part_salles_mauvais", "taux_fonctionnalite")

mod_tableau_bord_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(class = "page-intro", "Vue d'ensemble calculée à partir de la base. Les évolutions comparent l'année choisie à l'année précédente ; les couleurs indiquent si l'évolution est favorable."),
    uiOutput(ns("kpis")),
    section("Analyses", "Survolez les graphiques pour le détail ; agrandissez une carte avec l'icône en bas à droite"),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(4, 8)),
      carte("Répartition des élèves par cycle", plotlyOutput(ns("g_cycles"), height = 300)),
      carte("Comparaison des 13 régions",
            selectInput(ns("ind_reg"), NULL, INDICATEURS_TB, "ratio_eleves_salle_utilisable", width = "280px", selectize = FALSE),
            plotlyOutput(ns("g_regions"), height = 300))),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(8, 4)),
      carte("Évolution temporelle",
            selectInput(ns("ind_temps"), NULL, INDICATEURS_TB, "eff_t", width = "280px", selectize = FALSE),
            plotlyOutput(ns("g_temps"), height = 300), sous_titre = "par cycle"),
      carte("Urbain / rural", plotlyOutput(ns("g_milieu"), height = 340), sous_titre = "élèves")),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(5, 7)),
      carte("Répartition des déficits (coût estimé)", plotlyOutput(ns("g_deficits"), height = 320),
            pied = "Coût = déficit × coût unitaire indicatif (modifiable dans le module Optimisation)."),
      carte("Classement des régions", DTOutput(ns("t_regions")), sous_titre = "IMDE et ratios clés")),
    uiOutput(ns("mention"))
  )
}

mod_tableau_bord_server <- function(id, app) moduleServer(id, function(input, output, session) {

  sel <- function(df, f, annee = f$annee) df[as.character(df$cycle) == f$cycle & df$annee_debut == annee, ]

  output$kpis <- renderUI({
    f <- app$filtres(); n <- sel(app$D()$ind$national, f); p <- sel(app$D()$ind$national, f, f$annee - 1)
    exiger(nrow(n) == 1, "Aucune donnée pour cette sélection.")
    ev <- function(v) if (nrow(p) == 1) diviser(n[[v]], p[[v]]) - 1 else NULL
    layout_columns(
      col_widths = breakpoints(sm = 6, lg = 4, xl = 2),
      kpi("Établissements", fmt_nombre(n$nb_etab_t), icone = "school", evol = ev("nb_etab_t")),
      kpi("Élèves", fmt_compact(n$eff_t), icone = "children", evol = ev("eff_t")),
      kpi("Enseignants", fmt_nombre(n$nb_ens_t), icone = "chalkboard-user", ton = "violet", evol = ev("nb_ens_t")),
      kpi("Salles de classe", fmt_nombre(n$nb_salles_t), icone = "door-open", evol = ev("nb_salles_t")),
      kpi("Salles en mauvais état", fmt_nombre(n$nb_salles_mauvais), icone = "house-crack", ton = "orange",
          evol = ev("nb_salles_mauvais"), evol_bon_si_hausse = FALSE, sous_texte = fmt_pct(n$part_salles_mauvais)),
      kpi("Élèves / enseignant", fmt_nombre(n$ratio_eleves_ens, 1), icone = "user-group", ton = "violet",
          evol = ev("ratio_eleves_ens"), evol_bon_si_hausse = FALSE),
      kpi("Élèves / salle", fmt_nombre(n$ratio_eleves_salle, 1), icone = "people-roof", ton = "orange",
          evol = ev("ratio_eleves_salle"), evol_bon_si_hausse = FALSE),
      kpi("Couverture en eau", fmt_pct(n$taux_eau, 0), icone = "droplet", ton = "petrole", evol = ev("taux_eau"),
          jauge = n$taux_eau),
      kpi("Couverture en électricité", fmt_pct(n$taux_electricite, 0), icone = "bolt", ton = "orange",
          evol = ev("taux_electricite"), jauge = n$taux_electricite, couleur_jauge = COULEURS$orange),
      kpi("Couverture en latrines", fmt_pct(n$taux_latrines, 0), icone = "restroom", ton = "vert",
          evol = ev("taux_latrines"), jauge = n$taux_latrines, couleur_jauge = COULEURS$vert),
      kpi("Déficit en salles", fmt_nombre(n$deficit_salles), icone = "triangle-exclamation", ton = "rouge",
          evol = ev("deficit_salles"), evol_bon_si_hausse = FALSE),
      kpi("Déficit en enseignants", fmt_nombre(n$deficit_enseignants), icone = "user-minus", ton = "rouge",
          evol = ev("deficit_enseignants"), evol_bon_si_hausse = FALSE))
  })

  output$g_cycles <- renderPlotly({
    n <- app$D()$ind$national; f <- app$filtres()
    x <- n[n$annee_debut == f$annee & as.character(n$cycle) %in% names(CYCLES), ]
    plot_ly(x, labels = ~unname(CYCLES[as.character(cycle)]), values = ~eff_t, type = "pie", hole = 0.62, sort = FALSE,
            marker = list(colors = unname(PALETTE_CYCLES[as.character(x$cycle)]), line = list(color = "#fff", width = 2)),
            textinfo = "percent", hovertemplate = "%{label}<br>%{value:,.0f} élèves<extra></extra>") |>
      plotly_edu() |>
      layout(annotations = list(list(text = sprintf("<b>%s</b><br><span style='font-size:11px;color:#5A6B80'>élèves</span>",
                                                    fmt_compact(sum(x$eff_t))), showarrow = FALSE, font = list(size = 18))))
  })

  output$g_regions <- renderPlotly({
    f <- app$filtres(); v <- input$ind_reg
    r <- sel(app$D()$ind$region, f); n <- sel(app$D()$ind$national, f)
    exiger(nrow(r) > 0, "Aucune région dans votre périmètre pour cette sélection.")
    r <- r[order(r[[v]]), ]
    taux <- v %in% EST_TAUX
    val <- if (taux) 100 * r[[v]] else r[[v]]
    p <- plot_ly(r, x = val, y = ~factor(nom, levels = nom), type = "bar", orientation = "h",
                 marker = list(color = COULEURS$petrole, line = list(width = 0)),
                 hovertemplate = paste0("%{y}<br>%{x:,.1f}", if (taux) " %" else "", "<extra></extra>")) |>
      plotly_edu(legende = FALSE)
    if (nrow(n) == 1 && is.finite(n[[v]])) {
      nv <- if (taux) 100 * n[[v]] else n[[v]]
      p <- p |> layout(shapes = list(list(type = "line", x0 = nv, x1 = nv, y0 = -0.5, y1 = nrow(r) - 0.5,
                                          line = list(color = COULEURS$orange, dash = "dot", width = 2))),
                       annotations = list(list(x = nv, y = nrow(r) - 0.5, text = "Moyenne", showarrow = FALSE,
                                               yanchor = "bottom", font = list(color = COULEURS$orange, size = 11))))
    }
    p
  })

  output$g_temps <- renderPlotly({
    v <- input$ind_temps; n <- app$D()$ind$national
    taux <- v %in% EST_TAUX
    p <- plot_ly()
    for (cy in names(CYCLES)) {
      x <- n[as.character(n$cycle) == cy, ]; x <- x[order(x$annee_debut), ]
      y <- if (taux) 100 * x[[v]] else x[[v]]
      p <- add_trace(p, x = annee_scolaire(x$annee_debut), y = y, type = "scatter", mode = "lines+markers", name = CYCLES[[cy]],
                     line = list(color = PALETTE_CYCLES[[cy]], width = 2.5, shape = "spline"),
                     marker = list(size = 6, color = PALETTE_CYCLES[[cy]]),
                     hovertemplate = paste0(CYCLES[[cy]], ", %{x}<br>%{y:,.1f}<extra></extra>"))
    }
    plotly_edu(p, titre_y = names(INDICATEURS_TB)[INDICATEURS_TB == v])
  })

  output$g_milieu <- renderPlotly({
    n <- app$D()$ind$national; f <- app$filtres()
    x <- n[n$annee_debut == f$annee & as.character(n$cycle) %in% names(CYCLES), ]
    lib <- unname(CYCLES[as.character(x$cycle)])
    plot_ly(x, x = lib, y = ~eff_urbain_t, type = "bar", name = "Urbain", marker = list(color = COULEURS$petrole),
            hovertemplate = "%{x}, urbain<br>%{y:,.0f}<extra></extra>") |>
      add_trace(y = ~eff_rural_t, name = "Rural", marker = list(color = COULEURS$turquoise),
                hovertemplate = "%{x}, rural<br>%{y:,.0f}<extra></extra>") |>
      layout(barmode = "stack") |> plotly_edu()
  })

  output$g_deficits <- renderPlotly({
    f <- app$filtres(); n <- sel(app$D()$ind$national, f); req(nrow(n) == 1)
    cu <- setNames(COUTS_DEFAUT$cout_unitaire, COUTS_DEFAUT$ressource)
    x <- data.frame(lib = DEFICITS$libelle, unites = vapply(DEFICITS$code, function(v) n[[v]] %||% NA_real_, numeric(1)),
                    ressource = DEFICITS$ressource)
    x$cout <- x$unites * cu[x$ressource]
    x <- x[!is.na(x$cout) & x$cout > 0, ]; x <- x[order(x$cout), ]
    plot_ly(x, x = ~cout / 1e9, y = ~factor(lib, levels = lib), type = "bar", orientation = "h",
            text = ~sprintf("%s unités", fmt_nombre(unites)), textposition = "none",
            marker = list(color = c(PALETTE_DEFICIT[5], PALETTE_DEFICIT[4], "#E8B04E", COULEURS$petrole, COULEURS$turquoise, "#8A94A6", COULEURS$violet)[seq_len(nrow(x))]),
            hovertemplate = "%{y}<br>%{text}<br>%{x:,.1f} Md FCFA<extra></extra>") |>
      plotly_edu(titre_x = "Milliards de FCFA", legende = FALSE)
  })

  output$t_regions <- renderDT({
    f <- app$filtres(); D <- app$D()
    r <- sel(D$ind$region, f)
    im <- D$imde[[f$cycle]]; im <- im[im$annee_debut == f$annee, ]
    im_reg <- tapply(im$imde * im$eff_t, im$code_region, sum, na.rm = TRUE) / tapply(im$eff_t * !is.na(im$imde), im$code_region, sum)
    r$imde <- as.numeric(im_reg[r$code_geo])
    r <- r[order(-r$imde), ]
    tableau(data.frame(Rang = seq_len(nrow(r)), `Région` = r$nom, IMDE = round(r$imde, 1),
                       Classe = badge_classe(classer_imde(r$imde)), `Élèves` = fmt_nombre(r$eff_t),
                       `Élèves par salle utilisable` = round(r$ratio_eleves_salle_utilisable, 1), `Élèves par enseignant` = round(r$ratio_eleves_ens, 1),
                       `TBS (%)` = round(r$tbs, 1), `Accès à l'eau` = fmt_pct(r$taux_eau, 0), check.names = FALSE),
            page = 13, escape = FALSE)
  })

  output$mention <- renderUI(mention_donnees(app$D()$d$meta))
})
