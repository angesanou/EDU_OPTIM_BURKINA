# ==========================================================================
# MODULE 7 — ANALYSE ÉCONOMÉTRIQUE
# ==========================================================================

mod_econometrie_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(class = "page-intro", "Modèles de panel province × année : facteurs ASSOCIÉS aux déficits éducatifs. Les effets fixes neutralisent les caractéristiques permanentes de chaque province ; les erreurs-types sont robustes (cluster par province)."),
    alerte("avert", tags$b("Association n'est pas causalité. "),
           "Un coefficient significatif décrit une corrélation conditionnelle ; il ne prouve pas qu'agir sur la variable réduirait le déficit (variables omises, causalité inverse, erreurs de mesure).",
           icone = "scale-balanced"),
    div(class = "panneau-param mb-3",
        layout_columns(
          col_widths = breakpoints(sm = 12, md = c(3, 6, 3)),
          selectInput(ns("y"), "Variable expliquée", setNames(names(VARIABLES_EXPLIQUEES), VARIABLES_EXPLIQUEES), "imde", selectize = FALSE),
          selectizeInput(ns("x"), "Variables explicatives", setNames(names(VARIABLES_EXPLICATIVES), VARIABLES_EXPLICATIVES),
                         selected = c("taux_pauvrete", "part_urbain", "part_pdi", "indice_insecurite", "log_densite"),
                         multiple = TRUE, options = list(plugins = list("remove_button"))),
          selectInput(ns("modele"), "Modèle affiché", c("Effets fixes (within)" = "ef", "Effets aléatoires" = "ea",
                                                         "Double effets fixes (province + année)" = "ef2", "MCO poolé" = "mco"),
                      selectize = FALSE))),
    uiOutput(ns("kpis")),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      carte("Coefficients et intervalles de confiance à 95 %", plotlyOutput(ns("g_coef"), height = 360),
            pied = "Erreurs-types robustes à l'hétéroscédasticité et à l'autocorrélation (Arellano, cluster province)."),
      carte("Interprétation", uiOutput(ns("interpretation")))),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(7, 5)),
      carte("Tableau des coefficients", DTOutput(ns("t_coef"))),
      carte("Comparaison des modèles", DTOutput(ns("t_ajust")))),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(7, 5)),
      carte("Tests de spécification et diagnostics", DTOutput(ns("t_tests"))),
      carte("Multicolinéarité (VIF)", DTOutput(ns("t_vif")), pied = "VIF > 5 : colinéarité préoccupante ; > 10 : grave.")),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      carte("Bootstrap par grappes (provinces)",
            div(style = "display:flex;gap:10px;align-items:end",
                numericInput(ns("B"), "Réplications", 199, min = 49, max = 999, step = 50, width = "130px"),
                actionButton(ns("lancer_boot"), "Lancer le bootstrap", icon = icon("arrows-rotate"), class = "btn-light mb-3")),
            DTOutput(ns("t_boot")), pied = "Intervalles percentiles 2,5 %–97,5 % ; robustes à la dépendance intra-province."),
      carte("Relation brute", selectInput(ns("x_nuage"), NULL, choices = NULL, width = "300px", selectize = FALSE),
            plotlyOutput(ns("g_nuage"), height = 330), sous_titre = "nuage province × année et droite des MCO")),
    uiOutput(ns("mention"))
  )
}

mod_econometrie_server <- function(id, app) moduleServer(id, function(input, output, session) {

  panel <- reactive({
    preparer_panel_eco(app$D()$imde[[app$filtres()$cycle]])
  })
  # Variable expliquée : si celle choisie n'est pas renseignée pour ce cycle
  # (ex. IMDE non calculable au post-primaire), basculer sur la première disponible.
  observeEvent(panel(), {
    p <- panel()
    dispo <- names(VARIABLES_EXPLIQUEES)[vapply(names(VARIABLES_EXPLIQUEES), function(v) v %in% names(p) && sum(!is.na(p[[v]])) > 20, logical(1))]
    if (length(dispo) && !(isolate(input$y) %in% dispo))
      updateSelectInput(session, "y", choices = setNames(dispo, VARIABLES_EXPLIQUEES[dispo]), selected = dispo[1])
    else if (length(dispo)) updateSelectInput(session, "y", choices = setNames(dispo, VARIABLES_EXPLIQUEES[dispo]), selected = isolate(input$y))
  })
  observe({
    updateSelectInput(session, "x_nuage", choices = setNames(input$x, VARIABLES_EXPLICATIVES[input$x]), selected = input$x[1])
  })

  est <- reactive({
    req(length(input$x) >= 1)
    p <- panel()
    exiger(length(unique(p$code_geo)) >= 5, "Au moins 5 provinces sont nécessaires : le périmètre de votre profil est trop restreint pour un modèle de panel.")
    exiger(input$y %in% names(p) && sum(!is.na(p[[input$y]])) > 20,
           "La variable expliquée n'est pas renseignée pour ce cycle : choisissez-en une autre.")
    x <- input$x[vapply(input$x, function(v) v %in% names(p) && sum(!is.na(p[[v]])) > 20 && stats::var(p[[v]], na.rm = TRUE) > 0, logical(1))]
    exiger(length(x) >= 1, "Les variables choisies ne sont pas disponibles dans cette source (fichier de contexte absent ?).")
    tryCatch(estimer_modeles(p, input$y, x), error = function(e) { exiger(FALSE, paste("Estimation impossible :", conditionMessage(e))) })
  })
  coefs <- reactive(coefficients_robustes(est(), input$modele))
  diag <- reactive(diagnostics_modeles(est()))

  output$kpis <- renderUI({
    e <- est(); dg <- diag(); a <- dg$ajustement
    layout_columns(
      col_widths = breakpoints(sm = 6, xl = 3),
      kpi("Observations", fmt_nombre(nrow(e$donnees)), icone = "table", sous_texte = sprintf("%d provinces × %d années",
          length(unique(e$donnees$code_geo)), length(unique(e$donnees$annee_debut)))),
      kpi("R² intra (effets fixes)", fmt_nombre(a$r2[2], 3), icone = "bullseye", ton = "violet"),
      kpi("Variables significatives (5 %)", sum(coefs()$p_valeur < 0.05 & coefs()$terme != "(Intercept)"), icone = "check-double", ton = "vert",
          sous_texte = sprintf("sur %d", sum(coefs()$terme != "(Intercept)"))),
      kpi("Spécification retenue", {
        h <- dg$tests[grepl("Hausman", dg$tests$test), ]
        if (nrow(h) && h$p_valeur < 0.05) "Effets fixes" else "Effets aléatoires" }, icone = "code-branch", ton = "orange",
        sous_texte = "selon le test de Hausman"))
  })

  output$g_coef <- renderPlotly({
    c <- coefs(); c <- c[c$terme != "(Intercept)", ]
    exiger(nrow(c) > 0, "Aucun coefficient.")
    c$col <- ifelse(c$p_valeur < 0.05, ifelse(c$estimation > 0, COULEURS$rouge, COULEURS$vert), "#8A98AB")
    plot_ly(c, y = ~libelle) |>
      add_segments(x = ~ic_bas, xend = ~ic_haut, yend = ~libelle, line = list(color = "#C9D4E3", width = 5), hoverinfo = "none", showlegend = FALSE) |>
      add_markers(x = ~estimation, marker = list(color = ~col, size = 12, line = list(color = "#fff", width = 1.5)), showlegend = FALSE,
                  text = ~sprintf("%s<br>coef. %s [%s ; %s]<br>p = %s %s", libelle, fmt_nombre(estimation, 3), fmt_nombre(ic_bas, 3),
                                  fmt_nombre(ic_haut, 3), fmt_nombre(p_valeur, 4), significativite), hoverinfo = "text") |>
      layout(shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, yref = "paper", line = list(color = COULEURS$nuit, dash = "dot")))) |>
      plotly_edu(titre_x = paste("Effet associé sur :", VARIABLES_EXPLIQUEES[[input$y]]), legende = FALSE)
  })

  output$interpretation <- renderUI({
    txt <- interpreter(coefs(), VARIABLES_EXPLIQUEES[[input$y]])
    div(style = "font-size:14px;line-height:1.65", lapply(txt, function(t) if (startsWith(t, "⚠")) alerte("avert", substring(t, 3)) else p(t, style = "margin-bottom:6px")))
  })

  output$t_coef <- renderDT({
    c <- coefs()
    tableau(data.frame(Variable = c$libelle, Coefficient = round(c$estimation, 4), `Erreur-type robuste` = round(c$erreur_type, 4),
                       t = round(c$stat_t, 2), `p-valeur` = signif(c$p_valeur, 3), `IC 95 % bas` = round(c$ic_bas, 4),
                       `IC 95 % haut` = round(c$ic_haut, 4), ` ` = as.character(c$significativite), check.names = FALSE), boutons = TRUE)
  })
  output$t_ajust <- renderDT({
    a <- diag()$ajustement
    tableau(data.frame(`Modèle` = a$modele, N = a$n, `R²` = round(a$r2, 3), check.names = FALSE), boutons = FALSE)
  })
  output$t_tests <- renderDT({
    t <- diag()$tests
    tableau(data.frame(Test = t$test, Statistique = t$statistique, `p-valeur` = signif(t$p_valeur, 3), Conclusion = t$conclusion,
                       check.names = FALSE), boutons = FALSE, page = 6)
  })
  output$t_vif <- renderDT({
    v <- diag()$vif
    exiger(!is.null(v), "VIF non calculable (une seule variable explicative).")
    tableau(data.frame(Variable = v$libelle, VIF = v$vif), boutons = FALSE)
  })

  boot <- eventReactive(input$lancer_boot, {
    B <- min(999, max(49, input$B %||% 199))
    withProgress(message = sprintf("Bootstrap (%d réplications)…", B), value = 0.4, bootstrap_ef(est(), B = B))
  })
  output$t_boot <- renderDT({
    b <- boot()
    tableau(data.frame(Variable = b$libelle, `Coef. (EF)` = round(b$estimation, 4), `ET bootstrap` = round(b$bootstrap_et, 4),
                       `IC 2,5 %` = round(b$ic_bas, 4), `IC 97,5 %` = round(b$ic_haut, 4),
                       `Signe stable` = ifelse(sign(b$ic_bas) == sign(b$ic_haut), "Oui", "Non"), check.names = FALSE), boutons = FALSE)
  })

  output$g_nuage <- renderPlotly({
    p <- panel(); xv <- input$x_nuage; yv <- input$y; req(xv %in% names(p))
    req(yv %in% names(p))
    d <- p[!is.na(p[[xv]]) & !is.na(p[[yv]]), ]
    exiger(nrow(d) >= 10, "Pas assez d'observations renseignées pour ce couple de variables.")
    m <- stats::lm(d[[yv]] ~ d[[xv]])
    xs <- seq(min(d[[xv]]), max(d[[xv]]), length.out = 50)
    plot_ly(d, x = d[[xv]], y = d[[yv]], type = "scatter", mode = "markers", text = ~paste(nom, annee_debut),
            marker = list(color = COULEURS$petrole, opacity = 0.45, size = 7), hovertemplate = "%{text}<br>x = %{x:.3f}<br>y = %{y:.2f}<extra></extra>",
            name = "Province × année") |>
      add_lines(x = xs, y = coef(m)[1] + coef(m)[2] * xs, line = list(color = COULEURS$orange, width = 3), name = "Droite MCO", inherit = FALSE) |>
      plotly_edu(titre_x = VARIABLES_EXPLICATIVES[[xv]], titre_y = VARIABLES_EXPLIQUEES[[yv]])
  })

  output$mention <- renderUI(mention_donnees(app$D()$d$meta))
})
