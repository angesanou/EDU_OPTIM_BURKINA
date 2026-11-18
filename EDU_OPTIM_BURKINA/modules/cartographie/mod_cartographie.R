# ==========================================================================
# MODULE 6 — CARTOGRAPHIE ET ANALYSE SPATIALE
# ==========================================================================

VARIABLES_CARTE <- c("IMDE" = "imde", setNames(CATALOGUE_INDICATEURS$code, CATALOGUE_INDICATEURS$libelle))

mod_cartographie_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(class = "page-intro", "Module SIG : cartes choroplèthes, établissements, densité, accessibilité et regroupements géographiques (indice de Moran et indicateurs locaux d'association spatiale). Les couches s'activent dans le sélecteur en haut à droite de la carte."),
    if (!isTRUE(get0("EST_SYNTHETIQUE", ifnotfound = TRUE)))
      alerte("info", tags$b("Limites administratives : "),
             "45 provinces (ancien découpage), geoBoundaries d'après la Banque mondiale (2017), licence CC BY 4.0. ",
             "Aucun fichier d'établissements géolocalisés : les couches établissements, densité et accessibilité ne sont pas disponibles.",
             icone = "map")
    else alerte("avert", tags$b("Géométries synthétiques et approximatives : "),
           "les limites sont des polygones de Voronoï autour de chefs-lieux approchés et les établissements ont des coordonnées aléatoires. Ils ne correspondent à aucune limite officielle ni à aucun établissement réel.",
           icone = "map"),
    div(class = "panneau-param mb-3",
        layout_columns(
          col_widths = breakpoints(sm = 12, md = c(3, 4, 5)),
          radioButtons(ns("niveau"), "Niveau de la choroplèthe", c("Région", "Province", "Commune"), "Province", inline = TRUE),
          selectInput(ns("variable"), "Variable cartographiée", VARIABLES_CARTE, "imde", selectize = FALSE),
          sliderInput(ns("seuil_km"), "Distance maximale école–foyer (accessibilité)", 1, 10, 3, step = 0.5, post = " km"))),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(8, 4)),
      carte("Carte interactive", leafletOutput(ns("carte"), height = 640), sous_titre = "couches activables"),
      div(
        carte("Autocorrélation spatiale globale", uiOutput(ns("moran")), plein_ecran = FALSE),
        carte("Regroupements géographiques", plotlyOutput(ns("g_lisa"), height = 190), plein_ecran = FALSE,
              pied = "Une province appartient à un groupe de valeurs élevées lorsque sa valeur et celles de ses voisines sont toutes élevées (test statistique au seuil de 5 %). Pour l'IMDE, ce sont les zones de concentration des déficits."),
        carte("Accessibilité", uiOutput(ns("acces")), plein_ecran = FALSE))),
    carte("Indicateurs spatiaux par territoire", DTOutput(ns("t_spatial"))),
    uiOutput(ns("mention"))
  )
}

mod_cartographie_server <- function(id, app) moduleServer(id, function(input, output, session) {

  # Valeurs du niveau choisi pour l'année / le cycle sélectionnés
  valeurs <- reactive({
    D <- app$D(); f <- app$filtres()
    switch(input$niveau,
      "Province" = { x <- D$imde[[f$cycle]]; x[x$annee_debut == f$annee, ] },
      "Commune" = {
        exiger(!is.null(D$imde_communes), "Niveau communal indisponible pour cette source.")
        x <- D$imde_communes[[f$cycle]]; x[x$annee_debut == f$annee, ] },
      "Région" = {
        r <- D$ind$region; r <- r[as.character(r$cycle) == f$cycle & r$annee_debut == f$annee, ]
        im <- D$imde[[f$cycle]]; im <- im[im$annee_debut == f$annee, ]
        r$imde <- as.numeric((tapply(im$imde * im$eff_t, im$code_region, sum, na.rm = TRUE) /
                                tapply(im$eff_t * !is.na(im$imde), im$code_region, sum))[r$code_geo])
        r })
  })
  couche <- reactive({
    g <- app$D()$d$geometries
    exiger(!is.null(g), "Aucune géométrie : renseignez un fichier de limites dans config/config.yml.")
    c <- switch(input$niveau, "Région" = g$regions, "Province" = g$provinces, "Commune" = g$communes)
    exiger(!is.null(c), "Couche indisponible.")
    joindre_geo(c, valeurs())
  })
  couche_lisa <- reactive({
    lisa(couche(), input$variable)
  })
  acces <- reactive({
    D <- app$D(); f <- app$filtres()
    if (is.null(D$d$geometries$communes) || is.null(D$ind$etablissements)) return(NULL)
    cy <- if (f$cycle == "TOUS") "PRIMAIRE" else f$cycle
    a <- accessibilite(D$d$geometries$communes, D$ind$etablissements, cy, f$annee, input$seuil_km)
    if (is.null(a)) return(NULL)
    merge(D$d$geometries$communes, a, by = "code_geo")
  })

  output$carte <- renderLeaflet({
    g <- couche(); v <- input$variable; D <- app$D(); f <- app$filtres()
    lib <- names(VARIABLES_CARTE)[VARIABLES_CARTE == v]
    vals <- g[[v]]
    exiger(any(!is.na(vals)), "Cette variable n'est pas renseignée à ce niveau géographique (ex. TBS au niveau communal).")
    sens <- if (v == "imde") 1 else CATALOGUE_INDICATEURS$sens[CATALOGUE_INDICATEURS$code == v]
    pal <- if (v == "imde") pal_imde() else
      leaflet::colorNumeric(if (sens > 0) PALETTE_DEFICIT else rev(PALETTE_DEFICIT), domain = vals, na.color = "#E9EDF2")
    m <- carte_base(CONFIG$application$fond_carte_en_ligne) |>
      addMapPane("fond", zIndex = 410) |> addMapPane("dessus", zIndex = 440) |> addMapPane("points", zIndex = 460) |>
      addPolygons(data = g, group = "Choroplèthe", fillColor = pal(vals), fillOpacity = 0.85, color = "#fff", weight = 1,
                  options = pathOptions(pane = "fond"),
                  label = lapply(sprintf("<b>%s</b><br>%s : %s", g$nom, lib, fmt_nombre(vals, 2)), HTML),
                  highlightOptions = highlightOptions(weight = 2.5, color = COULEURS$nuit, bringToFront = TRUE)) |>
      addLegend("bottomright", pal = pal, values = vals, title = lib, opacity = 0.9, group = "Choroplèthe")
    # LISA
    gl <- couche_lisa()
    m <- m |> addPolygons(data = gl, group = "Regroupements géographiques", fillColor = unname(COULEURS_LISA[gl$lisa_classe]),
                          fillOpacity = 0.85, color = "#fff", weight = 1, options = pathOptions(pane = "dessus"),
                          label = lapply(sprintf("<b>%s</b><br>%s<br>I local = %s ; p = %s", gl$nom, gl$lisa_classe,
                                                 fmt_nombre(gl$lisa_I, 2), fmt_nombre(gl$lisa_p, 3)), HTML)) |>
      addLegend("bottomleft", colors = unname(COULEURS_LISA), labels = names(COULEURS_LISA), title = "Regroupement",
                group = "Regroupements géographiques", opacity = 0.9)
    groupes <- c("Choroplèthe", "Regroupements géographiques")
    # Établissements
    e <- D$ind$etablissements
    if (!is.null(e)) {
      e <- e[e$annee_debut == f$annee & (f$cycle == "TOUS" | as.character(e$cycle) == f$cycle), ]
      couleur <- ifelse(e$ferme == 1, COULEURS$rouge, ifelse(e$eau == 1 & e$latrines == 1, COULEURS$vert, COULEURS$orange))
      m <- m |> addCircleMarkers(data = e, lng = ~lon, lat = ~lat, radius = 4, stroke = FALSE, fillOpacity = 0.85,
                                 fillColor = couleur, group = "Établissements (synthétiques)",
                                 options = pathOptions(pane = "points"),
                                 clusterOptions = markerClusterOptions(disableClusteringAtZoom = 11, showCoverageOnHover = FALSE),
                                 popup = sprintf("<b>%s</b><br>%s, %s, %s<br>%s élèves, %s salles, %s enseignants<br>Eau : %s ; latrines : %s ; électricité : %s%s",
                                                 e$id_etab, CYCLES[as.character(e$cycle)], tolower(e$statut), tolower(e$milieu),
                                                 fmt_nombre(e$eff_t), e$nb_salles_t, e$nb_ens_t,
                                                 ifelse(e$eau == 1, "oui", "non"), ifelse(e$latrines == 1, "oui", "non"),
                                                 ifelse(e$electricite == 1, "oui", "non"), ifelse(e$ferme == 1, "<br><b style='color:#D1495B'>FERMÉ</b>", ""))) |>
        addLegend("topright", colors = c(COULEURS$vert, COULEURS$orange, COULEURS$rouge),
                  labels = c("Eau et latrines", "Équipement incomplet", "Fermé"), title = "Établissements",
                  group = "Établissements (synthétiques)", opacity = 0.9)
      groupes <- c(groupes, "Établissements (synthétiques)")
      hx <- densite_hex(D$d$geometries$regions, D$ind$etablissements, if (f$cycle == "TOUS") "PRIMAIRE" else f$cycle, f$annee)
      if (!is.null(hx)) {
        ph <- colorNumeric(c("#F4F6F9", "#9FD9D2", "#1FB5A8", "#0E6E7E", "#0B1A2E"), domain = hx$nb_etab)
        m <- m |> addPolygons(data = hx, group = "Densité (hexagones 25 km)", fillColor = ph(hx$nb_etab), fillOpacity = 0.8,
                              color = "#fff", weight = 0.5, options = pathOptions(pane = "dessus"),
                              label = sprintf("%d établissements, %s élèves", hx$nb_etab, fmt_nombre(hx$eleves))) |>
          addLegend("bottomleft", pal = ph, values = hx$nb_etab, title = "Établ. / hexagone", group = "Densité (hexagones 25 km)")
        groupes <- c(groupes, "Densité (hexagones 25 km)")
      }
    }
    a <- acces()
    if (!is.null(a)) {
      pa <- colorBin(c("#2E9E6B", "#A8D5BA", "#F3E3A6", "#F0A35E", "#D1495B"), domain = a$dist_km,
                     bins = c(0, 1, 2, 3, 5, Inf))
      m <- m |> addPolygons(data = a, group = "Accessibilité (distance moyenne)", fillColor = pa(a$dist_km), fillOpacity = 0.85,
                            color = "#fff", weight = 0.6, options = pathOptions(pane = "dessus"),
                            label = sprintf("%s : %s km en moyenne ; %s des foyers au-delà du seuil", a$nom,
                                            fmt_nombre(a$dist_km, 1), fmt_pct(a$part_au_dela_seuil, 0))) |>
        addLegend("bottomleft", pal = pa, values = a$dist_km, title = "Distance (km)", group = "Accessibilité (distance moyenne)")
      groupes <- c(groupes, "Accessibilité (distance moyenne)")
    }
    bb <- sf::st_bbox(g)
    m |> addLayersControl(baseGroups = character(0), overlayGroups = groupes,
                          options = layersControlOptions(collapsed = FALSE)) |>
      hideGroup(setdiff(groupes, "Choroplèthe")) |>
      fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]])
  })

  output$moran <- renderUI({
    g <- couche(); v <- input$variable
    exiger(nrow(g) >= 8, "Au moins 8 territoires sont nécessaires (élargissez le périmètre ou le niveau).")
    mg <- moran_global(g, v)
    exiger(!is.null(mg), "Pas assez de valeurs renseignées.")
    tagList(
      div(style = "display:flex;gap:18px;align-items:baseline",
          div(div(class = "kpi-titre", "I de Moran"), div(class = "kpi-valeur", style = "font-size:30px;font-weight:780", fmt_nombre(mg$I, 3))),
          div(style = "font-size:13px;color:#5A6B80", sprintf("E[I] = %s", fmt_nombre(mg$esperance, 3)), br(),
              sprintf("z = %s", fmt_nombre(mg$z, 2)), br(), sprintf("p (999 permutations) = %s", fmt_nombre(mg$p_permutation, 3)))),
      alerte(if (mg$p_permutation < 0.05) "avert" else "info", mg$interpretation),
      tags$small(style = "color:#8A98AB", "Voisinage : contiguïté « reine », poids standardisés en ligne."))
  })

  output$g_lisa <- renderPlotly({
    t <- as.data.frame(table(couche_lisa()$lisa_classe))
    plot_ly(t, x = ~Freq, y = ~Var1, type = "bar", orientation = "h",
            marker = list(color = unname(COULEURS_LISA[as.character(t$Var1)])), hovertemplate = "%{y} : %{x}<extra></extra>") |>
      plotly_edu(legende = FALSE)
  })

  output$acces <- renderUI({
    a <- acces()
    exiger(!is.null(a), "Accessibilité indisponible (établissements géolocalisés requis).")
    tagList(
      div(style = "display:flex;gap:22px",
          div(div(class = "kpi-titre", "Distance moyenne"), div(style = "font-size:24px;font-weight:760", paste(fmt_nombre(mean(a$dist_km), 1), "km"))),
          div(div(class = "kpi-titre", sprintf("Foyers > %s km", input$seuil_km)), div(style = "font-size:24px;font-weight:760", fmt_pct(mean(a$part_au_dela_seuil), 0)))),
      p(style = "font-size:13px;color:#5A6B80;margin:8px 0 0",
        "Distance euclidienne entre 5 foyers simulés par commune et l'établissement fonctionnel le plus proche du cycle. Approximation : la distance réelle par piste est plus longue."))
  })

  output$t_spatial <- renderDT({
    g <- couche_lisa(); v <- input$variable
    df <- sf::st_drop_geometry(g)
    out <- data.frame(Territoire = df$nom, Valeur = round(df[[v]], 2), `Regroupement géographique` = df$lisa_classe,
                      `I local` = round(df$lisa_I, 3), `p-valeur` = round(df$lisa_p, 3),
                      `Superficie (km²)` = round(df$superficie_km2 %||% NA), `Établissements pour 100 km²` = round(df$etab_100km2, 2),
                      check.names = FALSE)
    tableau(out, page = 10)
  })

  output$mention <- renderUI(mention_donnees(app$D()$d$meta))
})
