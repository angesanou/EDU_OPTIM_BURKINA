# ==========================================================================
# 30_ui_composants.R — Composants d'interface réutilisables
# ==========================================================================

#' Carte KPI premium.
kpi <- function(titre, valeur, unite = NULL, icone = "chart-simple", ton = "petrole",
                evol = NULL, evol_bon_si_hausse = TRUE, sous_texte = NULL, jauge = NULL, couleur_jauge = NULL) {
  ev <- NULL
  if (!is.null(evol) && !is.na(evol)) {
    hausse <- evol > 0
    classe <- if (abs(evol) < 0.0005) "neutre" else if (hausse == evol_bon_si_hausse) "bon" else "mauvais"
    ev <- span(class = paste("evol", classe), paste0(if (evol > 0) "▲ " else if (evol < 0) "▼ " else "", fmt_pct(abs(evol))))
  }
  div(class = paste("carte kpi", paste0("ton-", ton)),
      div(class = "kpi-haut", div(class = "kpi-titre", titre), div(class = "kpi-icone", icon(icone))),
      div(class = "kpi-valeur", valeur, if (!is.null(unite)) tags$small(unite)),
      div(class = "kpi-bas", ev, sous_texte),
      if (!is.null(jauge) && !is.na(jauge))
        div(class = "jauge", tags$span(style = sprintf("width:%.1f%%;background:%s", 100 * min(1, max(0, jauge)),
                                                        couleur_jauge %||% COULEURS$petrole))))
}

#' Titre de section.
section <- function(titre, sous_titre = NULL) div(class = "section-titre", h2(titre), if (!is.null(sous_titre)) p(sous_titre))

#' Carte avec en-tête (bslib::card) ; `plein_ecran` active le bouton d'agrandissement.
carte <- function(titre, ..., sous_titre = NULL, pied = NULL, plein_ecran = TRUE, hauteur = NULL, classe = NULL) {
  bslib::card(full_screen = plein_ecran, height = hauteur, class = classe,
              bslib::card_header(span(titre), if (!is.null(sous_titre)) span(class = "sous-titre", sous_titre)),
              bslib::card_body(...),
              if (!is.null(pied)) bslib::card_footer(pied))
}

alerte <- function(type = c("info", "avert", "critique", "ok"), ..., icone = NULL) {
  type <- match.arg(type)
  ic <- icone %||% switch(type, info = "circle-info", avert = "triangle-exclamation", critique = "circle-exclamation", ok = "circle-check")
  div(class = paste("alerte", type), span(class = "ic", icon(ic)), div(...))
}

badge_classe <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x), "–", sprintf('<span class="badge-classe classe-%s">%s</span>', x, x))
}

note_methodo <- function(...) div(class = "alerte info", style = "font-size:12.5px", span(class = "ic", icon("flask")), div(...))

#' Mention obligatoire (données synthétiques / source).
mention_donnees <- function(meta) {
  div(class = "mention",
      span(icon("database"), " ", meta$mention),
      span(sprintf("Source : %s, version du %s", basename(meta$fichier), format(as.Date(meta$date_fichier), "%d/%m/%Y"))))
}

# --- Graphiques plotly ------------------------------------------------------
plotly_edu <- function(p, titre_x = NULL, titre_y = NULL, legende = TRUE, marge_b = 10) {
  p |>
    plotly::layout(
      font = list(family = "Segoe UI, system-ui, Arial, sans-serif", size = 12, color = COULEURS$texte),
      xaxis = list(title = list(text = titre_x %||% "", font = list(size = 13, color = COULEURS$texte_2)),
                   gridcolor = "#EEF2F6", zeroline = FALSE, linecolor = "#E4E9F0", tickfont = list(size = 12.5)),
      yaxis = list(title = list(text = titre_y %||% "", font = list(size = 13, color = COULEURS$texte_2)),
                   gridcolor = "#EEF2F6", zeroline = FALSE, tickfont = list(size = 12.5)),
      legend = list(orientation = "h", x = 0, y = -0.16, font = list(size = 12.5)),
      showlegend = legende, margin = list(l = 10, r = 10, t = 10, b = marge_b),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      hoverlabel = list(bgcolor = COULEURS$nuit, bordercolor = COULEURS$nuit, font = list(color = "#FFFFFF", size = 12)),
      separators = ", ") |>
    plotly::config(displayModeBar = "hover", displaylogo = FALSE, locale = "fr",
                   modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines"),
                   toImageButtonOptions = list(format = "png", scale = 2, filename = "edu-optim"))
}

# --- Tableaux DT -------------------------------------------------------------
DT_FR <- list(sEmptyTable = "Aucune donnée", sInfo = "_START_–_END_ sur _TOTAL_", sInfoEmpty = "0 ligne",
              sInfoFiltered = "(filtré de _MAX_)", sLengthMenu = "_MENU_ par page", sLoadingRecords = "Chargement…",
              sProcessing = "Traitement…", sSearch = "", sSearchPlaceholder = "Rechercher…",
              sZeroRecords = "Aucun résultat", oPaginate = list(sFirst = "«", sLast = "»", sNext = "›", sPrevious = "‹"))

tableau <- function(df, page = 10, boutons = TRUE, escape = TRUE, ordre = NULL, filtre_colonnes = FALSE, ...) {
  DT::datatable(
    df, rownames = FALSE, escape = escape, selection = "single",
    extensions = if (boutons) "Buttons" else character(0),
    filter = if (filtre_colonnes) "top" else "none",
    options = list(pageLength = page, lengthMenu = c(10, 25, 50, 100), scrollX = TRUE, autoWidth = FALSE,
                   dom = if (boutons) "<'d-flex justify-content-between align-items-center mb-2'Bf>rt<'d-flex justify-content-between mt-2'ip>" else "frtip",
                   buttons = list(list(extend = "copy", text = "Copier"), list(extend = "csv", text = "CSV"),
                                  list(extend = "excel", text = "Excel")),
                   order = ordre %||% list(), language = DT_FR, ...),
    class = "compact hover")
}

# --- Cartes leaflet ----------------------------------------------------------
pal_imde <- function() leaflet::colorBin(PALETTE_DEFICIT, domain = c(0, 100), bins = SEUILS_IMDE, na.color = "#E9EDF2")
pal_numerique <- function(x, inverser = FALSE) {
  pal <- if (inverser) rev(PALETTE_DEFICIT) else PALETTE_DEFICIT
  leaflet::colorQuantile(pal, domain = x, n = 5, na.color = "#E9EDF2")
}

carte_base <- function(en_ligne = FALSE) {
  m <- leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.25, zoomDelta = 0.5, attributionControl = TRUE, minZoom = 5, maxZoom = 16))
  if (isTRUE(en_ligne)) m <- leaflet::addProviderTiles(m, "CartoDB.Positron", options = leaflet::providerTileOptions(opacity = 0.9))
  leaflet::setView(m, lng = -1.6, lat = 12.3, zoom = 6.25)
}

# --- ggplot (rapports PDF / PNG) -------------------------------------------------
theme_edu <- function(base = 10) {
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = base + 3, colour = COULEURS$texte),
                   plot.subtitle = ggplot2::element_text(colour = COULEURS$texte_2),
                   plot.caption = ggplot2::element_text(colour = "#8A98AB", size = base - 2, hjust = 0),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = "#EEF2F6"),
                   axis.text = ggplot2::element_text(colour = COULEURS$texte_2),
                   legend.position = "bottom")
}

#' Validation lisible pour l'utilisateur (message au lieu d'une erreur).
exiger <- function(cond, message) shiny::validate(shiny::need(cond, message))

#' Formule mise en forme (HTML : italiques, indices, exposants), sans Internet.
#' `html` est écrit dans le code de l'application (contenu sûr) ; `legende` est facultative.
eq <- function(html, legende = NULL) {
  div(class = "eq-bloc", div(class = "eq", HTML(html)), if (!is.null(legende)) div(class = "eq-legende", legende))
}

#' Mention des limites administratives selon la source des géométries.
libelle_limites <- function(court = FALSE) {
  if (isTRUE(get0("EST_SYNTHETIQUE", ifnotfound = TRUE)))
    return(if (court) "Limites approximatives, non officielles" else
      "Géométries approximatives : elles ne représentent pas les limites administratives officielles.")
  if (court) "Limites : geoBoundaries (Banque mondiale), CC BY 4.0" else
    "Limites : 45 provinces, geoBoundaries d'après la Banque mondiale, licence CC BY 4.0."
}

MESSAGE_SANS_IMDE <- paste(
  "IMDE non calculable pour ce cycle : moins de 3 dimensions renseignées.",
  "Les annuaires ne publient pas, par province, les salles, enseignants et équipements",
  "du post-primaire, du secondaire et du préscolaire. Consultez le Diagnostic",
  "(accès, TBS, parité) ou choisissez le Primaire.")

#' Exécute une expression et affiche une notification claire en cas d'erreur.
avec_garde <- function(expr, contexte = "Calcul") {
  tryCatch(expr, error = function(e) {
    shiny::showNotification(paste0(contexte, " : ", conditionMessage(e)), type = "error", duration = 8)
    NULL
  })
}
