# ==========================================================================
# server.R — Logique serveur : session, navigation, filtres, modules
# ==========================================================================

server <- function(input, output, session) {

  # --- Authentification ------------------------------------------------------
  auth <- isTRUE(CONFIG$application$authentification)
  utilisateur <- reactiveVal(if (auth) NULL else UTILISATEUR_ANONYME)
  if (auth) showModal(modal_connexion())

  observeEvent(input$connexion_valider, {
    u <- verifier_identifiants(input$connexion_id %||% "", input$connexion_mdp %||% "")
    if (is.null(u)) {
      output$connexion_msg <- renderUI(alerte("critique", "Identifiant ou mot de passe incorrect."))
      return()
    }
    utilisateur(u)
    journaliser("Connexion", sprintf("%s (%s, %s)", u$identifiant, PROFILS[[u$profil]], u$perimetre))
    removeModal()
    showNotification(sprintf("Bienvenue, %s. Périmètre : %s", u$nom_complet,
                             libelle_perimetre(u, DONNEES$d$geo)), type = "message", duration = 5)
  })
  observeEvent(input$deconnexion, session$reload())

  # Données restreintes au périmètre de l'utilisateur connecté
  D <- reactive({
    req(utilisateur())
    restreindre(DONNEES, provinces_autorisees(utilisateur(), DONNEES$d$geo))
  })

  # --- Filtres globaux ---------------------------------------------------------
  filtres <- reactive(list(annee = as.integer(input$annee %||% ANNEE_MAX), cycle = input$cycle %||% "PRIMAIRE"))

  # --- Navigation --------------------------------------------------------------
  page <- reactiveVal("accueil")
  aller <- function(p) {
    page(p)
    nav_select("pages", p)
    session$sendCustomMessage("activer_nav", p)
  }
  observeEvent(input$nav_page, aller(input$nav_page))
  output$titre_page <- renderText(PAGES$titre[PAGES$id == page()])
  output$fil <- renderText(paste("EDU-OPTIM  ›  ", PAGES$section[PAGES$id == page()]))

  output$puce_utilisateur <- renderUI({
    u <- utilisateur(); req(u)
    init <- toupper(substr(gsub("[^A-Za-z]", "", u$identifiant), 1, 2))
    div(class = "puce-utilisateur",
        div(class = "avatar", init),
        div(u$nom_complet, tags$small(sprintf("%s, %s", PROFILS[[u$profil]], libelle_perimetre(u, DONNEES$d$geo)))),
        if (auth) actionLink("deconnexion", icon("right-from-bracket"), title = "Se déconnecter",
                             style = "color:#8A98AB;margin-left:6px"))
  })

  # Résultats partagés entre modules (réutilisés par les rapports)
  resultats <- reactiveValues(optimisation = NULL, priorisation = NULL, simulations = NULL)

  app <- list(D = D, filtres = filtres, utilisateur = utilisateur, aller = aller,
              resultats = resultats, page = page)
  for (id in PAGES$id) get(paste0("mod_", id, "_server"))(id, app)
}
