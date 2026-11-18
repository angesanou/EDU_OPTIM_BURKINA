# ==========================================================================
# MODULE 14 — UTILISATEURS ET ADMINISTRATION
# ==========================================================================

mod_utilisateurs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(class = "page-intro", "Trois profils d'accès. Le périmètre est appliqué côté serveur, à la source : un utilisateur régional ne reçoit jamais les données des autres régions, un utilisateur provincial seulement celles de sa province."),
    uiOutput(ns("moi")),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(4, 4, 4)),
      carte("Profil national", plein_ecran = FALSE, p("Accès à toutes les régions et provinces ; gestion des comptes ; exports nationaux.", style = "font-size:14px")),
      carte("Profil régional", plein_ecran = FALSE, p("Accès aux seules provinces de sa région ; les agrégats « nationaux » sont recalculés sur le périmètre autorisé.", style = "font-size:14px")),
      carte("Profil provincial", plein_ecran = FALSE, p("Accès à sa seule province (et à ses communes et établissements) ; modèles de panel indisponibles (trop peu d'unités).", style = "font-size:14px"))),
    carte("Comptes", DTOutput(ns("t_comptes")), sous_titre = "les mots de passe ne sont jamais affichés ni stockés en clair"),
    uiOutput(ns("admin")),
    carte("Journal de la session", DTOutput(ns("t_journal"))),
    carte("Sécurité et déploiement", plein_ecran = FALSE, tags$ul(style = "font-size:14px;line-height:1.7",
      tags$li("Mots de passe : empreinte SHA-256 salée (16 caractères aléatoires par compte) dans config/utilisateurs.csv."),
      tags$li("Les comptes de démonstration sont créés automatiquement si le fichier est vide : supprimez-les avant tout usage réel."),
      tags$li("En production (Shiny Server, Posit Connect, ShinyProxy) : déléguer l'authentification au serveur (LDAP/SSO), servir en HTTPS, et conserver ce module pour les autorisations par périmètre."),
      tags$li("Pour désactiver l'écran de connexion en démonstration locale : authentification: false dans config/config.yml.")))
  )
}

mod_utilisateurs_server <- function(id, app) moduleServer(id, function(input, output, session) {
  ns <- session$ns
  rafraichir <- reactiveVal(0)

  output$moi <- renderUI({
    u <- app$utilisateur(); req(u)
    alerte("info", tags$b("Session : "), sprintf("%s (%s), profil %s, périmètre : %s", u$nom_complet, u$identifiant,
                                                 PROFILS[[u$profil]], libelle_perimetre(u, DONNEES$d$geo)), icone = "id-badge")
  })

  output$t_comptes <- renderDT({
    rafraichir()
    c <- lire_comptes()
    tableau(data.frame(Identifiant = c$identifiant, Nom = c$nom_complet, Profil = PROFILS[c$profil],
                       `Périmètre` = vapply(c$perimetre, function(p) if (p == "NAT") "Burkina Faso" else DONNEES$d$geo$nom_geo[match(p, DONNEES$d$geo$code_geo)], ""),
                       check.names = FALSE), boutons = FALSE)
  })

  output$admin <- renderUI({
    u <- app$utilisateur(); req(u)
    if (u$profil != "NATIONAL") return(alerte("avert", "La gestion des comptes est réservée au profil national."))
    g <- DONNEES$d$geo
    carte("Créer un compte", plein_ecran = FALSE,
          layout_columns(col_widths = breakpoints(sm = 12, md = c(2, 3, 2, 3, 2)),
            textInput(ns("n_id"), "Identifiant", placeholder = "minuscules, chiffres, - . _"),
            textInput(ns("n_nom"), "Nom complet"),
            selectInput(ns("n_profil"), "Profil", setNames(names(PROFILS), PROFILS), selectize = FALSE),
            selectInput(ns("n_perim"), "Périmètre", c("National" = "NAT",
                                                      setNames(g$code_geo[g$niveau_geo == "REGION"], paste("Région :", g$nom_geo[g$niveau_geo == "REGION"])),
                                                      setNames(g$code_geo[g$niveau_geo == "PROVINCE"], paste("Province :", g$nom_geo[g$niveau_geo == "PROVINCE"])))),
            passwordInput(ns("n_mdp"), "Mot de passe (8 car. min.)")),
          actionButton(ns("creer"), "Créer le compte", icon = icon("user-plus"), class = "btn-primary"))
  })

  observeEvent(input$creer, {
    req(app$utilisateur()$profil == "NATIONAL")
    prof <- input$n_profil; per <- input$n_perim
    ok_perim <- (prof == "NATIONAL" && per == "NAT") || (prof == "REGIONAL" && startsWith(per, "REG")) || (prof == "PROVINCIAL" && startsWith(per, "PRV"))
    if (!ok_perim) return(showNotification("Le périmètre ne correspond pas au profil (national → NAT, régional → région, provincial → province).", type = "warning"))
    r <- tryCatch(ajouter_compte(tolower(trimws(input$n_id)), input$n_nom, prof, per, input$n_mdp), error = function(e) e)
    if (inherits(r, "error")) return(showNotification(paste("Création impossible :", conditionMessage(r),
                                                            "(identifiant ≥ 3 caractères en minuscules, mot de passe ≥ 8 caractères)"), type = "error"))
    journaliser("Administration", sprintf("Compte créé : %s (%s, %s)", input$n_id, prof, per))
    showNotification("Compte créé.", type = "message")
    rafraichir(rafraichir() + 1)
  })

  output$t_journal <- renderDT({
    rafraichir(); app$page()
    j <- JOURNAL$entrees
    tableau(j[rev(seq_len(nrow(j))), ], boutons = FALSE)
  })
})
