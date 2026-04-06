rsconnect::setAccountInfo(name='cgm-ek',
                          token='F92E6674BBA3B0241D3B0687BD34CE85',
                          secret='4lKV0gbAKkzK8qNXDkwip8XYcAQWIlHp6S/yuZQQ')

setwd("C:/Users/caspe/Documents/2.SemesterGruppe7/EksamenGruppe7")

rsconnect::deployApp()

library(rsconnect)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

# ---- LOAD DATA ----
pca_df <- readRDS("kampe_pca_df.rds")
afleveringer_pca_df <- readRDS("afleveringer_pca_df.rds")
passesclustertest <- readRDS("passesclustertest.rds")
kampcluster <- readRDS("kampcluster.rds")

# ---- FIX CLUSTER ----
pca_df$cluster <- as.integer(as.character(pca_df$cluster))

kampcluster <- kampcluster %>%
  select(-Kamptype) %>%
  left_join(
    pca_df %>% select(cluster, Kamptype) %>% distinct(),
    by = "cluster"
  )

# ---- VARIABLE FILTERING ----
vars_to_exclude_pass <- c("MATCH_WYID", "TEAMNAME", "TEAM_WYID", "playername", "ROLENAME", "cluster_name", "cluster")
valid_vars <- setdiff(names(passesclustertest), vars_to_exclude_pass)

vars_to_exclude_match <- c("MATCH_WYID", "TEAM_WYID", "xG.y", "shots.y", "total_passes", "afleveringer", "cluster", "TEAMNAME", "Kamptype")
valid_vars_match <- setdiff(names(kampcluster), vars_to_exclude_match)

# ---- NICE LABELS ----
nice_names_match <- c(
  "Max minut" = "max_minute",
  "Skud" = "shots.x",
  "Skud på mål" = "SHOTONTARGET",
  "Expected Goals (xG)" = "xG.x",
  "xG difference" = "xGdiff",
  "Afleveringer" = "passes",
  "Boldbesiddelse difference" = "possession_diff",
  "Skud difference" = "skuddiff",
  "Progressive afleveringer" = "progressive",
  "Lange afleveringer" = "long_passes",
  "Indlæg" = "crosspass",
  "Succesfulde afleveringer" = "successful_passes",
  "Afleveringsnøjagtighed (%)" = "pass_accuracy",
  "Antal begivenheder" = "events",
  "Regelovertrædelser" = "infraction",
  "Dueller" = "duels"
)

nice_names_pass <- c(
  "Længde" = "LENGTH",
  "Vinkel" = "ANGLE",
  "Lokation op/ned af banen" = "LOCATIONX",
  "Lokation hen ad banen" = "LOCATIONY",
  "Ende lokation op/ned" = "ENDLOCATIONX",
  "Ende lokation hen ad" = "ENDLOCATIONY",
  "Progressive afleveringer" = "progressive",
  "Nøgle afleveringer" = "key_pass",
  "Afleveringer ind i boksen" = "into_box",
  "Høje afleveringer" = "high_pass",
  "Nøjagtige afleveringer" = "accurate"
)

# filtrer gyldige
nice_names_match <- nice_names_match[nice_names_match %in% valid_vars_match]
nice_names_pass <- nice_names_pass[nice_names_pass %in% valid_vars]

if (length(nice_names_match) == 0) {
  nice_names_match <- setNames(valid_vars_match, valid_vars_match)
}
if (length(nice_names_pass) == 0) {
  nice_names_pass <- setNames(valid_vars, valid_vars)
}

# ---- DEFAULT VALUES ----
default_x_match <- valid_vars_match[1]
default_y_match <- valid_vars_match[min(2, length(valid_vars_match))]
default_x_pass <- valid_vars[1]
default_y_pass <- valid_vars[min(2, length(valid_vars))]

# ---- UI ----
ui <- dashboardPage(skin = "green",
  
  dashboardHeader(title = "Opgave 4"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Clustering", icon = icon("project-diagram"), startExpanded = TRUE,
               menuSubItem("Afleveringer", tabName = "pass_tab"),
               menuSubItem("Kampe", tabName = "match_tab"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ---- PASS TAB ----
      tabItem(tabName = "pass_tab",
              fluidRow(
                box(width = 3, title = "Filtre",
                    
                    selectInput("team_filter_pass", "Vælg hold:",
                                choices = c("Alle hold", sort(unique(afleveringer_pca_df$TEAMNAME))),
                                selected = "Alle hold"),
                    
                    selectInput("xvar_pass", "Vælg X-akse:",
                                choices = nice_names_pass,
                                selected = default_x_pass),
                    
                    selectInput("yvar_pass", "Vælg Y-akse:",
                                choices = nice_names_pass,
                                selected = default_y_pass)
                ),
                
                box(width = 9, title = "Clustering afleveringer",
                    plotOutput("pca_plot"),
                    br(),
                    plotOutput("custom_scatter_plot"))
              )
      ),
      
      # ---- MATCH TAB ----
      tabItem(tabName = "match_tab",
              fluidRow(
                box(width = 3, title = "Filtre",
                    
                    selectInput("team_filter_match", "Vælg hold:",
                                choices = c("Alle hold", sort(unique(kampcluster$TEAMNAME))),
                                selected = "Alle hold"),
                    
                    selectInput("xvar_match", "Vælg X-akse:",
                                choices = nice_names_match,
                                selected = default_x_match),
                    
                    selectInput("yvar_match", "Vælg Y-akse:",
                                choices = nice_names_match,
                                selected = default_y_match)
                ),
                
                box(width = 9, title = "Clustering kampe",
                    plotlyOutput("kamp_pca_plot"),
                    br(),
                    plotlyOutput("custom_scatter_plot_kamp"))
              )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # ---- FILTERS ----
  filtered_pca <- reactive({
    if (input$team_filter_pass == "Alle hold") afleveringer_pca_df
    else afleveringer_pca_df %>% filter(TEAMNAME == input$team_filter_pass)
  })
  
  kamp_filtered_pca <- reactive({
    if (input$team_filter_match == "Alle hold") pca_df
    else pca_df %>% filter(TEAMNAME == input$team_filter_match)
  })
  
  # ---- PASS PCA (FAST) ----
  output$pca_plot <- renderPlot({
    df <- filtered_pca()
    req(nrow(df) > 0)
    
    ggplot(df, aes(PC1, PC2, color = cluster_name)) +
      geom_point(alpha = 0.5) +
      theme_minimal() +
      labs(
        x = "Defensiv zone → Offensiv zone",
        y = "Retningsbaseret progression",
        color = "Afleveringstype"
      )
  })
  
  # ---- PASS SCATTER ----
  output$custom_scatter_plot <- renderPlot({
    
    req(input$xvar_pass, input$yvar_pass)
    
    df <- passesclustertest
    
    if (input$team_filter_pass != "Alle hold") {
      df <- df[df$TEAMNAME == input$team_filter_pass, ]
    }
    
    x_label <- names(nice_names_pass)[nice_names_pass == input$xvar_pass]
    y_label <- names(nice_names_pass)[nice_names_pass == input$yvar_pass]
    
    if (length(x_label) == 0) x_label <- input$xvar_pass
    if (length(y_label) == 0) y_label <- input$yvar_pass
    
    ggplot(df,
           aes(x = .data[[input$xvar_pass]],
               y = .data[[input$yvar_pass]],
               color = cluster_name)) +
      geom_point() +
      theme_minimal() +
      labs(
        x = x_label,
        y = y_label,
        color = "Afleveringstype"
      )
  })
  
  # ---- MATCH PCA (INTERACTIVE) ----
  output$kamp_pca_plot <- renderPlotly({
    
    df <- kamp_filtered_pca()
    req(nrow(df) > 0)
    
    p <- ggplot(df,
                aes(PC1, PC2, color = Kamptype,
                    text = paste("Hold:", TEAMNAME,
                                 "<br>Kamp:", MATCHLABEL))) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(
        x = "Aktivitet / tempo / volumen",
        y = "Kvalitet / intensitet"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # ---- MATCH SCATTER ----
  output$custom_scatter_plot_kamp <- renderPlotly({
    
    req(input$xvar_match, input$yvar_match)
    
    df <- kampcluster
    
    if (input$team_filter_match != "Alle hold") {
      df <- df[df$TEAMNAME == input$team_filter_match, ]
    }
    
    x_label <- names(nice_names_match)[nice_names_match == input$xvar_match]
    y_label <- names(nice_names_match)[nice_names_match == input$yvar_match]
    
    if (length(x_label) == 0) x_label <- input$xvar_match
    if (length(y_label) == 0) y_label <- input$yvar_match
    
    p <- ggplot(df,
                aes(x = .data[[input$xvar_match]],
                    y = .data[[input$yvar_match]],
                    color = Kamptype,
                    text = paste("Hold:", TEAMNAME,
                                 "<br>Kamp:", MATCHLABEL))) +
      geom_point() +
      theme_minimal() +
      labs(x = x_label, y = y_label)
    
    ggplotly(p, tooltip = "text")
  })
}

# ---- RUN ----
shinyApp(ui, server)