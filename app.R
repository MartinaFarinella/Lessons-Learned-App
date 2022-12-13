#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# install.packages("slam")



library(ggplot2)
library(tidyverse)
library(lubridate)
library(tidymodels)
library(tidytext)
library(topicmodels)
library(ggtern)
R.methodsS3::setMethodS3("print", "ggplot", ggplot2:::print.ggplot)
R.methodsS3::setMethodS3("plot", "ggplot", ggplot2:::plot.ggplot)
R.methodsS3::setMethodS3("grid.draw", "ggplot", ggplot2:::grid.draw.ggplot)

library(FactoMineR)
library(factoextra)
library(corrplot)
# library(graphics)
# library(vcd)
# library(slam)
library(shiny)
library(shinydashboard)
library(shinyalert)
# library(R.methodsS3) # Needed to 
# library(devtools)
library(igraph)
library(ggraph)
library(qwraps2)
options(qwraps2_markup = "markdown")
library(knitr)


if(file.exists("../data/data_shiny_last.rda")){
  # Load the last results
  load("../data/data_shiny_last.rda")
}else{
  # Load the initial results
  load("../data/data_shiny.rda")
}



load("../data/pcr_lessons_learned_cleaned.rda")



base <- 
  pcr_lessons_learned %>%
  select(ID, lessons_learned_clean, language_report, Region) %>% 
  mutate(word_lemma = textstem::lemmatize_strings(lessons_learned_clean)) %>% 
  select(-lessons_learned_clean)


nb_reports <- nrow(pcr_lessons_learned)

# Words that need to be counted as stop words (so they will be removed from the analysis)
additional_words_to_remove <- 
  read_delim("../data/words_to_remove.txt", delim = ",") %>% 
  mutate(word = str_to_lower(word))

# Word for which we wish to change the spelling (old on the left, new on the right)
to_change <- 
  c("organisation", "organization",
    "subsidised", "subsidized",
    "initiatives", "initiative",
    "prioritisation", "prioritization",
    "favourable", "favorable",
    "unfavourable", "unfavorable",
    "programme", "program") %>% 
  matrix(ncol = 2, byrow = TRUE) %>% 
  as_tibble() %>% 
  magrittr::set_colnames(c("old", "new"))


words_reports <- 
  base %>% 
  unnest_tokens(word, word_lemma) %>% 
  anti_join(stop_words) %>% 
  anti_join(additional_words_to_remove) %>% 
  left_join(to_change, by = c("word" = "old")) %>% 
  mutate(word = ifelse(!is.na(new), new, word)) %>% 
  select(-new)


bigrams_reports <-
  base %>%
  unnest_tokens(bigram, word_lemma, token = "ngrams", n = 2) %>%
  separate(bigram, c("word_1", "word_2"), sep = " ", remove = FALSE)

list_of_words <- 
  words_reports %>% 
  anti_join(additional_words_to_remove) %>% 
  filter(language_report == "English") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:1000) %>% 
  magrittr::extract2("word")


# Adding words manually
list_of_words <- c(list_of_words, c("run")) %>% unique()

# seed_words_2 <-
#   list(
#     c("delivery", "social"),
#     c("performance", "poverty"),
#     c("impact", "change"),
#     NULL)

clusters <- 
  as_tibble(lda_seeded_2@gamma) %>% 
  mutate(ID = reports_dtm$dimnames$Docs) %>% 
  pivot_longer(cols = -ID, values_to = "posterior", names_to = "cluster") %>% 
  mutate(cluster = str_c("Topic ", str_sub(cluster, 2))) %>% 
  group_by(ID) %>% 
  arrange(desc(posterior)) %>% 
  slice(1) %>% 
  ungroup()



size_text <- 10
library(grid)
theme_mini <- 
  function(...)
    theme(text = element_text(size = size_text),
          plot.background = element_rect(fill="transparent", color=NA),
          panel.background = element_rect(fill = "transparent", color=NA),
          panel.border = element_blank(),
          axis.text = element_text(), 
          legend.text = element_text(size = rel(1.1)),
          legend.title = element_text(size = rel(1.1)),
          legend.background = element_rect(fill="transparent", color=NULL),
          legend.position = "bottom", 
          legend.direction = "horizontal", legend.box = "vertical",
          legend.key = element_blank(),
          panel.spacing = unit(1, "lines"),
          panel.grid.major = element_line(colour = "grey90"), 
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0, size = rel(1.3), face = "bold"),
          plot.title.position = "plot",
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          strip.background = element_rect(fill=NA, colour = NA),
          strip.text = element_text(size = rel(1.1)))

names_variables_chr <- c("Region", "language_report", "PAR Value",
                         "Cooperating Institution",
                         "Environmental and Social Category",
                         "Climate Risk Classification"
)


names_variables_num <- c("Number of SIS Missions",
                         "Total Project Financing", "Effectiveness Lag (months)",
                         "IFAD Total Financing",
                         "Domestic Cofinancing", "International Cofinancing"
)

colours_topics_2 <- 
  c("Topic 1" = "#D81B60",
    'Topic 2' = "#1E88E5",
    'Topic 3' = "#FFC107",
    'Topic 4' = "#004D40")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Preparation", tabName = "data_prep", icon = icon("database")),
      menuItem("Descriptive Statistics", tabName = "desc_stat", icon = icon("bar-chart-o")),
      menuItem("Model", tabName = "model", icon = icon("cog")),
      menuItem("Results", tabName = "results", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")),
    tags$link(rel = "stylesheet", href = "style.css"),
    useShinyalert(),
    fluidRow(
      tags$head(
        tags$style(
          HTML(".shiny-notification-warning {
               position: fixed;
               bottom: 10px;
               left: 15px;
               background-color: #66ff66;
               color: black;
               border: 1px solid #00cc66;
               }"),
          HTML(".shiny-notification-message {
               position: fixed;
               bottom: 60px;
               left: 15px;
               background-color: #66ff66;
               color: black;
               border: 1px solid #00cc66;
          }"
          )
        )
      ),
      tabItems(
        # ----------------------------------- #
        # Onglet de statistiques descriptives #
        # ----------------------------------- #
        tabItem(tabName = "data_prep",
                column(12,
                       box(
                         title = "Stop words",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         h3("Stop words"),
                         DT::dataTableOutput("table_details_stop_words"),
                         h3("Additional stopwords:"),
                         p("These words are obtained from the file `../data/words_to_remove.txt`."),
                         DT::dataTableOutput("table_details_additional_stop_words"),
                         h3("Add/Remove additional stopwords:"),
                         p(str_c("If you want to add or remove stop words (this will affect the txt file), list those in the box below. ",
                                 "Please note that the additional stop words will be turned to lowercase..")),
                         selectizeInput("item_modify_stopwords",
                                        label = "Modify stopwords",
                                        choices = additional_words_to_remove$word,
                                        selected = NA,
                                        multiple = TRUE,
                                        options = list(create = TRUE)
                         ),
                         actionButton("bouton_remove_word", "Remove from stopwords", width = "200px",
                                      style="color: #fff; background-color: #EE324E; border-color: #0033cc"),
                         actionButton("bouton_add_word", "Add to stopwords", width = "200px",
                                      style="color: #fff; background-color: #61C250; border-color: #A5D867")
                       )
                )
        ),
        # # ----------------------------------- #
        # # Onglet de statistiques descriptives #
        # # ----------------------------------- #
        tabItem(tabName = "desc_stat",
                column(12,
                       box(
                         title = "Most frequent words (1-gram)",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         plotOutput("plot_top_words"),
                         plotOutput("plot_top_words_docs"),
                       ),
                       box(
                         title = "Most frequent words (bigrams)",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         sliderInput("item_bigram_nb_words", 
                                     p("Minimum number of occurrence"), 
                                     min = 5, max = 100, step = 1, value = 30),
                         
                         plotOutput("plot_bigrams"),
                         h3("What are the words associated to another?"),
                         selectizeInput("input_bigram_word",
                                        label = "Enter a word",
                                        choices = list_of_words,
                                        selected = list_of_words[[1]],
                                        multiple = FALSE,
                                        options = list(create = FALSE)),
                         radioButtons(inputId = "item_bigram_position",
                                      label = "Would you like to have a look at the words after or before the selected word?",
                                      choices = list("Before" = "Before",
                                                     "After" = "After"),
                                      selected = "After",
                                      inline = TRUE),
                         h3("Number of occurrence of the selected word:"),
                         verbatimTextOutput("input_bigram_word_no_occurrences"),
                         h3("Number of documents in which the selected word occurs:"),
                         verbatimTextOutput("input_bigram_word_no_occurrences_docs"),
                         h3("Words next to the selected word:"),
                         DT::dataTableOutput("table_bigrams")
                       )
                       
                       
                       
                )
        ),
        # ---------------- #
        # Onglet de modele #
        # ---------------- #
        tabItem(tabName = "model",
                column(12,
                       sliderInput("nb_topics", 
                                   p("Number of clusters"), 
                                   min = 2, max = 4, step = 1, value = length(seed_words_2)),
                       selectizeInput("seeded_words_1",
                                      label = "Seed words for topic 1",
                                      choices = list_of_words,
                                      selected = seed_words_2[[1]],
                                      multiple = TRUE,
                                      options = list(create = FALSE)),
                       selectizeInput("seeded_words_2",
                                      label = "Seed words for topic 2",
                                      choices = list_of_words,
                                      selected = seed_words_2[[2]],
                                      multiple = TRUE,
                                      options = list(create = FALSE)),
                       conditionalPanel(condition = "input.nb_topics > 2",
                                        selectizeInput("seeded_words_3",
                                                       label = "Seed words for topic 3",
                                                       choices = list_of_words,
                                                       selected = seed_words_2[[3]],
                                                       multiple = TRUE,
                                                       options = list(create = FALSE))                
                       ),
                       conditionalPanel(condition = "input.nb_topics > 3",
                                        selectizeInput("seeded_words_4",
                                                       label = "Seed words for topic 4",
                                                       choices = list_of_words,
                                                       selected = NA,
                                                       multiple = TRUE,
                                                       options = list(create = FALSE))                
                       ),
                       p("Once you have set the desired number of topics, click on \"Run the model\" (and be patient)."),
                       p("Then move to the \"Result tab.\""),
                       actionButton("button_run_model", "Run the model")
                       # )
                )
        ), # Fin de l'element `model`
        # --------------------- #
        # Onglet des resultats  #
        # --------------------- #
        tabItem(tabName = "results",
                column(width=12,
                       box(
                         title = "Summary of settings",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         h3("Number of topics selected:"),
                         verbatimTextOutput("nb_topics_selected"),
                         h3("Seed words used:"),
                         verbatimTextOutput("seeded_words_selected")
                       ),
                       box(
                         title = "Description of the topics", 
                         width = NULL, solidHeader = TRUE, status = "primary",
                         plotOutput("plot_top_words_topics"),
                         downloadButton('download_top_words', 'Download')
                       ),
                       box(
                         title = "Correlation with categorical variables",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         selectInput("variable_corr_categorical",
                                     "Select a variable",
                                     choices = names_variables_chr,
                                     selected = "Region"),
                         plotOutput("plot_triangle"),
                         plotOutput("plot_ca"),
                         plotOutput("corr_plot")
                       ),
                       box(
                         title = "Correlation with numerical variables",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         selectInput("variable_corr_numerical",
                                     "Select a variable",
                                     choices = names_variables_num,
                                     selected = "Total Project Financing"),
                         plotOutput("plot_triangle_numerical")
                       ),
                       box(
                         title = "Descriptive statistics of the topics",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         h3("Descriptive statistics for a specific variable relative to the topic with the highest probability"),
                         selectInput("table_des_stat_topic_variable_name",
                                     "Select a variable",
                                     choices = c(names_variables_chr, names_variables_num),
                                     selected = "Total Project Financing"),
                         uiOutput("table_des_stat_topic_variable"),
                         h3("Occurrence of a specific word in the topics"),
                         selectizeInput("input_classification_word",
                                        label = "Enter a word",
                                        choices = list_of_words,
                                        selected = list_of_words[[1]],
                                        multiple = FALSE,
                                        options = list(create = FALSE)),
                         plotOutput("plot_distrib_clusters_word"),
                         downloadButton('download_distrib_clusters_word', 'Download'),
                         h5("Extract of the documents where the selected word appears"),
                         p(str_c("Please note that when the word appears multiple times in a document, ",
                                 "multiple extracts are returned (one per row).")),
                         p(str_c("The column \"posterior\" gives the estimated probability that the document contains the topic ",
                                 "provided in column \"cluster\".")),
                         sliderInput("item_classification_nb", 
                                     p("Number of characters preceeding and following the selected word:"), 
                                     min = 0, max = 1000, step = 1, value = 50),
                         DT::dataTableOutput("table_classification_word_extract"),
                         downloadButton('download_classification_word_extract', 'Download'),
                       ),
                       
                       
                       box(
                         title = "Save the results",
                         width = NULL, solidHeader = TRUE, status = "primary",
                         p(str_c("If you want to save the results of this model (including the inclusion/exclusion of stop words), ",
                                 "click on the \"Save results\" button below. Next time you launch this Shiny App, these results will be used.")),
                         actionButton("buttton_save_results", "Save results", width = "200px",
                                      style="color: #fff; background-color: #61C250; border-color: #A5D867"),
                         p(str_c("If you want to restore the initial results (from October 2020)",
                                 "click on the \"Delete all results\" button below. Note that there is no there is no going backwards ",
                                  "after clicking. The last results will be removed from the computer. You will then need to restart the app.")),
                         actionButton("buttton_restore_results", "Delete all results", width = "200px",
                                      style="color: #fff; background-color: #EE324E; border-color: #0033cc"),
                         checkboxInput(inputId = "buttton_restore_results_check",
                                       label = "Are you sure you want to delete all the previously estimated models ?",
                                       value = FALSE)
                       )
                )# End column
        ) # Fin de l'element `results`
      )# Fin de tabItems
    )# Fin de fluidRow
  )# Fin de dashboardBody
)# Fin de dashboardPage

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Stop words
  additional_words_to_remove <- 
    read_delim("../data/words_to_remove.txt", delim = ",") %>% 
    mutate(word = str_to_lower(word))
  
  data <- reactiveValues(
    nb_topics = 4,
    seed_words = seed_words_2,
    delta_s = delta_s_2,
    lda_seeded = lda_seeded_2,
    colours_topics = colours_topics_2,
    G = G,
    additional_words_to_remove = additional_words_to_remove,
    reports_dtm = reports_dtm,
    clusters = clusters
  )
  
  ########
  # DATA #
  ########
  {
    output$table_details_stop_words = DT::renderDataTable({
      DT::datatable(stop_words)
    })
    
    output$table_details_additional_stop_words = DT::renderDataTable({
      DT::datatable(data$additional_words_to_remove)
    })
    
    
    # Button to add stop words to the list
    observeEvent(input$bouton_add_word,{
      
      # Add values from the table in the reactive values
      data$additional_words_to_remove <- 
        data$additional_words_to_remove %>% 
        bind_rows(
          tibble(word = str_to_lower(input$item_modify_stopwords))
        ) %>%
        unique()
      
      # Updating the selectizeInput values
      updateSelectizeInput(session = session,
                           inputId = "item_modify_stopwords",
                           choices = data$additional_words_to_remove$word,
                           selected = NULL)
      
      new_list_of_words <- 
        list_of_words[! list_of_words %in% data$additional_words_to_remove$word]
      
      updateSelectizeInput(session = session,
                           inputId = "input_bigram_word",
                           choices = new_list_of_words,
                           selected = NULL)
      
      # updating the txt file
      write_delim(x = data$additional_words_to_remove, file = "../data/words_to_remove.txt", delim = ",")
      
    })
    
    # Button to remove stop words from the list
    observeEvent(input$bouton_remove_word,{
      
      # Removing values from the table in the reactive values
      data$additional_words_to_remove <- 
        data$additional_words_to_remove %>% 
        filter( !word %in% input$item_modify_stopwords )
      
      # Updating the selectizeInput values
      updateSelectizeInput(session = session,
                           inputId = "item_modify_stopwords",
                           choices = data$additional_words_to_remove$word,
                           selected = NULL)
      
      new_list_of_words <- 
        list_of_words[! list_of_words %in% data$additional_words_to_remove$word]
      
      updateSelectizeInput(session = session,
                           inputId = "input_bigram_word",
                           choices = new_list_of_words,
                           selected = NULL)
      
      # updating the txt file
      write_delim(x = data$additional_words_to_remove, file = "../data/words_to_remove.txt", delim = ",")
    })
    
    
    get_word_reports <- function(){
      words_reports %>%
        anti_join(stop_words) %>% 
        anti_join(data$additional_words_to_remove)
    }
    
    
    get_bigrams_reports_filtered <- function(){
      
      stop_words_all <- 
        stop_words %>% 
        bind_rows(data$additional_words_to_remove)
      
      
      bigrams_reports_filtered <- 
        bigrams_reports %>% 
        anti_join(stop_words_all, by = c("word_1" = "word")) %>% 
        anti_join(stop_words_all, by = c("word_2" = "word"))
    }
  }
  
  ##########################
  # DESCRIPTIVE STATISTICS #
  ##########################
  {
    
    
    output$plot_top_words <- renderPlot({
      
      words_reports <- get_word_reports()
      
      words_reports %>%
        count(word, sort = TRUE) %>%
        slice(1:20) %>% 
        # filter(n > 80) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n)) +
        geom_col() +
        xlab(NULL) +
        coord_flip() +
        labs(title = "Top 20 words that appear within all the reports.") +
        theme_mini()
      
    })
    

    output$download_top_words <- downloadHandler(
      filename = function() {
        paste('top_words_topics_', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        
        words_reports <- get_word_reports()
        
        data <- 
          words_reports %>%
          count(word, sort = TRUE) %>%
          slice(1:20) %>% 
          mutate(word = reorder(word, n))
        
        
        write_csv(data, con)
      }
    )
    
    
    output$plot_top_words_docs <- renderPlot({
      
      words_reports <- get_word_reports()
      
      words_reports %>% 
        group_by(ID) %>% 
        unique() %>% 
        ungroup() %>% 
        count(word, sort = TRUE) %>% 
        slice(1:20) %>% 
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n)) +
        geom_col() +
        labs(x = NULL, y = "Number of reports containing the word") +
        coord_flip() +
        labs(title = "Most recurrent words among the reports.") +
        theme_mini()
      
    })
    
    
    output$table_bigrams = DT::renderDataTable({
      
      bigrams_reports_filtered <- get_bigrams_reports_filtered()
      
      if(input$item_bigram_position == "Before"){
        res_table <- 
          bigrams_reports_filtered %>% 
          filter(word_2 == input$input_bigram_word) %>% 
          count(word_1, sort = TRUE)
        
      }else{
        res_table <- 
          bigrams_reports_filtered %>% 
          filter(word_1 == input$input_bigram_word) %>% 
          count(word_2, sort = TRUE)
      }
      
      DT::datatable(res_table)
    })
    
    # HTML box returned to the user to inform on the number of topics
    output$input_bigram_word_no_occurrences <- renderText({
      nn <- 
        words_reports %>% 
        filter(word == !!input$input_bigram_word) %>% 
        count(word) %>% 
        magrittr::extract2("n")
      
      HTML(str_c(input$input_bigram_word, " is repetead ", nn, " times overall."))
    })
    
    # HTML box returned to the user to inform on the number of topics
    output$input_bigram_word_no_occurrences_docs <- renderText({
      
      nn <- 
        words_reports %>% 
        filter(word == !!input$input_bigram_word) %>% 
        group_by(ID) %>% 
        slice(1) %>% 
        ungroup() %>% 
        nrow()
      
      HTML(str_c(input$input_bigram_word, " appears in ", nn, " reports out of ", nb_reports, "."))
    })
    
    
    output$plot_bigrams <- renderPlot({
      
      bigrams_reports_filtered <- get_bigrams_reports_filtered()
      
      bigram_counts <- 
        bigrams_reports_filtered %>% 
        count(word_1, word_2, sort = TRUE)
      
      bigram_graph <- 
        bigram_counts %>%
        filter(n > input$item_bigram_nb_words) %>%
        graph_from_data_frame()
      
      set.seed(1)
      ggraph(bigram_graph, layout = "fr") +
        geom_edge_link() +
        geom_node_point() +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1)
    })
    
    
  }
  
  #########
  # MODEL #
  #########
  {
    
    # Button to run the LDA Model
    observeEvent(input$button_run_model,{
      data$nb_topics <- input$nb_topics
      update_seed_words()
      update_dtm()
      get_delta_s()
      estimate_lda()
      update_colours()
      update_G()
      update_clusters()
    })
   
    
    # Function to get the words given by the user to be used as seedwords
    update_seed_words <- function(){
      
      seed_words <-
        list(
          input$seeded_words_1,
          input$seeded_words_2,
          input$seeded_words_3,
          input$seeded_words_4)
      
      seed_words <- seed_words[1:input$nb_topics]
      
      # Updating the reactive values
      data$seed_words <- seed_words
    }
    
    update_dtm <- function(){
      
      words_reports <- get_word_reports()
      
      reports_dtm <- 
        words_reports %>% 
        group_by(ID) %>% 
        count(word, sort=TRUE) %>% 
        cast_dtm(document = "ID", term = "word", value = "n")
      
      # Updating the reactive values
      data$reports_dtm <- reports_dtm
      
    }
    
    update_clusters <- function(){
      
      lda_seeded <- data$lda_seeded
      reports_dtm <- data$reports_dtm
      
      clusters <- 
        as_tibble(lda_seeded_2@gamma) %>% 
        mutate(ID = reports_dtm$dimnames$Docs) %>% 
        pivot_longer(cols = -ID, values_to = "posterior", names_to = "cluster") %>% 
        mutate(cluster = str_c("Topic ", str_sub(cluster, 2))) %>% 
        group_by(ID) %>% 
        arrange(desc(posterior)) %>% 
        slice(1) %>% 
        ungroup()
      
      # Updating the reactive values
      data$clusters <- clusters
      
    }
    
    # Function to extract the delta matrix
    get_delta_s <- function(){
      reports_dtm <- data$reports_dtm
      words_from_dtm <- reports_dtm$dimnames$Terms
      seed_words <- data$seed_words
      nb_topics <- data$nb_topics
      # Setting the topic number for each word
      i <- NULL
      for(k in 1:length(seed_words)) i <- c(i, rep(k, length(seed_words[[k]])))
      # Positions of the words in `list_of_words`
      j <- match(unlist(seed_words), list_of_words)
      
      seed_weight <- 500 - 0.1
      delta_s <- 
        slam::simple_triplet_matrix(i, j, 
                                    v = rep(seed_weight, length(i)),
                                    nrow = nb_topics,
                                    ncol = ncol(reports_dtm))
      
      # Updating the reactive values
      data$delta_s <- delta_s
    }
    
    # Function to estimate the LDA model
    estimate_lda <- function(){
      
      nb_topics <- data$nb_topics
      delta_s <- data$delta_s
      reports_dtm <- data$reports_dtm
      
      shinyalert("Information:", str_c("Estimation in progress with ", nb_topics, " topics. \n",
                                       "Please wait, this can take a moment. Another message will tell you when the estimation is done."),
                 type = "warning")
      
      set.seed(123)
      lda_seeded <- LDA(reports_dtm, k = nb_topics, method = "Gibbs", seedwords = delta_s)
      
      shinyalert("Information:", str_c("Estimation done. Move to the Results tab."),
                 type = "warning")
      
      # Updating the reactive values
      data$lda_seeded <- lda_seeded
    }
    
  }
  
  ###########
  # RESULTS #
  ###########
  {
    update_colours <- function(){
      nb_topics <- data$nb_topics
      if(nb_topics > 4){
        gg_color_hue <- function(n) {
          hues = seq(15, 375, length = n + 1)
          hcl(h = hues, l = 65, c = 100)[1:n]
        }
        colours_topics = gg_color_hue(nb_topics)
        names(colours_topics) <- str_c("Topic ", 1:nb_topics)
      }else{
        colours_topics <- 
          c("Topic 1" = "#D81B60",
            'Topic 2' = "#1E88E5",
            'Topic 3' = "#FFC107",
            'Topic 4' = "#004D40")
      }
      # Updating the reactive values
      data$colours_topics <- colours_topics
    }
    
    
    
    
    
    # HTML box returned to the user to inform on the number of topics
    output$nb_topics_selected <- renderText({
      HTML(data$nb_topics)
    })
    
    # HTML box returned to the user to inform the seeded words used
    output$seeded_words_selected <- renderText({
      seed_words <- data$seed_words
      res <- NULL
      for(i in 1:length(seed_words)){
        res <- str_c(res, "Topic ", i, ": ", str_c(seed_words[[i]], collapse = ", "), "\n")
      }
      HTML(res)
    })
    
    # Graph with the most common words
    get_graph_most_common_words <- function(){
      
      lda_seeded <- data$lda_seeded
      colours_topics <- data$colours_topics
      
      ap_seed_topics <- tidy(lda_seeded, matrix = "beta")
      # Top 10 words for each topic
      ap_seeded_topics_top_terms <- 
        ap_seed_topics %>%
        group_by(topic) %>%
        top_n(10, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
      
      nb_topics <- data$nb_topics
      
      # Graph
      ap_seeded_topics_top_terms %>%
        mutate(topic = factor(topic, levels = seq_len(nb_topics), 
                              labels = str_c("Topic ", seq_len(nb_topics)))) %>% 
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        tidytext::scale_x_reordered() +
        labs(y = "posterior proba (the higher it is, the more \"significant\" is the word in that cluster)",
             x = NULL,
             title = "Most common terms within each topic (Seeded Latent Dirichet Allocation)") +
        scale_fill_manual(NULL, values = colours_topics) +
        theme_mini()
      
    }
    
    output$plot_top_words_topics <- renderPlot({
      
      if(!is.null(data$delta_s)){
        p <- get_graph_most_common_words()
        print(p)
      }
    })
    
    update_G <- function(){
      lda_seeded <- data$lda_seeded
      nb_topics <- data$nb_topics
      seed_words <- data$seed_words
      reports_dtm <- data$reports_dtm
      
      Gamma <- lda_seeded@gamma
      Gamma_3 <- Gamma[,1:(nb_topics-1)]/apply(Gamma[,1:(nb_topics-1)],1,sum)
      G <- as_tibble(Gamma_3)
      names(G) <- sapply(seed_words[1:3], first)
      G <- G %>% mutate(ID = reports_dtm$dimnames$Docs)
      G <- 
        G %>% 
        left_join(pcr_lessons_learned, by = "ID")
      # Updating the reactive values
      data$G <- G
    }
    
    # Function to prepare data for the triangle graph
    format_G <- function(categorical = TRUE){
      G <- data$G
      
      if(categorical){
        variable_of_interest <- input$variable_corr_categorical
      }else{
        variable_of_interest <- input$variable_corr_numerical
      }
      
      G <- G %>% 
        mutate(variable_of_interest = !!sym(variable_of_interest)) %>% 
        filter(!is.na(variable_of_interest))
      
      if(categorical){
        if(!is.factor(G$variable_of_interest)){
          G <- G %>% 
            mutate(variable_of_interest = factor(variable_of_interest))
        }
      }else{
        breaks_q <- 
          quantile(G$variable_of_interest, probs = seq(0, 1, by = .2))
        breaks_q[1] <- breaks_q[1]-1
        
        G <- 
          G %>% 
          mutate(cut_quantiles = cut(variable_of_interest,
                                     breaks = breaks_q))
      }
      
      G
    }
    
    
    # Get the data to prepare the triangle graph
    get_DT <- function(){
      G <- format_G(categorical=TRUE)
      DT <- matrix(0,3,length(levels(G$variable_of_interest)))
      rownames(DT) <- names(G[,1:3])
      colnames(DT) <- levels(G$variable_of_interest)
      for(i in 1:nrow(G)){
        DT[,as.character(G$variable_of_interest[i])] <- 
          as.numeric(DT[,as.character(G$variable_of_interest[i])] + round(G[i,1:3]*100))
      }
      DT
    }
    
    # Triangle graph for categorical variables
    output$plot_triangle <- renderPlot({
      
      GG <- format_G(categorical=TRUE)
      
      names_G <- sapply(data$seed_words[1:3], first)
      
      GG <- GG %>% 
        rename(x = !!sym(names_G[1]),
               y = !!sym(names_G[2]),
               z = !!sym(names_G[3]))
      
      R.methodsS3::setMethodS3("print", "ggplot", ggtern:::print.ggplot)
      R.methodsS3::setMethodS3("plot", "ggplot", ggtern:::plot.ggplot)
      R.methodsS3::setMethodS3("grid.draw", "ggplot", ggtern:::grid.draw.ggplot)
      p <- 
        ggtern::ggtern(data = GG,
                       aes(x = x, y = y, z = z)) + 
        geom_point(aes(colour = variable_of_interest), size = 1, alpha = 0.5) +
        theme_mini() +
        guides(colour = guide_legend(nrow = 3)) +
        labs(x = names_G[1], y = names_G[2], z = names_G[3])
      
      if(!is.null(p)){
        print(p)
      }
      R.methodsS3::setMethodS3("print", "ggplot", ggplot2:::print.ggplot)
      R.methodsS3::setMethodS3("plot", "ggplot", ggplot2:::plot.ggplot)
      R.methodsS3::setMethodS3("grid.draw", "ggplot", ggplot2:::grid.draw.ggplot)
      
    })
    
    # Triangle graph for numerical variables
    output$plot_triangle_numerical <- renderPlot({
      
      GG <- format_G(categorical=FALSE)
      names_G <- sapply(data$seed_words[1:3], first)
      
      GG <- GG %>% 
        rename(x = !!sym(names_G[1]),
               y = !!sym(names_G[2]),
               z = !!sym(names_G[3]))
      
      R.methodsS3::setMethodS3("print", "ggplot", ggtern:::print.ggplot)
      R.methodsS3::setMethodS3("plot", "ggplot", ggtern:::plot.ggplot)
      R.methodsS3::setMethodS3("grid.draw", "ggplot", ggtern:::grid.draw.ggplot)
      
      nb <- GG$cut_quantiles %>% levels() %>% length()
      
      p <- 
        ggtern::ggtern(data = GG,
                       aes(x = x, y = y, z = z)) + 
        geom_point(aes(colour = cut_quantiles), size = 1) +
        theme_mini() +
        labs(x = names_G[1], y = names_G[2], z = names_G[3]) +
        guides(colour = guide_legend(nrow = 3)) +
        scale_colour_manual("Value", values = rev(heat.colors(nb)))
      
      if(!is.null(p)){
        print(p)
      }
      R.methodsS3::setMethodS3("print", "ggplot", ggplot2:::print.ggplot)
      R.methodsS3::setMethodS3("plot", "ggplot", ggplot2:::plot.ggplot)
      R.methodsS3::setMethodS3("grid.draw", "ggplot", ggplot2:::grid.draw.ggplot)
    })
    
    # Correlation plot
    output$corr_plot <- renderPlot({
      DT <- get_DT()
      corrplot::corrplot(chisq.test(DT)$residuals, is.cor = FALSE)
    })
    
    # Plot of the results from the correspondance analysis
    output$plot_ca <- renderPlot({
      DT <- get_DT()
      
      res_ca <- CA(DT, graph = FALSE)
      fviz_ca_biplot(res_ca, repel = TRUE)
      
    })
    
    
    # Descriptive statistics #
    # ---------------------- #
    
    output$table_des_stat_topic_variable = renderUI({
      
      clusters <- data$clusters
      
      clusters_df <- 
        pcr_lessons_learned %>% 
        left_join(clusters)
      
      variable_of_interest <- input$table_des_stat_topic_variable_name
      
      table_char <- 
        clusters_df %>% 
        dplyr::select(!!!syms(variable_of_interest), cluster) %>% 
        qwraps2::summary_table(by = "cluster") %>% 
        knitr::kable()
      
      
      
      HTML(knit2html(text=capture.output(table_char), fragment.only=TRUE))
    })
    
    
    get_reports_with_word <- function(){
      
      clusters <- data$clusters
      selected_word <- input$input_classification_word
      
      clusters_df <- 
        pcr_lessons_learned %>% 
        left_join(clusters)
      
      reports_with_word <- 
        clusters_df %>% 
        select(ID, posterior, cluster, lessons_learned_clean) %>% 
        rowwise() %>% 
        mutate(positions = str_locate_all(lessons_learned_clean, !!selected_word)) %>% 
        unnest(cols = positions) %>% 
        mutate(start = positions[, 1],
               end = positions[, 2]) %>% 
        ungroup() %>% 
        mutate(extract = str_sub(lessons_learned_clean,
                                 start-input$item_classification_nb,
                                 start+input$item_classification_nb)) %>% 
        select(-positions, -start, -end, -lessons_learned_clean)
      reports_with_word
    }
    
    output$plot_distrib_clusters_word <- renderPlot({
      
      
      reports_with_word <- get_reports_with_word()
      colours_topics <- data$colours_topics
      
      reports_with_word %>% 
        select(ID, cluster) %>% 
        unique() %>% 
        group_by(cluster) %>% 
        count() %>% 
        ungroup() %>% 
        mutate(proportion = round(100 * n / sum(n)),
               proportion_lab = str_c(proportion, "%")) %>% 
        ggplot(data = ., aes(x = fct_reorder(cluster, proportion), y = proportion, fill = cluster)) +
        geom_bar(stat = "identity") +
        labs(x = NULL, y = "Proportion",
             title = str_c("Distribution of clusters containing the word: ", input$input_classification_word)) +
        scale_fill_manual(NULL, values = colours_topics) +
        theme_mini()
      
    })
    
    output$download_distrib_clusters_word <- downloadHandler(
      filename = function() {
        paste('distrib_clusters_word_', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        
        
        reports_with_word <- get_reports_with_word()
        data <- 
          reports_with_word %>% 
          select(ID, cluster) %>% 
          unique() %>% 
          group_by(cluster) %>% 
          count() %>% 
          ungroup() %>% 
          mutate(proportion = round(100 * n / sum(n)),
                 proportion_lab = str_c(proportion, "%")) %>% 
          mutate(word = input$input_classification_word)
        
        
        write_csv(data, con)
      }
    )
    
    
    output$table_classification_word_extract = DT::renderDataTable({
      
      reports_with_word <- get_reports_with_word()
      
      DT::datatable(reports_with_word)
    })
    
    
    
    output$download_classification_word_extract <- downloadHandler(
      filename = function() {
        paste('classification_word_extract_', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        
        
        reports_with_word <- get_reports_with_word()
        
        write_csv(reports_with_word, con)
      }
    )
    
    
    
    # Save the results
    observeEvent(input$buttton_save_results,{
      
      lda_seeded_2 <- data$lda_seeded
      seed_words_2 <- data$seed_words
      delta_s_2 <- data$delta_s
      G <- data$G
      reports_dtm <- data$reports_dtm
      
      save(lda_seeded_2,pcr_lessons_learned, seed_words_2, delta_s_2, G, reports_dtm, file = "../data/data_shiny_last.rda")
      
      shinyalert("Information:", "Results saved to \"../data/data_shiny_last.rda\"",
                 type = "warning")
      
    })
    
    
    
    
    # Restore initial results
    observeEvent(input$buttton_restore_results,{
      
      if(input$buttton_restore_results_check){
        system("rm ../data/data_shiny_last.rda")
        shinyalert("Information:", "Results removed. Please close the app to restore the initial results.",
                   type = "warning")
        
        updateCheckboxInput(session = session,
                            inputId = "buttton_restore_results_check",
                            value = FALSE)
        
      }else{
        shinyalert("Information:", "The results were NOT deleted. You need to check the validation box first.",
                   type = "warning")
      }
      
      
      
      
    })
    
    
  }
  
}


# Run the application 
shinyApp(ui = ui, server = server)
