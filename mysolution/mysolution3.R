# Zadanie 3 - rozprzestrzenianie się informacji w sieciach - dane rzeczywiste + Shiny Apps

library(igraph)
library(ggplot2)
library(shiny)
library(here)

set.seed(42)

# funckja przygotowania danych + pre-cache sąsiedztwa i wag
prepare_network <- function() {
  # data_file <- here("css", "out.radoslaw_email_email")
  # dla shiny:
  data_file <- "https://bergplace.org/share/out.radoslaw_email_email"
  raw_df <- read.table(data_file, skip=2)
  
  edge_df <- raw_df[, 1:2]
  colnames(edge_df) <- c("from", "to")
  
  # konwersja typów
  edge_df$from <- as.character(edge_df$from)
  edge_df$to <- as.character(edge_df$to)
  
  g_raw <- graph_from_data_frame(edge_df, directed=TRUE)
  
  # obliczenie wag przed uproszczeniem grafu wg wzoru:
  #   w_ij = cnt_ij / cnt_i
  #   gdzie cnt_ij = liczba maili wysłanych z i do j (liczona z originalnego df)
  #   cnt_i  = liczba wszystkich maili wysłanych przez i

  # cnt_ij (count)
  edge_counts <- as.data.frame(table(edge_df$from, edge_df$to))
  colnames(edge_counts) <- c("from", "to", "count")
  edge_counts$from <- as.character(edge_counts$from)
  edge_counts$to <- as.character(edge_counts$to)
  edge_counts <- edge_counts[edge_counts$count > 0, ]
  
  # cnt_i (total)
  total_counts <- aggregate(count ~ from, edge_counts, sum)
  colnames(total_counts) <- c("from", "total")
  
  edge_counts <- merge(edge_counts, total_counts, by="from")
  edge_counts$weight <- edge_counts$count / edge_counts$total
  
  # tworzenie uproszczonego grafu (bez duplikatów i pętli)
  g <- simplify(g_raw, remove.multiple=TRUE, remove.loops=TRUE)
  
  # weryfikacja liczby wierzchołków (167) i krawędzi (5783)
  cat("Liczba węzłów:", vcount(g), "| Krawędzie:", ecount(g), "\n")
  
  # dodanie wag do uproszczonego grafu
  edge_list <- as_edgelist(g, names=TRUE)
  g_edges <- data.frame(
    from = as.character(edge_list[, 1]),
    to = as.character(edge_list[, 2]),
    stringsAsFactors = FALSE
  )
  
  # mapowanie wag na krawędzie
  g_edges <- merge(g_edges, edge_counts[, c("from", "to", "weight")],
                   by=c("from", "to"), all.x=TRUE, sort=FALSE)
  
  if (any(is.na(g_edges$weight))) {
    g_edges$weight[is.na(g_edges$weight)] <- 0.001
  }
  
  E(g)$weight <- g_edges$weight
  
  # stworzenie listy sąsiadów i wag dla każdego węzła dla szybkiego dostępu
  n <- vcount(g)
  neighbor_list <- vector("list", n)
  weight_list <- vector("list", n)
  
  # pobranie wszystkich krawędzi i wag
  all_edges <- as_edgelist(g, names=FALSE)
  all_weights <- E(g)$weight
  
  # wypełnienie list
  for (i in 1:n) {
    outgoing <- which(all_edges[, 1] == i)
    
    if (length(outgoing) > 0) {
      neighbor_list[[i]] <- all_edges[outgoing, 2]
      weight_list[[i]] <- all_weights[outgoing]
    } else {
      neighbor_list[[i]] <- integer(0)
      weight_list[[i]] <- numeric(0)
    }
  }
  
  # zapisywanie pre-cache do grafu jako atrybut
  g$neighbor_list <- neighbor_list
  g$weight_list <- weight_list
  
  return(g)
}

# funckja do wyboru węzłów początkowych według różnych strategii

# Jako piątą miarę wybrano miarę PageRank, ponieważ podobnie jak betweenness i closeness
# mierzy "ważność" węzłów w sieci, ale z innej perspektywy. PageRank uwzględnia 
# tez jakość tych połączeń (czyli czy są one z ważnych węzłów). W kontekście sieci email,
# węzły zwysokim PageRank mogą reprezentować użytkowników, którzy są kluczowi 
# w rozprzestrzenaniu informacji, ponieważ są one często kontaktowane przez innych ważnych
# użytkowników.
select_initial_nodes <- function(g, method, percentage=0.05) {
  # liczba węzłów do wybrania
  n_nodes <- ceiling(vcount(g) * percentage)
  
  if (method == "outdegree") {
    # 1. węzły o największym outdegree
    od <- degree(g, mode="out")
    selected <- order(od, decreasing=TRUE)[1:n_nodes]
  } else if (method == "betweenness") {
    # 2. węzły najbardziej centralne
    btw <- betweenness(g, directed=TRUE)
    selected <- order(btw, decreasing=TRUE)[1:n_nodes]
  } else if (method == "closeness") {
    # 3. węzły o największej wartości closeness
    cls <- closeness(g, mode="out")
    selected <- order(cls, decreasing=TRUE)[1:n_nodes]
  } else if (method == "random") {
    # 4. węzły wybrane losowo
    selected <- sample(1:vcount(g), n_nodes)
  } else if (method == "pagerank") {
    # 5. węzły wybrane według PageRank
    pr <- page_rank(g, directed=TRUE)$vector
    selected <- order(pr, decreasing=TRUE)[1:n_nodes]
  }
  
  return(selected)
}

# symulacja
simulate_cascade <- function(g, initial_nodes, weight_multiplier=1.0) {
  n <- vcount(g)
  
  # inicjalizacja - wszystkie węzły nieaktywne, za wyjątkiem
  # węzłow zainicjowanych
  activated <- rep(FALSE, n)
  activated[initial_nodes] <- TRUE
  
  # aktywność w czasie
  activation_history <- c(length(initial_nodes))

  newly_activated <- initial_nodes
  
  # hash set do przechowywania prób aktywacji jako (node, neighbor)
  tried <- new.env(hash = TRUE, size = n * 10)
  
  # pobranie pre-cache struktury
  neighbor_list <- g$neighbor_list
  weight_list <- g$weight_list
  
  iteration <- 0
  max_iteration <- 100
  
  while (length(newly_activated) > 0 && iteration < max_iteration) {
    iteration <- iteration + 1
    next_wave <- integer(0)
    
    # każdy nowo aktywowany węzeł próbuje aktywować swoich sąsiadów
    for (node in newly_activated) {
      # pobranie sąsiadów i wag z pre-cache dla danego węzła
      neighbors <- neighbor_list[[node]]
      weights <- weight_list[[node]]
      
      if (length(neighbors) == 0) next
      
      # próba aktywacji każdego sąsiada
      for (idx in seq_along(neighbors)) {
        neighbor <- neighbors[idx]
        
        if (activated[neighbor]) next
        
        # sprawdzenie czy węzeł i sąsiad były już próbowane
        key <- paste0(node, "_", neighbor)
        if (exists(key, envir = tried, inherits = FALSE)) next
        
        assign(key, TRUE, envir = tried)
        
        prob <- weights[idx] * weight_multiplier
        prob <- min(prob, 1.0)
        
        # próba aktywacji
        if (runif(1) < prob) {
          activated[neighbor] <- TRUE
          next_wave <- c(next_wave, neighbor)
        }
      }
    }
    
    # aktualizacja historii aktywacji
    if (length(next_wave) > 0) {
      activation_history <- c(activation_history, length(next_wave))
      newly_activated <- next_wave[!duplicated(next_wave)]
    } else {
      newly_activated <- integer(0)
    }
  }
  
  return(activation_history)
}

# eksperyment (100 powtorzeń dla każdej strategii)
run_experiment <- function(g, n_simulations = 100, weight_multiplier = 1.0, 
                           max_iterations = 10) {
  methods <- c("outdegree", "betweenness", "closeness", "random", "pagerank")
  method_names <- c(
    "Największy OutDegree",
    "Największy Betweenness",
    "Największy Closeness", 
    "Losowe węzły",
    "Największy PageRank"
  )
  
  results <- list()
  
  for (i in 1:length(methods)) {
    method <- methods[i]
    cat("Symulacja dla metody:", method_names[i], "\n")
    
    # macierz do przechowywania wyników wszystkich symulacji
    all_simulations <- matrix(0, nrow = n_simulations, ncol = max_iterations)
    
    for (sim in 1:n_simulations) {
      initial <- select_initial_nodes(g, method)
      history <- simulate_cascade(g, initial, weight_multiplier)
      
      len <- min(length(history), max_iterations)
      all_simulations[sim, 1:len] <- history[1:len]
    }
    
    avg_history <- colMeans(all_simulations)
    
    results[[method]] <- list(
      name = method_names[i],
      history = avg_history
    )
  }
  
  return(results)
}

# interfejs shiny
ui <- fluidPage(
  titlePanel("Symulacja rozprzestrzeniania się informacji w sieci email"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Parametry symulacji"),
      
      sliderInput("weight_mult",
                  "Mnożnik prawdopodobieństwa aktywacji (% wij):",
                  min = 10,
                  max = 200,
                  value = 100,
                  step = 10,
                  post = "%"),
      
      sliderInput("max_iter",
                  "Maksymalna liczba iteracji:",
                  min = 1,
                  max = 50,
                  value = 10,
                  step = 1),
      
      actionButton("run_sim", "Uruchom symulację", 
                   class = "btn-primary btn-lg"),
      
      hr(),
      
      h5("Informacje o sieci:"),
      textOutput("network_info"),
      
      hr(),
      
      h5("Metody wyboru węzłów początkowych:"),
      tags$ul(
        tags$li(strong("OutDegree:"), "węzły wysyłające najwięcej maili"),
        tags$li(strong("Betweenness:"), "węzły będące mostami w sieci"),
        tags$li(strong("Closeness:"), "węzły najbliższe wszystkim innym"),
        tags$li(strong("Losowe:"), "przypadkowe węzły"),
        tags$li(strong("PageRank:"), "węzły ważne według algorytmu Google")
      )
    ),
    
    mainPanel(
      plotOutput("diffusion_plot", height = "600px"),
      hr(),
      h4("Podsumowanie wyników"),
      tableOutput("summary_table")
    )
  )
)

server <- function(input, output, session) {
  
  # przygotowanie grafu (jednokrotnie przy starcie)
  g <- prepare_network()
  
  output$network_info <- renderText({
    paste0("Węzły: ", vcount(g), " | Krawędzie: ", ecount(g),
           " | Początkowe węzły: ", ceiling(vcount(g) * 0.05), " (5%)")
  })
  
  # uruchomienie symulacji
  sim_results <- eventReactive(input$run_sim, {
    withProgress(message = 'Uruchamianie symulacji...', value = 0, {
      weight_mult <- input$weight_mult / 100
      max_iter <- input$max_iter
      
      incProgress(0.2, detail = "Przygotowanie danych")
      
      start_time <- Sys.time()
      results <- run_experiment(g, n_simulations = 100, 
                                weight_multiplier = weight_mult,
                                max_iterations = max_iter)
      elapsed <- difftime(Sys.time(), start_time, units = "secs")
      
      cat("Czas wykonania:", round(elapsed, 2), "sekund\n")
      
      incProgress(0.8, detail = "Finalizacja")
      results
    })
  })
  
  # wykres
  output$diffusion_plot <- renderPlot({
    results <- sim_results()
    
    # przygotowanie danych do wykresu
    plot_data <- data.frame()
    for (method in names(results)) {
      history <- results[[method]]$history
      temp_df <- data.frame(
        Iteration = 1:length(history),
        Activated = history,
        Method = results[[method]]$name
      )
      plot_data <- rbind(plot_data, temp_df)
    }
    
    # tworzenie wykresu
    ggplot(plot_data, aes(x = Iteration, y = Activated, color = Method, group = Method)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(
        title = "Rozprzestrzenianie się informacji w sieci email",
        subtitle = paste0("Model Independent Cascades | Mnożnik prawdopodobieństwa: ", 
                          input$weight_mult, "% | Średnia ze 100 symulacji"),
        x = "Numer iteracji",
        y = "Średnia liczba aktywowanych węzłów",
        color = "Metoda wyboru węzłów"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12, color = "gray40")
      ) +
      scale_color_brewer(palette = "Set1")
  })
  
  # tabela podsumowująca
  output$summary_table <- renderTable({
    results <- sim_results()
    
    summary_df <- data.frame()
    for (method in names(results)) {
      history <- results[[method]]$history
      total <- sum(history)
      max_in_iter <- max(history)
      
      summary_df <- rbind(summary_df, data.frame(
        Metoda = results[[method]]$name,
        "Suma aktywacji" = round(total, 2),
        "Max w iteracji" = round(max_in_iter, 2),
        "Średnia na iterację" = round(mean(history[history > 0]), 2)
      ))
    }
    
    colnames(summary_df) <- c("Metoda wyboru węzłów", "Suma aktywacji", 
                              "Maksimum w iteracji", "Średnia na iterację")
    summary_df
  }, striped = TRUE, hover = TRUE)
}

shinyApp(ui = ui, server = server)
