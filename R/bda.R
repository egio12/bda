library(rdrobust)
library(ggplot2)
library(cluster)
library(igraph)


bda_anova <- function(...) {

  # Cattura i vettori passati come argomenti
  vettori <- list(...)

  # Controlla se tutti gli argomenti sono vettori numerici
  if (!all(sapply(vettori, is.numeric))) {
    stop("Tutti gli argomenti devono essere vettori numerici.")
  }

  # Verifica delle ipotesi di validità dell'ANOVA
  cat("\nVerifica delle ipotesi di validità dell'ANOVA:\n")

  # 1. Normalità dei residui
  residui <- unlist(vettori) - rep(sapply(vettori, mean), sapply(vettori, length))
  test_normalita <- shapiro.test(residui)
  if (test_normalita$p.value < 0.05) {
    cat("- Normalità dei residui: NON SODDISFATTA (p-value =", test_normalita$p.value, ")\n")
  } else {
    cat("- Normalità dei residui: soddisfatta\n")
  }

  # 2. Omogeneità delle varianze (test di Bartlett)
  test_bartlett <- bartlett.test(vettori)
  if (test_bartlett$p.value < 0.05) {
    cat("- Omogeneità delle varianze: NON SODDISFATTA (p-value =", test_bartlett$p.value, ")\n")
  } else {
    cat("- Omogeneità delle varianze: soddisfatta\n")
  }

  # Esecuzione dell'ANOVA (anche se le ipotesi non sono soddisfatte)
  dati_anova <- data.frame(valore = unlist(vettori), gruppo = factor(rep(1:length(vettori), sapply(vettori, length))))
  modello_anova <- aov(valore ~ gruppo, data = dati_anova)
  risultati_anova <- summary(modello_anova)

  # Estrazione dei risultati
  f_stat <- risultati_anova[[1]][["F value"]][1]
  p_value <- risultati_anova[[1]][["Pr(>F)"]][1]

  # Decisione statistica
  if (p_value < 0.05) {
    decisione <- "Rifiuto H0, Accetto H1"
  } else {
    decisione <- "Non rifiuto H0"
  }

  # Output formattato
  cat("\nRisultati dell'ANOVA:\n")
  cat("--------------------\n")
  cat("Statistica F:", f_stat, "\n")
  cat("P-value:", p_value, "\n")
  cat("Decisione:", decisione, "\n")
}

bda_anova_f <- function(formula, data, alpha = 0.05) {

  # Verifica se l'input è un dataframe
  if (!is.data.frame(data)) {
    stop("L'input 'data' deve essere un dataframe.")
  }

  # Verifica delle ipotesi di validità dell'ANOVA
  cat("\nVerifica delle ipotesi di validità dell'ANOVA:\n")

  # 1. Normalità dei residui
  modello_anova <- aov(formula, data = data)
  residui <- residuals(modello_anova)
  test_normalita <- shapiro.test(residui)
  if (test_normalita$p.value < alpha) {
    cat("- Normalità dei residui: NON SODDISFATTA (p-value =", test_normalita$p.value, ")\n")
  } else {
    cat("- Normalità dei residui: soddisfatta\n")
  }

  # 2. Omogeneità delle varianze (test di Bartlett)
  test_bartlett <- bartlett.test(formula, data = data)
  if (test_bartlett$p.value < alpha) {
    cat("- Omogeneità delle varianze: NON SODDISFATTA (p-value =", test_bartlett$p.value, ")\n")
  } else {
    cat("- Omogeneità delle varianze: soddisfatta\n")
  }

  # Esecuzione dell'ANOVA
  risultati_anova <- summary(modello_anova)

  # Estrazione dei risultati
  f_stat <- risultati_anova[[1]][["F value"]][1]
  p_value <- risultati_anova[[1]][["Pr(>F)"]][1]

  # Decisione statistica
  if (p_value < alpha) {
    decisione <- "Rifiuto H0, Accetto H1"
  } else {
    decisione <- "Non rifiuto H0"
  }

  # Output formattato
  cat("\nRisultati dell'ANOVA:\n")
  cat("--------------------\n")
  cat("Statistica F:", f_stat, "\n")
  cat("P-value:", p_value, "\n")
  cat("Decisione:", decisione, "\n")
}


bda_dataframes_per_categoria <- function(dati, colonna_categoria, ...) {

  # Verifica se il dataset è un dataframe
  if (!is.data.frame(dati)) {
    stop("L'input 'dati' deve essere un dataframe.")
  }

  # Verifica se la colonna categoria esiste
  if (!(colonna_categoria %in% names(dati))) {
    stop("La colonna specificata non esiste nel dataframe.")
  }

  # Estrai le categorie uniche
  categorie_uniche <- unique(dati[[colonna_categoria]])

  # Se non sono state specificate colonne di valori, prendi tutte le colonne
  colonne_valori <- if (length(list(...)) == 0) {
    names(dati)
  } else {
    unlist(list(...))
  }

  # Verifica se le colonne dei valori esistono nel dataframe
  if (!all(colonne_valori %in% names(dati))) {
    stop("Alcune colonne specificate non esistono nel dataframe.")
  }

  # Lista per memorizzare i dataframes risultanti
  dataframes_risultanti <- list()

  # Crea un dataframe per ogni categoria
  for (categoria in categorie_uniche) {
    # Filtra il dataframe originale per la categoria corrente
    dati_filtrati <- dati[dati[[colonna_categoria]] == categoria, colonne_valori]

    # Assegna un nome significativo al dataframe
    nome_dataframe <- paste("dati_", categoria, sep = "")

    # Aggiungi il dataframe alla lista
    dataframes_risultanti[[nome_dataframe]] <- dati_filtrati
  }

  return(dataframes_risultanti)
}

bda_valuta_ipotesi <- function(p_value, alpha = 0.05) {
  cat("\n")
  cat("**************************************************\n")
  cat("*                Valutazione Ipotesi              *\n")
  cat("**************************************************\n\n")

  cat("Livello di significatività (alpha):", alpha, "\n")
  cat("P-value:", p_value, "\n\n")

  if (p_value < alpha) {
    cat("Conclusione:\n")
    cat("    Rifiutiamo l'ipotesi nulla (H0)\n")
    cat("    Accettiamo l'ipotesi alternativa (H1)\n\n")
  } else {
    cat("Conclusione:\n")
    cat("    Non possiamo rifiutare l'ipotesi nulla (H0)\n")
    cat("    Non abbiamo evidenza sufficiente per accettare H1\n\n")
  }
  cat("**************************************************\n")
}


bda_NE_ATT_ATU_ATE <- function() {

  # Input dati utente (con formattazione)
  cat("\n\n")
  cat("---------------------------------------------\n")
  cat("    Inserisci i dati per il calcolo degli effetti causali\n")
  cat("---------------------------------------------\n\n")

  # Gruppo non trattato
  cat("Gruppo NON trattato:\n")
  y0_non_trattato <- as.numeric(readline(prompt = "  Outcome SENZA trattamento (Y0): "))
  y1_non_trattato <- as.numeric(readline(prompt = "  Outcome CON trattamento (Y1): "))
  perc_non_trattato <- as.numeric(readline(prompt = "  Percentuale di individui NON trattati (%): ")) / 100

  # Gruppo trattato
  cat("\nGruppo trattato:\n")
  y0_trattato <- as.numeric(readline(prompt = "  Outcome SENZA trattamento (Y0): "))
  y1_trattato <- as.numeric(readline(prompt = "  Outcome CON trattamento (Y1): "))
  perc_trattato <- 1 - perc_non_trattato

  # Calcolo degli indicatori
  NE <- y1_trattato - y0_non_trattato
  ATT <- y1_trattato - y0_trattato
  ATU <- y1_non_trattato - y0_non_trattato
  ATE <- perc_trattato * ATT + perc_non_trattato * ATU

  # Output formattato (con colori e spaziatura)
  cat("\n\n")
  cat("\033[1;34m*********************************************\033[0m\n")
  cat("\033[1;34m*         Effetti Causali del Trattamento        *\033[0m\n")
  cat("\033[1;34m*********************************************\033[0m\n\n")

  cat("  NE (Average Treatment Effect):", NE, "\n")
  cat("     Formula: E(Y1 | D = 1) - E(Y0 | D = 0)\n\n")

  cat("  ATT (Average Treatment Effect on the Treated):", ATT, "\n")
  cat("     Formula: E(Y1 | D = 1) - E(Y0 | D = 1)\n\n")

  cat("  ATU (Average Treatment Effect on the Untreated):", ATU, "\n")
  cat("     Formula: E(Y1 | D = 0) - E(Y0 | D = 0)\n\n")

  cat("  ATE (Average Treatment Effect):", ATE, "\n")
  cat("     Formula: p * ATT + (1 - p) * ATU\n\n")

  cat("\033[1;34m*********************************************\033[0m\n")
}

bda_sensitivity_specificity_accuracy_precision <- function() {

  # Input dati utente (con formattazione)
  cat("\n\n")
  cat("---------------------------------------------\n")
  cat("  Inserisci i dati per il calcolo degli indicatori\n")
  cat("---------------------------------------------\n\n")

  # True Positive, True Negative, False Positive, False Negative
  TP <- as.numeric(readline(prompt = " True Positive (TP): "))
  TN <- as.numeric(readline(prompt = " True Negative (TN): "))
  FP <- as.numeric(readline(prompt = " False Positive (FP): "))
  FN <- as.numeric(readline(prompt = " False Negative (FN): "))

  # Calcolo degli indicatori
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  precision <- TP / (TP + FP)
  missclassification_error <- 1-accuracy  # Missclassification Error

  # Output formattato (con colori e spaziatura)
  cat("\n\n")
  cat("\033[1;34m*********************************************\033[0m\n")
  cat("\033[1;34m*    Indicatori di Performance del Modello    *\033[0m\n")
  cat("\033[1;34m*********************************************\033[0m\n\n")

  cat(" Sensitivity (True Positive Rate):", sensitivity, "\n")
  cat("  Formula: TP / (TP + FN)\n\n")

  cat(" Specificity (True Negative Rate):", specificity, "\n")
  cat("  Formula: TN / (TN + FP)\n\n")

  cat(" Accuracy:", accuracy, "\n")
  cat("  Formula: (TP + TN) / (TP + TN + FP + FN)\n\n")

  cat(" Precision (Positive Predictive Value):", precision, "\n")
  cat("  Formula: TP / (TP + FP)\n\n")

  cat(" Missclassification Error:", missclassification_error, "\n")
  cat("  Formula: 1-accuracy\n\n")

  cat("\033[1;34m*********************************************\033[0m\n")
}

bda_shapiro.test <- function(data) {
  # Esecuzione del test di Shapiro-Wilk
  test_result <- shapiro.test(data)

  # Formattazione del risultato
  cat("\n")
  cat("Risultati del Test di Shapiro-Wilk\n")
  cat("----------------------------------\n")
  cat("Statistica W:", test_result$statistic, "\n")
  cat("P-value:", test_result$p.value, "\n\n")

  # Interpretazione del risultato
  if (test_result$p.value > 0.05) {
    cat("Conclusione: I dati NON presentano evidenza significativa per rifiutare l'ipotesi di normalità. (i dati SONO normali)\n")
  } else {
    cat("Conclusione: I dati presentano evidenza significativa per rifiutare l'ipotesi di normalità (i dati NON sono normali).\n")
  }
  cat("\n")
}

bda_t.test <- function(data1, data2, var.equal = FALSE, tipo_test = "bilatero", alpha = 0.05) {
  # Calcolo manuale di n0, n1, s0, s1
  n0 <- length(data1)
  n1 <- length(data2)
  s0 <- var(data1)
  s1 <- var(data2)

  # Definisci l'argomento 'alternative' in base a 'tipo_test'
  alternative <- switch(tipo_test,
                        "bilatero" = "two.sided",
                        "sinistro" = "less",
                        "destro" = "greater",
                        stop("Tipo di test non valido. Usa 'bilatero', 'sinistro' o 'destro'."))

  # Esecuzione del t-test
  test_result <- t.test(data1, data2, var.equal = var.equal, alternative = alternative)

  # Formattazione del risultato
  cat("\n")
  cat("Risultati del T-Test\n")
  cat("--------------------\n")
  cat("Varianze uguali:", ifelse(var.equal, "Sì", "No"), "\n")

  # Descrizione del tipo di test
  tipo_test_descrizione <- switch(tipo_test,
                                  "bilatero" = "m1 ≠ m0",
                                  "sinistro" = "m1 < m0",
                                  "destro" = "m1 > m0")

  cat("H1:", tipo_test_descrizione, "\n")
  cat("Statistica t:", test_result$statistic, "\n")
  cat("Gradi di libertà:", test_result$parameter, "\n")
  cat("P-value:", test_result$p.value, "\n\n")

  # Interpretazione del risultato basata su alpha
  if (test_result$p.value > alpha) {
    cat("Conclusione: NON c'è evidenza significativa di differenza tra le medie dei due gruppi.\n")
  } else {
    cat("Conclusione: C'è evidenza significativa di differenza tra le medie dei due gruppi.\n")
  }
  cat("\n")

  # Calcolo e visualizzazione della varianza pooled (se var.equal = TRUE)
  if (var.equal) {
    s_p <- ((n0 - 1) * s0 + (n1 - 1) * s1) / (n0 + n1 - 2) # Varianza pooled
    cat("Varianza pooled:", s_p, "\n")
  }
}



bda_t.test_valori_noti <- function(x0, x1, n0, n1, s0, s1, alpha = 0.05, tipo_test = "bilatero") {
  # Calcolo della varianza pooled
  s_p <- ((n0 - 1) * s0 + (n1 - 1) * s1) / (n0 + n1 - 2)

  # Calcolo della statistica t
  t_stat <- (x1 - x0) / (sqrt(s_p) * sqrt(1/n0 + 1/n1))

  # Calcolo del p-value in base al tipo di test
  if (tipo_test == "bilatero") {
    p_value <- 2 * (1 - pt(abs(t_stat), df = n0 + n1 - 2))
  } else if (tipo_test == "sinistro") {
    p_value <- pt(t_stat, df = n0 + n1 - 2)
  } else if (tipo_test == "destro") {
    p_value <- pt(t_stat, df = n0 + n1 - 2, lower.tail = FALSE)
  } else {
    stop("Tipo di test non valido. Scegliere tra 'bilatero', 'sinistro' o 'destro'.")
  }

  # Calcolo del quantile di riferimento in base al tipo di test
  if (tipo_test == "bilatero") {
    quantile_rif <- qt(1 - alpha/2, df = n0 + n1 - 2)
  } else if (tipo_test == "sinistro") {
    quantile_rif <- qt(alpha, df = n0 + n1 - 2)
  } else if (tipo_test == "destro") {
    quantile_rif <- qt(1 - alpha, df = n0 + n1 - 2)
  } else {
    stop("Tipo di test non valido. Scegliere tra 'bilatero', 'sinistro' o 'destro'.")
  }

  # Decisione statistica
  if (p_value < alpha) {
    decisione <- "Rifiuto H0, Accetto H1"
  } else {
    decisione <- "Non rifiuto H0"
  }

  # Descrizione del tipo di test
  tipo_test_descrizione <- switch(tipo_test,
                                  "bilatero" = "m1 ≠ m0",
                                  "sinistro" = "m1 < m0",
                                  "destro" = "m1 > m0")

  # Output formattato
  cat("\n")
  cat("Risultati del T-Test (Gruppo 0 vs. Gruppo 1)\n")
  cat("--------------------------------------------\n")
  cat("H1:", tipo_test_descrizione, "\n")
  cat("Statistica t:", t_stat, "\n")
  cat("Gradi di libertà:", n0 + n1 - 2, "\n")
  cat("P-value:", p_value, "\n")
  cat("Quantile di riferimento (alpha =", alpha, "):", quantile_rif, "\n")
  cat("Varianza pooled:", s_p, "\n")
  cat("Decisione:", decisione, "\n\n")
}


bda_diametro_matrice_adiacenze <- function(adj_matrix) {
  floyd_warshall <- function(adj_matrix) {
    n <- nrow(adj_matrix)
    dist_matrix <- adj_matrix

    for (k in 1:n) {
      for (i in 1:n) {
        for (j in 1:n) {
          dist_matrix[i, j] <- min(dist_matrix[i, j], dist_matrix[i, k] + dist_matrix[k, j])
        }
      }
    }

    return(dist_matrix)
  }

  # Esempio di matrice di adiacenze
  cat("Matrice di adiacenze:\n")
  print(adj_matrix)
  cat("\n")

  # Sostituisci i 0 con Inf tranne sulla diagonale principale
  adj_matrix[adj_matrix == 0 & row(adj_matrix) != col(adj_matrix)] <- Inf

  dist_matrix <- floyd_warshall(adj_matrix)
  diag(dist_matrix) <- 0  # Assicurarsi che la diagonale principale sia 0

  diameter <- max(dist_matrix[dist_matrix != Inf])

  cat("Il diametro della rete è:", diameter, "\n\n")
}

# Funzione per visualizzare un grafico RDD fuzzy
#
# Args:
#   data: DataFrame contenente i dati.
#   var_risultato: Nome della colonna da usare come asse y (risultato).
#   var_assegnazione: Nome della colonna da usare come asse x (assegnazione).
#   var_trattamento: Nome della colonna da usare come fattore per il colore dei punti (trattamento).
#   soglia: Valore dell'intercetta verticale (la linea tratteggiata rossa).
#
# Esempio:
#   plot_fuzzy_rdd(db, "produttivita", "liquidita", "corso_soft_skills", 1.1)
bda_sharp_or_fuzzy <- function(data, var_risultato, var_assegnazione, var_trattamento, soglia) {
  # Estrazione delle colonne richieste
  x <- data[[var_assegnazione]]
  y <- data[[var_risultato]]
  z <- factor(data[[var_trattamento]])

  # Creazione del plot
  ggplot(data = data, aes_string(x = var_assegnazione, y = var_trattamento)) +
    geom_point(aes(colour = z)) +
    geom_vline(xintercept = soglia, linetype = "dashed", color = "red") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title = "Sharp or Fuzzy RDD?")
}

bda_pca_analysis <- function(dataset_num) {
  # Controllo che il dataset sia numerico
  if (!all(sapply(dataset_num, is.numeric))) {
    stop("Il dataset deve contenere solo dati numerici.")
  }

  # Calcolo la matrice di covarianza
  s <- cov(dataset_num)
  print("Matrice di covarianza (prime righe):")
  print(head(s))

  # Visualizzo il boxplot delle varianze
  varianze <- diag(s)
  boxplot(varianze, main = "Boxplot delle varianze")

  # Valuto se conviene standardizzare
  if (max(varianze) / min(varianze) > 10) {
    print("Conviene standardizzare i dati poiché le varianze differiscono di molto.")
    risposta <- readline(prompt = "Vuoi procedere con la standardizzazione? (s/n): ")

    if (tolower(risposta) == 's') {
      # Standardizzo il dataset
      dataset_st <- scale(dataset_num)
      s <- cov(dataset_st)
      print("Dati standardizzati. Matrice di covarianza (prime righe) dopo la standardizzazione:")
      print(head(s))
    } else {
      dataset_st <- dataset_num
      print("Non si è proceduto con la standardizzazione.")
    }
  } else {
    dataset_st <- dataset_num
    print("Non è necessario standardizzare i dati poiché le varianze sono simili.")
  }

  # Eseguo la PCA
  pca <- prcomp(dataset_st, scale. = FALSE)
  print("Risultati della PCA:")
  print(summary(pca))

  return(pca)
}

bda_cluster_analysis <- function(dataset, method, distance) {
  # Controllo validità degli input
  valid_methods <- c("complete", "average", "single")
  valid_distances <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

  if (!(method %in% valid_methods)) {
    stop("Metodo non valido. Scegli tra: 'complete', 'average', 'single'.")
  }
  if (!(distance %in% valid_distances)) {
    stop("Distanza non valida. Scegli tra: 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary', 'minkowski'.")
  }

  # Controllo che il dataset sia una matrice o un data frame con almeno due colonne
  if (!is.data.frame(dataset) && !is.matrix(dataset)) {
    stop("Il dataset deve essere un data frame o una matrice.")
  }
  if (ncol(dataset) < 2) {
    stop("Il dataset deve avere almeno due colonne.")
  }

  # Calcolo la distanza
  dist_matrix <- dist(dataset, method = distance)

  # Eseguo il clustering gerarchico
  hclust_result <- hclust(dist_matrix, method = method)

  # Plot del dendrogramma
  plot(hclust_result, main = paste("Dendrogramma (Metodo:", method, ", Distanza:", distance, ")"),
       xlab = "", sub = "", cex.main = 1.2)

  # Chiedo all'utente dove tagliare
  cut_number <- as.numeric(readline(prompt = "Inserisci il numero di cluster in cui tagliare il dendogramma: "))

  # Taglio il dendrogramma
  clusters <- cutree(hclust_result, cut_number)

  # Calcolo il numero dei cluster e la numerosità dei cluster
  num_clusters <- cut_number
  cluster_sizes <- table(clusters)

  # Calcolo la media intracluster usando tapply
  cluster_means <- sapply(dataset, function(x) tapply(x, clusters, mean))

  # Formattazione output
  cat("\nRisultati del clustering:\n")
  cat("-------------------------\n")
  cat("Numero di cluster:", num_clusters, "\n\n")
  cat("Numerosità dei cluster:\n")
  print(cluster_sizes)
  cat("\nMedia intracluster (ColMeans):\n")
  print(cluster_means)

  return(list(num_clusters = num_clusters, cluster_sizes = cluster_sizes, cluster_means = cluster_means))
}


bda_summary_rete <- function(rete) {
  # Numero di nodi
  num_nodi <- vcount(g)

  # Numero di link
  num_link <- ecount(g)

  # Densità della rete
  densita_rete <- graph.density(g)

  # Grado minimo della rete e nodi corrispondenti
  grado_minimo <- min(degree(g))
  nodi_grado_minimo <- V(g)[degree(g) == grado_minimo]

  # Grado massimo della rete e nodi corrispondenti
  grado_massimo <- max(degree(g))
  nodi_grado_massimo <- V(g)[degree(g) == grado_massimo]

  # Diametro della rete
  diametro <- diameter(g)

  # Numero di componenti connesse
  num_componenti_connesse <- length(components(g)$csize)

  # Lunghezza media dei cammini
  lunghezza_media_cammini <- mean_distance(g)

  # Creazione del riepilogo
  riepilogo <- list(
    "Numero di nodi" = num_nodi,
    "Numero di link" = num_link,
    "Densità della rete" = densita_rete,
    "Grado minimo della rete" = grado_minimo,
    "Nodi con grado minimo" = nodi_grado_minimo,
    "Grado massimo della rete" = grado_massimo,
    "Nodi con grado massimo" = nodi_grado_massimo,
    "Diametro della rete" = diametro,
    "Numero di componenti connesse" = num_componenti_connesse,
    "Lunghezza media dei cammini" = lunghezza_media_cammini
  )

  return(riepilogo)
}


#DA SISTEMARE VEDENDO NOTION
bda_centralita_rete <- function(g, directed = TRUE, normalized = TRUE, mode='all') {
  # Betweenness centrality
  betweenness_g <- as.data.frame(betweenness(g, directed = directed, normalized = normalized))

  # Closeness centrality
  closeness_g <- as.data.frame(closeness(g, mode = mode, normalized = normalized))

  # Eigenvector centrality
  eigenvector_g <- as.data.frame(eigen_centrality(g, directed = directed, scale = TRUE)$vector)

  # Page Rank
  page_rank_g <- as.data.frame(page_rank(g, directed = directed)$vector)

  # Authority score
  authority_score_g <- as.data.frame(authority.score(g, scale = normalized)$vector)

  # Hub score
  hub_score_g <- as.data.frame(hub.score(g, scale = normalized)$vector)

  # K-core centrality
  kcore_centralities_g <- as.data.frame(kcoreness(g)$ncore)

  # Average nearest neighbor degree
  avg_nearest_neighbour_degree_g <- as.data.frame(knn(g)$knn)

  # Clustering coefficient
  clustering_coefficient_g <- as.data.frame(transitivity(g, isolates = "zero", type = "local"))
  row.names(clustering_coefficient_g) <- V(g)$name

  # Unione di tutti gli indicatori in una lista
  indicatori <- list(
    "Betweenness centrality" = betweenness_g,
    "Closeness centrality" = closeness_g,
    "Eigenvector centrality" = eigenvector_g,
    "Page Rank" = page_rank_g,
    "Authority score" = authority_score_g,
    "Hub score" = hub_score_g,
    "K-core centrality" = kcore_centralities_g,
    "Average nearest neighbor degree" = avg_nearest_neighbour_degree_g,
    "Clustering coefficient" = clustering_coefficient_g
  )

  return(indicatori)
}

bda_analisi_community <- function(graph, is_directed = TRUE, weights = NA) {

  # Se la rete è diretta e is_directed è TRUE, la trasformiamo in non direzionata
  if (is_directed && is_directed(graph)) {
    g_comm <- as.undirected(graph)
  } else {
    g_comm <- graph
  }

  # Greedy Newman
  c_greedy <- cluster_fast_greedy(g_comm)
  mod_greedy <- modularity(c_greedy)
  sizes_greedy <- sizes(c_greedy)
  num_communities_greedy <- length(sizes_greedy)
  mean_size_greedy <- mean(sizes_greedy)
  sd_size_greedy <- sd(sizes_greedy)

  # Louvain
  c_louvain <- cluster_louvain(g_comm)
  mod_louvain <- modularity(c_louvain)
  sizes_louvain <- sizes(c_louvain)
  num_communities_louvain <- length(sizes_louvain)
  mean_size_louvain <- mean(sizes_louvain)
  sd_size_louvain <- sd(sizes_louvain)

  # Leiden
  c_leiden <- cluster_leiden(g_comm, objective_function='modularity')
  mod_leiden <- c_leiden$quality
  sizes_leiden <- sizes(c_leiden)
  num_communities_leiden <- length(sizes_leiden)
  mean_size_leiden <- mean(sizes_leiden)
  sd_size_leiden <- sd(sizes_leiden)

  # Girvan-Newman (Edge Betweenness)
  c_betw <- cluster_edge_betweenness(g_comm, weights = weights)
  mod_betw <- modularity(c_betw)
  sizes_betw <- sizes(c_betw)
  num_communities_betw <- length(sizes_betw)
  mean_size_betw <- mean(sizes_betw)
  sd_size_betw <- sd(sizes_betw)

  # Creiamo una tabella di output
  results <- data.frame(
    Algorithm = c("Greedy Newman", "Louvain", "Leiden", "Girvan-Newman"),
    Modularity = c(mod_greedy, mod_louvain, mod_leiden, mod_betw),
    Num_Communities = c(num_communities_greedy, num_communities_louvain, num_communities_leiden, num_communities_betw),
    Mean_Community_Size = c(mean_size_greedy, mean_size_louvain, mean_size_leiden, mean_size_betw),
    SD_Community_Size = c(sd_size_greedy, sd_size_louvain, sd_size_leiden, sd_size_betw),
    stringsAsFactors = FALSE
  )

  return(results)
}

bda_tune_random_forest <- function(formula, data, ntree = 1000, abline_val = NULL, seed=1000) {
  # Numero massimo di variabili (caratteristiche) nel dataset
  num_vars <- ncol(data) - 1 # Escludendo la variabile di risposta

  # Array per memorizzare gli errori OOB per ogni valore di mtry
  oob.err <- rep(0, num_vars)

  # Loop attraverso i possibili valori di mtry
  for(mtry in 1:num_vars) {
    set.seed(seed)
    fit <- randomForest(formula, data = data, mtry = mtry, ntree = ntree)
    oob.err[mtry] <- fit$mse[ntree]
  }

  # Creazione della tabella di output
  results <- data.frame(
    mtry = 1:num_vars,
    oob_err = oob.err
  )

  # Plotting the results using matplot
  matplot(1:num_vars, oob.err, pch = 19, type = "b", col = "blue",
          xlab = "Number of Variables (mtry)", ylab = "OOB Error",
          main = "OOB Error vs. mtry")

  if (!is.null(abline_val)) {
    abline(h = abline_val, col = "red", lty = 2)
    first_below_abline <- which(oob.err < abline_val)[1]
    if (!is.na(first_below_abline)) {
      legend_text <- paste("Primo mtry sotto l'abline:", first_below_abline)
      cat("\n", legend_text, "\n")
    } else {
      legend_text <- "Nessun mtry sotto l'abline"
      cat("\n", legend_text, "\n")
    }
  } else {
    legend_text <- NULL
  }

  return(list(Results = results, Abline_Info = legend_text))
}

