# BDA Package

BDA è un pacchetto R sperimentale che raccoglie funzioni di supporto per diverse analisi statistiche. Tutto il codice si trova nel file `R/bda.R` e viene esportato tramite il file `NAMESPACE`.

## Struttura del repository

- `R/bda.R` - contiene l'implementazione di tutte le funzioni del pacchetto.
- `man/hello.Rd` - semplice esempio di documentazione.
- `DESCRIPTION` e `NAMESPACE` - metadati del pacchetto R.

## Funzioni principali

Nel file `R/bda.R` sono implementate numerose funzioni per l'analisi dei dati, tra cui:

- **bda_anova** e **bda_anova_f**: procedure per eseguire l'ANOVA e valutarne le ipotesi.
- **bda_dataframes_per_categoria**: suddivide un data frame in base a una colonna di categoria.
- **bda_valuta_ipotesi**: helper per interpretare il p-value di un test.
- **bda_NE_ATT_ATU_ATE**: calcolo di alcuni effetti causali (NE, ATT, ATU, ATE).
- **bda_sensitivity_specificity_accuracy_precision**: calcola indicatori di classificazione.
- **bda_shapiro.test** e **bda_t.test**: versioni estese dei test di Shapiro-Wilk e t-test.
- **bda_diametro_matrice_adiacenze**, **bda_summary_rete**, **bda_centralita_rete**, **bda_analisi_community**: strumenti per l'analisi di reti con `igraph`.
- **bda_pca_analysis** e **bda_cluster_analysis**: funzioni per PCA e clustering gerarchico.
- **bda_tune_random_forest**: supporto per la scelta del parametro *mtry* in Random Forest.
- **bda_classificazione_da_modello_reg_logistica** e **bda_summary_missclass_table**: utilità per modelli di regressione logistica e valutazione delle prestazioni.
- **bda_cv.glmnet**: wrapper semplificato per la cross‑validation di `glmnet`.

## Installazione

Essendo un progetto sperimentale, il pacchetto non è pubblicato su CRAN. Per installarlo si può clonare questa repository e usare `devtools::install()` o `R CMD INSTALL`:

```R
# con devtools
# devtools::install("percorso/della/cartella/bda")
```

Il file `DESCRIPTION` elenca le dipendenze necessarie (tra le altre: `ggplot2`, `igraph`, `randomForest`, `glmnet`).

## Uso

Dopo l'installazione è possibile caricare il pacchetto e richiamare le funzioni, ad esempio:

```R
library(BDA)
result <- bda_anova(vettore1, vettore2, vettore3)
```

Ogni funzione stampa o restituisce in output i risultati dell'analisi corrispondente.

