
# Lezione di biodiversità con R 

# --- 1. Pacchetti necessari ---
install.packages(c("vegan", "SpadeR", "hillR", "tidyverse"))
library(vegan)
library(SpadeR)
library(hillR)
library(tidyverse)

# --- 2. Caricamento dati ---
dati <- read_csv("GFBI_filtered.csv")
dati_f <- as.data.frame(dati) |> 
  select(-PlotID, -LON, -LAT, -DSN, -YR, -PA, -N, -B, -n_specie_presenti, -n_zeri)

# --- 3. Indici diversità ---
S <- specnumber(dati_f)
H <- diversity(dati_f, index = "shannon")
D <- diversity(dati_f, index = "simpson")
#eveness
pielou <- H / log(S)

# Visualizzazione
df_alpha <- tibble(
  Plot = rownames(dati_f),
  Ricchezza = S,
  Shannon = H,
  Simpson = D,
  Pielou = pielou
)
# Prepara i dati
df_alpha_long <- df_alpha %>%
  pivot_longer(cols = -Plot, names_to = "Indice", values_to = "Valore")

# Aggiungi gruppo "Tipo" per separare
df_alpha_long <- df_alpha_long %>%
  mutate(Tipo = ifelse(Indice == "Ricchezza", "Ricchezza", "Altri"))

# Facet su Tipo
ggplot(df_alpha_long, aes(x = factor(Plot), y = Valore, fill = Indice)) +
  geom_col(data = filter(df_alpha_long, Tipo == "Ricchezza"), width = 0.5) +
  geom_point(data = filter(df_alpha_long, Tipo == "Altri"),
             aes(color = Indice), size = 3, position = position_dodge(width = 0.4)) +
  facet_wrap(~Tipo, scales = "free_y") +
  labs(title = "Indici alfa-diversità: Ricchezza vs altri",
       x = "Plot", y = "Valore", fill = "Indice", color = "Indice") +
  theme_minimal()

df_alpha_long %>%
  filter(Indice != "Ricchezza") %>%
  ggplot(aes(x = Indice, y = Valore, fill = Indice)) +
  geom_col(position = "dodge") +
  facet_wrap(~Plot) +
  labs(title = "Indici Shannon, Simpson, Pielou",
       y = "Valore", x = "Indice") +
  theme_minimal()


# --- 4. Stime di ricchezza (Chao1) ---
# Stime per i due plot
out1 <- ChaoSpecies(data = as.numeric(dati_f[1, ]), datatype = "abundance")
out2 <- ChaoSpecies(data = as.numeric(dati_f[2, ]), datatype = "abundance")



# Estrai le tabelle
tab1 <- out1$Species_table
tab2 <- out2$Species_table

# Stima osservata = prima riga ("Homogeneous Model")
obs1 <- tab1[1, "Estimate"]
obs2 <- tab2[1, "Estimate"]

# Trova la riga che contiene la parola "Chao1"
row_chao1_tab1 <- grep("Chao1", rownames(tab1), value = FALSE)[1]
row_chao1_tab2 <- grep("Chao1", rownames(tab2), value = FALSE)[1]

# Stima Chao1 (prima riga che contiene "Chao1")
chao1 <- tab1[row_chao1_tab1, "Estimate"]
chao2 <- tab2[row_chao1_tab2, "Estimate"]

# Crea il dataframe per il grafico
df_chao <- tibble(
  Plot = rep(c("Plot1", "Plot2"), each = 2),
  Tipo = rep(c("Osservata", "Chao1"), 2),
  Valore = c(obs1, chao1, obs2, chao2)
)

ggplot(df_chao, aes(x = Plot, y = Valore, fill = Tipo)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  geom_text(aes(label = round(Valore, 1)), 
            position = position_dodge(width = 0.6), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Osservata" = "darkolivegreen3", "Chao1" = "steelblue")) +
  labs(
    title = "Confronto tra ricchezza osservata e stimata (Chao1)",
    y = "Numero di specie",
    x = "Plot",
    fill = "Tipo di stima"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

#la stima Chao1 è uguale alla ricchezza osservata perché il campione 
#è probabilmente completo: le specie sono abbondanti e distribuite, 
#e non ci sono quasi specie rare che indicherebbero la presenza di specie “nascoste”.

# 1. Prendiamo un plot reale (il primo della tabella)
plot_reale <- as.numeric(dati_f[1, ])

# 2. Creiamo una copia modificata con specie rare
plot_raro <- plot_reale
plot_raro[1:3] <- 1  # 3 specie presenti 1 volta (singleton)
plot_raro[4:5] <- 2  # 2 specie presenti 2 volte (doubleton)

# 3. Calcoliamo le stime con ChaoSpecies()
stima_reale <- ChaoSpecies(plot_reale, datatype = "abundance")
stima_rara <- ChaoSpecies(plot_raro, datatype = "abundance")

# 4. Estraiamo i valori di ricchezza osservata e Chao1 stimata
# (ricchezza osservata = prima riga, Chao1 = prima riga con "Chao1" nel nome)
obs_reale <- stima_reale$Species_table[1, "Estimate"]
chao_reale <- stima_reale$Species_table[grep("Chao1", rownames(stima_reale$Species_table))[1], "Estimate"]

obs_rara <- stima_rara$Species_table[1, "Estimate"]
chao_rara <- stima_rara$Species_table[grep("Chao1", rownames(stima_rara$Species_table))[1], "Estimate"]

# 5. Prepariamo i dati per il grafico
df <- tibble(
  Campione = rep(c("Reale", "Con specie rare"), each = 2),
  Tipo = rep(c("Osservata", "Chao1"), 2),
  Valore = c(obs_reale, chao_reale, obs_rara, chao_rara)
)

# 6. Facciamo il grafico
ggplot(df, aes(x = Campione, y = Valore, fill = Tipo)) +
  geom_col(position = "dodge", width = 0.5) +
  geom_text(aes(label = round(Valore, 1)), 
            position = position_dodge(width = 0.5), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Osservata" = "darkseagreen", "Chao1" = "steelblue")) +
  labs(
    title = "Ricchezza osservata vs stimata (Chao1)",
    subtitle = "Effetto della presenza di specie rare",
    x = "Tipo di campione",
    y = "Numero di specie",
    fill = "Tipo di stima"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")


# --- 5. Numeri di Hill ---
library(hillR)

# Plot 1
hill0_1 <- hill_taxa(as.numeric(dati_f[1, ]), q = 0)
hill1_1 <- hill_taxa(as.numeric(dati_f[1, ]), q = 1)
hill2_1 <- hill_taxa(as.numeric(dati_f[1, ]), q = 2)

# Plot 2
hill0_2 <- hill_taxa(as.numeric(dati_f[2, ]), q = 0)
hill1_2 <- hill_taxa(as.numeric(dati_f[2, ]), q = 1)
hill2_2 <- hill_taxa(as.numeric(dati_f[2, ]), q = 2)

# Tabella per confronto
df_hill <- tibble(
  Plot = rep(c("Plot1", "Plot2"), each = 3),
  q = rep(c(0, 1, 2), 2),
  Hill = c(hill0_1, hill1_1, hill2_1,
           hill0_2, hill1_2, hill2_2)
)

# Grafico
ggplot(df_hill, aes(x = factor(q), y = Hill, fill = Plot)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  geom_text(aes(label = round(Hill, 1)), 
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 4) +
  labs(
    title = "Numeri di Hill (q = 0, 1, 2)",
    x = "Parametro q",
    y = "Diversità effettiva",
    fill = "Plot"
  ) +
  theme_minimal(base_size = 14)


##AED
AED <- function(H0, H1, H2){
  aed <- H0+((H1^2)/(2*H2))
  return(aed)
}

AED1 <- AED(H0 = hill0_1,H1= hill1_1, H2= hill2_1)
AED2 <- AED(H0 = hill0_2,H1= hill1_2, H2= hill2_2)

# --- 6. Beta-diversità ---
# Whittaker beta-diversità
S1 <- specnumber(as.numeric(dati_f[1, ]))  # ricchezza plot 1
S2 <- specnumber(as.numeric(dati_f[2, ]))  # ricchezza plot 2
alpha_mean <- mean(c(S1, S2))

gamma <- specnumber(colSums(dati_f))  # numero di specie totali nei 2 plot uniti

beta_whittaker <- (gamma / alpha_mean) - 1
print(beta_whittaker)

df_whittaker <- tibble(
  Tipo = c("Media α", "Gamma", "Beta Whittaker"),
  Valore = c(alpha_mean, gamma, beta_whittaker)
)

ggplot(df_whittaker, aes(x = Tipo, y = Valore, fill = Tipo)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = round(Valore, 1)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Media α" = "lightblue", 
                               "Gamma" = "steelblue", 
                               "Beta Whittaker" = "darkblue")) +
  labs(
    title = "Composizione della beta-diversità di Whittaker",
    y = "Numero di specie / Indice",
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


bray <- vegdist(dati_f, method = "bray")
jaccard <- vegdist(dati_f, method = "jaccard")

print(beta_whittaker)
print(as.matrix(bray))
print(as.matrix(jaccard))

df_beta <- tibble(
  Plot = "Plot1 vs Plot2",
  Bray_Curtis = as.numeric(bray),
  Jaccard = as.numeric(jaccard)
) %>%
  pivot_longer(cols = -Plot, names_to = "Metodo", values_to = "Distanza")

ggplot(df_beta, aes(x = Metodo, y = Distanza, fill = Metodo)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = round(Distanza, 2)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Bray_Curtis" = "coral", "Jaccard" = "darkseagreen3")) +
  labs(
    title = "Beta-diversità: Distanze Bray-Curtis e Jaccard",
    y = "Distanza (0 = uguali, 1 = completamente diverse)",
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


# Whittaker plot semplice per un plot
whittaker_plot <- function(abbondanze, plot) {
  abb <- abbondanze[abbondanze > 0]          # togli gli zeri
  abb_ord <- sort(abb, decreasing = TRUE)
  
  df <- tibble(
    Rank = seq_along(abb_ord),
    Abbondanza = abb_ord
  )
  
  ggplot(df, aes(x = Rank, y = Abbondanza)) +
    geom_line() +
    scale_y_log10() +
    labs(
      title = paste("Whittaker Plot -", plot),
      x = "Rango della specie",
      y = "Abbondanza (log10)"
    ) +
    theme_minimal(base_size = 13)
}


whittaker_plot(as.numeric(dati_f[1, ]), "Plot 1")
whittaker_plot(as.numeric(dati_f[2, ]), "Plot 2") 
#se la curva scende rapidamente, vuol dire che poche specie dominano.
# Se è più piatta, la comunità è più equa.


# Simula un dataset con 10 plot e 30 specie
set.seed(123)
fake_data <- matrix(rpois(300, lambda = 2), nrow = 10, ncol = 30)

# Curve di accumulo
acc_fake <- specaccum(fake_data, method = "random")

plot(acc_fake)
plot(acc_fake,
     main = "Curva di accumulo delle specie (esempio simulato)",
     xlab = "Numero di campioni",
     ylab = "Numero di specie",
     ci.type = "poly", col = "darkgreen", lwd = 2,
     ci.col = scales::alpha("darkgreen", 0.4))

# Curve stimate
pool_fake <- poolaccum(fake_data)
plot(pool_fake)
plot(pool_fake,
     main = "Stime di ricchezza (esempio simulato)",
     xlab = "Numero di campioni",
     ylab = "Specie stimate",
     lwd = 2,
     col = c("black", "blue", "red", "purple"))

# ---  Curve ABC e indice W ---
rescale <- function(x) ((x/max(x)*100))

dati_B <- read.csv("GFBI_b.csv")

dati_B_f <- dati_B %>% 
  select(-PlotID, -LON, -LAT, -DSN, -YR, -PA, -N, -B)

ab_Plot1<- data.frame(cum_ab= sort(colSums(dati_f[1,]), decreasing = T) %>% 
                       cumsum()) 
bas_Plot1<- data.frame(cum_bas= sort(colSums(dati_B_f [2,]), decreasing = T) %>% 
                       cumsum())

ab_Plot1 <- rescale(ab_Plot1)

bas_Plot1 <- rescale(bas_Plot1)

w_Plot1<- sum((bas_Plot1 - ab_Plot1)/ (50 * (nrow(bas_Plot1)-1))) 

###ggplot object
ab_Plot1$cum_bas <- NA
ab_Plot1$Variable <-as.factor("Abundance")

bas_Plot1$cum_ab <- NA
bas_Plot1$Variable <- as.factor("Basal Area")


df_Plot1 <- rbind(ab_Plot1, bas_Plot1)



# Plot ABC
ggplot() +
  geom_line(
    data = subset(df_Plot1, Variable == "Abundance"),
    aes(x = log(rank(cum_ab)), y = cum_ab, color = "Abundance"),
    linewidth = 1.2
  ) +
  geom_line(
    data = subset(df_Plot1, Variable == "Basal Area"),
    aes(x = log(rank(cum_bas)), y = cum_bas, color = "Basal Area"),
    linewidth = 1.2
  ) +
  scale_color_manual(values = c("Abundance" = "red", "Basal Area" = "blue")) +
  labs(
    title = "Curve ABC Plot1",
    subtitle = paste0("Indice W = ", round(w_Plot1, 3)),
    x = "Rango (log)",
    y = "Cumulata (%)",
    color = "Tipo"
  ) +
  theme_minimal(base_size = 14)

ab_Plot2<- data.frame(cum_ab= sort(colSums(dati_f[2,]), decreasing = T) %>% 
                        cumsum()) 
bas_Plot2<- data.frame(cum_bas= sort(colSums(dati_B_f [1,]), decreasing = T) %>% 
                         cumsum())

ab_Plot2 <- rescale(ab_Plot2)

bas_Plot2 <- rescale(bas_Plot2)

w_Plot2 <- sum((bas_Plot2 - ab_Plot2)/ (50 * (nrow(bas_Plot2)-1))) 

###ggplot object
ab_Plot2$cum_bas <- NA
ab_Plot2$Variable <-as.factor("Abundance")

bas_Plot2$cum_ab <- NA
bas_Plot2$Variable <- as.factor("Basal Area")


df_Plot2<- rbind(ab_Plot2, bas_Plot2)


ggplot() +
  geom_line(
    data = subset(df_Plot2, Variable == "Abundance"),
    aes(x = log(rank(cum_ab)), y = cum_ab, color = "Abundance"),
    linewidth = 1.2
  ) +
  geom_line(
    data = subset(df_Plot2, Variable == "Basal Area"),
    aes(x = log(rank(cum_bas)), y = cum_bas, color = "Basal Area"),
    linewidth = 1.2
  ) +
  scale_color_manual(values = c("Abundance" = "red", "Basal Area" = "blue")) +
  labs(
    title = "Curve ABC Plot2",
    subtitle = paste0("Indice W = ", round(w_Plot2, 3)),
    x = "Rango (log)",
    y = "Cumulata (%)",
    color = "Tipo"
  ) +
  theme_minimal(base_size = 14)
                       
