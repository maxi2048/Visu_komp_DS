library("ggplot2")
library("ggpubr")
library("tidyr")
library("dplyr")
library("reshape2")
dat
sum(is.na(dat))

############################################################################################################################################################
# Deskriptive Statistik

# Indikationen
# Liste der relevanten Spalten mit den Erkrankungen
indikationen <- c("Sepsis", "Schock", "Bacteraemia", "Catheter", "BJI", 
                  "Endocarditis", "CNS", "Gastrointestinal", "Genitourinary", 
                  "Pulmonary", "SSTI")

# Datensatz auf die relevanten Spalten reduzieren
dat_subset <- dat[, indikationen]

# Sicherstellen, dass nur "yes" oder "no" vorhanden sind
dat_subset[] <- lapply(dat_subset, function(x) factor(x, levels = c("yes", "no")))

# Häufigkeiten von "yes" und "no" für jede Erkrankung berechnen
df_counts <- as.data.frame(sapply(dat_subset, function(x) table(x)))

df_long <- dat_subset %>%
  pivot_longer(cols = everything(), names_to = "Indikationen", values_to = "Antwort") %>%
  group_by(Indikationen, Antwort) %>%
  summarise(Anzahl = n(), .groups = "drop")

order_krankheiten <- df_long %>%
  filter(Antwort == "yes") %>%
  arrange(desc(Anzahl)) %>%
  pull(Indikationen)

# Faktor-Level für Erkrankungen setzen (damit ggplot sie richtig sortiert)
df_long$Indikationen <- factor(df_long$Indikationen, levels = order_krankheiten)

# Balkendiagramm mit ggplot2
ggplot(df_long, aes(x = Indikationen, y = Anzahl, fill = Antwort)) +
  geom_bar(stat = "identity", position = "dodge") +  # Gruppierte Balken
  theme_minimal() +  # Schönes Theme
  labs(title = "Anzahl der Indikationen",
       x = "Indikationen",
       y = "Anzahl",
       fill = "Antwort") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Begleiterkrankungen
begleiterkrankungen <- c("Cardiovascular", "Hypertension", "CHF", "CKD", "COPD", "DM", "Malignancy")
dat_subset1 <- dat[ ,begleiterkrankungen]

dat_subset1[] <- lapply(dat_subset1, function(x) factor(x, levels = c("yes", "no")))

df_counts1 <- as.data.frame(sapply(dat_subset1, function(x) table(x)))

df_long1 <- dat_subset1 %>% 
  pivot_longer(cols = everything(), names_to = "Vorerkrankungen", values_to = "Antwort") %>%
  group_by(Vorerkrankungen, Antwort) %>%
  summarise(Anzahl = n(), .groups = "drop")
order_vorerkrankungen <- df_long1 %>% 
  filter(Antwort == "yes") %>%
  arrange(desc(Anzahl)) %>%
  pull(Vorerkrankungen)

df_long1$Vorerkrankungen <- factor(df_long1$Vorerkrankungen, levels = order_vorerkrankungen)

ggplot(df_long1, aes(x = Vorerkrankungen, y = Anzahl, fill = Antwort)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() +
  labs(title = "Anzahl der Vorerkrankungen",
       x = "Vorerkrankungen",
       y = "Anzahl",
       fill = "Antwort") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
############################################################################################################################################################
SCR_VAL <- dat %>%
  select(SCrStart, SCr24, SCr48, SCr72, SCrEnd) %>%  
  pivot_longer(cols = everything(), names_to = "Zeitpunkt_SCR", values_to = "WertSCR")

SCR_na_counts <- SCR_VAL %>%
  group_by(Zeitpunkt_SCR) %>%
  summarise(NAs = sum(is.na(WertSCR)))

eGFR_VAL <- dat %>% 
  select(eGFRStart, eGFR24, eGFR48, eGFR72, eGFREnd) %>%
  pivot_longer(cols = everything(), names_to = "Zeitpunkt_eGFR", values_to = "WerteGFR")

eGFR_na_counts <- eGFR_VAL %>% 
  group_by(Zeitpunkt_eGFR) %>%
  summarise(NAs = sum(is.na(WerteGFR)))

SCR_VAL$Zeitpunkt_SCR <- factor(SCR_VAL$Zeitpunkt_SCR, levels = c("SCrStart", "SCr24", "SCr48", "SCr72", "SCrEnd"))

ggplot(SCR_VAL, aes(x = Zeitpunkt_SCR, y = WertSCR, fill = Zeitpunkt_SCR)) +
  geom_boxplot() +
  geom_text(data = SCR_na_counts, aes(x = Zeitpunkt_SCR, y = max(SCR_VAL$WertSCR, na.rm = TRUE) + 0.1, label = paste0("NAs: ", NAs)), size = 4) +
  theme_minimal() +
  labs(title = "Verlauf der Kreatinin-Werte", x = "Zeitpunkt", y = "Serum-Kreatinin (mg/dL)") +
  theme(legend.position = "none")

eGFR_VAL$Zeitpunkt_eGFR <- factor(eGFR_VAL$Zeitpunkt_eGFR, levels = c("eGFRStart", "eGFR24", "eGFR48", "eGFR72", "eGFREnd"))

ggplot(eGFR_VAL, aes(x = Zeitpunkt_eGFR, y = WerteGFR, fill = Zeitpunkt_eGFR)) +
  geom_boxplot() +
  geom_text(data = eGFR_na_counts, aes(x = Zeitpunkt_eGFR, y = max(eGFR_VAL$WerteGFR, na.rm = TRUE) + 0.1, label = paste0("NAs: ", NAs)), size = 4) +
  theme_minimal() +
  labs(title = "Verlauf der Glomerulären Filtrationsrate", x = "Zeitpunkt", y = "Geschätzte glomeruläre Filtrationsrate") +
  theme(legend.position = "none")

############################################################################################################################################################

#Hypothese: Therapie senkt Kreatinin Werte, gepaarter t-Test
# Direkter Vergleich der Boxplots Start und Ende

ggplot(SCR_VAL[SCR_VAL$Zeitpunkt_SCR %in% c("SCrStart", "SCrEnd"),], aes(x = Zeitpunkt_SCR, y = log(WertSCR), fill = Zeitpunkt_SCR)) +
  geom_boxplot() 


dat$DIFF_SCr <- dat$SCrStart-dat$SCrEnd

ggplot(dat, aes(x = dat$DIFF_SCr)) +
  geom_histogram(binwidth = .01, fill = "lightblue", color = "lightblue") +
  labs(title = "Histogramm der Differenzen",
       x = "Differenzen (Vorher - Ende)",
       y = "Häufigkeit") +
  theme_minimal()

shapiro.test(dat$DIFF_SCr)
# H0 das Die Daten normalverteilt sind wird abgelehnt 
# Gepaarter t-Test ist nicht ratsam also Alternative: Rangsummen Test 

wilcox.test(dat$SCrStart, dat$SCrEnd, paired = TRUE)
# H0 Mediane sind gleich
# H1 Mediane sind nicht gleich 
# p < 2.2e-16, daher lässt sich H0 relativ sicher verwerfen 

ggplot(SCR_VAL[SCR_VAL$Zeitpunkt_SCR %in% c("SCrStart","SCrEnd"),], aes(x = Zeitpunkt_SCR, y = log(WertSCR), fill = Zeitpunkt_SCR)) +
  geom_violin(trim = FALSE, alpha = 0.6) + 
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) + 
  # stat_compare_means(method = "wilcox.test", paired = TRUE) +
  labs(title = "Violinplot der eGFR-Werte zur Visualiserung des Tests",
       x = "Messzeitpunkt",
       y = "Logarithmisch skalierte eGFR-Werte") +
  theme_minimal()


############################################################################################################################################################

#Analog für eGFR, niedrige eGFR ist i.A. Indikator für Nierenschäden, hilft die Therapie die eGFR zu erhöhen ?

ggplot(eGFR_VAL[eGFR_VAL$Zeitpunkt_eGFR %in% c("eGFRStart","eGFREnd"),], aes(x = Zeitpunkt_eGFR, y = log(WerteGFR), fill = Zeitpunkt_eGFR)) +
  geom_boxplot()

# Ist der Unterschied statistisch signifikant
dat$DIFF_eGFR <- dat$eGFREnd-dat$eGFRStart

ggplot(dat, aes(x = DIFF_eGFR)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count..), color = "red", size = 1) +  # Dichtekurve
  labs(title = "Histogramm der eGFR-Differenzen",
       x = "Differenz (eGFREnd - eGFRStart)",
       y = "Häufigkeit") +
  theme_minimal()

# ggplot(dat, aes(x = DIFF_eGFR)) +
#   geom_histogram(binwidth = 1, fill = "lightblue", color = "lightblue") +
#   labs(title = "Histogramm der Differenzen",
#        x = "Differenzen (Vorher - Ende)",
#        y = "Häufigkeit") +
#   theme_minimal()

shapiro.test(dat$DIFF_eGFR)
# H0 das Die Daten normalverteilt sind wird abgelehnt 
# Gepaarter t-Test ist nicht ratsam also Alternative: Rangsummen Test 

wilcox.test(dat$eGFRStart, dat$eGFREnd, paired = TRUE)
# H0 Mediane sind gleich
# H1 Mediane sind nicht gleich 
# p < 2.2e-16, daher lässt sich H0 relativ sicher verwerfen

ggplot(eGFR_VAL[eGFR_VAL$Zeitpunkt_eGFR %in% c("eGFRStart","eGFREnd"),], aes(x = Zeitpunkt_eGFR, y = log(WerteGFR), fill = Zeitpunkt_eGFR)) +
  geom_violin(trim = FALSE, alpha = 0.6) + 
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) + 
  # stat_compare_means(method = "wilcox.test", paired = TRUE) +
  labs(title = "Violinplot der eGFR-Werte zur Visualiserung des Tests",
       x = "Messzeitpunkt",
       y = "Logarithmisch skalierte eGFR-Werte") +
  theme_minimal()



############################################################################################################################################################
# Haben die Vorerkrankungen einen Einfluss auf den Verlauf der Nierenwerte ? Cardiovascular und Hypertension machen den Groteil aus 
# Cardiovascular auf eGFR
dat_cv <- dat[dat$Cardiovascular == "yes",]
eGFR_VAL_cv <- dat %>% 
  filter(Cardiovascular == "yes") %>%
  select(eGFRStart, eGFR24, eGFR48, eGFR72, eGFREnd) %>%
  pivot_longer(cols = everything(), names_to = "Zeitpunkt_eGFR", values_to = "WerteGFR")

eGFR_na_counts_cv <- eGFR_VAL_cv %>% 
  group_by(Zeitpunkt_eGFR) %>%
  summarise(NAs = sum(is.na(WerteGFR)))

ggplot(eGFR_VAL_cv[eGFR_VAL_cv$Zeitpunkt_eGFR %in% c("eGFRStart","eGFREnd"),], aes(x = Zeitpunkt_eGFR, y = log(WerteGFR), fill = Zeitpunkt_eGFR)) +
  geom_boxplot()

# Ist der Unterschied statistisch signifikant
dat_cv$DIFF_eGFR <- dat_cv$eGFREnd-dat_cv$eGFRStart

ggplot(dat_cv, aes(x = DIFF_eGFR)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count..), color = "red", size = 1) +  # Dichtekurve
  labs(title = "Histogramm der eGFR-Differenzen",
       x = "Differenz (eGFREnd - eGFRStart)",
       y = "Häufigkeit") +
  theme_minimal()

# ggplot(dat, aes(x = DIFF_eGFR)) +
#   geom_histogram(binwidth = 1, fill = "lightblue", color = "lightblue") +
#   labs(title = "Histogramm der Differenzen",
#        x = "Differenzen (Vorher - Ende)",
#        y = "Häufigkeit") +
#   theme_minimal()

shapiro.test(dat_cv$DIFF_eGFR)
# H0 das Die Daten normalverteilt sind wird abgelehnt 
# Gepaarter t-Test ist nicht ratsam also Alternative: Rangsummen Test 

wilcox.test(dat_cv$eGFRStart, dat_cv$eGFREnd, paired = TRUE)
# H0 Mediane sind gleich
# H1 Mediane sind nicht gleich 
# p < 2.2e-16, daher lässt sich H0 relativ sicher verwerfen

ggplot(eGFR_VAL_cv[eGFR_VAL_cv$Zeitpunkt_eGFR %in% c("eGFRStart","eGFREnd"),], aes(x = Zeitpunkt_eGFR, y = log(WerteGFR), fill = Zeitpunkt_eGFR)) +
  geom_violin(trim = FALSE, alpha = 0.6) + 
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) + 
  # stat_compare_means(method = "wilcox.test", paired = TRUE) +
  labs(title = "Violinplot der eGFR-Werte zur Visualiserung des Tests",
       x = "Messzeitpunkt",
       y = "Logarithmisch skalierte eGFR-Werte") +
  theme_minimal()

dat_ht <- dat[dat$Hypertension == "yes",]
eGFR_VAL_ht <- dat %>% 
  filter(Hypertension == "yes") %>%
  select(eGFRStart, eGFR24, eGFR48, eGFR72, eGFREnd) %>%
  pivot_longer(cols = everything(), names_to = "Zeitpunkt_eGFR", values_to = "WerteGFR")

eGFR_na_counts_ht <- eGFR_VAL_ht %>% 
  group_by(Zeitpunkt_eGFR) %>%
  summarise(NAs = sum(is.na(WerteGFR)))

ggplot(eGFR_VAL_ht[eGFR_VAL_ht$Zeitpunkt_eGFR %in% c("eGFRStart","eGFREnd"),], aes(x = Zeitpunkt_eGFR, y = log(WerteGFR), fill = Zeitpunkt_eGFR)) +
  geom_boxplot()

# Ist der Unterschied statistisch signifikant

# Hypertrophie und eGFR
dat_ht$DIFF_eGFR <- dat_ht$eGFREnd-dat_ht$eGFRStart

ggplot(dat_ht, aes(x = DIFF_eGFR)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count..), color = "red", size = 1) +  # Dichtekurve
  labs(title = "Histogramm der eGFR-Differenzen",
       x = "Differenz (eGFREnd - eGFRStart)",
       y = "Häufigkeit") +
  theme_minimal()

# ggplot(dat, aes(x = DIFF_eGFR)) +
#   geom_histogram(binwidth = 1, fill = "lightblue", color = "lightblue") +
#   labs(title = "Histogramm der Differenzen",
#        x = "Differenzen (Vorher - Ende)",
#        y = "Häufigkeit") +
#   theme_minimal()

shapiro.test(dat_ht$DIFF_eGFR)
# H0 das Die Daten normalverteilt sind wird abgelehnt 
# Gepaarter t-Test ist nicht ratsam also Alternative: Rangsummen Test 

wilcox.test(dat_ht$eGFRStart, dat_ht$eGFREnd, paired = TRUE)
# H0 Mediane sind gleich
# H1 Mediane sind nicht gleich 
# p < 2.2e-16, daher lässt sich H0 relativ sicher verwerfen

ggplot(eGFR_VAL_ht[eGFR_VAL_ht$Zeitpunkt_eGFR %in% c("eGFRStart","eGFREnd"),], aes(x = Zeitpunkt_eGFR, y = log(WerteGFR), fill = Zeitpunkt_eGFR)) +
  geom_violin(trim = FALSE, alpha = 0.6) + 
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) + 
  # stat_compare_means(method = "wilcox.test", paired = TRUE) +
  labs(title = "Violinplot der eGFR-Werte zur Visualiserung des Tests",
       x = "Messzeitpunkt",
       y = "Logarithmisch skalierte eGFR-Werte") +
  theme_minimal()


