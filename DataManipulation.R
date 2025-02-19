library("ggplot2")
library("tidyr")
library("dplyr")
dat
sum(is.na(dat))

ggplot(data = dat, aes(x = SCrStart))+geom_boxplot()

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

ggplot(SCR_VAL, aes(x = Zeitpunkt_SCR, y = WertSCR, fill = Zeitpunkt_SCR)) +
  geom_boxplot() +
  geom_text(data = SCR_na_counts, aes(x = Zeitpunkt_SCR, y = max(SCR_VAL$WertSCR, na.rm = TRUE) + 0.1, label = paste0("NAs: ", NAs)), size = 4) +
  theme_minimal() +
  labs(title = "Verlauf der Kreatinin-Werte", x = "Zeitpunkt", y = "Serum-Kreatinin (mg/dL)") +
  theme(legend.position = "none")

ggplot(eGFR_VAL, aes(x = Zeitpunkt_eGFR, y = WerteGFR, fill = Zeitpunkt_eGFR)) +
  geom_boxplot() +
  geom_text(data = eGFR_na_counts, aes(x = Zeitpunkt_eGFR, y = max(eGFR_VAL$WerteGFR, na.rm = TRUE) + 0.1, label = paste0("NAs: ", NAs)), size = 4) +
  theme_minimal() +
  labs(title = "Verlauf der Glomerulären Filtrationsrate", x = "Zeitpunkt", y = "Geschätzte glomeruläre Filtrationsrate") +
  theme(legend.position = "none")

