pacman::p_load(tree, tidyverse, randomForest, gbm, BART, readxl, haven, rpart)

# Assembling data ---------------------------------------------------------

dataset <- read_xlsx("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Dataset/Universe (11.10.21).xlsx",
                     sheet = "All Municipalities", n_max = 1121)

data2017 <- dataset %>%
  select(coddpto, codmpio, `Recruitment 2017`, `ex-Farc Murders 2017`, 
         `Displacement 2017`, `Socail Leaders Murders 2017`, `Press Freedom Violations 2017`,
         `Extortion 2017`, `Terrorism 2017`, `Kidnapping 2017`,
         `School desertation 2017 (%)`, `Murders 2017`, `Marihuana Seizures 2017`, 
         `Cocaine Seizures 2017`, `Base de Coca Seizures 2017`, `Basuco Seizures 2017`,
         `Heroína Seizures 2017`, `Coca Production 2017`, `PNIS or NOT`, `ETCR or NOT`,
         `NAR or NOT`, `PDET or NOT`, `Dissident FARC (Rearmed)`,
         `Dissident FARC (Residuales)`, `Dissident FARC Combined`,
         `Interrelated Criminal Group Existence`) %>%
  rename(D_code = coddpto, M_code = codmpio, recruitment = `Recruitment 2017`,
         exfarc_killings = `ex-Farc Murders 2017`, displacement = `Displacement 2017`,
         social_leaders = `Socail Leaders Murders 2017`, 
         press_freedom = `Press Freedom Violations 2017`, 
         extortion = `Extortion 2017`, terrorism = `Terrorism 2017`, 
         kidnapping = `Kidnapping 2017`, school_desert = `School desertation 2017 (%)`,
         murders = `Murders 2017`, marihuana = `Marihuana Seizures 2017`,
         cocaine = `Cocaine Seizures 2017`, coca_base = `Base de Coca Seizures 2017`,
         basuco = `Basuco Seizures 2017`, heroin = `Heroína Seizures 2017`,
         coca = `Coca Production 2017`, pnis = `PNIS or NOT`, etcr = `ETCR or NOT`,
         nar = `NAR or NOT`, pdet = `PDET or NOT`,
         dissidents_rearmed = `Dissident FARC (Rearmed)`, 
         dissidents_residuals = `Dissident FARC (Residuales)`,
         dissidents_combined = `Dissident FARC Combined`, 
         criminal_groups = `Interrelated Criminal Group Existence`) %>%
  mutate(displacement = as.numeric(displacement),
         year = 2017)

data2018 <- dataset %>%
  select(coddpto, codmpio, `Recruitment 2018`, `ex-Farc Murders 2018`, 
         `Displacement 2018`, `Socail Leaders Murders 2018`, `Press Freedom Violations 2018`,
         `Extortion 2018`, `Terrorism 2018`, `Kidnapping 2018`,
         `School desertation 2018 (%)`, `Murders 2018`, `Marihuana Seizures 2018`, 
         `Cocaine Seizures 2018`, `Base de Coca Seizures 2018`, `Basuco Seizures 2018`,
         `Heroína Seizures 2018`, `Coca Production 2018`, `PNIS or NOT`, `ETCR or NOT`,
         `NAR or NOT`, `PDET or NOT`, `Dissident FARC (Rearmed)`,
         `Dissident FARC (Residuales)`, `Dissident FARC Combined`,
         `Interrelated Criminal Group Existence`) %>%
  rename(D_code = coddpto, M_code = codmpio, recruitment = `Recruitment 2018`,
         exfarc_killings = `ex-Farc Murders 2018`, displacement = `Displacement 2018`,
         social_leaders = `Socail Leaders Murders 2018`, 
         press_freedom = `Press Freedom Violations 2018`, 
         extortion = `Extortion 2018`, terrorism = `Terrorism 2018`, 
         kidnapping = `Kidnapping 2018`, school_desert = `School desertation 2018 (%)`,
         murders = `Murders 2018`, marihuana = `Marihuana Seizures 2018`,
         cocaine = `Cocaine Seizures 2018`, coca_base = `Base de Coca Seizures 2018`,
         basuco = `Basuco Seizures 2018`, heroin = `Heroína Seizures 2018`,
         coca = `Coca Production 2018`, pnis = `PNIS or NOT`, etcr = `ETCR or NOT`,
         nar = `NAR or NOT`, pdet = `PDET or NOT`,
         dissidents_rearmed = `Dissident FARC (Rearmed)`, 
         dissidents_residuals = `Dissident FARC (Residuales)`,
         dissidents_combined = `Dissident FARC Combined`, 
         criminal_groups = `Interrelated Criminal Group Existence`) %>%
  mutate(year = 2018)

data2019 <- dataset %>%
  select(coddpto, codmpio, `Recruitment 2019`, `ex-Farc Murders 2019`, 
         `Displacement 2019`, `Socail Leaders Murders 2019`, `Press Freedom Violations 2019`,
          `Extortion 2019`, `Terrorism 2019`, `Kidnapping 2019`,
         `School desertation 2019 (%)`, `Murders 2019`, `Marihuana Seizures 2019`, 
         `Cocaine Seizures 2019`, `Base de Coca Seizures 2019`, `Basuco Seizures 2019`,
         `Heroína Seizures 2019`, `Coca Production 2019`, `PNIS or NOT`, `ETCR or NOT`,
         `NAR or NOT`, `PDET or NOT`, `Dissident FARC (Rearmed)`,
         `Dissident FARC (Residuales)`, `Dissident FARC Combined`,
         `Interrelated Criminal Group Existence`) %>%
  rename(D_code = coddpto, M_code = codmpio, recruitment = `Recruitment 2019`,
         exfarc_killings = `ex-Farc Murders 2019`, displacement = `Displacement 2019`,
         social_leaders = `Socail Leaders Murders 2019`, 
         press_freedom = `Press Freedom Violations 2019`, 
         extortion = `Extortion 2019`, terrorism = `Terrorism 2019`, 
         kidnapping = `Kidnapping 2019`, school_desert = `School desertation 2019 (%)`,
         murders = `Murders 2019`, marihuana = `Marihuana Seizures 2019`,
         cocaine = `Cocaine Seizures 2019`, coca_base = `Base de Coca Seizures 2019`,
         basuco = `Basuco Seizures 2019`, heroin = `Heroína Seizures 2019`,
         coca = `Coca Production 2019`, pnis = `PNIS or NOT`, etcr = `ETCR or NOT`,
         nar = `NAR or NOT`, pdet = `PDET or NOT`,
         dissidents_rearmed = `Dissident FARC (Rearmed)`, 
         dissidents_residuals = `Dissident FARC (Residuales)`,
         dissidents_combined = `Dissident FARC Combined`, 
         criminal_groups = `Interrelated Criminal Group Existence`) %>%
  mutate(murders = as.numeric(murders),
         year = 2019)

data2020 <- dataset %>%
  select(coddpto, codmpio, `Recruitment 2020`, `ex-Farc Murders 2020`, 
         `Displacement 2020`, `Socail Leaders Murders 2020`, `Press Freedom Violations 2020`,
         `Extortion 2020`, `Terrorism 2020`, `Kidnapping 2020`,
         `School desertation 2020 (%)`, `Murders 2020`, `Marihuana Seizures 2020`, 
         `Cocaine Seizures 2020`, `Base de Coca Seizures 2020`, `Basuco Seizures 2020`,
         `Heroína Seizures 2020`, `Coca Production 2020`, `PNIS or NOT`, `ETCR or NOT`,
         `NAR or NOT`, `PDET or NOT`, `Dissident FARC (Rearmed)`,
         `Dissident FARC (Residuales)`, `Dissident FARC Combined`,
         `Interrelated Criminal Group Existence`) %>%
  rename(D_code = coddpto, M_code = codmpio, recruitment = `Recruitment 2020`,
         exfarc_killings = `ex-Farc Murders 2020`, displacement = `Displacement 2020`,
         social_leaders = `Socail Leaders Murders 2020`, 
         press_freedom = `Press Freedom Violations 2020`, 
         extortion = `Extortion 2020`, terrorism = `Terrorism 2020`, 
         kidnapping = `Kidnapping 2020`, school_desert = `School desertation 2020 (%)`,
         murders = `Murders 2020`, marihuana = `Marihuana Seizures 2020`,
         cocaine = `Cocaine Seizures 2020`, coca_base = `Base de Coca Seizures 2020`,
         basuco = `Basuco Seizures 2020`, heroin = `Heroína Seizures 2020`,
         coca = `Coca Production 2020`, pnis = `PNIS or NOT`, etcr = `ETCR or NOT`,
         nar = `NAR or NOT`, pdet = `PDET or NOT`,
         dissidents_rearmed = `Dissident FARC (Rearmed)`, 
         dissidents_residuals = `Dissident FARC (Residuales)`,
         dissidents_combined = `Dissident FARC Combined`, 
         criminal_groups = `Interrelated Criminal Group Existence`) %>%
  mutate(social_leaders = as.numeric(social_leaders),
         year = 2020)

dataset <- bind_rows(data2017, data2018, data2019, data2020)

rm(data2017, data2018, data2019, data2020)

farc_presence <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Conflict/Database_URosario_1996_2019.dta") %>%
  filter(year == 2011 | year == 2012) %>%
  mutate(atfarc = farcd*atgue) %>%
  group_by(M_code) %>%
  count(wt = atfarc, name = "atfarc")

farc_presence <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Panel_municipios/PANEL_CARACTERISTICAS_GENERALES(2020).dta") %>%
  filter(ano == 2012) %>%
  select(codmpio, pobl_tot) %>%
  rename(M_code = codmpio) %>%
  filter(!(M_code == 88001 | M_code == 88564)) %>%
  left_join(farc_presence, by = "M_code") %>%
  mutate(atfarc = ifelse(is.na(atfarc), 0, atfarc),
         atfarc = (atfarc / pobl_tot) * 10000,
         p50_farc = median(atfarc[atfarc>0]),
         farc = as.numeric((atfarc > p50_farc))) %>%
  filter(is.na(M_code) == F) %>%
  select(M_code, farc)

year <- rep(2017, 1120)
farc_presence_17 <- bind_cols(farc_presence, year)
year <- rep(2018, 1120)
farc_presence_18 <- bind_cols(farc_presence, year)
year <- rep(2019, 1120)
farc_presence_19 <- bind_cols(farc_presence, year)
year <- rep(2020, 1120)
farc_presence_20 <- bind_cols(farc_presence, year)
farc_presence <- bind_rows(farc_presence_17, farc_presence_18, farc_presence_19,
                           farc_presence_20)
colnames(farc_presence) <- c("M_code", "farc", "year")

farc_presence <- farc_presence %>%
  arrange(M_code, year)

rm(year, farc_presence_17, farc_presence_18, farc_presence_19, farc_presence_20)

eln_other_presence <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Conflict/Database_URosario_1996_2019.dta") %>%
  filter(year > 2010 & year < 2015) %>%
  mutate(ateln = elnd*atgue) %>%
  group_by(M_code) %>%
  summarize(ateln = sum(ateln),
            atpar = sum(atpar))

eln_other_presence <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Panel_municipios/PANEL_CARACTERISTICAS_GENERALES(2020).dta") %>%
  filter(ano == 2012) %>%
  select(codmpio, pobl_tot) %>%
  rename(M_code = codmpio) %>%
  filter(!(M_code == 88001 | M_code == 88564)) %>%
  left_join(eln_other_presence, by = "M_code") %>%
  mutate(ateln = ifelse(is.na(ateln), 0, ateln),
         ateln = (ateln / pobl_tot) * 10000,
         p50_eln = median(ateln[ateln>0]),
         eln = as.numeric((ateln > p50_eln)),
         atpar = ifelse(is.na(atpar), 0, atpar),
         atpar = (atpar / pobl_tot) * 10000,
         p50_par = median(atpar[atpar>0]),
         paramilitary = as.numeric((atpar > p50_par)),
         other = as.numeric(eln > 0 | paramilitary > 0)) %>%
  filter(is.na(M_code) == F) %>%
  select(M_code, eln, paramilitary, other)

year <- rep(2017, 1120)
eln_other_presence_17 <- bind_cols(eln_other_presence, year)
year <- rep(2018, 1120)
eln_other_presence_18 <- bind_cols(eln_other_presence, year)
year <- rep(2019, 1120)
eln_other_presence_19 <- bind_cols(eln_other_presence, year)
year <- rep(2020, 1120)
eln_other_presence_20 <- bind_cols(eln_other_presence, year)
eln_other_presence <- bind_rows(eln_other_presence_17, eln_other_presence_18,
                                eln_other_presence_19, eln_other_presence_20)
colnames(eln_other_presence) <- c("M_code", "eln", "paramilitary", "other", "year")

eln_other_presence <- eln_other_presence %>%
  arrange(M_code, year)

rm(year, eln_other_presence_17, eln_other_presence_18, eln_other_presence_19, eln_other_presence_20)

disputed_groups <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Conflict/Database_URosario_1996_2019.dta") %>%
  filter(year > 2015) %>%
  mutate(atfarc = farcd*atgue,
         ateln = elnd*atgue,
         police = atgov*pold,
         military = atgov*ffmmd) %>%
  group_by(M_code, year) %>%
  summarize(atfarc = sum(atfarc),
            ateln = sum(ateln),
            atpar = sum(atpar),
            police = sum(police),
            military = sum(military),
            disputed = atfarc + ateln + atpar)

disputed_groups <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Panel_municipios/PANEL_CARACTERISTICAS_GENERALES(2020).dta") %>%
  filter(ano > 2015) %>%
  select(codmpio, ano, pobl_tot) %>%
  rename(M_code = codmpio,
         year = ano) %>%
  filter(!(M_code == 88001 | M_code == 88564)) %>%
  left_join(disputed_groups, by = c("M_code", "year")) %>%
  mutate(atfarc = ifelse(is.na(atfarc), 0, atfarc),
         ateln = ifelse(is.na(ateln), 0, ateln),
         atpar = ifelse(is.na(atpar), 0, atpar),
         disputed = ifelse(is.na(disputed), 0, disputed),
         police = ifelse(is.na(police), 0, police),
         military = ifelse(is.na(military), 0, military),
         atfarc = as.numeric((atfarc > 0)),
         ateln = as.numeric((ateln > 0)),
         atpar = as.numeric((atpar > 0)),
         disputed = as.numeric((disputed > 0)),
         police = as.numeric((police > 0)),
         police = as.numeric((police > 0)),
         year = ifelse(year == 2019, 2020, year),
         year = ifelse(year == 2018, 2019, year),
         year = ifelse(year == 2017, 2018, year),
         year = ifelse(year == 2016, 2017, year)) %>%
  filter(is.na(M_code) == F) %>%
  select(M_code, year, atfarc, ateln, atpar, police, military, disputed) 

conflict <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Conflict/Database_URosario_1996_2019.dta") %>%
  filter(year > 2015) %>%
  group_by(M_code, year) %>%
  summarize(kciv = sum(kciv),
            iciv = sum(iciv))

conflict <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Panel_municipios/PANEL_CARACTERISTICAS_GENERALES(2020).dta") %>%
  filter(ano > 2015) %>%
  select(codmpio, ano, pobl_tot) %>%
  rename(M_code = codmpio,
         year = ano) %>%
  filter(!(M_code == 88001 | M_code == 88564)) %>%
  left_join(conflict, by = c("M_code", "year")) %>%
  mutate(kciv = ifelse(is.na(kciv), 0, kciv),
         iciv = ifelse(is.na(iciv), 0, iciv),
         year = ifelse(year == 2019, 2020, year),
         year = ifelse(year == 2018, 2019, year),
         year = ifelse(year == 2017, 2018, year),
         year = ifelse(year == 2016, 2017, year)) %>%
  filter(is.na(M_code) == F) %>%
  select(M_code, year, kciv, iciv)

panel_farc_killed <- dataset %>%
  inner_join(farc_presence, by = c("M_code", "year")) %>%
  inner_join(eln_other_presence, by = c("M_code", "year")) %>%
  inner_join(disputed_groups, by = c("M_code", "year")) %>%
  inner_join(conflict, by = c("M_code", "year")) %>%
  arrange(M_code, year)

panel_farc_killed <- panel_farc_killed %>%
  mutate(paras = as.numeric(criminal_groups == "Only Paramilitaries"),
         no_groups = as.numeric(criminal_groups == "No Armed Groups"),
         para_farc = as.numeric(criminal_groups == "Paramilitaries+Dissident FARC"),
         para_eln = as.numeric(criminal_groups == "Paramilitaries+ELN"),
         para_farc_eln = as.numeric(criminal_groups == "Paramilitaries+Dissident FARC+ELN"),
         onlyeln = as.numeric(criminal_groups == "Only ELN"),
         onlyfarc = as.numeric(criminal_groups == "Only Dissident FARC"),
         farc_eln = as.numeric(criminal_groups == "Dissident FARC+ELN")) %>%
  select(-criminal_groups)

panel_farc_killed <- panel_farc_killed %>%
  mutate(killed = factor(ifelse(exfarc_killings > 0, "Yes", "No")))

for(i in 1:dim(panel_farc_killed)[1]){
  if(sum(is.na(panel_farc_killed[i,])) > 0){
    panel_farc_killed <- panel_farc_killed[-i, ]
  }
}

dropping <- panel_farc_killed %>%
  mutate(obs = 1) %>%
  group_by(M_code) %>%
  summarize(total_obs = sum(obs)) %>%
  filter(total_obs < 4) %>%
  select(M_code)

panel_farc_killed <- panel_farc_killed %>%
  filter(!(M_code %in% dropping$M_code))

# Predictive model --------------------------------------------------------

# Classification tree -----------------------------------------------------

# Prediction on time ------------------------------------------------------

# Defining train and test sets of variables

x.train <- panel_farc_killed %>%
  mutate(train = as.numeric((year %in% c(2017, 2018, 2019)))) %>%
  select(-(1:5)) %>%
  filter(train == 1) %>%
  select(-train)

y.train <- x.train$killed

x.test <- panel_farc_killed %>%
  mutate(train = as.numeric((year %in% c(2017, 2018, 2019)))) %>%
  select(-(1:5)) %>%
  filter(train == 0) %>%
  select(-train)

y.test <- x.test$killed

# Running main classification tree

set.seed (1111)

tree.killings <- tree(killed ~ ., data = x.train)

summary(tree.killings)

plot(tree.killings)
text(tree.killings, pretty = 0)

killings.pred <- predict(tree.killings , x.test, type = "class")

tab <- table(killings.pred, y.test)

(error_main_tree_time <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2]))

# Test classification error (training and testing sets)

set.seed (1111)

tree.killings <- tree(killed ~ ., data = x.train)

cv.killings <- cv.tree(tree.killings)

test_error_time <- tibble(
  size = numeric(length(cv.killings$size) - 1),
  test_train = numeric(length(cv.killings$size) - 1),
  test_test = numeric(length(cv.killings$size) - 1)
)

test_error_time <- test_error_time %>%
  mutate(size = cv.killings$size[-length(cv.killings$size)])

for(i in seq_along(cv.killings$size[-length(cv.killings$size)])){
  cv.model.pruned <- prune.misclass(tree.killings, best = cv.killings$size[i])
  killings.pred.train <- predict(cv.model.pruned , x.train, type = "class")
  tab <- table(killings.pred.train, y.train)
  test_error_time[i, 2] <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2])
  
  killings.pred.test <- predict(cv.model.pruned , x.test, type = "class")
  tab <- table(killings.pred.test, y.test)
  test_error_time[i, 3] <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2])
  rm(cv.model.pruned, killings.pred.train, killings.pred.test, tab)
}

test_error_time <- test_error_time %>%
  arrange(size)

# Best model

set.seed (1111)

best.size <- cv.killings$size[which(cv.killings$dev==min(cv.killings$dev))]

cv.model.pruned <- prune.misclass(tree.killings, best=best.size)
summary(cv.model.pruned)

prune.killings.best <- prune.misclass(tree.killings , best = best.size)

killings.pred.best <- predict(prune.killings.best , x.test, type = "class")

tab <- table(killings.pred, y.test)

(error_best_tree_time <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2]))

# Predictions on geography ------------------------------------------------

sample <- panel_farc_killed %>%
  filter(year == 2017)

train <- sample(1:nrow(sample), nrow(sample)/2)

municipalities_train <- sample[train, "M_code"]

x.train <- panel_farc_killed %>%
  filter(M_code %in% as_vector(municipalities_train)) %>%
  select(-(1:5))

y.train <- x.train$killed

x.test <- panel_farc_killed %>%
  filter(!(M_code %in% as_vector(municipalities_train))) %>%
  select(-(1:5))

y.test <-x.test$killed

# Running main classification tree

set.seed (1111)

tree.killings <- tree(killed ~., data = x.train)

summary(tree.killings)

killings.pred <- predict(tree.killings , x.test, type = "class")

tab <- table(killings.pred, y.test)

(error_main_tree_space <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2]))

# Test classification error (training and testing sets)

set.seed (1111)

tree.killings <- tree(killed ~ ., data = x.train)

cv.killings <- cv.tree(tree.killings)

test_error_space <- tibble(
  size = numeric(length(cv.killings$size) - 1),
  test_train = numeric(length(cv.killings$size) - 1),
  test_test = numeric(length(cv.killings$size) - 1)
)

test_error_space <- test_error_space %>%
  mutate(size = cv.killings$size[-length(cv.killings$size)])

for(i in seq_along(cv.killings$size[-length(cv.killings$size)])){
  cv.model.pruned <- prune.misclass(tree.killings, best = cv.killings$size[i])
  killings.pred.train <- predict(cv.model.pruned , x.train, type = "class")
  tab <- table(killings.pred.train, y.train)
  test_error_space[i, 2] <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2])
  
  killings.pred.test <- predict(cv.model.pruned , x.test, type = "class")
  tab <- table(killings.pred.test, y.test)
  test_error_space[i, 3] <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2])
  rm(cv.model.pruned, killings.pred.train, killings.pred.test, tab)
}

test_error_space <- test_error_space %>%
  arrange(size)

# Best model

set.seed (1111)

best.size <- cv.killings$size[which(cv.killings$dev==min(cv.killings$dev))]

cv.model.pruned <- prune.misclass(tree.killings, best=best.size)
summary(cv.model.pruned)

prune.killings.best <- prune.misclass(tree.killings , best = best.size)

killings.pred.best <- predict(prune.killings.best , x.test, type = "class")

tab <- table(killings.pred, y.test)

(error_best_tree_space <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2]))

# Bagging and random forest -----------------------------------------------

# Prediction on time ------------------------------------------------------

x.train <- panel_farc_killed %>%
  mutate(train = as.numeric((year %in% c(2017, 2018, 2019)))) %>%
  select(-(1:5)) %>%
  filter(train == 1) %>%
  select(-train)

y.train <- x.train$killed

x.test <- panel_farc_killed %>%
  mutate(train = as.numeric((year %in% c(2017, 2018, 2019)))) %>%
  select(-(1:5)) %>%
  filter(train == 0) %>%
  select(-train)

y.test <- x.test$killed

# Bagging

set.seed (1111)

test_error_bag_time <- tibble(
  trees = numeric(100),
  test_error = numeric(100),
  test_oob = numeric(100)
)

for(i in 1:100) {
  bag.killing <- randomForest(killed ~., data = x.train, mtry = 41, ntree = i, 
                              na.action = na.omit)
  killings.pred <- predict(bag.killing , x.test, type = "class")
  tab <- table(killings.pred, y.test)
  test_error_bag_time[i, 1] <- i
  test_error_bag_time[i, 2] <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2])
  test_error_bag_time[i, 3] <- mean(bag.killing$err.rate[1:i])
  rm(bag.killing, killings.pred, tab)
}

# Best model

best.size <- test_error_bag_time$trees[which(test_error_bag_time$test_error==min(test_error_bag_time$test_error))]

bag.killing <- randomForest(killed ~., data = x.train, mtry = 41, ntree = best.size, 
                            na.action = na.omit)

killings.pred.best.bag <- predict(bag.killing , x.test, type = "class")

tab <- table(killings.pred.best.bag, y.test)

(error_best_bag_time <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2]))

# Random forest

set.seed (1111)

test_error_random_time <- tibble(
  trees = numeric(100),
  test_error = numeric(100),
  test_oob = numeric(100)
)

for(i in 1:100) {
  rf.killing <- randomForest(killed ~., data = x.train, ntree = i, na.action = na.omit)
  killings.pred <- predict(rf.killing , x.test, type = "class")
  tab <- table(killings.pred, y.test)
  test_error_random_time[i, 1] <- i
  test_error_random_time[i, 2] <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2])
  test_error_random_time[i, 3] <- mean(rf.killing$err.rate[1:i])
  rm(rf.killing, killings.pred, tab)
}

# Best model

best.size <- test_error_random_time$trees[which(test_error_random_time$test_error==min(test_error_random_time$test_error))]

rf.killing <- randomForest(killed ~., data = x.train, ntree = best.size)

killings.pred.best.random <- predict(rf.killing , x.test, type = "class")

tab <- table(killings.pred.best.random, y.test)

(error_best_random_time <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2]))

importance(rf.killing)

varImpPlot(rf.killing)
  
# Predictions on geography ------------------------------------------------

sample <- panel_farc_killed %>%
  filter(year == 2017)

train <- sample(1:nrow(sample), nrow(sample)/2)

municipalities_train <- sample[train, "M_code"]

x.train <- panel_farc_killed %>%
  filter(M_code %in% as_vector(municipalities_train)) %>%
  select(-(1:5))

y.train <- x.train$killed

x.test <- panel_farc_killed %>%
  filter(!(M_code %in% as_vector(municipalities_train))) %>%
  select(-(1:5))

y.test <- x.test$killed

# Bagging

set.seed (1111)

test_error_bag_space <- tibble(
  trees = numeric(100),
  test_error = numeric(100),
  test_oob = numeric(100)
)

for(i in 1:100) {
  bag.killing <- randomForest(killed ~., data = x.train, mtry = 41, ntree = i, 
                              na.action = na.omit)
  killings.pred <- predict(bag.killing , x.test, type = "class")
  tab <- table(killings.pred, y.test)
  test_error_bag_space[i, 1] <- i
  test_error_bag_space[i, 2] <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2])
  test_error_bag_space[i, 3] <- mean(bag.killing$err.rate[1:i])
  rm(bag.killing, killings.pred, tab)
}

# Best model

best.size <- test_error_bag_space$trees[which(test_error_bag_space$test_error==min(test_error_bag_space$test_error))]

bag.killing <- randomForest(killed ~., data = x.train, mtry = 41, ntree = best.size, 
                            na.action = na.omit)

killings.pred.best.bag <- predict(bag.killing , x.test, type = "class")

tab <- table(killings.pred.best.bag, y.test)

(error_best_bag_space <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2]))

# Random forest

set.seed (1111)

test_error_random_space <- tibble(
  trees = numeric(100),
  test_error = numeric(100),
  test_oob = numeric(100)
)

for(i in 1:100) {
  rf.killing <- randomForest(killed ~., data = x.train, ntree = i, na.action = na.omit)
  killings.pred <- predict(rf.killing , x.test, type = "class")
  tab <- table(killings.pred, y.test)
  test_error_random_space[i, 1] <- i
  test_error_random_space[i, 2] <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2])
  test_error_random_space[i, 3] <- mean(rf.killing$err.rate[1:i])
  rm(rf.killing, killings.pred, tab)
}

# Best model

best.size <- test_error_random_space$trees[which(test_error_random_space$test_error==min(test_error_random_space$test_error))]

rf.killing <- randomForest(killed ~., data = x.train, ntree = best.size)

killings.pred.best.random <- predict(rf.killing , x.test, type = "class")

tab <- table(killings.pred.best.random, y.test)

(error_best_random_space <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2]))

varImpPlot(rf.killing)  

# Boosting ----------------------------------------------------------------

# Prediction on time ------------------------------------------------------

panel_farc_killed <- panel_farc_killed %>%
  mutate(killed = as.numeric(killed == "Yes"))

x.train <- panel_farc_killed %>%
  mutate(train = as.numeric((year %in% c(2017, 2018, 2019)))) %>%
  select(-(1:5)) %>%
  filter(train == 1) %>%
  select(-train)

y.train <- x.train$killed

x.test <- panel_farc_killed %>%
  mutate(train = as.numeric((year %in% c(2017, 2018, 2019)))) %>%
  select(-(1:5)) %>%
  filter(train == 0) %>%
  select(-train)

y.test <- x.test$killed

set.seed (1111)

test_error_boost_time <- tibble(
  trees = numeric(3),
  test_error = numeric(3)
)

for(i in 1:3) {
  boost.killing <- gbm(killed ~., data = x.train,
                       distribution = "bernoulli", n.trees = i,
                       interaction.depth = 4)
  killings.pred <- predict(boost.killing , x.test, type = "response", n.trees = i)
  prediction <- as.numeric(killings.pred >= summary(killings.pred)[6])
  tab <- table(prediction, y.test)
  test_error_boost_time[i, 1] <- i
  test_error_boost_time[i, 2] <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2])
  rm(boost.killing, killings.pred, prediction, tab)
}

# Best model

best.size <- test_error_boost_time$trees[which(test_error_boost_time$test_mse==min(test_error_boost_time$test_mse))]

boost.killing <- gbm(killed ~., data = x.train,
                     distribution = "bernoulli", n.trees = 3,
                     interaction.depth = 4)

killings.pred.best.boost <- predict(boost.killing , x.test, type = "response", n.trees = 3)

prediction <- as.numeric(killings.pred.best.boost >= summary(killings.pred.best.boost)[6])
tab <- table(prediction, y.test)

(error_best <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2]))

# Predictions on geography ------------------------------------------------

sample <- panel_farc_killed %>%
  filter(year == 2017)

train <- sample(1:nrow(sample), nrow(sample)/2)

municipalities_train <- sample[train, "M_code"]

x.train <- panel_farc_killed %>%
  filter(M_code %in% as_vector(municipalities_train)) %>%
  select(-(1:5))

y.train <- x.train$killed

x.test <- panel_farc_killed %>%
  filter(!(M_code %in% as_vector(municipalities_train))) %>%
  select(-(1:5))

y.test <- x.test$killed

set.seed (1111)

test_error_boost_space <- tibble(
  trees = numeric(3),
  test_error = numeric(3)
)

for(i in 1:3) {
  boost.killing <- gbm(killed ~., data = x.train,
                       distribution = "bernoulli", n.trees = i,
                       interaction.depth = 4)
  killings.pred <- predict(boost.killing , x.test, type = "response", n.trees = i)
  prediction <- as.numeric(killings.pred >= summary(killings.pred)[6])
  tab <- table(prediction, y.test)
  test_error_boost_space[i, 1] <- i
  test_error_boost_space[i, 2] <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2])
  rm(boost.killing, killings.pred, prediction, tab)
}

# Best model

best.size <- test_error_boost_space$trees[which(test_error_boost_space$test_error==min(test_error_boost_space$test_error))]

boost.killing <- gbm(killed ~., data = x.train,
                     distribution = "bernoulli", n.trees = best.size,
                     interaction.depth = 4)

killings.pred.best.boost <- predict(boost.killing , x.test, type = "response", n.trees = best.size)

prediction <- as.numeric(killings.pred.best.boost >= summary(killings.pred.best.boost)[6])
tab <- table(prediction, y.test)

(error_best <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2]))


# Bayesian Additive Regression Tree ---------------------------------------

# Prediction on time ------------------------------------------------------

panel_farc_killed <- panel_farc_killed %>%
  mutate(killed = as.numeric(killed == "Yes"))

xtrain <- panel_farc_killed %>%
  mutate(train = as.numeric((year %in% c(2017, 2018, 2019)))) %>%
  select(-(1:5)) %>%
  filter(train == 1) %>%
  select(-train)

ytrain <- xtrain$killed

xtrain <- xtrain %>%
  select(-killed)

xtest <- panel_farc_killed %>%
  mutate(train = as.numeric((year %in% c(2017, 2018, 2019)))) %>%
  select(-(1:5)) %>%
  filter(train == 0) %>%
  select(-train)

ytest <- xtest$killed

xtest <- xtest %>%
  select(-killed)

set.seed (1111)

test_error_bart_time <- tibble(
  trees = numeric(3),
  test_error = numeric(3))

for(i in 1:3){
  bartfit <- gbart(x.train = ytrain, y.train = ytrain, x.test = as.matrix(xtest),
                   ntree = i, type = "lbart", ndpost = 1, mc.cores = 2)
  prediction <- as.numeric(bartfit$prob.test.mean >= summary(bartfit$prob.test.mean)[6])
  tab <- table(prediction, ytest)
  test_error_bart_time[i, 1] <- i
  test_error_bart_time[i, 2] <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2])
  rm(bartfit, prediction, tab)
}

# Best model

best.size <- test_error_bart_time$trees[which(test_error_bart_time$test_error==min(test_error_bart_time$test_error))]

bartfit <- gbart(x.train = ytrain, y.train = ytrain, x.test = as.matrix(xtest),
                 ntree = best.size, type = "lbart", ndpost = 1, mc.cores = 2)

prediction <- as.numeric(bartfit$prob.test.mean >= summary(bartfit$prob.test.mean)[6])

tab <- table(prediction, y.test)

(error_best <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2]))

# Predictions on geography ------------------------------------------------

sample <- panel_farc_killed %>%
  filter(year == 2017)

train <- sample(1:nrow(sample), nrow(sample)/2)

municipalities_train <- sample[train, "M_code"]

xtrain <- panel_farc_killed %>%
  filter(M_code %in% as_vector(municipalities_train)) %>%
  select(-(1:5))

ytrain <- xtrain$killed

xtrain <- xtrain %>%
  select(-killed)

xtest <- panel_farc_killed %>%
  filter(!(M_code %in% as_vector(municipalities_train))) %>%
  select(-(1:5))

ytest <- xtest$killed

xtest <- xtest %>%
  select(-killed)

set.seed (1111)

test_error_bart_space <- tibble(
  trees = numeric(3),
  test_error = numeric(3))

for(i in 1:3){
  bartfit <- gbart(x.train = ytrain, y.train = ytrain, x.test = as.matrix(xtest),
                   ntree = i, type = "lbart", ndpost = 1, mc.cores = 2)
  prediction <- as.numeric(bartfit$prob.test.mean >= summary(bartfit$prob.test.mean)[6])
  tab <- table(prediction, ytest)
  test_error_bart_space[i, 1] <- i
  test_error_bart_space[i, 2] <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2])
  rm(bartfit, prediction, tab)
}

# Best model

best.size <- test_error_bart_space$trees[which(test_error_bart_space$test_error==min(test_error_bart_space$test_error))]

bartfit <- gbart(x.train = ytrain, y.train = ytrain, x.test = as.matrix(xtest),
                 ntree = best.size, type = "lbart", ndpost = 1, mc.cores = 2)

prediction <- as.numeric(bartfit$prob.test.mean >= summary(bartfit$prob.test.mean)[6])

tab <- table(prediction, y.test)

(error_best <- (tab[2,1] + tab [1, 2]) / (tab[1,1] + tab[1,2] + tab[2,1] + tab[2,2]))


# Figures -----------------------------------------------------------------

# Classification trees

ggplot(test_error_time) +
  geom_line(aes(x = size, y = test_train, color = "red")) +
  geom_line(aes(x = size, y = test_test, color = "blue")) +
  ylim(0, 0.1) +
  labs(color = 'Errors', x = "Size", y = "Classification error") +
  scale_color_manual(labels = c("Training set", "Testing set"), values = c("red", "blue")) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(test_error_space) +
  geom_line(aes(x = size, y = test_train, color = "red")) +
  geom_line(aes(x = size, y = test_test, color = "blue")) +
  ylim(0, 0.1) +
  labs(color = 'Errors', x = "Size", y = "Classification error") +
  scale_color_manual(labels = c("Training set", "Testing set"), values = c("red", "blue")) +
  theme_bw() +
  theme(legend.position = "bottom")

# Bagging

ggplot(test_error_bag_time) +
  geom_line(aes(x = trees, y = test_error, color = "red")) +
  geom_line(aes(x = trees, y = test_oob, color = "blue")) +
  ylim(0, 0.075) +
  labs(color = 'Errors', x = "Number of trees", y = "Error rate") +
  scale_color_manual(labels = c("Missclassification", "OOB"), values = c("red", "blue")) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(test_error_bag_space) +
  geom_line(aes(x = trees, y = test_error, color = "red")) +
  geom_line(aes(x = trees, y = test_oob, color = "blue")) +
  ylim(0, 0.075) +
  labs(color = 'Errors', x = "Number of trees", y = "Error rate") +
  scale_color_manual(labels = c("Missclassification", "OOB"), values = c("red", "blue")) +
  theme_bw() +
  theme(legend.position = "bottom")

# Random forest

ggplot(test_error_random_time) +
  geom_line(aes(x = trees, y = test_error, color = "red")) +
  geom_line(aes(x = trees, y = test_oob, color = "blue")) +
  ylim(0, 0.075) +
  labs(color = 'Errors', x = "Number of trees", y = "Error rate") +
  scale_color_manual(labels = c("Missclassification", "OOB"), values = c("red", "blue")) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(test_error_random_space) +
  geom_line(aes(x = trees, y = test_error, color = "red")) +
  geom_line(aes(x = trees, y = test_oob, color = "blue")) +
  ylim(0, 0.075) +
  labs(color = 'Errors', x = "Number of trees", y = "Error rate") +
  scale_color_manual(labels = c("Missclassification", "OOB"), values = c("red", "blue")) +
  theme_bw() +
  theme(legend.position = "bottom")

# 


