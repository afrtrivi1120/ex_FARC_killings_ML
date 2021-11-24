pacman::p_load(tidyverse, haven, readxl, stargazer, fixest)


# Conflict measures (FARC) ------------------------------------------------

farc_presence <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Conflict/Database_URosario_1996_2019.dta") %>%
  filter(year == 2011 | year == 2012) %>%
  mutate(atfarc = farcd*atgue) %>%
  group_by(M_code) %>%
  count(wt = atfarc, name = "atfarc")

farc_presence <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Panel_municipios/PANEL_CARACTERISTICAS_GENERALES(2019).dta") %>%
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


# Conflict measures (ELN and paramilitary groups) -------------------------

eln_other_presence <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Conflict/Database_URosario_1996_2019.dta") %>%
  filter(year > 2010 & year < 2015) %>%
  mutate(ateln = elnd*atgue) %>%
  group_by(M_code) %>%
  summarize(ateln = sum(ateln),
            atpar = sum(atpar))

eln_other_presence <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Panel_municipios/PANEL_CARACTERISTICAS_GENERALES(2019).dta") %>%
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


# Disputed areas ----------------------------------------------------------

disputed_groups <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Conflict/Database_URosario_1996_2019.dta") %>%
  filter(year > 2016 & year < 2020) %>%
  mutate(atfarc = farcd*atgue,
         ateln = elnd*atgue) %>%
  group_by(M_code) %>%
  summarize(atfarc = sum(atfarc),
            ateln = sum(ateln),
            atpar = sum(atpar),
            disputed = atfarc + ateln + atpar)
  
disputed_groups <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Panel_municipios/PANEL_CARACTERISTICAS_GENERALES(2019).dta") %>%
  filter(ano == 2012) %>%
  select(codmpio, pobl_tot) %>%
  rename(M_code = codmpio) %>%
  filter(!(M_code == 88001 | M_code == 88564)) %>%
  left_join(disputed_groups, by = "M_code") %>%
  mutate(atfarc = ifelse(is.na(atfarc), 0, atfarc),
         ateln = ifelse(is.na(ateln), 0, ateln),
         atpar = ifelse(is.na(atpar), 0, atpar),
         disputed = ifelse(is.na(disputed), 0, disputed),
         atfarc = as.numeric((atfarc > 0)),
         ateln = as.numeric((ateln > 0)),
         atpar = as.numeric((atpar > 0)),
         disputed = as.numeric((disputed > 0))) %>%
  filter(is.na(M_code) == F) %>%
  select(M_code, atfarc, ateln, atpar, disputed) 


# Ex-FARC combatants killed -----------------------------------------------

exfarc_kill <- read_xlsx("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Ex-FARC/DATABASEFINAL.xlsx",
                        sheet = "General Data", n_max = 254) %>%
  mutate(ID = paste0(`Department Name`, `Municipality Name`),
         ID = str_replace_all(ID, "[.]", ""),
         ID = str_replace_all(ID, "[,]", ""),
         ID = str_replace_all(ID, "[ ]", ""),
         killed_exfarc = 1) %>%
  select(ID, killed_exfarc)

exfarc_kill <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Panel_municipios/PANEL_CARACTERISTICAS_GENERALES(2019).dta") %>%
  filter(ano == 2012, !(codmpio == 88001 | codmpio == 88564)) %>%
  mutate(depto_id = str_to_upper(depto),
         mun_id = str_to_upper(municipio),
         ID = paste0(depto_id, mun_id),
         ID = str_replace_all(ID, "[.]", ""),
         ID = str_replace_all(ID, "[,]", ""),
         ID = str_replace_all(ID, "[ ]", "")) %>%
  select(codmpio, ID) %>%
  rename(M_code = codmpio) %>%
  right_join(exfarc_kill, by = "ID") %>%
  mutate(M_code = ifelse(ID == "ANTIOQUIACACERES", 5120, M_code),
         M_code = ifelse(ID == "CAQUETÁPUERTORICO(CAQUETÁ)", 18592, M_code),
         M_code = ifelse(ID == "CASANARENUNCHIA", 85225, M_code),
         M_code = ifelse(ID == "CAUCACALOTO", 19142, M_code),
         M_code = ifelse(ID == "CAUCATORIBÍO", 19821, M_code),
         M_code = ifelse(ID == "CHOCÓCARMENDELDARIÉN", 27150, M_code),
         M_code = ifelse(ID == "CHOCÓRIOSUCIO", 27615, M_code),
         M_code = ifelse(ID == "METAPUERTORICO(META)", 50590, M_code),
         M_code = ifelse(ID == "METAVILLAVICIENCIO", 50001, M_code),
         M_code = ifelse(ID == "NARIÑOBARBACOAS", 52079, M_code),
         M_code = ifelse(ID == "NARIÑOCUMBAL", 52227, M_code),
         M_code = ifelse(ID == "NARIÑOELCHARCO", 52250, M_code),
         M_code = ifelse(ID == "NARIÑOIPIALES", 52356, M_code),
         M_code = ifelse(ID == "NARIÑOLEIVA", 52405, M_code),
         M_code = ifelse(ID == "NARIÑOMAGÜÍ", 52427, M_code),
         M_code = ifelse(ID == "NARIÑORICAURTE", 52612, M_code),
         M_code = ifelse(ID == "NARIÑOSAMANIEGO", 52678, M_code),
         M_code = ifelse(ID == "NARIÑOSANANDRÉSDETUMACO", 52835, M_code),
         M_code = ifelse(ID == "NARIÑOSANTABÁRBARA", 52696, M_code),
         M_code = ifelse(ID == "NARIÑOSANTACRUZ", 52699, M_code),
         M_code = ifelse(ID == "VALLEDELCAUCABUGA", 76111, M_code)) %>%
  filter(is.na(M_code) == F) %>%
  group_by(M_code) %>%
  count(wt = killed_exfarc, name = "killed_exfarc")

exfarc_kill <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Panel_municipios/PANEL_CARACTERISTICAS_GENERALES(2019).dta") %>%
  filter(ano == 2014, !(codmpio == 88001 | codmpio == 88564)) %>%
  rename(M_code = codmpio) %>%
  left_join(exfarc_kill, by = "M_code") %>%
  mutate(killed_exfarc = ifelse(is.na(killed_exfarc), 0, killed_exfarc),
         killed_exfarc_pc = (killed_exfarc / pobl_tot) * 10000,
         killed_exfarc_d = as.numeric((killed_exfarc > 0))) %>%
  select(M_code, killed_exfarc, killed_exfarc_pc, killed_exfarc_d)


# Livestock ---------------------------------------------------------------

livestock <- read_xlsx("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Ganado/BOVINOS-CENSO-2020.xlsx",
                    sheet = "BOVINOS Y PREDIOS", range = "A5:Q1128") %>%
  select(`CODIGO MUNICIPIO`, `TOTAL BOVINOS - 2019`) %>%
  mutate(`CODIGO MUNICIPIO` = as.numeric(`CODIGO MUNICIPIO`)) %>%
  rename(M_code = `CODIGO MUNICIPIO`,
         livestock_tot = `TOTAL BOVINOS - 2019`) %>%
  filter(is.na(M_code) == F, !(M_code == 88001 | M_code == 88564)) %>%
  mutate(livestock_p50 = as.numeric((livestock_tot > median(livestock_tot[livestock_tot>0])))) %>%
  select(M_code, livestock_p50)


# Homicides ---------------------------------------------------------------

homicides_panel <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Panel_municipios/PANEL_CONFLICTO_Y_VIOLENCIA(2019).dta") %>%
  filter(ano == 2016, !(codmpio == 88001 | codmpio == 88564)) %>%
  rename(M_code = codmpio) %>%
  select(M_code, homicidios)

homicides_panel <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Panel_municipios/PANEL_CARACTERISTICAS_GENERALES(2019).dta") %>%
  filter(ano == 2016, !(codmpio == 88001 | codmpio == 88564)) %>%
  rename(M_code = codmpio) %>%
  select(M_code, pobl_tot) %>%
  inner_join(homicides_panel, by = "M_code") %>%
  mutate(homicidios = (homicidios / pobl_tot) * 10000) %>%
  select(M_code, homicidios)


# Assembling dataset ------------------------------------------------------

panel_crosssection <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Raw/Panel_municipios/PANEL_CARACTERISTICAS_GENERALES(2019).dta") %>%
  filter(ano == 2015, !(codmpio == 88001 | codmpio == 88564)) %>%
  select(codmpio, coddepto, areaoficialhm2, pobl_tot, indrural,
         areaoficialkm2, altura, discapital, disbogota) %>%
  mutate(pop = (pobl_tot < 100000)) %>%
  rename(M_code = codmpio) %>%
  mutate(pobl_tot = pobl_tot + 0.01,
         pobl_tot = log(pobl_tot))

panel_crosssection <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Auxiliar/panel_prillegal.dta") %>%
  filter(year == 2012, !(M_code == 88001 | M_code == 88564)) %>%
  mutate(share_ille = (areaprmined_adj_sqkm/areamuni_sqkm) * 100,
         share_ille_p50 = as.numeric((share_ille > median(share_ille[share_ille>0]))),
         share_ille_d = as.numeric(share_ille > 0)) %>%
  select(M_code, share_ille, share_ille_p50, share_ille_d) %>%
  right_join(panel_crosssection, by = "M_code")

panel_crosssection <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Auxiliar/coca.dta") %>%
  filter(year == 2016) %>%
  select(M_code, H_coca, coca) %>%
  right_join(panel_crosssection, by = "M_code") %>%
  mutate(sh_coca = (H_coca / areaoficialhm2) * 100,
         coca_p50 = as.numeric((sh_coca > median(sh_coca[sh_coca>0])))) %>%
  select(-c(H_coca, areaoficialhm2))

panel_crosssection <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Auxiliar/complete_sample.dta") %>%
  filter(year == 2011) %>%
  rename(M_code = muni_code) %>%
  mutate(candidate = as.numeric(vote_share > 0)) %>%
  filter(candidate == 1) %>%
  select(M_code, left_mayor, right_mayor, other_mayor) %>%
  right_join(panel_crosssection, by = "M_code")

panel_crosssection <- read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Auxiliar/suitability.dta") %>%
  mutate(d_suitability = as.numeric((suitability > 1))) %>%
  select(M_code, d_suitability) %>%
  right_join(panel_crosssection, by = "M_code")

panel_crosssection <- panel_crosssection %>%
  inner_join(livestock, by = "M_code") %>%
  inner_join(farc_presence, by = "M_code") %>%
  inner_join(eln_other_presence, by = "M_code") %>%
  inner_join(disputed_groups, by = "M_code") %>%
  inner_join(exfarc_kill, by = "M_code") %>%
  inner_join(homicides_panel, by = "M_code") %>%
  inner_join(read_dta("/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Data/Auxiliar/peace_mun.dta"),
             by = "M_code")


# Regression --------------------------------------------------------------


# Base model --------------------------------------------------------------

reg_1 <- feols(killed_exfarc ~ farc + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")
  
reg_3 <- feols(killed_exfarc_d ~ farc + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory",
       keep = "%farc",
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc = "FARC",
              coddepto = "Department"),
          digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c("1,058", "1,058", "1,058")),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T1.tex",
       replace = TRUE)
  

# Heterogeneous effects model ---------------------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_share_ille = share_ille*farc)

reg_1 <- feols(killed_exfarc ~ farc_share_ille + farc + share_ille + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_share_ille + farc + share_ille + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_share_ille + farc + share_ille + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (illegal mining)",
       keep = c("%farc_share_ille", "%farc", "%share_ille"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_share_ille = "FARC $\\times$ Illegal mining",
              farc = "FARC",
              share_ille = "Illegal mining",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c("1,029", "1,029", "1,029")),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T2.tex",
       replace = TRUE)


# Coca cultivation --------------------------------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_coca = coca*farc)
  
reg_1 <- feols(killed_exfarc ~ farc_coca + farc + coca + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_coca + farc + coca + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_coca + farc + coca + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (coca crops)",
       keep = c("%farc_coca", "%farc", "%coca"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_coca = "FARC $\\times$ Coca",
              farc = "FARC",
              coca = "Coca",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c("1,058", "1,058", "1,058")),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T3.tex",
       replace = TRUE)


# Coca suitability --------------------------------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_suitability = d_suitability*farc)

reg_1 <- feols(killed_exfarc ~ farc_suitability + farc + d_suitability + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_suitability + farc + d_suitability + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_suitability + farc + d_suitability + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (coca suitability)",
       keep = c("%farc_suitability", "%farc", "%d_suitability"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_suitability = "FARC $\\times$ Coca suitability",
              farc = "FARC",
              d_suitability = "Coca suitability",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c(reg_1$nobs, reg_2$nobs, reg_3$nobs)),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T4.tex",
       replace = TRUE)


# Livestock ---------------------------------------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_livestock = livestock_p50*farc)

reg_1 <- feols(killed_exfarc ~ farc_livestock + farc + livestock_p50 + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_livestock + farc + livestock_p50 + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_livestock + farc + livestock_p50 + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (livestock)",
       keep = c("%farc_livestock", "%farc", "%livestock_p50"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_livestock = "FARC $\\times$ Livestock",
              farc = "FARC",
              livestock_p50 = "Livestock",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c(reg_1$nobs, reg_2$nobs, reg_3$nobs)),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T5.tex",
       replace = TRUE)


# Disputed territory ------------------------------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_disputed = disputed*farc)

reg_1 <- feols(killed_exfarc ~ farc_disputed + farc + disputed + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_disputed + farc + disputed + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_disputed + farc + disputed + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (disputed territory)",
       keep = c("%farc_disputed", "%farc", "%disputed"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_disputed = "FARC $\\times$ Disputed",
              farc = "FARC",
              disputed = "Disputed",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c(reg_1$nobs, reg_2$nobs, reg_3$nobs)),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T6.tex",
       replace = TRUE)


# Disputed territory by FARC ----------------------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_disputed = atfarc*farc)

reg_1 <- feols(killed_exfarc ~ farc_disputed + farc + atfarc + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_disputed + farc + atfarc + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_disputed + farc + atfarc + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (disputed territory by FARC)",
       keep = c("%farc_disputed", "%farc", "%atfarc"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_disputed = "FARC $\\times$ Disputed by FARC",
              farc = "FARC",
              atfarc = "Disputed by FARC",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c(reg_1$nobs, reg_2$nobs, reg_3$nobs)),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T7.tex",
       replace = TRUE)


# Disputed territory by ELN -----------------------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_disputed = ateln*farc)

reg_1 <- feols(killed_exfarc ~ farc_disputed + farc + ateln + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_disputed + farc + ateln + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_disputed + farc + ateln + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (disputed territory by ELN)",
       keep = c("%farc_disputed", "%farc", "%ateln"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_disputed = "FARC $\\times$ Disputed by ELN",
              farc = "FARC",
              ateln = "Disputed by ELN",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c(reg_1$nobs, reg_2$nobs, reg_3$nobs)),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T8.tex",
       replace = TRUE)


# Disputed territory by paramilitary groups -------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_disputed = atpar*farc)

reg_1 <- feols(killed_exfarc ~ farc_disputed + farc + atpar + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_disputed + farc + atpar + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_disputed + farc + atpar + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (disputed territory by paramilitary groups)",
       keep = c("%farc_disputed", "%farc", "%atpar"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_disputed = "FARC $\\times$ Disputed by paramilitary",
              farc = "FARC",
              atpar = "Disputed by paramilitary",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c(reg_1$nobs, reg_2$nobs, reg_3$nobs)),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T9.tex",
       replace = TRUE)


# Left-wing mayor ---------------------------------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_left_mayor = left_mayor*farc)

reg_1 <- feols(killed_exfarc ~ farc_left_mayor + farc + left_mayor + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_left_mayor + farc + left_mayor + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_left_mayor + farc + left_mayor + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (left-wing mayor)",
       keep = c("%farc_left_mayor", "%farc", "%left_mayor"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_left_mayor = "FARC $\\times$ Left-wing mayor",
              farc = "FARC",
              left_mayor = "Left-wing mayor",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c(reg_1$nobs, reg_2$nobs, reg_3$nobs)),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T10.tex",
       replace = TRUE)


# Right-wing mayor --------------------------------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_right_mayor = right_mayor*farc)

reg_1 <- feols(killed_exfarc ~ farc_right_mayor + farc + right_mayor + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_right_mayor + farc + right_mayor + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_right_mayor + farc + right_mayor + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (right-wing mayor)",
       keep = c("%farc_right_mayor", "%farc", "%right_mayor"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_right_mayor = "FARC $\\times$ Right-wing mayor",
              farc = "FARC",
              right_mayor = "Right-wing mayor",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c(reg_1$nobs, reg_2$nobs, reg_3$nobs)),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T11.tex",
       replace = TRUE)


# Neither left or right ---------------------------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_other_mayor = other_mayor*farc)

reg_1 <- feols(killed_exfarc ~ farc_other_mayor + farc + other_mayor + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_other_mayor + farc + other_mayor + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_other_mayor + farc + other_mayor + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (other mayor)",
       keep = c("%farc_other_mayor", "%farc", "%other_mayor"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_other_mayor = "FARC $\\times$ Other mayor",
              farc = "FARC",
              other_mayor = "Other mayor",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c(reg_1$nobs, reg_2$nobs, reg_3$nobs)),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T12.tex",
       replace = TRUE)


# Homicide rate -----------------------------------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_homicides = homicidios*farc)

reg_1 <- feols(killed_exfarc ~ farc_homicides + farc + homicidios + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_homicides + farc + homicidios + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_homicides + farc + homicidios + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (homicides)",
       keep = c("%farc_homicides", "%farc", "%homicidios"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_homicides = "FARC $\\times$ Homicides",
              farc = "FARC",
              homicidios = "Homicides",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c(reg_1$nobs, reg_2$nobs, reg_3$nobs)),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T13.tex",
       replace = TRUE)


# PDET --------------------------------------------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_pdet = pdet*farc)

reg_1 <- feols(killed_exfarc ~ farc_pdet + farc + pdet + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_pdet + farc + pdet + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_pdet + farc + pdet + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (PDET municipalities)",
       keep = c("%farc_pdet", "%farc", "%pdet"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_pdet = "FARC $\\times$ PDET",
              farc = "FARC",
              pdet = "PDET",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c(reg_1$nobs, reg_2$nobs, reg_3$nobs)),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T14.tex",
       replace = TRUE)


# PNIS --------------------------------------------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_pnis = pnis*farc)

reg_1 <- feols(killed_exfarc ~ farc_pnis + farc + pnis + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_pnis + farc + pnis + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_pnis + farc + pnis + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (PNIS municipalities)",
       keep = c("%farc_pnis", "%farc", "%pnis"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_pnis = "FARC $\\times$ PNIS",
              farc = "FARC",
              pnis = "PNIS",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c(reg_1$nobs, reg_2$nobs, reg_3$nobs)),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T15.tex",
       replace = TRUE)


# ZOMAC -------------------------------------------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_zomac = zomac*farc)

reg_1 <- feols(killed_exfarc ~ farc_zomac + farc + zomac + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_zomac + farc + zomac + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_zomac + farc + zomac + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (ZOMAC municipalities)",
       keep = c("%farc_zomac", "%farc", "%zomac"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_zomac = "FARC $\\times$ ZOMAC",
              farc = "FARC",
              zomac = "ZOMAC",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c(reg_1$nobs, reg_2$nobs, reg_3$nobs)),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T16.tex",
       replace = TRUE)


# ETCR --------------------------------------------------------------------

panel_crosssection <- panel_crosssection %>%
  mutate(farc_etcr = etcr*farc)

reg_1 <- feols(killed_exfarc ~ farc_etcr + farc + etcr + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_2 <- feols(killed_exfarc_pc ~ farc_etcr + farc + etcr + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

reg_3 <- feols(killed_exfarc_d ~ farc_etcr + farc + etcr + pobl_tot + indrural + areaoficialkm2 + altura + discapital + disbogota,
               panel_crosssection, subset = (panel_crosssection$pop == TRUE), cluster = "coddepto",
               se = "cluster", panel.id = ~M_code, fixef = "coddepto")

esttex(reg_1, reg_2, reg_3, title="Ex-FARC fighters have been killed in FARC-controlled territory (ETCR municipalities)",
       keep = c("%farc_etcr", "%farc", "%etcr"),
       style.tex = style.tex("aer"),
       fixef.group=list("Department FE"=""),
       dict=c(killed_exfarc = "Total",
              killed_exfarc_pc = "Per capita",
              killed_exfarc_d = "Dummy",
              farc_etcr = "FARC $\\times$ ETCR",
              farc = "FARC",
              etcr = "ETCR",
              coddepto = "Department"),
       digits = 3, digits.stats = 3, fitstat = c("my", "ar2", "n"),
       group = list("-Controls" = c("pobl_tot", "indrural", "areaoficialkm2", "altura", "discapital", "disbogota")),
       extralines = list("_Municipalities" = c(reg_1$nobs, reg_2$nobs, reg_3$nobs)),
       file = "/Users/piperiveratrivino/Dropbox/URosario/CP_work/Ex-FARC/ex_farc/Output/T17.tex",
       replace = TRUE)


