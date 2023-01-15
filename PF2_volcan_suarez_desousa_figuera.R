
# Librerias ---------------------------------------------------------------
library(tidyverse)    ## Manejo de bases de datos y gráficos
library(readxl)       ## Leer archivos excel
library(tmap)         ## Para los mapas
library(gganimate)    ## Graficos animados
library(gifski)       ## Guardar como GIF
library(ggdark)       ## Temas
library(wesanderson)  ## Temas
library(sandwich)     ## Comandos para regresion
library(broom)        ## Comandos para regresion
library(writexl)      ## Exportar excel
library(ggcorrplot)   ## Mapa de calor
library(tigris)       ## Mapa US
library(sjPlot)       ## Herramientas adicionales
library(haven)
library(data.table)
library(sjlabelled)
library(tigris)       ##Mapa US
library(sf)



# Carga de datos ----------------------------------------------------------
#### Archivo 1. tasa de homicidios ####
# Fuente: Banco Mundial
tasa_homicidio <- read_excel("tasa-homocidio.xls")


#### Archivo 2. datos UNDOC #####
# Sex & Age = Total = Total
data_UNDOC <- read_excel("policy_convicted.xlsx")


#### Archivo 3. Crecimiento #####
## Fuente: Banco Mundial
crec_econ <- read_excel("econ_growth.xls")


#### Archivo 4. Datos del Banco mundial#####
## Fuente: World bank
data_WB1 <- read_excel("data_WB1.xlsx")


#### Archivo 5. Trafico de armas #####
## Trafico de armas
## Fuente: Global initiative against crime
global_oc <- read_excel("global_oc.xlsx")


#### Archivo 6. Governanza#####
## Fuente: Banco Mundial
governance <- read_excel("governance.xlsx")


### Archivo 7. Tasa real crimen #####
#### Rasa real ###
tasa_real <- read_excel("tasa_real.xlsx") %>% 
  dplyr::rename(
    real_homicidio = `Any crime`
  )

# Procesamiento de los datos ----------------------------------------------
#### Archivo 1. Tasa de homocidio ####
tasa_homicidio <- tasa_homicidio %>%
  #Pivoteo los anios
  pivot_longer(as.character(1960:2021),
               values_to = "t_homicidio",
               names_to = "year") %>%
  #Quito columnas innecesarias
  select(!c(`Indicator Name`, `Indicator Code`)) %>%
  #Renombro las dos primeras
  rename(
    country = `Country Name`,
    code = `Country Code`
  ) %>%
  mutate(
    year = as.numeric(year)
  ) 


#### Archivo 2. Datos UNDOC ####
###
data_UNDOC <- data_UNDOC %>%
  #Filtro por los indicadores que se seleccionaron
  filter(
    Category %in% c("Police personel", "Professional Judges or Magistrate")
    ) %>%
  #Quito columnas innecesarias
  select(!c("Sex", "Age", `Unit of measurement`, "Indicator", "Subregion", "Source", "Dimension")) %>%
  #Renombro
  rename(
    code = Iso3_code,
    country = Country,
    continente = Region,
  ) %>%
  #Hago pivot_wider de categoria para que queden como columnas las variables
  pivot_wider(
  names_from = Category,
  values_from = VALUE,
  values_fn = mean
  ) %>%
  #Renombro
  rename(
    year = Year,
    police = `Police personel`,
    jugdes = `Professional Judges or Magistrate`
  ) %>%
  #Corrigo los valores de policia y judges que se divieron entre 2
  mutate(
    police = 2*police,
    jugdes = 2*jugdes,
    year = as.numeric(year)
  )

#### Archivo 3. Crecimiento economico ####
# Idem al archivo 1
crec_econ <- crec_econ %>%
  pivot_longer(as.character(1960:2021),
               values_to = "crecimiento",
               names_to = "year") %>%
  select(!c(`Indicator Name`, `Indicator Code`)) %>%
  rename(
    country = `Country Name`,
    code = `Country Code`
  ) %>%
  mutate(
    year = as.numeric(year)
  )


#### Archivo 4. Datos World Bank ########
data_WB1 <- data_WB1 %>%
  # Filtro por las series relevantes
  filter(`Series Name` %in%
           c(
             "Armed forces personnel (% of total labor force)",
             "Current education expenditure, total (% of total expenditure in public institutions)",
             "Arms imports (SIPRI trend indicator values)",
             "Gini index",
             "Government expenditure on education, total (% of GDP)",
             "Government expenditure on education, total (% of government expenditure)",
             "Population density (people per sq. km of land area)",
             "Population, total",
             "Primary completion rate, total (% of relevant age group)",
             "Unemployment, total (% of total labor force) (national estimate)",
             "Urban population (% of total population)",
             "GDP (current US$)")) %>%
  # Renombro
    rename(
      country = `Country Name`,
      code = `Country Code`,
      indicador = `Series Name`,
      quitar1 = `Series Code`
    ) %>%
  select(!quitar1) %>%
  # Pivoteo
  pivot_longer(!c(country, code, indicador),
               names_to = "year",
               values_to = "valores") %>%
  # Separo la parte de anios que tiene un string
  separate(year,
          c("year", NA)) %>%
  # Pivoteo para que quede una variable en cada columna
    pivot_wider(names_from = indicador,
                values_from = valores) %>%
  # Renombro
    rename(
  personal_militar = "Armed forces personnel (% of total labor force)",
  gasto_etotal = "Current education expenditure, total (% of total expenditure in public institutions)",
  arm_import = "Arms imports (SIPRI trend indicator values)",
  gini = "Gini index",
  gastop_educ = "Government expenditure on education, total (% of GDP)",
  gastop_educpres = "Government expenditure on education, total (% of government expenditure)",
  pop_density = "Population density (people per sq. km of land area)",
  pop = "Population, total",
  deseempleo = "Unemployment, total (% of total labor force) (national estimate)",
  pop_urbana = "Urban population (% of total population)",
  gdp_total = "GDP (current US$)") %>%
  mutate(
    year = as.numeric(year)
  )

#### Archivo 5. Trafico de armas #####
#Data para 2021
global_oc <- global_oc %>%
  #Solo renombro y aniado año
  rename(
    arms_traf = `Arms trafficking`,
    code = `Country Code`
  ) %>% 
  #Me quedo con trafico de armas
  select(code, arms_traf)

#### Archivo 6. Governanza ####
governance <- governance %>%
  #Quito columnas que no me interesan
  select(!`Series Code`) %>%
  #Renombro algunas
  rename(
    country = `Country Name`,
    code = `Country Code`,
    indicador = `Series Name`
  ) %>%
  #Pivoteo
  pivot_longer(
    !c(country, code, indicador),
    names_to = "year",
    values_to = "valores"
  ) %>%
  #Separo los caracteres en year
  separate(year,
           c("year", NA)) %>%
  #Filtro NA's
  filter(country != is.na(country) &
         code != is.na(code) &
         year != is.na(year) &
        indicador != is.na(indicador)) %>%
  #Pivoteo los indicadores para que queden por columna
  pivot_wider(
    names_from = indicador,
    values_from = valores
  ) %>%
  #Renombro
  rename(
    control_corruption = `Control of Corruption: Estimate`,
    effectiveness = `Government Effectiveness: Estimate`,
    regulatory_capacity = `Regulatory Quality: Estimate`,
    rule_of_law = `Rule of Law: Estimate`,
    pol_stability = `Political Stability and Absence of Violence/Terrorism: Estimate`,
    voice_account = `Voice and Accountability: Estimate`
  ) %>%
  mutate(
    year = as.numeric(year)
  )




# Pegado en un mismo dataframe y exportacion-----------------------------------
# Se usa una seguidilla de left_joins para unir todos los dataframes en uno solo
##### Pegado #####
data_proyecto <- left_join(data_WB1,
                           data_UNDOC,
                           by = c("code", "year")) %>%
                left_join(
                  governance,
                  by = c("code", "year")
                ) %>%
                left_join(
                  crec_econ,
                  by = c("code", "year")
                ) %>%
                left_join(
                  tasa_homicidio,
                  by = c("code", "year")
                ) %>%
                left_join(
                  global_oc,
                  by = "code") %>%
                left_join(
                  tasa_real,
                  by = "code") %>% 
  rename(country = country.x) %>% 
  select(!country.y)

data("World")
World <- World %>%
  rename(code = iso_a3)

data_mapa <- left_join(World, data_proyecto, by = "code" ) %>% 
  mutate(
    continent = case_when(continent == "Africa" ~ "Africa",
                          continent == "Asia" ~ "Asia",
                          continent == "Europe" ~ "Europa",
                          continent == "North America" ~ "Norte America",
                          continent == "South America" ~ "Sur America"
      
    )
  ) %>% select(!geometry) %>%
  select(code, name, pop, personal_militar, arm_import, gini, deseempleo, 
         pop_urbana, police, control_corruption, effectiveness, pol_stability,
         regulatory_capacity, rule_of_law, voice_account, crecimiento, t_homicidio,
         arms_traf, real_homicidio, year, continent)

unique(data_mapa$continent)



 # Graficos  ---------------------------------------------------------------
### Preparacion #####
# Carga de colores 
colores <- c("#93032E", "#FFFFFF", "#531253", "#A0ACAD", "#004643", "#0C1618")
colores1 <- c("#93032E", "#531253", "#A0ACAD", "#004643", "#0C1618")
# Nombre de variables elegidas para posterior cambio
elegidos <- c("Importaciones de armas", "Poblacion urbana", "Crecimiento economico", "Desigualdad", "Personal policial",
              "Personal militar", "Trafico de armas", "Control de corrupcion", "
              Efecitividad gubernamental", "Estabilidad politica", "Calidad regulatoria",
              "Efectividad de leyes", "Poder de voz", "Desempleo", "Tasa de crimen oficial")

### Realizacion de los graficos #####

#### 1. Correlograma todas las variables 2010 ####
data_proyecto %>%
  filter(year == 2010) %>% 
  select(c(arm_import, pop_urbana,
           crecimiento, gini, police, personal_militar, arms_traf,
           control_corruption, effectiveness, pol_stability,
           regulatory_capacity, deseempleo, rule_of_law, voice_account, t_homicidio)) %>% 
  setnames(old = colnames(.), new = elegidos) %>% 
  mutate(
    across(everything(), as.numeric),
  ) %>%
  na.omit() %>%
  cor() %>%
  round(digits = 1) %>%
  ggcorrplot(
             outline.col = "white",
             ggtheme = ggplot2::theme_gray,
             colors = colores,
             lab = T) +
  labs(
    title = "Mapa de correlacion",
    subtitle = "Variables que pudiesen afectar a la tasa de homicidio",
    y = "",
    x = "") +
  guides(color=guide_legend(title="Correlación")) +

  theme_minimal(base_size = 12, base_family = "Georgia") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        title = element_text(face = "bold", size = 15),
        plot.subtitle=element_text(size=12, color="black"))

#### 2. Correlograma de crecimiento economico 1997 y tasas #####
data_proyecto %>%
  filter(year == 1997 & t_homicidio <= 40) %>% 
  select(c(
           crecimiento, t_homicidio,
           real_homicidio)) %>% 
  mutate(
    across(everything(), as.numeric),
  ) %>% 
  na.omit() %>% 
  cor() %>%
  round(digits = 1) %>% 
  ggcorrplot(
    outline.col = "white",
    ggtheme = ggplot2::theme_gray,
    colors = colores,
    lab = T) +
  labs(
    title = "Mapa de correlacion",
    subtitle = "Variables que pudiesen afectar a la tasa de homicidio",
    y = "",
    x = "") +
  guides(color=guide_legend(title="Correlación")) +
  
  theme_minimal(base_size = 12, base_family = "Georgia") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        title = element_text(face = "bold", size = 15),
        plot.subtitle=element_text(size=12, color="black"))

#### 2. Tasa de homicidio - mapa #####
tmap_mode("view")
tm_shape(data_mapa) +
  tm_polygons("t_homicidio",
              palette = "Reds",
              title = "Tasa de homicidio por cada 100.000 habitantes",
              frame = F,
              breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 80))

#### 3. Tasa de homicidio real vs oficial ####
data_proyecto %>%
  filter( year == 1997) %>%
  select(c(real_homicidio, t_homicidio)) %>%
  mutate(
    across(everything(), as.numeric),
  ) %>%
  rename(
    `Tasa oficial` = t_homicidio,
    `Tasa real` = real_homicidio,
  ) %>%
  pivot_longer(c(`Tasa oficial`, `Tasa real`),
               names_to = "tipo",
               values_to = "valor"
  ) %>%
  na.omit() %>%
  ggplot(aes( x = tipo, y = valor, fill = tipo,
              color = factor(tipo))) +
  ggdist::stat_halfeye(adjust = 0.5, justification = -.2,
                       .width = 0,
                       point_colour = NA) +
  labs(
    title = "Tasa real de homicidio vs oficial",
    subtitle = "",
    y = "",
    x = "Densidad (%)") +
    theme_minimal(base_size = 12, base_family = "Georgia") +
    theme(legend.position=c(0.7,0.7)) +
    scale_color_manual(values = colores1) +
    scale_fill_manual(values = colores1) +
    guides(fill =guide_legend(title="Tipo de homicidio"),
           color = F) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        title = element_text(face = "bold", size = 15),
        plot.subtitle=element_text(size=12, color="black")) +
    coord_flip()

# Seleccionadas: factores institucionales, policia, crecimiento, gun control, efectividad
data_mapa2 <- data_mapa %>%
  mutate(across(c(rule_of_law,
                 pol_stability,
                 control_corruption,
                 effectiveness,
                 regulatory_capacity,
                 voice_account), as.numeric)) %>%
  mutate(ranking =
           (rule_of_law + pol_stability + control_corruption + effectiveness + regulatory_capacity + voice_account)/5) 


# Mapa ranking
data_mapa2 %>%
  filter(year == 2010) %>%
  tm_shape() +
  tm_polygons("ranking",
              palette = "Blues",
              title = "Ranking",
              frame = F)

#### 5.Numero de policias #####
data_mapa2 %>%
  filter(year == 2010) %>%
  mutate(policia = as.numeric(police),
        pop = as.numeric(pop)) %>%
  mutate(policia = policia/pop*100000) %>%
  tm_shape() +
  tm_polygons("policia",
              palette = "Purples",
              title = "Policias por 100.000 habitantes",
              frame = F,
              breaks = c(0,25,50,100,150,200,300,400,500,700))


#### 7. Personal militar #####
data_mapa2 %>%
  filter(year == 2010) %>%
  mutate(
    personal_militar = as.numeric(personal_militar)
  ) %>%
  tm_shape() +
  tm_polygons("personal_militar",
              palette = "Greens",
              title = "Personal militar (% fuerza laboral)",
              frame = F,
              breaks = c(0, 0.25, 0.5, 1, 1.5, 2, 3, 4, 5, 7.5, 10, 15))

#### 8. Personal militar vs homicidio #####
### General
data_mapa2 %>%
  filter(year == 2010) %>%
  filter(continent != "Seven seas (open ocean)" &
           continent != "Antarctica" ) %>%
  ggplot(aes(x = as.numeric(personal_militar), y = t_homicidio, color = continent)) +
  geom_point() +

  labs(
    title = "Tasa de homicidio oficial vs militares",
    subtitle = "Dispersión y regresión ajustada",
    y = "Tasa de homicidio oficial",
    x = "Militar personal (% fuerza de trabajo)",
  ) +

  theme_bw(base_size = 12, base_family = "Georgia") +
  theme(legend.position=c(0.6,0.5)) +
  guides(color=guide_legend(title="Continente"),
         size=guide_legend(title = "Gini")) +
  scale_color_manual(values = c(colores1)) +
  stat_smooth(method="lm",se=FALSE, color = "black") +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        title = element_text(face = "bold", size = 15),
        plot.subtitle=element_text(size=12, color="black")) +
  ### Por continente
  facet_grid(continent~.) 


#### 9. Gini vs homicidio #####
### General
data_mapa2 %>%
  filter(year == 2010) %>%
  filter(continent != "Seven seas (open ocean)" &
           continent != "Antarctica" ) %>%
  ggplot(aes(x = as.numeric(gini), y = as.numeric(t_homicidio), color = continent)) +
  geom_point() +
  stat_smooth(method="lm",se=FALSE, color = "black") + 

  theme_bw(base_size = 12, base_family = "Georgia") +
  theme(legend.position=c(0.6,0.70)) +
  guides(color=guide_legend(title="Continente"),
         size=guide_legend(title = "Gini")) +
  scale_color_manual(values = c(colores1)) +
  stat_smooth(method="lm",se=FALSE, color = "black") +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        title = element_text(face = "bold", size = 15),
        plot.subtitle=element_text(size=12, color="black")) +
  labs(title = "Tasa de homicidio oficial vs Gini",
       subtitle = "Dispersión y regresión ajustada",
       y = "Tasa de homicidio oficial",
       x = "Coeficiente Gini",
  ) +
  ### Por continente
  facet_grid(continent~.)

#### 10. Policia vs homicidio ######
data_mapa2 %>%
  filter(year == 2010) %>%
  filter(continent != "Seven seas (open ocean)" &
           continent != "Antarctica" ) %>%
  ggplot(aes(x = as.numeric(police), y = t_homicidio, color = continent)) +
  geom_point() +
  
  labs(
    title = "Tasa de homicidio oficial vs policias",
    subtitle = "Dispersión y regresión ajustada",
    y = "Tasa de homicidio oficial",
    x = "Numero de policias",
  ) +
  
  theme_bw(base_size = 12, base_family = "Georgia") +
  theme(legend.position=c(0.6,0.5)) +
  guides(color=guide_legend(title="Continente"),
         size=guide_legend(title = "Gini")) +
  scale_color_manual(values = c(colores1)) +
  stat_smooth(method="lm",se=FALSE, color = "black") +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        title = element_text(face = "bold", size = 15),
        plot.subtitle=element_text(size=12, color="black")) +
  ### Por continente
  facet_grid(continent~.) 

#### 10. Desempleo vs homicidio
data_mapa2 %>%
  filter(year == 2010) %>%
  filter(continent != "Seven seas (open ocean)" &
           continent != "Antarctica" ) %>%
  ggplot(aes(x = as.numeric(deseempleo), y = t_homicidio, color = continent)) +
  geom_point() +
  
  labs(
    title = "Tasa de homicidio oficial vs policias",
    subtitle = "Dispersión y regresión ajustada",
    y = "Tasa de homicidio oficial",
    x = "Numero de policias",
  ) +
  
  theme_bw(base_size = 12, base_family = "Georgia") +
  theme(legend.position=c(0.6,0.2)) +
  guides(color=guide_legend(title="Continente"),
         size=guide_legend(title = "Gini")) +
  scale_color_manual(values = c(colores1)) +
  stat_smooth(method="lm",se=FALSE, color = "black") +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        title = element_text(face = "bold", size = 15),
        plot.subtitle=element_text(size=12, color="black")) +
  ### Por continente
  facet_grid(continent~.) 


#### 11. Gobernanza vs crimen #####

data_mapa2 %>%
  filter(year == 2010) %>%
  filter(continent != "Seven seas (open ocean)" &
           continent != "Antarctica" ) %>%
  ggplot(aes(x = ranking, y = t_homicidio, color = continent)) +
  geom_point() +
  
  labs(
    title = "Tasa de homicidio oficial vs gobernanza",
    subtitle = "Ranking de gobernanza",
    y = "Tasa de homicidio oficial",
    x = "Ranking de gobernanza",
  ) +
  
  theme_bw(base_size = 12, base_family = "Georgia") +
  theme(legend.position=c(0.2,0.8)) +
  guides(color=guide_legend(title="Continente"),
         size=guide_legend(title = "Gini")) +
  scale_color_manual(values = c(colores1)) +
  stat_smooth(method="lm",se=FALSE, color = "black") +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        title = element_text(face = "bold", size = 15),
        plot.subtitle=element_text(size=12, color="black")) +
  ### Por continente
  facet_grid(continent~.) 


# Caso especial -----------------------------------------------------------

crimen <- read_excel('crimen_violento.xlsx', col_names = c("Estado", 1979:2020))
estados <- crimen$Estado[match('Alabama', crimen$Estado):match('Wyoming', crimen$Estado)]

crimen_limpio <- filter(crimen, Estado %in% estados)

# Leer base de datos y procesar el censo de poblacion (limpiar, y anualizar los datos)

poblacion <- read_excel('population_census.xlsx')

impares <- seq(2, ncol(poblacion), 2)
anos <- 1979:2020

poblacion_limpio_decada <- poblacion %>%
  select(!c(`This cell is intentionally blank....10`)) %>%
  select(c(1,impares)) %>%
  .[,1:7] %>%
  filter(`U.S. Department of Commerce` %in% estados) %>%
  set_names(c('Estado', seq(2020, 1970, -10)))

poblacion_limpio_decada[,c(2:7)] <- apply(poblacion_limpio_decada[ , c(2:7)], 2,
                                          function(x) as.numeric(as.character(x)))

poblacion_limpio_anual <- poblacion_limpio_decada %>%
  cbind('1980' = replicate(9,poblacion_limpio_decada$`1980`)) %>%
  cbind('1990' = replicate(9,poblacion_limpio_decada$`1990`)) %>%
  cbind('2000' = replicate(9,poblacion_limpio_decada$`2000`)) %>%
  cbind('2010' = replicate(9,poblacion_limpio_decada$`2010`)) %>%
  .[,order(colnames(.),decreasing=FALSE)] %>%
  set_names(c(anos, 'Estado')) %>%
  .[,c(43,1:42)]

# Crear data frame con la tasa de crimen en funcion de la poblacion y los crimenes reportados

tasa_crimen <- poblacion_limpio_anual

for (a in 2:ncol(poblacion_limpio_anual)) {tasa_crimen[a] <- crimen_limpio[a] / poblacion_limpio_anual[a] * 100000

}

# Crear data frame de la variacion de la tasa de crimen respecto a la base 1985

var_crimen_1985 <- tasa_crimen

for (a in 2:ncol(poblacion_limpio_anual)) {var_crimen_1985[a] <- (tasa_crimen[a] - tasa_crimen$`1985`) / tasa_crimen$`1985`

}

# Crear variables para

anos_criminales <- 9:50
anos_relevantes <- 1979:2002

estados_legalizadores_1970 <- c("Alaska","New York","California","Washington","Hawaii")
estados_legalizadores_1973 <- estados[!estados %in% estados_legalizadores_1970]

legalizadores_1970 <- var_crimen_1985 %>%
  filter(Estado %in% estados_legalizadores_1970) %>%
  select(!Estado) %>% colMeans()
legalizadores_1973 <- var_crimen_1985 %>%
  filter(Estado %in% estados_legalizadores_1973) %>%
  select(!Estado) %>% colMeans()

avg <- as.data.frame(bind_cols(anos, anos_criminales, legalizadores_1970, legalizadores_1973))
colnames(avg) <- c('Ano','Anos desde 1970','avg var legalizadores 1970','avg var legalizadores 1973')

labels <- data.frame(tipo = c('Primeros en legalizar (1970)', 'Resto de EEUU (1973)'),
                     x = c(23, 17), y = c(-0.05, 0.3))
ilustracion <- data.frame(ano = c('1995','1998'), x = c(25, 28), y = c(0.147462801, 0.147462801))


grafico1 <- avg %>%
  filter(`Ano` %in% anos_relevantes) %>%
  ggplot(aes(x = `Anos desde 1970`)) +
  geom_line(aes(y = `avg var legalizadores 1973`, color = "#9593B4"), linewidth = 1.2) +
  geom_line(aes(y = `avg var legalizadores 1970`, color = "#93032E"), linewidth = 1.2) +
  geom_text(data = labels, aes(x, y, label = tipo, color = c("#93032E","#9593B4")), size = 4, fontface = "bold") +
  geom_point(data = ilustracion, aes(x, y), size = 3, alpha = 0.7) +
  geom_line(data = ilustracion, aes(x, y), linetype = 2, alpha = 0.7) +
  annotate('text', x = 25, y = 0.177462801, label = '1995') +
  annotate('text', x = 28, y = 0.117462801, label = '1998') +
  theme_bw() + 
  labs(title = "Crimen: Estados que legalizaron tempranamente vs. el resto") +
  ylab ('Variacion promedio del crimen (base 1985)') +
  xlab ('Anos desde 1970') +
  theme(legend.position = "none", plot.title = element_text(size = rel(1)),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.border = element_rect(fill = NA)) +
  scale_color_manual(values = colores1) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1, face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        title = element_text(face = "bold", size = 12),
        plot.subtitle=element_text(size=12, color="black"))

grafico1

write_xlsx(avg,"/Users/alevolcan/Desktop/Archivos_data\\crimen_average.xlsx")

### Mapa US

US <- tigris::states(class = "sf")
US <- US %>% 
  mutate(
    early = ifelse(
      NAME %in% estados_legalizadores_1973, yes = "Resto EEUU", no = "Anticipado"
    )
  )
 
tm_shape(US) +
  tm_polygons("early",
              style = "cat",
              palette = "Purples",
              title = "Numero de policias",
              frame = F) +
  tm_text("NAME", size = 1) 


# Exportacion de bases ----------------------------------------------------

data_set <- list(data_proyecto, avg)

write_xlsx(data_set,"/Users/alevolcan/Desktop/data_proyecto.xlsx")
