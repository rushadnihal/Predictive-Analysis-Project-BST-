########################################
# Section 1: Research Data Preparation
########################################

library(dplyr)
library(ggplot2)
library(scales)

# Read and select relevant columns
AfricaDataset <- read.csv("RESEARCH_AFRO_Rushad.csv", header = TRUE) %>%
  select(
    PublicationCountry = countryName,
    PublicationYear = Yearyearofpublicationifon,
    PublicationTitle = Articletitle,
    PublicationStudyDesign = studyDesign,
    PublicationStudyType = qualitative,
    incomeGroup
  )

# Keep only the six target countries and arrange by country and year
target_countries <- c("South Africa", "Nigeria", "Ethiopia", "Ghana", "Kenya", "Cameroon")

AfricaDataset <- AfricaDataset %>%
  filter(PublicationCountry %in% target_countries) %>%
  select(-incomeGroup) %>%
  arrange(PublicationCountry, PublicationYear) %>%
  group_by(PublicationCountry) %>%
  mutate(
    PublicationCount = row_number(),
    CumulativePublications = cumsum(rep(1, n()))
  ) %>%
  ungroup()

# Quick structure and preview
str(AfricaDataset)
head(AfricaDataset)

#View(AfricaDataset)
# Visualization: Cumulative publications over time
ggplot(AfricaDataset, aes(
  x = PublicationYear, y = CumulativePublications,
  color = PublicationCountry, group = PublicationCountry
)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  scale_color_manual(values = c(
    "Cameroon" = "orange", "Ethiopia" = "lightgreen", "Ghana" = "red",
    "Kenya" = "skyblue", "Nigeria" = "lightpink", "South Africa" = "purple"
  )) +
  scale_x_continuous(breaks = seq(1960, 2025, by = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Cumulative Publications Over Time by Country",
    subtitle = "Tracking growth of physical activity research output in six African countries",
    x = "Year of Publication",
    y = "Cumulative Number of Publications",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted", color = "grey85"),
    panel.grid.major.y = element_line(linetype = "dotted", color = "grey90"),
    axis.title = element_text(face = "bold")
  )

########################################
# Section 2: GDP Data Integration and Analysis
########################################

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(broom)

# Read the GDP dataset
gdp_data <- read_csv("Africa_GDP.csv", show_col_types = FALSE)

# Reshape from wide to long (Year | Country | GDP)
gdp_long <- gdp_data %>%
  pivot_longer(
    cols = -Year,
    names_to = "PublicationCountry",
    values_to = "GDP"
  ) %>%
  mutate(
    PublicationYear = as.integer(Year),
    GDP = as.numeric(GDP)
  ) %>%
  select(PublicationCountry, PublicationYear, GDP)

# Keep only the six target countries
target_countries <- c("South Africa", "Nigeria", "Ethiopia", "Ghana", "Kenya", "Cameroon")
gdp_long <- gdp_long %>% filter(PublicationCountry %in% target_countries)

# Merge with your cleaned AfricaDataset
AfricaDataset <- AfricaDataset %>%
  left_join(gdp_long, by = c("PublicationCountry", "PublicationYear"))

# Check structure and missing values
str(AfricaDataset)
summary(AfricaDataset$GDP)

# Optional: check any missing GDP entries by country/year
AfricaDataset %>%
  filter(is.na(GDP)) %>%
  count(PublicationCountry, PublicationYear, name = "Missing_GDP") %>%
  arrange(PublicationCountry, PublicationYear)

# Visualization: GDP growth over time
ggplot(AfricaDataset, aes(
  x = PublicationYear, 
  y = GDP, 
  color = PublicationCountry, 
  group = PublicationCountry
)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "GDP Growth Over Time (Six African Countries)",
    x = "Year",
    y = "GDP (constant US$)",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_si("B"))
  )

# Visualization: Economic and Research Output Growth
df_scaled <- AfricaDataset %>%
  group_by(PublicationCountry) %>%
  mutate(Pub_scaled = CumulativePublications * (max(GDP, na.rm = TRUE) / max(CumulativePublications, na.rm = TRUE)))

ggplot(df_scaled, aes(x = PublicationYear, group = PublicationCountry)) +
  geom_line(aes(y = GDP, color = "GDP"), size = 1.2) +
  geom_line(aes(y = Pub_scaled, color = "Publications"), size = 1.2, linetype = "dashed") +
  facet_wrap(~ PublicationCountry, scales = "free_y") +
  scale_y_continuous(
    name = "GDP (constant US$)",
    labels = label_number(scale_cut = cut_si("B")),
    sec.axis = sec_axis(~ . / (max(df_scaled$GDP, na.rm = TRUE) / max(df_scaled$CumulativePublications, na.rm = TRUE)),
                        name = "Cumulative Publications")
  ) +
  scale_color_manual(values = c("GDP" = "#1f77b4", "Publications" = "#ff7f0e")) +
  labs(
    title = "Economic and Research Output Growth by Country",
    x = "Year", 
    color = "Metric"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Correlation between GDP and research output per country
gdp_pub_corr <- AfricaDataset %>%
  group_by(PublicationCountry) %>%
  summarize(
    correlation = cor(GDP, CumulativePublications, use = "complete.obs", method = "pearson"),
    n_obs = n()
  ) %>%
  arrange(desc(correlation))

print(gdp_pub_corr)

ggplot(gdp_pub_corr, aes(x = reorder(PublicationCountry, correlation), y = correlation, fill = PublicationCountry)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(correlation, 2)), hjust = -0.1, size = 5) +
  labs(
    title = "Correlation Between GDP and Cumulative Research Output",
    x = "Country",
    y = "Pearson Correlation (GDP vs. Publications)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  ylim(-1, 1)

# Regression analysis: Publications ~ GDP
gdp_pub_models <- AfricaDataset %>%
  group_by(PublicationCountry) %>%
  do(tidy(lm(CumulativePublications ~ GDP, data = .))) %>%
  filter(term == "GDP") %>%
  rename(Slope = estimate, StdError = std.error, t_value = statistic, p_value = p.value)

print(gdp_pub_models)

ggplot(AfricaDataset, aes(x = GDP, y = CumulativePublications, color = PublicationCountry)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ PublicationCountry, scales = "free") +
  labs(
    title = "Relationship Between GDP and Research Output",
    x = "GDP (constant US$)",
    y = "Cumulative Publications"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

########################################
# Section 3: Gender Inequality Index (GII) Data Integration and Analysis
########################################

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(scales)
library(broom)

# Read the GII dataset
gii_data <- read_csv("Gender Inequality Index.csv", show_col_types = FALSE)

# Select only relevant columns and reshape (Year | Country | GII)
gii_long <- gii_data %>%
  select(Country, starts_with("Gender Inequality Index")) %>%
  pivot_longer(
    cols = starts_with("Gender Inequality Index"),
    names_to = "Year",
    values_to = "GII"
  ) %>%
  mutate(
    PublicationYear = as.integer(str_extract(Year, "\\d{4}")),
    PublicationCountry = Country,
    GII = as.numeric(GII)
  ) %>%
  select(PublicationCountry, PublicationYear, GII)

# Keep only the six target countries
target_countries <- c("South Africa", "Nigeria", "Ethiopia", "Ghana", "Kenya", "Cameroon")
gii_long <- gii_long %>%
  filter(PublicationCountry %in% target_countries)

# Merge GII into existing AfricaDataset
AfricaDataset <- AfricaDataset %>%
  left_join(gii_long, by = c("PublicationCountry", "PublicationYear"))

# Check structure and missingness
str(AfricaDataset)
summary(AfricaDataset$GII)

# Check missing GII values
AfricaDataset %>%
  filter(is.na(GII)) %>%
  count(PublicationCountry, PublicationYear, name = "Missing_GII") %>%
  arrange(PublicationCountry, PublicationYear)

# Visualization: Gender Inequality Index over time
ggplot(AfricaDataset, aes(
  x = PublicationYear,
  y = GII,
  color = PublicationCountry
)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Gender Inequality Index Over Time (Six African Countries)",
    x = "Year",
    y = "GII (0 = Equality, 1 = High Inequality)",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Visualization: GII and Publications (scaled)
df_scaled_gii <- AfricaDataset %>%
  group_by(PublicationCountry) %>%
  mutate(Pub_scaled = CumulativePublications * (max(GII, na.rm = TRUE) / max(CumulativePublications, na.rm = TRUE)))

ggplot(df_scaled_gii, aes(x = PublicationYear, group = PublicationCountry)) +
  geom_line(aes(y = GII, color = "GII"), size = 1.2) +
  geom_line(aes(y = Pub_scaled, color = "Publications"), size = 1.2, linetype = "dashed") +
  facet_wrap(~ PublicationCountry, scales = "free_y") +
  scale_y_continuous(
    name = "Gender Inequality Index (GII)",
    sec.axis = sec_axis(~ . / (max(df_scaled_gii$GII, na.rm = TRUE) / max(df_scaled_gii$CumulativePublications, na.rm = TRUE)),
                        name = "Cumulative Publications")
  ) +
  scale_color_manual(values = c("GII" = "#1f77b4", "Publications" = "#ff7f0e")) +
  labs(
    title = "Gender Inequality and Research Output Growth by Country",
    x = "Year",
    color = "Metric"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Correlation between GII and research output per country
gii_pub_corr <- AfricaDataset %>%
  group_by(PublicationCountry) %>%
  summarize(
    correlation = cor(GII, CumulativePublications, use = "complete.obs", method = "pearson"),
    n_obs = n()
  ) %>%
  arrange(desc(correlation))

print(gii_pub_corr)

# Visualization: Correlation between GII and publications
ggplot(gii_pub_corr, aes(x = reorder(PublicationCountry, correlation), y = correlation, fill = PublicationCountry)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(correlation, 2)), hjust = -0.1, size = 5) +
  labs(
    title = "Correlation Between GII and Cumulative Research Output",
    x = "Country",
    y = "Pearson Correlation (GII vs. Publications)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  ylim(-1, 1)

# Regression analysis: Publications ~ GII
gii_pub_models <- AfricaDataset %>%
  group_by(PublicationCountry) %>%
  do(tidy(lm(CumulativePublications ~ GII, data = .))) %>%
  filter(term == "GII") %>%
  rename(Slope = estimate, StdError = std.error, t_value = statistic, p_value = p.value)

print(gii_pub_models)

# Visualization: Relationship between GII and research output
ggplot(AfricaDataset, aes(x = GII, y = CumulativePublications, color = PublicationCountry)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ PublicationCountry, scales = "free") +
  labs(
    title = "Relationship Between Gender Inequality and Research Output",
    x = "Gender Inequality Index (Higher = More Inequality)",
    y = "Cumulative Publications"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

########################################
# Section 4: Human Development Index (HDI) Data Integration and Analysis
########################################

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(broom)

# Read the HDR Excel file
hdr_data <- read_excel("hdr-data.xlsx", sheet = "Data")

# Filter for Human Development Index (HDI) values only
hdi_data <- hdr_data %>%
  filter(indexCode == "HDI") %>%
  select(
    PublicationCountry = country,
    PublicationYear = year,
    HDI = value
  )

# Keep only the six target countries
target_countries <- c("South Africa", "Nigeria", "Ethiopia", "Ghana", "Kenya", "Cameroon")

hdi_data <- hdi_data %>%
  filter(PublicationCountry %in% target_countries)

# Ensure PublicationYear has consistent type (integer)
AfricaDataset <- AfricaDataset %>%
  mutate(PublicationYear = as.integer(PublicationYear))

hdi_data <- hdi_data %>%
  mutate(PublicationYear = as.integer(PublicationYear))

# Merge with your existing AfricaDataset
AfricaDataset <- AfricaDataset %>%
  left_join(hdi_data, by = c("PublicationCountry", "PublicationYear"))

# Check structure and missing values
str(AfricaDataset)
summary(AfricaDataset$HDI)

AfricaDataset %>%
  filter(is.na(HDI)) %>%
  count(PublicationCountry, PublicationYear, name = "Missing_HDI") %>%
  arrange(PublicationCountry, PublicationYear)

# Cleanup: keep valid years and HDI values
AfricaDataset <- AfricaDataset %>%
  filter(PublicationYear >= 1990 & PublicationYear <= 2023) %>%
  mutate(HDI = as.numeric(HDI)) %>%
  filter(!is.na(HDI) & HDI >= 0 & HDI <= 1)

# Visualization: Human Development Index (HDI) trend
ggplot(AfricaDataset, aes(
  x = PublicationYear,
  y = HDI,
  color = PublicationCountry
)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 5)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::number_format(accuracy = 0.1)) +
  labs(
    title = "Human Development Index (HDI) Over Time (Six African Countries)",
    x = "Year",
    y = "HDI (0 = Low Development, 1 = High Development)",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Scale publications for plotting alongside HDI
df_scaled_hdi <- AfricaDataset %>%
  group_by(PublicationCountry) %>%
  mutate(Pub_scaled = CumulativePublications *
           (max(HDI, na.rm = TRUE) / max(CumulativePublications, na.rm = TRUE)))

# Visualization: HDI and Publications
ggplot(df_scaled_hdi, aes(x = PublicationYear, group = PublicationCountry)) +
  geom_line(aes(y = HDI, color = "HDI"), size = 1.2) +
  geom_line(aes(y = Pub_scaled, color = "Publications"), size = 1.2, linetype = "dashed") +
  facet_wrap(~ PublicationCountry, scales = "free_y") +
  scale_y_continuous(
    name = "Human Development Index (HDI)",
    sec.axis = sec_axis(~ . / (max(df_scaled_hdi$HDI, na.rm = TRUE) /
                                 max(df_scaled_hdi$CumulativePublications, na.rm = TRUE)),
                        name = "Cumulative Publications")
  ) +
  scale_color_manual(values = c("HDI" = "#1f77b4", "Publications" = "#ff7f0e")) +
  labs(
    title = "Human Development and Research Output Growth by Country",
    x = "Year",
    color = "Metric"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Correlation between HDI and research output per country
hdi_pub_corr <- AfricaDataset %>%
  group_by(PublicationCountry) %>%
  summarize(
    correlation = cor(HDI, CumulativePublications, use = "complete.obs", method = "pearson"),
    n_obs = n()
  ) %>%
  arrange(desc(correlation))

print(hdi_pub_corr)

# Visualization: Correlation between HDI and publications
ggplot(hdi_pub_corr, aes(x = reorder(PublicationCountry, correlation), y = correlation, fill = PublicationCountry)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(correlation, 2)), hjust = -0.1, size = 5) +
  labs(
    title = "Correlation Between HDI and Cumulative Research Output",
    x = "Country",
    y = "Pearson Correlation (HDI vs. Publications)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  ylim(-1, 1)

# Regression analysis: Publications ~ HDI
hdi_pub_models <- AfricaDataset %>%
  group_by(PublicationCountry) %>%
  do(tidy(lm(CumulativePublications ~ HDI, data = .))) %>%
  filter(term == "HDI") %>%
  rename(Slope = estimate, StdError = std.error, t_value = statistic, p_value = p.value)

print(hdi_pub_models)

# Visualization: Relationship between HDI and research output
ggplot(AfricaDataset, aes(
  x = HDI,
  y = CumulativePublications,
  color = PublicationCountry
)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ PublicationCountry, scales = "free") +
  labs(
    title = "Relationship Between Human Development and Research Output",
    x = "Human Development Index (HDI)",
    y = "Cumulative Publications"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

########################################
# Section 5: Inequality-Adjusted Human Development Index (IHDI) Data Integration and Analysis
########################################

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(broom)

# Read the IHDI Excel file
ihdi_data <- read_excel("ihdidata.xlsx", sheet = "Data")

# Filter for the actual IHDI indicator (exclude inequality coefficients)
ihdi_data <- ihdi_data %>%
  filter(indexCode == "IHDI", indicatorCode == "ihdi") %>%
  select(
    PublicationCountry = country,
    PublicationYear = year,
    IHDI = value
  )

# Keep only the six target countries
target_countries <- c("South Africa", "Nigeria", "Ethiopia", "Ghana", "Kenya", "Cameroon")

ihdi_data <- ihdi_data %>%
  filter(PublicationCountry %in% target_countries)

# Ensure PublicationYear is an integer
ihdi_data <- ihdi_data %>%
  mutate(PublicationYear = as.integer(PublicationYear))

AfricaDataset <- AfricaDataset %>%
  mutate(PublicationYear = as.integer(PublicationYear))

# Merge with the existing AfricaDataset
AfricaDataset <- AfricaDataset %>%
  left_join(ihdi_data, by = c("PublicationCountry", "PublicationYear"))

# Inspect structure and missingness
str(AfricaDataset)
summary(AfricaDataset$IHDI)

AfricaDataset %>%
  filter(is.na(IHDI)) %>%
  count(PublicationCountry, PublicationYear, name = "Missing_IHDI") %>%
  arrange(PublicationCountry, PublicationYear)

# Clean and visualize IHDI trends (valid range 0–1, 2010–2023)
AfricaDataset <- AfricaDataset %>%
  filter(PublicationYear >= 2010 & PublicationYear <= 2023) %>%
  mutate(IHDI = as.numeric(IHDI)) %>%
  filter(!is.na(IHDI) & IHDI >= 0 & IHDI <= 1)

# Visualization: Inequality-Adjusted Human Development Index (IHDI) trend
ggplot(AfricaDataset, aes(
  x = PublicationYear,
  y = IHDI,
  color = PublicationCountry
)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2010, 2023, by = 2)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::number_format(accuracy = 0.1)) +
  labs(
    title = "Inequality-Adjusted Human Development Index (IHDI) Over Time (Six African Countries)",
    x = "Year",
    y = "IHDI (0 = Low Development, 1 = High Development)",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Scale publications for plotting alongside IHDI
df_scaled_ihdi <- AfricaDataset %>%
  group_by(PublicationCountry) %>%
  mutate(Pub_scaled = CumulativePublications *
           (max(IHDI, na.rm = TRUE) / max(CumulativePublications, na.rm = TRUE)))

# Visualization: IHDI and Publications
ggplot(df_scaled_ihdi, aes(x = PublicationYear, group = PublicationCountry)) +
  geom_line(aes(y = IHDI, color = "IHDI"), size = 1.2) +
  geom_line(aes(y = Pub_scaled, color = "Publications"), size = 1.2, linetype = "dashed") +
  facet_wrap(~ PublicationCountry, scales = "free_y") +
  scale_y_continuous(
    name = "Inequality-Adjusted Human Development Index (IHDI)",
    sec.axis = sec_axis(~ . / (max(df_scaled_ihdi$IHDI, na.rm = TRUE) /
                                 max(df_scaled_ihdi$CumulativePublications, na.rm = TRUE)),
                        name = "Cumulative Publications")
  ) +
  scale_color_manual(values = c("IHDI" = "#1f77b4", "Publications" = "#ff7f0e")) +
  labs(
    title = "Inequality-Adjusted Human Development and Research Output Growth by Country",
    x = "Year",
    color = "Metric"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Correlation between IHDI and research output per country
ihdi_pub_corr <- AfricaDataset %>%
  group_by(PublicationCountry) %>%
  summarize(
    correlation = cor(IHDI, CumulativePublications, use = "complete.obs", method = "pearson"),
    n_obs = n()
  ) %>%
  arrange(desc(correlation))

print(ihdi_pub_corr)

# Visualization: Correlation between IHDI and publications
ggplot(ihdi_pub_corr, aes(x = reorder(PublicationCountry, correlation), y = correlation, fill = PublicationCountry)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(correlation, 2)), hjust = -0.1, size = 5) +
  labs(
    title = "Correlation Between IHDI and Cumulative Research Output",
    x = "Country",
    y = "Pearson Correlation (IHDI vs. Publications)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  ylim(-1, 1)

# Regression analysis: Publications ~ IHDI
ihdi_pub_models <- AfricaDataset %>%
  group_by(PublicationCountry) %>%
  do(tidy(lm(CumulativePublications ~ IHDI, data = .))) %>%
  filter(term == "IHDI") %>%
  rename(Slope = estimate, StdError = std.error, t_value = statistic, p_value = p.value)

print(ihdi_pub_models)

# Visualization: Relationship between IHDI and research output
ggplot(AfricaDataset, aes(
  x = IHDI,
  y = CumulativePublications,
  color = PublicationCountry
)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ PublicationCountry, scales = "free") +
  labs(
    title = "Relationship Between Inequality-Adjusted Human Development and Research Output",
    x = "Inequality-Adjusted Human Development Index (IHDI)",
    y = "Cumulative Publications"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Note: Research expenditure for GDP was not used as it did not have reliable values for 5 out of the 6 countries.

########################################
# Section 6: Total Population Data Integration and Analysis
########################################

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(broom)

# Read the Excel file (skip metadata rows)
population_raw <- read_excel("API_SP.POP.TOTL_DS2_en_excel_v2_115066.xls", skip = 3)

# Select relevant columns and filter for total population indicator
population_data <- population_raw %>%
  select(`Country Name`, `Country Code`, `Indicator Code`, starts_with("19"), starts_with("20")) %>%
  filter(`Indicator Code` == "SP.POP.TOTL") %>%
  rename(PublicationCountry = `Country Name`) %>%
  select(-`Country Code`, -`Indicator Code`)

# Reshape data from wide to long format
population_long <- population_data %>%
  pivot_longer(
    cols = -PublicationCountry,
    names_to = "PublicationYear",
    values_to = "TotalPopulation"
  ) %>%
  mutate(
    PublicationYear = as.integer(PublicationYear),
    TotalPopulation = as.numeric(TotalPopulation)
  ) %>%
  filter(!is.na(TotalPopulation))

# Keep only the six target countries
target_countries <- c("South Africa", "Nigeria", "Ethiopia", "Ghana", "Kenya", "Cameroon")

population_long <- population_long %>%
  filter(PublicationCountry %in% target_countries)

# Merge with existing AfricaDataset
AfricaDataset <- AfricaDataset %>%
  left_join(population_long, by = c("PublicationCountry", "PublicationYear"))

# Inspect structure and summary
str(AfricaDataset)
summary(AfricaDataset$TotalPopulation)

# Check missing population data by country and year
AfricaDataset %>%
  filter(is.na(TotalPopulation)) %>%
  count(PublicationCountry, PublicationYear, name = "Missing_Population") %>%
  arrange(PublicationCountry, PublicationYear)

# Visualization: Total Population Growth
ggplot(population_long, aes(
  x = PublicationYear,
  y = TotalPopulation,
  color = PublicationCountry,
  group = PublicationCountry
)) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  scale_y_continuous(labels = label_number(scale_cut = cut_si("M"))) +
  scale_x_continuous(breaks = seq(1960, 2025, by = 10)) +
  labs(
    title = "Total Population Growth Over Time (Six African Countries)",
    subtitle = "Population trends from World Bank data",
    x = "Year",
    y = "Total Population (People)",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted", color = "grey85"),
    panel.grid.major.y = element_line(linetype = "dotted", color = "grey90"),
    axis.title = element_text(face = "bold")
  )

# Visualization: Population and Research Output
df_scaled_pop <- AfricaDataset %>%
  group_by(PublicationCountry) %>%
  mutate(Pub_scaled = CumulativePublications *
           (max(TotalPopulation, na.rm = TRUE) / max(CumulativePublications, na.rm = TRUE)))

ggplot(df_scaled_pop, aes(x = PublicationYear, group = PublicationCountry)) +
  geom_line(aes(y = TotalPopulation, color = "Total Population"), size = 1.2) +
  geom_line(aes(y = Pub_scaled, color = "Publications"), size = 1.2, linetype = "dashed") +
  facet_wrap(~ PublicationCountry, scales = "free_y") +
  scale_y_continuous(
    name = "Total Population",
    labels = label_number(scale_cut = cut_si("M")),
    sec.axis = sec_axis(~ . / (max(df_scaled_pop$TotalPopulation, na.rm = TRUE) /
                                 max(df_scaled_pop$CumulativePublications, na.rm = TRUE)),
                        name = "Cumulative Publications")
  ) +
  scale_color_manual(values = c("Total Population" = "#1f77b4", "Publications" = "#ff7f0e")) +
  labs(
    title = "Population and Research Output Growth by Country",
    x = "Year",
    color = "Metric"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

# Correlation and Regression: Population vs Research Output
pop_pub_corr <- AfricaDataset %>%
  group_by(PublicationCountry) %>%
  summarize(
    correlation = cor(TotalPopulation, CumulativePublications, use = "complete.obs", method = "pearson"),
    n_obs = n()
  ) %>%
  arrange(desc(correlation))

print(pop_pub_corr)

ggplot(pop_pub_corr, aes(x = reorder(PublicationCountry, correlation), y = correlation, fill = PublicationCountry)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = round(correlation, 2)), hjust = -0.1, size = 5) +
  labs(
    title = "Correlation Between Total Population and Research Output",
    x = "Country",
    y = "Pearson Correlation (Population vs. Publications)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  ylim(-1, 1)

pop_pub_models <- AfricaDataset %>%
  group_by(PublicationCountry) %>%
  do(tidy(lm(CumulativePublications ~ TotalPopulation, data = .))) %>%
  filter(term == "TotalPopulation") %>%
  rename(Slope = estimate, StdError = std.error, t_value = statistic, p_value = p.value)

print(pop_pub_models)

ggplot(AfricaDataset, aes(x = TotalPopulation, y = CumulativePublications, color = PublicationCountry)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ PublicationCountry, scales = "free") +
  labs(
    title = "Relationship Between Population Size and Research Output",
    x = "Total Population",
    y = "Cumulative Publications"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))




########################################
# Section 7: R&D Researchers per Million People Integration
########################################
library(jsonlite)

# Fetch the data
df <- read.csv("https://ourworldindata.org/grapher/researchers-in-rd-per-million-people.csv?v=1&csvType=full&useColumnShortNames=true")

df

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(broom)

# Filter df to include only the six target African countries
target_countries <- c("South Africa", "Nigeria", "Ethiopia", "Ghana", "Kenya", "Cameroon")

rd_data <- df %>%
  filter(Entity %in% target_countries) %>%
  rename(
    PublicationCountry = Entity,
    PublicationYear = Year,
    ResearchersPerMillion = sp_pop_scie_rd_p6
  ) %>%
  mutate(
    PublicationYear = as.integer(PublicationYear),
    ResearchersPerMillion = as.numeric(ResearchersPerMillion)
  )

# Merge into the existing AfricaDataset
AfricaDataset <- AfricaDataset %>%
  left_join(rd_data, by = c("PublicationCountry", "PublicationYear"))

# Inspect merged structure
str(AfricaDataset)
summary(AfricaDataset$ResearchersPerMillion)

# Visualization: R&D Researchers per Million People over time
ggplot(rd_data, aes(
  x = PublicationYear,
  y = ResearchersPerMillion,
  color = PublicationCountry,
  group = PublicationCountry
)) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  scale_y_continuous(labels = label_number(scale_cut = cut_si("k"))) +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  labs(
    title = "R&D Researchers per Million People Over Time (Six African Countries)",
    x = "Year",
    y = "Researchers per Million People",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

########################################
# Section 8: Literacy Rate Data Integration and Analysis
########################################

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)

# Read the Literacy Rate dataset (skip metadata rows)
literacy_raw <- read_csv("API_SE.ADT.LITR.ZS_DS2_en_csv_v2_220.csv", skip = 4, show_col_types = FALSE)

# Select relevant columns (Country, Indicator, and yearly data)
literacy_data <- literacy_raw %>%
  select(`Country Name`, `Indicator Code`, starts_with("19"), starts_with("20")) %>%
  filter(`Indicator Code` == "SE.ADT.LITR.ZS") %>%  # Literacy rate indicator
  rename(PublicationCountry = `Country Name`) %>%
  select(-`Indicator Code`)

# Reshape data from wide to long format
literacy_long <- literacy_data %>%
  pivot_longer(
    cols = -PublicationCountry,
    names_to = "PublicationYear",
    values_to = "LiteracyRate"
  ) %>%
  mutate(
    PublicationYear = as.integer(PublicationYear),
    LiteracyRate = as.numeric(LiteracyRate)
  ) %>%
  filter(!is.na(LiteracyRate))

# Keep only the six target countries
target_countries <- c("South Africa", "Nigeria", "Ethiopia", "Ghana", "Kenya", "Cameroon")

literacy_long <- literacy_long %>%
  filter(PublicationCountry %in% target_countries)

# Merge Literacy data with AfricaDataset
AfricaDataset <- AfricaDataset %>%
  left_join(literacy_long, by = c("PublicationCountry", "PublicationYear"))

# Inspect structure
str(AfricaDataset)
summary(AfricaDataset$LiteracyRate)

# Visualize Literacy Rate Trends Over Time
ggplot(literacy_long, aes(x = PublicationYear, y = LiteracyRate, color = PublicationCountry, group = PublicationCountry)) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = seq(1980, 2025, by = 5)) +
  labs(
    title = "Adult Literacy Rate (% of Population Ages 15+) Over Time (Six African Countries)",
    x = "Year",
    y = "Literacy Rate (%)",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

#View(AfricaDataset)

