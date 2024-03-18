reference_df <- read.xlsx(
  paste0(path_files, "Consolidação Localização Radares - API-LAI-CET.xlsx"),
  sheet = "de-para")

# names(reference_df)
# 
# reference_df %>% 
#   add_count(cod_unico) %>% 
#   filter(n>1)

reference_df <- reference_df %>% 
  mutate_all(~ifelse(. == "#N/A", NA, .))  %>%
  mutate(ativacao_lai = as.numeric(ativacao_lai),
         data_ativacao = 
           case_when(!is.na(data_publicacao_api) ~ as.numeric(data_publicacao_api),
                     is.na(data_publicacao_api) & !is.na(ativacao_lai) ~ as.numeric(ativacao_lai),
                     TRUE ~ NA_integer_),
         data_ativacao = 
           ifelse(is.na(data_ativacao), NA_Date_, as.Date(data_ativacao, origin = "1899-12-30")),
         year = year(data_ativacao),
         month = month(data_ativacao),
         lote = as.character(as.numeric(lote)),
         faixas = ifelse(faixas == "NÃO", NA, as.numeric(faixas)),
         faixas_api = ifelse(str_detect(faixas_api, "REF"), NA, as.numeric(faixas_api))) %>% 
  mutate_at(
    c("cod_familia", "cod_unico", "cod_local"), 
    ~ifelse(str_detect(., "-"), ., as.character(as.numeric(.)))) %>% 
  group_by(cod_familia) %>% 
  mutate_at(vars(lon_rev, lat_rev), max, na.rm = TRUE) %>% 
  ungroup()

# table(is.na(reference_df$cod_familia))
# 
# reference_df %>% 
#   filter(is.na(lon_rev) | is.na(lat_rev))

reference_df <- reference_df %>% 
  filter(!is.na(lon_rev) & !is.na(lat_rev)) %>% 
  mutate(lon_rev = as.numeric(lon_rev),
         lat_rev = as.numeric(lat_rev)) %>% 
  st_as_sf(coords = c("lon_rev", "lat_rev"), crs = 4326)

# table(reference_df$faixas, reference_df$faixas_api)

helper_ids <- reference_df %>% 
  st_set_geometry(NULL) %>% 
  mutate(id = str_sub(cod_familia, 1, 4),
         data_ativacao = as.Date(data_ativacao)) %>% 
  select(id, cod_familia, cod_unico, cod_local, lote, data_ativacao, year, month, faixas, faixas_api) 

table(is.na(helper_ids$cod_familia))
table(is.na(helper_ids$cod_unico))
table(is.na(helper_ids$cod_local))

helper_geo <- reference_df %>% 
  mutate(id = str_sub(cod_familia, 1, 4)) %>% 
  select(id, cod_familia, cod_unico, lote, endereco_api, sentido_api, sentido)

remove(reference_df)
