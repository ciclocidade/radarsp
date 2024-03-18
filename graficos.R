#### ###################################################################### ####
####             GRÁFICOS PARA A BASE VOLUME POR 15 MINUTOS                 ####
#### ###################################################################### ####
df_radares %>% 
  group_by(data, hora, min) %>% 
  summarise(volume = sum(volume)) %>% 
  ungroup() %>% 
  mutate(min = factor(min, levels = c("04", "03", "02", "01"))) %>% 
  ggplot(aes(x = hora, y = volume, fill = min))+
  geom_bar(stat="identity")+
  facet_wrap(~data)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Hora",
       y = "Volume") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

df_radares %>% 
  group_by(data, hora, min) %>% 
  summarise(volume = sum(volume)) %>% 
  ungroup() %>% 
  mutate(min = factor(min, levels = c("04", "03", "02", "01"))) %>% 
  ggplot(aes(x = hora, y = volume, fill = min))+
  geom_bar(stat="identity", position = "fill")+
  geom_hline(yintercept = 0.25, linetype = "dashed", size = 0.25)+
  geom_hline(yintercept = 0.50, linetype = "dashed", size = 0.25)+
  geom_hline(yintercept = 0.75, linetype = "dashed", size = 0.25)+
  facet_wrap(~data)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Hora",
       y = "Volume") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

df_radares %>% 
  group_by(data, hora, min) %>% 
  summarise(volume = sum(volume)) %>% 
  ungroup() %>% 
  mutate(type = ifelse(min %in% c("01", "02"), "early", "late"),
         type = factor(type, levels = c("late", "early"))) %>% 
  ggplot(aes(x = hora, y = volume, fill = type))+
  geom_bar(stat="identity", position = "fill")+
  geom_hline(yintercept = 0.50, linetype = "dashed", size = 0.25)+
  facet_wrap(~data)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Hora",
       y = "Volume") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#### ###################################################################### ####
####                GRÁFICOS PARA A BASE VOLUME POR HORA                    ####
#### ###################################################################### ####
by_day_loc_hora %>% 
  group_by(data, hora) %>% 
  summarise(volume = sum(volume)) %>% 
  ungroup() %>% 
  ggplot(aes(x = hora, y = volume))+
  geom_bar(stat="identity")+
  facet_wrap(~data)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Hora",
       y = "Volume") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

by_day_hora %>% 
  ggplot(aes(x = vel_p, y = volume, color = data))+
  geom_point(size=2) +
  # facet_wrap(~data)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Velocidade pontual média",
       y = "Volume total") 

by_day_loc_hora %>% 
  ggplot(aes(x = vel_p, y = volume, color = local), alpha = 0.1)+
  geom_point(size=2) +
  facet_wrap(~data)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Velocidade pontual média",
       y = "Volume total") +
  theme(legend.position = "none")

by_day_loc_hora %>% 
  ggplot(aes(x = vel_p, y = volume, color = data))+
  geom_point(size=2) +
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Velocidade pontual média",
       y = "Volume total") 

radar_6774 <- df_radares %>% filter(local == "6774")

radar_6774 %>% 
  group_by(data, local, hora) %>% 
  summarise(volume = n(),
            vel_p_sd = sd(as.numeric(vel_p), na.rm = TRUE),
            vel_p = mean(as.numeric(vel_p), na.rm = TRUE)) %>% 
  ggplot(aes(x = vel_p, y = volume, color = hora))+
  geom_point(size=2) +
  facet_wrap(~data)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Velocidade pontual média",
       y = "Volume total") 

df_radares %>% 
  ggplot(aes(x = as.numeric(vel_p))) +
  geom_histogram() +
  facet_wrap(~hora)

vec_locais <- unique(df_radares$local)[1:12]

df_radares %>% 
  filter(local %in% vec_locais) %>% 
  ggplot(aes(x = as.numeric(vel_p), fill = hora)) +
  geom_histogram() +
  facet_wrap(~local)



#### ###################################################################### ####
####                GRÁFICOS PARA A BASE VOLUME POR DIA                     ####
#### ###################################################################### ####
by_day_loc %>% 
  group_by(data) %>% 
  arrange(volume) %>% 
  mutate(ordem = row_number()) %>%
  ungroup() %>% 
  ggplot(aes(x = ordem, y = volume))+
  geom_bar(stat="identity")+
  facet_wrap(~data)+
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ","))+
  labs(x = "Local",
       y = "Volume") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())