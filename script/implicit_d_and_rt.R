implicit <- 
  gnat_raw %>% 
  #filter only important trials and only go_trials 
  filter(target %in%important_targets,
         trial_type == 'go_trial') %>% 
  #filtering out participants that didnt complete the second round
  filter(!id %in% excluded_id) %>%
  #checking for block error rate per participant
  group_by(id, target) %>% 
  mutate(correct_block = 1 - mean(error)) %>% 
  ungroup() %>% 
  # checking for overall error rate per participant
  group_by(id) %>% 
  mutate(correct_all = 1 - mean(error)) %>% 
  ungroup() %>% 
  # filter overall and block error rates, filter too quick and too slow responses
  filter(correct_block > 0.6 & correct_all > 0.8 & error == 0,
         rt > 300 & rt < 1399) %>%
  #calculating d-scores
  group_by(measurement, id) %>% 
  mutate(pers_sd = sd(rt)) %>% 
  ungroup() %>% 
  group_by(measurement, id, target, pers_sd) %>% 
  summarise(pers_block_avg = mean(rt)) %>% 
  mutate(d = pers_block_avg/pers_sd) %>% 
  ungroup() %>% 
  select(-pers_sd) %>%
  tidyr::pivot_wider(id_cols = id, names_from = target, values_from = c(d, pers_block_avg), names_glue = "{target}_{.value}") %>% 
  view()
  mutate(challenge_d = chall_neg - chall_pos,
         crit_d = crit_neg - crit_pos)

final <- left_join(explicit, implicit, by = c("id", "measurement")) %>% 
  filter(neptun_1 != "q69vn2",
         neptun_1 != "bm0vx1") %>% 
  pivot_wider(id_cols = neptun_1, names_from = measurement, values_from = c(3:11, 13:20)) %>% 
  mutate(gender = gender_1_1, 
         age = age_1_1,
         group = group_1,
         time_end_diff = time_end_diff_1) %>% 
  select(-c(gender_1_1, gender_1_2, age_1_1, age_1_2, group_1, group_2, time_end_diff_1, time_end_diff_2))

results_basic <- final %>% 
  select(c(2:13, 16:27)) %>% 
  corrr::correlate(method = "spearman") %>% 
  corrr::focus(matches("1$")) %>% 
  select(c(1, 8:13)) %>% 
  slice(7:12) %>% 
  view()
