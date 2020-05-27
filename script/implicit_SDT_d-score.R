# signal detection theory based d-score
implicit <- 
  gnat_raw %>% 
  #filter only important trials and only go_trials 
  filter(target %in%important_targets) %>% 
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
  filter(correct_block > 0.6 & correct_all > 0.8,
         rt > 300) %>%
  #calculating d-scores
  mutate(correct = ifelse(trial_type == "go_trial" & rt < max_rt, 1, 
                          ifelse(trial_type == "nogo_trial" & rt == max_rt, 1, 0)),
         true_hit = ifelse(trial_type == "go_trial" & correct == 1, 1, 0),
         false_hit = ifelse(trial_type == "nogo_trial" & correct == 0, 1, 0)) %>% 
  group_by(measurement, id, target) %>% 
  summarise(count_true_hit = sum(true_hit),
            count_false_hit = sum(false_hit),
            count_go = sum(trial_type == "go_trial"),
            count_nogo = sum(trial_type == "nogo_trial")) %>% 
  mutate(prop_go = count_true_hit/count_go,
         prop_nogo = count_false_hit/count_nogo,
         probit_go = probitlink(prop_go),
         probit_nogo = probitlink(prop_nogo),
         d = probit_go - probit_nogo) %>% 
  select(measurement, id, target, d) %>%
  spread(target, d) %>% 
  mutate(challenge_d = chall_neg - chall_pos,
         crit_d = crit_neg - crit_pos) %>% 
  view()
