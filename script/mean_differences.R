t.ch_d <- t.test(final$chall_pos_rt_1, final$chall_neg_rt_1, paired = T)
t.ch_rt <- t.test(final$chall_pos_rt_2, final$chall_neg_rt_2, paired = T)
t.cr_d <- t.test(final$crit_pos_rt_1, final$crit_neg_rt_1, paired = T)
t.cr_rt <- t.test(final$crit_pos_rt_1, final$crit_neg_rt_2, paired = T)

t.ch_d
t.ch_rt
t.cr_d
t.cr_rt


final %>%
  summarise(
    #challenge d-score
    M_chall_pos_rt_1 = mean(chall_pos_rt_1),
    SD_chall_pos_rt_1 = sd(chall_pos_rt_1),
    M_chall_neg_rt_1 = mean(chall_neg_rt_1),
    SD_chall_neg_rt_1 = sd(chall_neg_rt_1),
    #criticism d-score
    M_chall_pos_rt_2 = mean(chall_pos_rt_2),
    SD_chall_pos_rt_2 = sd(chall_pos_rt_2),
    M_chall_neg_rt_2 = mean(chall_neg_rt_2),
    SD_chall_neg_rt_2 = sd(chall_neg_rt_2),
    #challenge rt
    M_crit_pos_rt_1 = mean(crit_pos_rt_1),
    SD_crit_pos_rt_1 = sd(crit_pos_rt_1),
    M_crit_neg_rt_1 = mean(crit_neg_rt_1),
    SD_crit_neg_rt_1 = sd(crit_neg_rt_1),
    #criticism rt
    M_crit_pos_rt_1 = mean(crit_pos_rt_1),
    SD_crit_pos_rt_1 = sd(crit_pos_rt_1),
    M_crit_neg_rt_2 = mean(crit_neg_rt_2),
    SD_crit_neg_rt_2 = sd(crit_neg_rt_2)
  ) %>% 
  write_csv("D:/Documents/Egyebek/Thesis/data/Temp/katanak.csv")
