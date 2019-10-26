cases <- tribble(
  ~ID, ~i01, ~i02, ~i03, ~i04, ~i05, ~i06, ~i07, ~i08, ~i09, ~i10, ~i11, ~i12, ~i13, ~i14, ~i15, ~i16, ~i17, ~i18, ~i19, ~i20,
  "A", 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1,
  "B", 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1
)
tall_cases <- cases %>%
  gather(var, val, 2:ncol(cases)) %>%
  spread(ID, val) %>% 
  rename(item = var)
