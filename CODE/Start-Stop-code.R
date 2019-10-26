suppressMessages(library(here))
library(magrittr)
suppressMessages(suppressWarnings(library(tidyverse)))
library(runner)
suppressMessages(library(data.table))

# Initialize input parameters and read input file
scale <- c("PHY")
last_item <- 37
first_item_orig_name <- c("PHY06")
last_item_orig_name <- c("PHY48")

input_orig_names <- suppressMessages(
  read_csv(
    here(
      paste0('INPUT-FILES/SCORED-ITEMS/DP4-', scale, '-interview-scored-items.csv')
    )
  )
) 

####### FIRST SECTION OF CODE DEALS WITH START RULE / BASAL

# represent input item names as rows in a df. Thus, `df_names` provides key for
# matching input item names to sequential item numbering, the latter being
# represented in the df_names$value col. The single bracket [] is an extraction
# of only the columns that are actual items (e.g., not IDnum, age, etc.)
df_names <- enframe(names(input_orig_names)[3:last_item+2])

# rename items to sequential numbering, `names(input)[1:2]` extracts first two
# var names, which don't change; `sprintf()` is a string function that allows
# you to create sequences with leading 0, e.g., '01, 02, 03'. %02d means "format
# the integer with 2 digits, left padding it with zeroes"
input <- input_orig_names
names(input) <- c(names(input)[1:2], c(paste0('i', sprintf("%02d", 1:last_item))))

# Create a df with just IDnum age and total raw score, for later evalution of stop rule.
input_ID_raw_score <- input %>%   
  mutate(
  TOT_raw = rowSums(.[3:last_item+2])
) %>% 
  mutate(agestrat = case_when(
    age_in_months <= 23 ~ "0-0 to 1-11",
    inrange(age_in_months, 24, 47, incbounds=TRUE) ~ "2-0 to 3-11",
    inrange(age_in_months, 48, 71, incbounds=TRUE) ~ "4-0 to 5-11",
    inrange(age_in_months, 72, 258, incbounds=TRUE) ~ "6-0+",
    TRUE ~ NA_character_
  )
  ) %>% 
  select(IDnum, age_in_months, agestrat, TOT_raw) %>% 
  arrange(IDnum)

# For downstream code, we need a gathered table in which columns of the original
# input are transformed into rows in the gathered table.
input_tidy <- input %>%
  # gather columns to express each cell as a key-value pair: col_name-cell_value
  gather(col, val, i01:!!as.name(paste0('i', last_item))) %>% 
  # group by IDnum, this allows a set of rows in the gathered table to be
  # identified with their origin row in the input table.
  group_by(IDnum) %>%
  # sort by IDnum
  arrange(IDnum) %>%
  # drop age column
  select(-age_in_months) %>% 
  # mutate creates new var to capture the original left-to-right column number
  # of each item from original input file, row_number() returns row number of
  # current piped object (a gathered table where input columns are expressed as
  # rows), but because object is grouped by IDnum, row_number() resets when
  # IDnum changes to next value; in calculating `col_num` we add 2 because in
  # origin input table, the item columns start on col 3
  mutate(col_num = row_number() + 2) 

# Purpose of next block is to find the first 0 for each case, and then find the
# length of the string of 1s immediately above that first 0. We start this
# pipeline with the IDnum column only from the original input table.
output_basal <- input[,1] %>% 
  # Combine these IDnums with summary of each IDnum's first zero, filter reduces the
  # table so that it contains only the 0 cells from the original input rows
  left_join(input_tidy %>% filter(val == 0) %>%
              # In the input object there are multiple 0 rows per IDnum, and going
              # down the table they are arranged in ascending order of item
              # numbers going from left-to-right in the original input table.
              # first() returns only the first of those 0 rows, which, because
              # of the correspondence between the current row order and the
              # column order of the original input table, returns the column
              # name of the first item scored 0.
              summarize(first_0_name = first(col),
                        # analogously, this next use of first() returns the
                        # origin column location of the first item scored 0.
                        first_0_loc = first(col_num))) %>%
  # Combine with length of each IDnum's first post-0 streak of 1's. First join
  # first_0_name and first_0_loc columns to gathered table from upstream. At
  # this point, the piped object has one row for each cell in the crossing of
  # cases x items. The input object is grouped by IDnum, and within each group
  # of rows, the ascending, left-to-right item order of the origin table is
  # represented descending (going down the rows) of the piped table
  left_join(input_tidy %>%
              # this filter pares rows out of each IDnum group, such that, for each
              # IDnum, the remaining rows begin with the row that holds the first 1
              # score after the first 0 score, and end with the row that holds
              # the last consecutive 1 score before the next 0 score (i.e., this
              # number of rows (for each IDnum) is now equal to the length of the
              # string of consecutive 1 scores above the first 0 score)
              #
              # More detail on how the filter works: it catches rows that 1)
              # represent a 1 score on an item (val = 1); 2) are part of the
              # string of consecutive 1 scores (val = 1) that appear obove an 0
              # response for that IDnum (lag(val, default = 1) == 0) -- lag gets
              # the value of val from the row preceding the current row, so when
              # that value is 0, lag will catch it; default = 1 deals with first
              # row, which has no lag row, and would return NA if the default
              # were not set to 1; 3) the cumsum() wrapper keeps a running count
              # of 1 streaks above a 0 (identified by the compound logic val = 1
              # & lag(val) = 0), and by setting the logical condition of
              # cumsum() = 1, it catches only rows that appear in the first such
              # streak, where the value of that running count would be 1.
              filter(val == 1 & cumsum(val == 1 & lag(val, default = 1) == 0) == 1) %>% 
              # this summarize uses n() to create a new variable that is simply
              # the number of rows within each IDnum grouping, which as noted above
              # is equivalent to the length of the streak of 1 scores above the
              # first 0 score.
              summarize(streak_1 = n())) %>% 
              mutate(streak_1 = as.numeric(streak_1)) %>% 
              left_join(input, by = 'IDnum')


# Now we get a freq table of counts of streak length (streak of 1s above first
# 0) over entire input sample
freq_1streak <- output_basal %>% 
  drop_na(streak_1) %>% 
  count(streak_1) %>% 
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4)
    )

# after examining frequency table `freq_1streak`, designate a streak_1_length to
# implement for each case in the input data.
streak_1_length <- 4

# Next sequence gets start item for start rule of streak_1_length + 1 (meaning you
# must get a win streak of streak_1_length + 1 in order to continue testing
# forward). Why is the start rule "+1"? Because the rule has stay active all the
# way through this streak_1_length, so that it can catch the 0 above the streak.
start_data <- input_tidy %>%
  # group_by(IDnum) allows you to find start item per case
  group_by(IDnum) %>% 
  # From the tidy input, label rows that contain correct responses (val = 1) and
  # are part of a win streak equal to or greater than streak_1_length.
  # runner::streak_run gets count, for each cell in val col, what is the current
  # length of streak of consecutive identical values?
  mutate(
    streak_x = case_when(
      val == 1 & streak_run(val) >= streak_1_length ~ 1,
      TRUE ~ NA_real_
    ),
    # First step of isolating the start item: apply a label to only the last row
    # of streak_x, which you find by testing whether, in the lead row to any
    # row, the value of streak_x is NA. If that condition is true, then the
    # current row is the last correct response of streak_x. The label you apply
    # is the column number that lags the row you chose by streak_1_length.
    # Start_item now holds the column number of the desired start item. Why?
    # Because if your start rule is streak_1_length + 1, this is the item for
    # which implementing that start rule guarantees that you will catch the next
    # 0 and be forced to test backwards to satisfy the start rule.
    start_item = case_when(
      streak_x == 1 & is.na(lead(streak_x)) ~ col_num - streak_1_length,
      TRUE ~ NA_real_
    )
  ) %>% 
  # Now that you know the col num of desired start item per case, you have to
  # isolate the rows that actually contain those items, which you accomplish by
  # selecting (filter) rows whose value of `col_num` equals the value of
  # `start_item`` in the row that leads (is ahead) of the row you want by
  # streak_1_length. (Note that `streak_1_length` is passed as the second argument
  # to `lead()` so that it retuns a row that leads by more than one row (default
  # would be leading by one row)). 
  
  # To understand the operation of the filter, generate and view the data object
  # up to this point in the pipeline, look for a row that contains a value for
  # `start_item`, lag this row by streak_1_length, and you end up on the row that
  # contains the `col_num` corresponding to the desired start_item. You will see
  # that the value of `col_num` in the lagged row is identical to the value of
  # `start_item` in the row you started on.
  filter(col_num == lead(start_item, streak_1_length)) %>% 
  # recode start_item so that it contains the actual name of the start item,
  # which is found in column `col` from the tidy input.
  mutate(start_item = col) %>% 
  # At this point the data object contains a start item for each streak >=
  # streak_1_length. We only want the last (highest) of these start items, because
  # that is the one that corresponds to the highest basal streak (so it will
  # yield a true basal). We use summarize() to return only the last start item
  # for each case (remember, the data object is already grouped by ID, so it can
  # be summarized)
  summarize(start = last(start_item)) %>% 
  # need next full_join to bring back NA rows (e.g., rows that have no basal
  # streaks because they are a 1 follwed by all 0s, etc.), as well as items
  # themselves and other input data.
  full_join(input, by = "IDnum") %>% 
  # create `agestrat` var for downstream analysis.
  mutate(agestrat = case_when(
    age_in_months <= 23 ~ "0-0 to 1-11",
    inrange(age_in_months, 24, 47, incbounds=TRUE) ~ "2-0 to 3-11",
    inrange(age_in_months, 48, 71, incbounds=TRUE) ~ "4-0 to 5-11",
    inrange(age_in_months, 72, 258, incbounds=TRUE) ~ "6-0+",
    TRUE ~ NA_character_
  )
  ) %>% 
  select(IDnum, agestrat, age_in_months, start, everything()) %>% 
  arrange(agestrat, age_in_months)

# freq table of start items within agestrats (DP-4 start ages).
freq_start <- start_data %>% 
  group_by(agestrat) %>% 
  drop_na(start) %>% 
  count(start) %>% 
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4)
  )

# Drop all rows where start item is highest possible item (last_item-streak_1_length) - there
# are so many of these that it skews histogram and makes the graph difficult to
# read.
hist_data <- start_data %>% filter(start != paste0('i', last_item-streak_1_length))


# histograms of start items within agestrats (DP-4 start ages).
hist_plot <-
  ggplot(data = hist_data, aes(start)) +
  stat_count(width = 0.5) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  labs(title = "Frequency Distribution", x = "Each bin is a count of a start item", y = "Each histogram is an agestrat") +
  facet_wrap( ~ agestrat)
print(hist_plot)


####### SECOND SECTION OF CODE DEALS WITH STOP RULE / CEILING

# Replicate tidy input with original item names.
input_tidy_orig_names <- input_orig_names %>%
  # gather columns to express each cell as a key-value pair: col_name-cell_value
  gather(col, val, first_item_orig_name:last_item_orig_name) %>% 
  # group by IDnum, this allows a set of rows in the gathered table to be
  # identified with their origin row in the input table.
  group_by(IDnum) %>%
  # sort by IDnum
  arrange(IDnum) %>%
  # drop age column
  select(-age_in_months) %>% 
  # mutate creates new var, row_number() returns row number of input object, but
  # because object is grouped by IDnum, row_number() resets when IDnum changes to next
  # value; col_num adds 2 because in origin input table, the item columns start
  # on col 3
  mutate(col_num = row_number() + 2) 

# The purpose of the next snippet is to get the length of streak of Os below the
# last (highest) 1 response for each case.
output_ceiling <- input_orig_names[,1] %>% 
  left_join(input_tidy_orig_names) %>% 
  group_by(IDnum) %>%
  arrange(IDnum) %>%
  # Pare table to only rows containing 1 item responses
  filter(val == 1) %>% 
  # Use slice() to pare table by row position. tail() returns the last part of
  # an object, in this case, because the data are grouped by IDnum, it returns
  # the last rows of each IDnum group. Within tail(), we pass row_number(), and
  # 2 to return the last two rows within each IDnum group. These two rows
  # contain, by definition, the last two 1 responses for each IDnum.
  slice(tail(row_number(), 2)) %>% 
  # Summarize step creates columns holding the name and original input col
  # location for the highest (last) 1 and second-highest 1, for each case.
    summarize(
      last_1_name = last(col),
      last_1_loc = last(col_num),
      sec_last_1_name = first(col),
      sec_last_1_loc = first(col_num)
    ) %>% 
  # now create a new var `streak_0` that holds length of streak of 0s below
  # highest 1. First of all, there only is a streak when the column location of
  # the highest 1 is greater than the column location of the second-highest 1.
  # For these cases, the length of the streak of Os is simply the difference
  # between the two column locations, minus 1.
  mutate(
    streak_0 = case_when(
      last_1_loc > sec_last_1_loc ~ (last_1_loc - sec_last_1_loc) - 1,
      TRUE ~ NA_real_
    )
  )
  
# Next snippet gets freq counts of streak_0 accross entire sample
freq_0streak <- output_ceiling %>% 
  drop_na(streak_0) %>% 
  count(streak_0) %>% 
  mutate(
    perc = round(100*(n/sum(n)), 4), 
    cum_per = round(100*(cumsum(n)/sum(n)), 4)
  )

# after examining frequency table `freq_0streak`, designate a streak_0_length to
# implement for each case in the input data.
streak_0_length <- 4

####### THIRD SECTION OF CODE RESCORES INPUT DATA WITH STOP/START RULES APPLIED

# Next sequence rescores input data by applying start rule of streak_1_length +
# 1 and stop rule of streak_0_length + 1. The approach is to find the highest
# start rule streak for each case, and recode all 0s below this streak to 1, and
# to find the lowest stop rule streak and recode all 1s above this streak to O.
# The code accomplishes this dual-recoding in a single pass.
rescore_data <- input_tidy %>%
  # group_by(IDnum) allows you to find streaks indepently within each case.
  group_by(IDnum) %>% 
  # From the tidy input, label rows that contain correct responses (val = 1) and
  # are part of a win streak equal to or greater than streak_1_length.
  # runner::streak_run gets count, for each cell in val col, what is the current
  # length of streak of consecutive identical values? Repeat for val = 0.
  mutate(
    streak_x_1 = case_when(
      val == 1 & streak_run(val) >= streak_1_length + 1 ~ 1,
      TRUE ~ NA_real_
    ),
    streak_x_0 = case_when(
      val == 0 & streak_run(val) >= streak_0_length + 1 ~ 1,
      TRUE ~ NA_real_
    ),
    # `start_item` and `stop_item` will contain the items from which to commence
    # upward and downward rescoring (application of start/stop rules). First
    # step of isolating the start item: apply a label to only the last row of
    # streak_x, which you find by testing whether, in the lead row to any row,
    # the value of streak_x is NA. If that condition is TRUE, then the current
    # row is the last correct response of streak_x. The label you apply is the
    # column number of that row. To apply this same function for streaks of 0,
    # you change the lead() wrapper to lag(), and if this modified `is.na`
    # condition is TRUE, the current row is the first 0 response of the streak
    # of 0s.
    start_item = case_when(
      streak_x_1 == 1 & is.na(lead(streak_x_1)) ~ col_num,
      TRUE ~ NA_real_
    ),
    stop_item = case_when(
      streak_x_0 == 1 & is.na(lag(streak_x_0)) ~ col_num,
      TRUE ~ NA_real_
    )
  ) %>% 
  # To rescore according to the stop/start rules, we need all cells in the
  # `start_item` and `stop_item` columns, within each case, to hold the items
  # that are the thresholds for rescoring above and below. We use two fill()
  # steps to accomplish this, the first one replicating the existing value
  # (replacing NA) of start_item and stop_item down the table, within each case.
  fill(start_item, stop_item) %>% 
  # second fill step reverses direction and fills existing values going up the
  # table.
  fill(start_item, stop_item, .direction = "up") %>% 
  # now we use `mutate()` to recode `start_item` to its maximum value per case,
  # and stop_item` to its minimum value per case, so the start rule is applied
  # below the highest winning streak of streak_1_length +1, and the stop rule is
  # applied above the lowest losing streak of streak_0_length+1. This recode
  # step only affects cases that contain more than one value for `start_item`
  # and/or `stop_item`.
  mutate(start_item = max(
    start_item
  ),
  stop_item = min(
    stop_item
  ),
  # now we are in a position to recode all values of `val` to 1 below the
  # desired start_item, and to 0 above the desire stop item. We do this
  # squentially, by isolating rows whose col_num is less than the col_num of teh
  # start item, and recoding `val` to 1, and then isolating rows whose col_num
  # is greater than the col_num of the stop item, and recoding `val` to 0.
  val = case_when(
    col_num < start_item ~ 1,
    TRUE ~ val
  ),
  val = case_when(
    col_num > stop_item ~ 0,
    TRUE ~ val
  )
  ) %>% 
  # spread data to get total score per case with stop rule applied. First drop interim cols.
  select(-col_num, -streak_x_1, -start_item, -streak_x_0, -stop_item) %>% 
  spread(col, val) %>% 
  ungroup() %>% 
  mutate(
    TOT_rescore = rowSums(.[2:last_item+1])
  )

# compare TOT_raw and TOT_rescore
TOT_compare <- input_ID_raw_score %>% 
  full_join(rescore_data, by = 'IDnum') %>% 
  select(IDnum, age_in_months, agestrat, TOT_raw, TOT_rescore) %>% 
  group_by(agestrat)

agestrat_corr_mean_diff <- TOT_compare %>% 
  summarize(r = cor(TOT_raw, TOT_rescore),
            mean_diff = mean(TOT_raw - TOT_rescore)) %>% 
  mutate(r = round(r, 3),
         mean_diff = round(mean_diff, 3))





