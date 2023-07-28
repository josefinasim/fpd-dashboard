library(dm4f)

Sys.setlocale("LC_ALL", "English") #set language to English (for charts)


dm4f_dpd <- zcz_dpd()

##client type
loans_dm4f<- zcz_loans()
agr_dm4f <- zcz_agreements()
apps_dm4f <- zcz_applications()

agr <- agr_dm4f %>% 
  filter(type == 'MAIN') %>% 
  select(loan_id, application_id, type)

first_loan <-loans_dm4f %>%
  group_by(client_id) %>% 
  filter(loan_id == min(loan_id)) %>% 
  ungroup() %>% 
  
  left_join(.,agr , by = "loan_id") %>% 
  left_join(.,apps_dm4f %>% select(-client_id), by = "application_id") %>% 
  filter(resolution == 'APPROVED') %>% 
  
  select(client_id, first_approved = application_id)


client_type <- apps_dm4f %>% 
  group_by(client_id) %>% 
  filter(entity_created == max(entity_created)) %>% 
  ungroup() %>% 

  left_join(., first_loan, by = "client_id") %>% 
  mutate(client_type = if_else(application_id > first_approved, "RETURNING", "NEW")) %>% 
  
  select(client_id, client_type)
  #NAs (new unapproved/rejected clients) will be filtered out as they do not have fpd/dpd

# amount
dm4f_accounts <- zcz_accounts()




amount <- dm4f_dpd %>% 
  # filter(loan_id == 833937) %>%
  left_join(., dm4f_accounts
            %>% filter(!is.na(amount))
            , by = 'loan_id') %>% 
  
  select(loan_id, due_date, booking_date, amount) %>% 
  
  filter(due_date >= booking_date) %>%
  group_by(loan_id) %>% 
  dplyr::summarise(amount = sum(amount)) %>% 
  mutate(amount = format(round(amount,0), scientific = FALSE))
  
  

## final table
# dels1 <- dm4f_dpd %>% 
#   # filter(fpd != 0 | dpd != 0) %>% 
#   
#   left_join(., loans_dm4f %>% select(loan_id, client_id, loan_entity_created = entity_created), by = 'loan_id') %>% 
#   left_join(., client_type, by = 'client_id') %>% 
#   left_join(., amount, by = 'loan_id') %>% 
# 
#   mutate(fpd_grp = as.factor(if_else(fpd90 == 1, "fpd90",
#                            if_else(fpd30 == 1 & fpd90 == 0, "fpd30", "fpd5"))))
# 

#### charts ####

#custom order


#dataframe
# ch1 <- dels1 %>% 
#   filter(client_type == 'NEW' & 
#            fpd != 0 &
#            # due_date >= as.Date('2022-01-01') &
#            # due_date <= as.Date('2022-12-31')) %>% 
#             due_date >= as.Date('2023-01-01')) %>% 
#   select(due_date, fpd_grp) %>% 
#   group_by(month = lubridate::floor_date(as.Date(due_date), "month"), fpd_grp) %>%
#   # group_by(year = lubridate::floor_date(as.Date(due_date), "year"), fpd_grp) %>%
#   summarise(n = n())
# 
# #custom values
# custom_lims <- c(0, max(ch1$n) +10) #custom limits, ceiling = max +10
# custom_brk_incr<- if_else(max(ch1$n) > 100, 20, 10)
# 
# #ggplot
# ggplot(ch1,
#        aes(x = month, y = n, group = fpd_grp, color = fpd_grp)) +
#   
#   geom_line() +
#   geom_point()+
#   scale_y_continuous(
#     limits = custom_lims, #limits = 0 and max value +10
#     breaks = seq(custom_lims[1], custom_lims[2], custom_brk_incr), #breaks start at 0 and end at 2nd number of custom_lims
#     name = "Number of occurences", expand = c(0,0), #force start at 0
#     minor_breaks =NULL) +
#   scale_x_date(breaks = date_breaks('month'), labels = date_format('%b'), minor_breaks = NULL, name = "Month") +
#   scale_color_discrete(breaks = fpd_order, name = "FPD group")
# 
# 
# 
# ##### for dashboard test
# 
# install.packages("plotly")
# install.packages("flexdashboard")
# install.packages("shiny")
# 
# library(shiny)
# library(plotly)
# library(flexdashboard)
# #### ####
# 
# dels_yr <- dm4f_dpd %>% 
#   filter(fpd != 0) %>%
#   
#   left_join(., loans_dm4f %>% select(loan_id, client_id, loan_entity_created = entity_created), by = 'loan_id') %>% 
#   left_join(., client_type, by = 'client_id') %>% 
#   left_join(., amount, by = 'loan_id', 'due_date') %>% 
#   
#   mutate(fpd_grp = as.factor(if_else(fpd90 == 1, "fpd90",
#                                      if_else(fpd30 == 1 & fpd90 == 0, "fpd30", "fpd5")))) %>% 
#   
#   mutate(dd_year = substr(due_date, 1, 4)) %>% 
#   mutate(lc_year = substr(loan_entity_created, 1, 4)) %>% 
# arrange(desc(amount))
#   





# plot_ly(dels1 %>% 
#         group_by(dd_year, fpd_grp) %>% summarise(n = n()),
#         
#         x = ~dd_year,
#         y = n,
#         color = ~fpd_grp,
#         type = "scatter",
#         mode = "lines")

### currently using as of 20072023 ###
dels2 <- dm4f_dpd %>% 
  # filter(fpd != 0 | dpd != 0) %>% 
  
  left_join(., loans_dm4f %>% select(loan_id, client_id, loan_entity_created = entity_created), by = 'loan_id') %>% 
  left_join(., client_type, by = 'client_id') %>% 
  left_join(., amount, by = 'loan_id') %>% 
  
  mutate(fpd_grp = as.factor(if_else(fpd90 == 1, "fpd90",
                                     if_else(fpd30 == 1 & fpd90 == 0, "fpd30", 
                                             if_else(fpd5 == 1 & fpd30==0 & fpd90 == 0,"fpd5", NA))))) %>% 
  
  mutate(loan_entity_created = as.Date(as.POSIXct(loan_entity_created, tz = 'Prague'))) %>% 
  mutate(mdue_date = lubridate::floor_date(as.Date(due_date), "month")) %>% 
  mutate(mlne_created = lubridate::floor_date(as.Date(loan_entity_created), "month"))
         
dels2$mdue_date <- as.Date(dels2$mdue_date)
dels2$mlne_created <- as.Date(dels2$mlne_created)
dels2$fpd_grp <- as.factor(dels2$fpd_grp)
dels2$amount <- as.numeric(dels2$amount)
# str(dels2)


write.csv(dels2, "C:/Users/josefina.simkova/Documents/R/Delinquents/dels2.csv")
