# shs_process_extracted_data <- function(data_path) {
#   data_path <- readRDS(data_path)
#   data_path <- data_path %>% #creates the 'tidy' dataset
#     gather(key="Year",
#      value = "Percent",
#      X_19992000,
#      X_20012002,
#      X_20032004,
#      X_20052006,
#      X_20072008,
#      X_20092010,
#      X_2011,
#      X_2012,
#      X_2013,
#      X_2014,
#      X_2015,
#      X_2016,
#      X_2017) %>% #change the order of the data to be 'tidy', adding a new column called Percent
#     mutate(Percent = as.numeric(Percent),
#            #changes Percent column to numeric data
#            hk2 = factor(hk2,
#                         levels = c( #manually updating order of variable so plot is in correct order
#                           "Manage well",
#                           "Get by alright",
#                           "Don't manage well",
#                           "Are in deep financial trouble",
#                           "All",
#                           "Base"
#                         )
#            )) %>%
#     group_by(council, Year) %>% #groups to make new n column have different base for each LA
#     mutate(
#       n = Percent[hk2 == "Base"], #creates a new column that contains base for each LA/year
#       design_factor = case_when(
#         #add column for design factor, manually update each years' design factor
#         Year == '1999'| Year == '2000' | Year == '1999-2000' ~ 1.2,
#         Year == '2001'| Year == '2002' | Year == '2001-2002' ~ 1.2,
#         Year == '2003'| Year == '2004' | Year == '2003-2004' ~ 1.2,
#         Year == '2005' ~ 1.2,
#         Year == '2006' ~ 1.3,
#         Year == '2005-2006' ~ 1.2,
#         Year == '2007'| Year == '2008' | Year == '2007-2008' ~ 1.2 ,
#         Year == '2009'| Year == '2010' | Year == '2009-2010' ~ 1.2 ,
#         Year == '2011' ~ 1.3,
#         Year == '2012' ~ 1.19,
#         Year == '2013' ~ 1.16,
#         Year == '2014' ~ 1.15,
#         Year == '2015' ~ 1.15,
#         Year == '2016' ~ 1.16,
#         Year == '2017' ~ 1.18),
#       #create a new column that contains the sig.value (using formula)
#       sig_value = 1.96 * design_factor * (sqrt((Percent / 100) * (1 - (Percent / 100)) / n)),
#       #lower 95% CL
#       sig_lower = Percent - (100 * sig_value),
#       #round sig levels to 1 d.p more than estimate - USUALLY TO 1d.p
#       sig_lower = round(sig_lower, 1),
#       #upper 95% CL
#       sig_upper = Percent + (100 * sig_value),
#       #round sig levels to 1 d.p more than estimate - USUALLY TO 1d.p
#       sig_upper = round(sig_upper, 1),
#       #round Percent column to 1d.p
#       Percent = round(Percent, 1)
#     ) %>%
#     ungroup() %>%    #ungroups council and year
#     rename(Status = hk2) #renames the 'hk2' column to 'Status'
#
#   #create dataset in correct form for tables (without significance columns)
#   values <- data_path %>%
#     select("council", "Status","Year", "Percent") %>%
#     spread(key = "Year", value = "Percent" )
#
#   #Do the same for lower confidence values
#   sig_lower <- data_path %>%
#     select("council", "Status","Year", "sig_lower") %>%
#     spread(key = "Year", value = "sig_lower" ) %>%
#     rename(  "council_l" = "council",
#              "Status_l" = "Status",
#              `1999-2000_l`=  `1999-2000`,
#              `2001-2002_l`=  `2001-2002`,
#              `2003-2004_l`=  `2003-2004`,
#              `2005-2006_l` = `2005-2006`,
#              `2007-2008_l`= `2007-2008`,
#              `2009-2010_l`= `2009-2010`,
#              `2011_l`= `2011`,
#              `2012_l`= `2012`,
#              `2013_l`= `2013`,
#              `2014_l`= `2014`,
#              `2015_l`= `2015`,
#              `2016_l`= `2016`,
#              `2017_l`= `2017`)
#
#   #And upper confidence values
#   sig_upper <- data_path %>%
#     select("council", "Status","Year", "sig_upper") %>%
#     spread(key = "Year", value = "sig_upper" ) %>%
#     rename(  "council_u" = "council",
#              "Status_u" = "Status",
#              `1999-2000_u`=  `1999-2000`,
#              `2001-2002_u`=  `2001-2002`,
#              `2003-2004_u`=  `2003-2004`,
#              `2005-2006_u` = `2005-2006`,
#              `2007-2008_u`= `2007-2008`,
#              `2009-2010_u`= `2009-2010`,
#              `2011_u`= `2011`,
#              `2012_u`= `2012`,
#              `2013_u`= `2013`,
#              `2014_u`= `2014`,
#              `2015_u`= `2015`,
#              `2016_u`= `2016`,
#              `2017_u`= `2017`)
#
#   #Combine the 3 datasets together to have full dataset with all significance values
#   fig6.1_all <- bind_cols(c(values, sig_lower, sig_upper)) %>%
#     select(-council_l, -council_u, -Status_l, -Status_u)
#
# }
