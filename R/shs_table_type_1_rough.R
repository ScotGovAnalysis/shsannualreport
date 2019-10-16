# library(tidyverse)
#
# design <- readRDS(design_factors_path)
# extracted_data_path <- "extracted 16-Oct-2019 12.11.53"
# extracted_dataset_path <- file.path(extracted_data_path, "dataset")
#
# df <- readRDS(file.path(extracted_dataset_path, "Figure 6.1_LA.Rds"))
#
# factor_levels <- unique(df[2])
#
# df <- subset(df, select=-c(All))
#
# df <- df %>%
#   gather(key = "Year",
#          value = "Percent",
#          `1999/2000`,
#          `2001/2002`,
#          `2003/2004`,
#          `2005/2006`,
#          `2007/2008`,
#          `2009/2010`,
#          `2011`,
#          `2012`,
#          `2013`,
#          `2014`,
#          `2015`,
#          `2016`,
#          `2017`,
#          `2018`) %>% #change the order of the data to be 'tidy', adding a new column called Percent
#   mutate(Percent = as.numeric(Percent),
#          #changes Percent column to numeric data
#          hk2 = factor(hk2,
#                       levels = c( #manually updating order of variable so plot is in correct order
#                         "Manage very well and manage quite well",
#                         "Get by alright",
#                         "Dont manage very well and have some financial difficulties",
#                         "Are in deep financial trouble",
#                         "All",
#                         "Base"
#                       )
#          )) %>%
#   group_by(Council, Year) %>%
#   mutate(n = Percent[hk2 == "Base"]) %>%
#   merge(design, by = "Year") %>%
#   dplyr::mutate(sig_value = 1.96 * Factor * (sqrt((Percent / 100) * (1 - (Percent / 100)) / n)),
#     sig_lower = Percent - (100 * sig_value),
#     sig_lower = round(sig_lower, 1),
#     sig_upper = Percent + (100 * sig_value),
#     sig_upper = round(sig_upper, 1),
#     Percent = round(Percent, 1)
#   ) %>%
#   ungroup()
#
# values_df <- df %>%
#   select("Council", "hk2","Year", "Percent") %>%
#   tidyr::spread(key = "Year", value = "Percent" )
# #
# #Do the same for lower confidence values
# sig_lower_df <- df %>%
#   select("Council", "hk2","Year", "sig_lower") %>%
#   tidyr::spread(key = "Year", value = "sig_lower" ) %>%
#   dplyr::rename(  "Council_l" = "Council",
#            "hk2_l" = "hk2",
#            `1999/2000_l`=  `1999/2000`,
#            `2001/2002_l`=  `2001/2002`,
#            `2003/2004_l`=  `2003/2004`,
#            `2005/2006_l` = `2005/2006`,
#            `2007/2008_l`= `2007/2008`,
#            `2009/2010_l`= `2009/2010`,
#            `2011_l`= `2011`,
#            `2012_l`= `2012`,
#            `2013_l`= `2013`,
#            `2014_l`= `2014`,
#            `2015_l`= `2015`,
#            `2016_l`= `2016`,
#            `2017_l`= `2017`)
#
# #And upper confidence values
# sig_upper_df <- df %>%
#   select("Council", "hk2","Year", "sig_upper") %>%
#   tidyr::spread(key = "Year", value = "sig_upper" ) %>%
#   dplyr::rename(  "Council_u" = "Council",
#            "hk2_u" = "hk2",
#            `1999/2000_u`=  `1999/2000`,
#            `2001/2002_u`=  `2001/2002`,
#            `2003/2004_u`=  `2003/2004`,
#            `2005/2006_u` = `2005/2006`,
#            `2007/2008_u`= `2007/2008`,
#            `2009/2010_u`= `2009/2010`,
#            `2011_u`= `2011`,
#            `2012_u`= `2012`,
#            `2013_u`= `2013`,
#            `2014_u`= `2014`,
#            `2015_u`= `2015`,
#            `2016_u`= `2016`,
#            `2017_u`= `2017`)
#
# #Combine the 3 datasets together to have full dataset with all significance values
# df <- bind_cols(c(values_df, sig_lower_df, sig_upper_df)) %>%
#   select(-Council_l, -Council_u, -hk2_l, -hk2_u)
#
#
# View(df)
