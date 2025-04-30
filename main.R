# install.packages("tidyverse")

library(tidyverse)

############################## Extract ###############################

source("confluence.R")

############################### Test #################################

test.page <-
    pages %>%
    filter(is.latest &
           str_detect(body, "quasi-services"))

test.page %>% pull(body) %>% write("test/data/services-etc.xml")

########################## Transform, Load ###########################
# To be continued...
