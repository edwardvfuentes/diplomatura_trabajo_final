library(haven)


# Del año 2010
famecon2010 <- read_dta("data_source/CFPS/data/2010/ecfps2010famecon_201906.dta")
famconf2010 <- read_dta("data_source/CFPS/data/2010/ecfps2010famconf_nat072016.dta")

# Dek año 2012
famecon2012 <- read_dta("data_source/CFPS/data/2012/ecfps2012famecon_201906.dta")
famconf2012 <- read_dta("data_source/CFPS/data/2012/ecfps2012famconf_092015.dta")

# Del año 2014
famecon2014 <- read_dta("data_source/CFPS/data/2014/ecfps2014famecon_201906.dta")
famconf2014 <- read_dta("data_source/CFPS/data/2014/ecfps2014famconf_170630.dta")
comm2014    <- read_dta("data_source/CFPS/data/2014/ecfps2014comm_201906.dta")

# Del año 2016
famecon2016 <- read_dta("data_source/CFPS/data/2016/ecfps2016famecon_201807.dta")
famconf2016 <- read_dta("data_source/CFPS/data/2016/ecfps2016famconf_201804.dta")

# Del año 2018
famecon2018 <- read_dta("data_source/CFPS/data/2018/ecfps2018famecon_202101.dta")
famconf2018 <- read_dta("data_source/CFPS/data/2018/ecfps2018famconf_202008.dta")

# Del año 2020
famecon2020 <- read_dta("data_source/CFPS/data/2020/ecfps2020famecon_202306.dta")
famconf2020 <- read_dta("data_source/CFPS/data/2020/ecfps2020famconf_202306.dta")
adult2020   <- read_dta("data_source/CFPS/data/2020/ecfps2020person_202306.dta")


# Del año 2022
famecon2022 <- read_dta("data_source/CFPS/data/2022/ecfps2022famecon_202410.dta")
famconf2022 <- read_dta("data_source/CFPS/data/2022/ecfps2022famconf_202410.dta")