library(haven)

## read_dta()

# Del año 2010
famecon2010 <- read_dta("data_source/CFPS/data/2010/ecfps2010famecon_201906.dta")
famconf2010 <- read_dta("data_source/CFPS/data/2010/ecfps2010famconf_nat072016.dta")
comm2010    <- read_dta("data_source/CFPS/data/2010/ecfps2010comm_201906.dta")
child2010   <- read_dta("data_source/CFPS/data/2010/ecfps2010child_201906.dta")
adult2010   <- read_dta("data_source/CFPS/data/2010/ecfps2010adult_201906.dta")

# Dek año 2012
famecon2012     <- read_dta("data_source/CFPS/data/2012/ecfps2012famecon_201906.dta")
famconf2012     <- read_dta("data_source/CFPS/data/2012/ecfps2012famconf_092015.dta")
crossyearid2012 <- read_dta("data_source/CFPS/data/2012/ecfps2012crossyearid_032015.dta")
child2012       <- read_dta("data_source/CFPS/data/2012/ecfps2012child_201906.dta")
adult2012       <- read_dta("data_source/CFPS/data/2012/ecfps2012adult_202505.dta")

# Del año 2014
famecon2014 <- read_dta("data_source/CFPS/data/2014/ecfps2014famecon_201906.dta")
famconf2014 <- read_dta("data_source/CFPS/data/2014/ecfps2014famconf_170630.dta")
comm2014    <- read_dta("data_source/CFPS/data/2014/ecfps2014comm_201906.dta")
child2014   <- read_dta("data_source/CFPS/data/2014/ecfps2014child_201906.dta")
adult2014   <- read_dta("data_source/CFPS/data/2014/ecfps2014adult_201906.dta")

# Del año 2018
famecon2018 <- read_dta("data_source/CFPS/data/2018/ecfps2018famecon_202101.dta")
famconf2018 <- read_dta("data_source/CFPS/data/2018/ecfps2018famconf_202008.dta")
child2018   <- read_dta("data_source/CFPS/data/2018/ecfps2018childproxy_202012.dta")
adult2018   <- read_dta("data_source/CFPS/data/2018/ecfps2018person_202012.dta")

# Del año 2020
famecon2020 <- read_dta("data_source/CFPS/data/2020/ecfps2020famecon_202306.dta")
famconf2020 <- read_dta("data_source/CFPS/data/2020/ecfps2020famconf_202306.dta")
child2020   <- read_dta("data_source/CFPS/data/2020/ecfps2020childproxy_202012.dta")
adult2020   <- read_dta("data_source/CFPS/data/2020/ecfps2020person_202012.dta")
