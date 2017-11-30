# import asthma related tweets
con <- dbConnect(MySQL(),
                 user = 'kimchon',
                 password = 'CkMj1527!%@&',
                 host = '140.226.57.147',
                 dbname='testdb',
                 port =3306)

twitter <- dbReadTable(conn = con, name = 'twitter')
write.csv(twitter, "twitter.csv")
dbDisconnect(con)