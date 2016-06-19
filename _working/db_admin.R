

creds <- list(uid = "XXX", pwd = "XXX")
PlayPen <- dbConnect(RMySQL::MySQL(), username = creds$uid, password = creds$pwd)
dbSendQuery(PlayPen, "CREATE USER 'analyst'@'localhost';")
dbSendQuery(PlayPen, "GRANT SELECT ON * . * TO 'analyst'@'localhost';")
