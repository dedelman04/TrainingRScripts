constr <- paste("Server=msedxeus.database.windows.net",
		"Database=DAT209x01",
		"uid=RLogin",
		"pwd=P@ssw0rd",
		"Driver={SQL Server}",
		sep=";")

conn <- odbcDriverConnect(constr)

sqlColumns(conn, "sentiment")[c("COLUMN_NAME","TYPE_NAME")]

sqlQuery(conn, "select count(*) from bi.sentiment")

sqlQuery(conn, "select avg(score), date 
			from bi.sentiment 
			where state = 'WA'
			group by date 
			")



