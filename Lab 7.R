my.data.frame<-sqlQuery(conn, 
		"SELECT SUM(Revenue), SUM(Units), ProductID
            FROM bi.salesFact
            WHERE Date > '2013-12-31' AND Date < '2015-01-01'
            GROUP BY ProductID")

sqlQuery(conn, "Select top 5 sum(units), ProductID
		FROM bi.salesFact
		where Date > '2013-12-31' and date < '2015-01-01'
		group by productID
		order by sum(units) desc")

sqlQuery(conn, "Select top 5 sum(revenue), ProductID
		FROM bi.salesFact
		where Date > '2013-12-31' and date < '2015-01-01'
		group by productID
		order by sum(revenue) desc")