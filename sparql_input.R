library("SPARQL")
library("sp")

prod_endpoint <- "http://api.talis.com/stores/jisc-prod-dev1/services/sparql"

query= "PREFIX foaf:  <http://xmlns.com/foaf/0.1/>
PREFIX prod:  <http://prod.cetis.ac.uk/vocab/>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
SELECT *
WHERE {    
 ?subject prod:comment ?comment . 
 ?subject dc:author ?author .
}"

result <- SPARQL(url=prod_endpoint, query)$results