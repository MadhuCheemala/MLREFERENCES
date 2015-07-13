#Ref:http://cran.r-project.org/web/packages/rmongodb/vignettes/rmongodb_introduction.html

library(devtools)
install_github(repo = "mongosoup/rmongodb")

library(rmongodb)
# connect to MongoDB
mongo = mongo.create(host = "localhost")

mongo

mongo.is.connected(mongo)
mongo.get.databases(mongo)


if(mongo.is.connected(mongo) == TRUE) {
  mongo.get.databases(mongo)
}

if(mongo.is.connected(mongo) == TRUE) {
  db <- "test"
  mongo.get.database.collections(mongo, db)
}

coll <- "test.zips"


if(mongo.is.connected(mongo) == TRUE) {
  help("mongo.count")
  mongo.count(mongo, coll)
}



if(mongo.is.connected(mongo) == TRUE) {
  mongo.find.one(mongo, coll)
}


if(mongo.is.connected(mongo) == TRUE) {
  res <- mongo.distinct(mongo, coll, "city")
  head(res, 2)
}



if(mongo.is.connected(mongo) == TRUE) {
  pop <- mongo.distinct(mongo, coll, "pop")
  hist(pop)
  boxplot(pop)
  
  nr <- mongo.count(mongo, coll, list('pop' = list('$lte' = 2)))
  print( nr )
  pops <- mongo.find.all(mongo, coll, list('pop' = list('$lte' = 2)))
  head(pops, 2)
}



# insert data
a <- mongo.bson.from.JSON( '{"ident":"a", "name":"Markus", "age":33}' )
b <- mongo.bson.from.JSON( '{"ident":"b", "name":"MongoSoup", "age":1}' )
c <- mongo.bson.from.JSON( '{"ident":"c", "name":"UseR", "age":18}' )

if(mongo.is.connected(mongo) == TRUE) {
  icoll <- paste(db, "test", sep=".")
  mongo.insert.batch(mongo, icoll, list(a,b,c) )
  
  dbs <- mongo.get.database.collections(mongo, db)
  print(dbs)
  mongo.find.all(mongo, icoll)
}











