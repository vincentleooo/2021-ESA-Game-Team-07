# Supposed to hold SQL queries, but instead is merged mostly into functions.R

createNewPlayerQuery <- function(conn,user_id,password) {
  querytemplate <-
    "INSERT INTO User (username,password) VALUES (?id1,?id2);"
  query <- sqlInterpolate(conn, querytemplate, id1 = user_id, id2 = password)
}