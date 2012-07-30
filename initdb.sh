createuser -P cumulus
createdb -E UTF-8 -O cumulus cumulus
psql -d cumulus -f data/createdb.sql
