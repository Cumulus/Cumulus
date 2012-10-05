createuser -P cumulus
createdb -E UTF-8 -O cumulus cumulus
psql -U cumulus -d cumulus -f createdb.sql
