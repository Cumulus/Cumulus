createuser cumulus
createdb -E UTF-8 -O cumulus cumulus
psql -U cumulus -f data/createdb.sql cumulus
