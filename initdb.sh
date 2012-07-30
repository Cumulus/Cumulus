createuser cumulus
createdb -E UTF-8 -O cumulus cumulus
psql -d cumulus < data/createdb.sql
echo "grant all privileges on database cumulus to cumulus;" | psql
