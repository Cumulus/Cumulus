OPAM_DEPENDS="ocamlfind cumulus-deps"

echo "yes" | sudo add-apt-repository ppa:avsm/ocaml41+opam11
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
sudo apt-get install libpcre3-dev libssl-dev libsqlite3-dev
export OPAMYES=1
opam init
opam repository add ocsigendev https://github.com/ocsigen/opam-ocsigen.git
opam update
opam repository add cumulus https://github.com/Cumulus/opam-cumulus.git
opam install ${OPAM_DEPENDS}
eval `opam config env`
make
