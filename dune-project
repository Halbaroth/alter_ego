(lang dune 3.6)

(name alter_ego)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Pierre Villemot")

(maintainers "Pierre Villemot")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name alter_ego)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  fmt
  logs
  base
  ppx_deriving
  ppx_sexp_conv
  ppx_compare
  sexplib)
 (tags
  (topics "to describe" your project)))
