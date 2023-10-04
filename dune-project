(lang dune 3.6)

(name dbf)
(version 0.2.0)
(implicit_transitive_deps false)
(generate_opam_files true)

(source (uri git+https://github.com/pveber/dbf.git))
(homepage "https://github.com/pveber/dbf")
(bug_reports "https://github.com/pveber/dbf/issues")
(license ISC)
(authors "Léo Andrès" "Philippe Veber")

(maintainers "philippe.veber@gmail.com")

(package
  (name dbf)
  (synopsis "DBF format parsing")
  (description "This is a small library meant to parse dBase database files. Many cases/variants are not handled, but it might already be useful.")
  (depends
    (cstruct-unix (>= 4.0.0))
    (ppx_cstruct (>= "4.0.0"))))
