# Solveur DPLL récursif

## Members

- Mohamed Laidouni
- Ayman Khemakhem

## Development environment setup

Install [Opam](https://opam.ocaml.org/doc/Install.html), the OCaml
package manager, on your system. Since your system runs
[Guix](https://guix.gnu.org/), you can easily obtain a suitable
throw-away programming environment using

```
$ guix shell -m .guix/manifest.scm
```

For convenience, we setup a [local](https://opam.ocaml.org/blog/opam-local-switches/) Opam distribution, using the following commands:

```
$ opam switch create .
$ eval $(opam env)
```
