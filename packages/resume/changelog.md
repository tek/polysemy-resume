# Unreleased

# 0.6.0.0

* Add `replaceStop`, shorthand for `mapStop . const`.
* Add `∀` to `Stop` interpreters.
* Add resumers that transform to `Fail`.
* Move `Scoped` interpreters from `polysemy-conc`, since `Scoped` is now in `polysemy`.
* Support GHC 9.4.

# 0.5.0.0

* Add interceptors.
* Change modules for effects to `Polysemy.Resume.Effect`.
* Add `stopEitherAs`.
* Add exception catching `stop` combinators.

# 0.2.0.0

* Add combinators `resumeOr` and `resumingOr`, which take an additional branch for the success case.
* Improve `raiseResumable`:
  * Don't discard resources
  * Allow interpreter to change type of `a`
* Add operator versions of `resume` (`!!`) and `resumeAs` (`<!`), (`!>`).
* Add combinators `resumeWith` and `resumingWith` that ignore the error and execute an action, plus their operator
  variants `(!>>)` and `(<<!)`.

# 0.1.0.0

* initial release
