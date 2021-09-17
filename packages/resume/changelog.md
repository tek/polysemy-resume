# Unreleased

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
