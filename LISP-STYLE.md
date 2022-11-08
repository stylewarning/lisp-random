# DRAFT A Lisp Style Guide DRAFT

## Syntax and Data Should Be Distinguished

Keywords that function as syntax should be written literally.

***

Keywords that function as data should be quoted.

Example:

```
(foo :arg ':val)
```

***

Quasiquote syntax should be reserved for templating valid Common Lisp syntax.

***

Quasiquote syntax should be avoided to build list-based data (e.g., alists).

***

Use quasiquote (instead of quote) even when there is no unquote if Common Lisp forms are being written.


## Side-Effects Should Be Treated Specially

In most cases, avoid side effects. In the majority of the remaining cases, keep side effects local.

***

Side effects should *not* be avoided if performance or clarity are gravely affected.

***

Never use `if` if the branches contain side effects. Always use `cond`, `when`, or `unless`.

***

Avoid relying on the return value of a side effect. Repeat yourself if necessary.

Example:

```
(defun f (x)
  (setf x 5)
  x)
```

## Docstrings

Document anything substantial (functions, classes, etc.), especially with complicated APIs, with a docstring.

***

In a function docstring, every argument should be referenced by name in all-caps in a docstring, along with its type (either formally or informally), default values, as well as restrictions on its values. Describe the return values, possibly using an enumerated list.

***

Docstrings should be written in full English sentences with proper capitalization and punctuation. Docstrings can be written as a series of paragraphs.

***

Lines should not be broken in a docstring.


## The Structure of a Function

Use `assert` or `when`-`error` to detect early invariant violations.

***

Prefer an expression-oriented style for function implementations.

***

Restrict the use of `return-from` only when the equivalent expression-oriented code is obtuse or difficult to follow.

***

Do not use `&optional` with `&rest` or `&key`.

***

A function is never permitted to return different numbers of values. All returns should have the same shape.

Example:

```
;;; Wrong:

(defun f (x)
  (if (zerop x)
      0
      (values x 1)))

;;; Right:
(defun f (x)
  (if (zerop x)
      (values 0 nil)
      (values x 1)))
```

***

Functions may be tail recursive.

## The Role of `nil`

The value of `when`, `unless`, an empty `cond`, etc. should never be relied upon. Always be explicit when returning `nil`.

Example:

```
;;; Wrong:
(cond
  ((= 1 x) ':true)
  ((= 2 x) ':false))

;;; Right:
(cond
  ((= 1 x) ':true)
  ((= 2 x) ':false)
  (t       nil))
```

## Namespace Pollution

When possible, do not use `flet`, `labels`, `let`, `macrolet`, etc. at the toplevel. Prefer global definitions, prepending `%` to signify private usage.

***

Avoid `intern` at run-time. Use `find-symbol` if an existing symbol is needed.

***

Don't use the keyword package, or `cl-user`, as a place for generated symbols.

***

Bulk generated, interned symbols used for a specific purpose should be home to a special package designated for such.

***

When reasonable, prefer a global function to an `flet` or `labels`.

***

Subordinate functions to a principal function should be prepended with a `%`.


## Iteration and `LOOP` style

Loop keywords should be keywords, and that includes `:=`.

***

A `let` that solely wraps a `loop` should be folded into the loop with `:with`.

***

A `loop` that is followed by a return value `v` should be rewritten to `:finally (return v))`.

***

A `loop` with a single clause followed by `:do` should be one line, with the body forms following:

```
(loop :for k :being :the :hash-keys :of ht :do
  something
  something
  something)
```

***

Prefer `dolist` over a single `loop`-`:for`-`:in`.

***

Prefer `dotimes` over a single `loop`-`:for`-`:below`.

***

Prefer `loop` over `do` or `do*`.

***

Only use `mapc` or `map nil` when a named function is in scope.

***

Sparing use of `map nil` with a `lambda` argument can be used if the sequence type needs to be general. Prefer, however, having an abstraction called `doseq`:

```
(doseq (x seq ret)
  body)

;;; ==>

(progn
  (map nil (lambda (x) body) seq)
  ret)
```

## Macros

Do not use a macro when a function can do the same job.

***

Do not use a macro when an inlined function can do the same job.

***

If a macro is just compressing code size, it's probably wrong.

***

If a macro reflects how you would have designed Lisp (e.g., `let+`, `defun*`, etc.), it's probably wrong.

***

Every macro that introduces new syntax must be accompanied with a comprehensive description of the syntax in the docstring, including examples.

***

Any macro that does S-expression parsing must be carefully parsed so as to produce good error messages.


## Efficiency

Localize unsafe declarations to the smallest possible area.

***

If an entire file needs to have an unsafe declaration, try to minimize its contents.

***

Prefer `declaim` to `proclaim`.

***

Declarations that are repeatedly used should be put into a variable.

Example:

```
(defparameter *fast-unsafe* '((speed 3) (safety 0) (debug 0)))

;;; later

(declaim #.*fast-unsafe*)

;;; or

(defun f ()
  (declaim #.*fast-unsafe*)
  ...)
```

## Structures and Classes

Structure slots should be marked as `:read-only t` whenever possible.

***

Class slots should only expose a reader if an accessor is not needed.

If an accessor is private but a reader is public, provide both with appropriate naming.

Example:

```
(defgeneric x (obj)
  (:documentation "A protocol method for ..."))

(defclass foo ()
  ((x :initarg :x
      :reader foo-x   ; provides a direct reader
      :reader x       ; implements a protocol
      :accessor %x))) ; for internal use only
```

***

In general, use `:default-initargs` instead of `:initform`.


## Nits

Follow ANSI comment style with only minor deviations.

```
;;;; File header

(defvar *blah*)

;;; Top-level comment

(defun f ()
  ;; In-form comment
  'done)

(f)                  ; Line comment
```

***

At the top of every file, write:

```
#### filename.lisp
####
#### Author: Alyssa Hacker

(defpackage #:file-package
  (:use #:cl))

(in-package #:file-package)

;;; A long-form descriptipn of what this file is for.
;;; This will span multiple lines and possibly multiple
;;; paragraphs.
;;;
;;; Paragraph separations still need the triple-semicolon.
```

The `defpackage` is only needed if the project is roughly following a one-package-per-file rule.

***

Never give a package a global nickname. Assume (and use) package-local nicknames.

***

Never write `#'` in front of functions.

***

Use `#+#:comment`, where `comment` can be a descriptive word, to comment out the succeeding form. Do not use `#+(or)`, `#+(and)`, or variants.

***

Add parentheses to `case`-related clauses.

Example:

```
;;; Wrong:
(case x
  (1 "one")
  (2 "two")
  (t "idk"))

;;; Right
(case x
  ((1)       "one")
  ((2)       "two")
  (otherwise "idk"))
```

***

If `cond`, `case`, or `typecase` clauses all fit on one short line, then align them nicely. If not, then put each branch on its own line.

***

Always include an `otherwise` case in `case`-related forms if the value of the `case` expression is depended upon.

***

Do not write `progn` inside of `cond`, `case`, or `typecase` branches.

***

Prefer `case` with an `otherwise`-`error` if you have a better error message than `ecase` could supply.

***

In `defpackage` and `in-package`, use `#:uninterned-symbols` for the package names.

***

Uninitialized `let` variables should be written at the end of the binding list without parentheses. If a variable is initialized to `nil`, it is presumed that `nil` is meaningful.

```
;;; Wrong:
(let (w
      (x 1)
      (y nil)   ; initialized later
      (z))
  ...)

;;; Right:
(let ((x 1)
      w y z)
  ...)

;;; or:

(let ((x 1)
      w
      y
      z)
  ...)
```

***

Do not use an uninitialized `let` variable for its `nil` value. Instead, bind it to `nil`.

***

If you're operating on a list data structure, use: `first`, `rest`, `nth`, `endp`. Either `length` or `list-length` may be used.

***

If you're operating on a cons data structure, use: `car`, `cdr`, `nthcdr`, `null`.

***

Do not use `let*` gratuitously. Do not write `let*` when `let` will do.

***

When possible, prefer to write constants first in comparisons.

Example:

```
;;; Wrong:
(= x 1)

;;; Right:
(= 1 x)
```

***

In general, strive for parentheses to stack at the end of the line. In most cases, if there is a close parenthesis that is followed by anything but a close parenthesis, it should likely be followed by a newline.

***

Be generous with breaking expressions up with newlines.

***

For half-open inequalities, write them in "mathematical order":

Example:

```
(and (< 1 x) (<= x 5))
```

***

Almost always prefer `plusp`, `minusp`, and `zerop` to their inequality counterparts, unless inequality operators would be more consistent with the surrounding context.

***

Align keyword arguments in a function call.

***

If a function relies on the value of a special variable, consider binding it as a keyword argument instead of just using it implicitly.

Example:

```
(defun foo (x &key (stream *standard-output*))
  ...)
```

