# Roxygen convention for R6 Classes
## Based on patterns in mlr3, torch, and httr2

---

## Overview

Documenting R6 classes with roxygen2 requires a different mental model than documenting ordinary
functions. This document was prepared based on review of **mlr3** (heavily R6-centric ML framework),
**torch** (neural network modules), and **httr2** (OAuth client objects) — represent a spectrum
of approaches, from mlr3's exhaustive, formally codified system to torch's leaner PyTorch-translated
style to httr2's functional/tidyverse wrapper approach. The patterns that appear across all three
are an attempt for a well rounded convention for roxygen2 use in pizzarr.

---

## 1. Enable Markdown and R6 Support in DESCRIPTION

All three packages set this in `DESCRIPTION`:

```
Roxygen: list(markdown = TRUE, r6 = TRUE)
```

The `r6 = TRUE` flag (available in roxygen2 7.0+) enables first-class roxygen support for R6:
`@field` tags are recognized, and roxygen generates proper documentation sections for public
fields, active bindings, and methods automatically. Without it you must fake everything with
`@section`. Enable both options — there is no reason not to.

---

## 2. Top-Level Class Header

Place a single roxygen block above the `R6Class()` call (or the `nn_module()` / constructor
function call in torch) covering the whole class. Use explicit tags rather than relying on
positional parsing:

```r
#' @title Param Object
#'
#' @description
#' Abstract base class for parameters. Describes the domain of a single
#' hyperparameter. Concrete subclasses include [ParamDbl] and [ParamInt].
#'
#' @format [R6::R6Class] object inheriting from [BaseClass].
#'
#' @family Parameter classes
#' @export
```

**Key decisions visible across packages:**

- **`@title`** is always explicit. mlr3's style guide mandates title case (use
  `tools::toTitleCase()` as a check). torch module titles tend to be short noun phrases
  matching PyTorch convention (e.g., "Linear module").
- **`@description`** is always on its own tag line, not fused into the implicit first paragraph,
  which makes multi-paragraph descriptions easier to maintain.
- **`@format`** declares the inheritance chain. mlr3 does this consistently; torch often omits
  it for leaf modules because the inheritance is implied.
- **`@usage NULL`** is recommended by mlr3 (and commonly seen in httr source) to suppress the
  unhelpful R6 "Usage" section. With `r6 = TRUE` set in DESCRIPTION this is often no longer
  needed, but check what roxygen generates for your version.
- **`@family`** groups related classes so "See also" cross-references are generated
  automatically. mlr3 uses this aggressively (e.g., `@family Task`, `@family OAuth`). httr2
  uses `@family OAuth` on all OAuth-related functions and classes.

---

## 3. Type Annotation Convention

The most consistent cross-package pattern is the **backtick-typed parameter** idiom:

```r
#' @param id (`character(1)`)\cr
#'   Identifier of the object.
#' @param special_vals (`list()`)\cr
#'   Arbitrary special values. Extends the domain of the parameter.
#' @param other_class ([ParamSet])\cr
#'   A linked class; will be cross-referenced automatically.
```

The anatomy is:
- **Name** — plain, no backticks
- **Type** — in backticks and parentheses, using R constructor notation for scalars
  (`character(1)`, `logical(1)`, `integer()`), or `[ClassName]` for R6/S3 objects with a
  hyperlink
- **`\cr`** — hard line break separating the type annotation from the description
- **Two-space indent** on the description continuation line (mlr3 standard; torch uses one space
  but the `\cr` pattern is the same)

For alternative types use `|` with spaces: `` (`integer() | character()`) ``

httr2 uses a looser convention on simple functions (just plain prose after `@param name`)
but tightens up to the type-annotated pattern on any function that takes R6 objects as arguments.

**End every `@param` and `@field` with a period.** mlr3 enforces this explicitly; torch
follows it; httr2 does too on complex parameters.

---

## 4. Documenting Fields

With `r6 = TRUE`, use `@field` directly above the field declaration inside the `public` list:

```r
Param = R6Class("Param",
  public = list(
    #' @field id (`character(1)`)\cr
    #' Identifier of the object.
    id = NULL,

    #' @field tags (`character()`)\cr
    #' Arbitrary tags for grouping and subsetting.
    tags = NULL,
  ),
  ...
)
```

**Rules:**
- Keep field descriptions short. If a field is set in `initialize()`, the constructor's
  `@param` block should carry the detailed explanation; the `@field` entry just confirms the
  type and gives a one-liner.
- Active bindings use `@field` too (roxygen2 distinguishes them automatically from the `active`
  list placement). Append "Read-only." when appropriate:

```r
  active = list(
    #' @field class (`character(1)`)\cr
    #' R6 class name. Read-only.
    class = function() class(self)[[1L]],
  )
```

---

## 5. Documenting the Constructor (`initialize`)

mlr3 and torch both place the constructor block directly above the `initialize` function
definition, not at the top of the class:

```r
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' Note that this object is typically constructed via derived classes,
    #' e.g., [ParamDbl].
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier of the object.
    #' @param default (`any`)\cr
    #'   Default value. Can be `NULL`.
    initialize = function(id, default = NO_DEF) {
      ...
    },
```

The `[R6][R6::R6Class]` cross-reference pattern links the text "R6" to the `R6Class`
help page — a recurring convention in mlr3 to make class lineage navigable.

**For torch**, which wraps R6 construction behind `nn_module()`, the constructor arguments
are documented at the top-level roxygen block for the factory function rather than inline,
since users never call `$new()` directly:

```r
#' Linear module
#'
#' Applies a linear transformation: `y = xA^T + b`
#'
#' @param in_features size of each input sample
#' @param out_features size of each output sample
#' @param bias If `FALSE`, no bias term is added. Default: `TRUE`
#'
#' @export
nn_linear <- nn_module(...)
```

This is a meaningful divergence: when R6 is hidden behind a functional API, document
the function, not the class.

---

## 6. Documenting Methods

Methods go inline, directly above the method definition:

```r
    #' @description
    #' Repeats this parameter n times by cloning.
    #' Each clone is named `"[id]_rep_[k]"`.
    #'
    #' @param n (`integer(1)`)\cr
    #'   Number of repetitions.
    #' @return [ParamSet].
    rep = function(n) {
      ...
    },
```

**Rules:**
- Every non-trivial method gets `@description`, `@param` for each argument, and `@return`.
- `@return` uses `[ClassName]` for R6 returns, `self` (as plain text) for mutating methods
  that return invisibly, and `` `NULL` `` for side-effect-only methods like `print`.
- Nullary methods (no arguments) still get `@description` and `@return`.
- Do **not** re-document inherited methods. Neither mlr3 nor httr2 use `@inheritParams` to pull
  in superclass method docs — inherited methods simply aren't documented again in subclasses.
  The superclass page is the authoritative source.

---

## 7. Custom `@section` Blocks

torch uses `@section` to document things that have no direct roxygen analogue, particularly
tensor shape contracts and module attributes — a pattern borrowed from the PyTorch documentation
format:

```r
#' @section Shape:
#'
#' - Input: `(N, *, H_in)` where `H_in = in_features`
#' - Output: `(N, *, H_out)` where `H_out = out_features`
#'
#' @section Attributes:
#'
#' - `weight`: learnable weights, shape `(out_features, in_features)`
#' - `bias`: learnable bias, shape `(out_features)`
```

mlr3's older style guide used `@section` for the constructor, fields, and methods before
`r6 = TRUE` existed. If you are on an older roxygen2 that lacks R6 support, `@section` is
the fallback for documenting fields.

**When to use `@section` today:**
- Domain-specific structural information (shapes, invariants, mathematical properties)
- Notes that apply to the class as a whole but don't fit `@description` (e.g., thread safety,
  reference semantics warnings)
- Anything the standard `@field`/method machinery doesn't capture

---

## 8. Cross-Linking

All three packages use markdown cross-links rather than `\link{}` Rd syntax:

| Pattern | When to use |
|---|---|
| `[ClassName]` | Any exported class, function, or dataset in the same package |
| `[pkg::function()]` | Exported functions from other packages (e.g., `[data.table::data.table()]`) |
| `[R6::R6Class]` | Linking to R6 itself to clarify the inheritance chain |
| `\CRANpkg{pkgname}` | Referring to CRAN packages in prose (mlr3 convention) |
| `` `character(1)` `` | Scalar types — backtick only, no cross-link |

**Link everything that is not a base R type.** mlr3's guide is explicit: character(), logical(),
integer() get no link; everything else does. Don't link the same target twice on the same
`@param` line.

---

## 9. `@include` for Collation Order

When a class inherits from another class defined in a different file in the same package,
control the `Collate:` order with:

```r
#' @include ParamSet.R
```

This is essential for avoiding "object not found" errors when the package is loaded from source.
Torch and mlr3 both use it at the top of any file that depends on classes in other files.

---

## 10. Templates for Repeated Documentation

mlr3 stores reusable roxygen fragments in `man-roxygen/` and references them with:

```r
#' @template param_id
#' @template param_backend
```

Use this pattern when five or more different classes share identical `@param` or `@field`
descriptions (e.g., an `id` argument that means the same thing everywhere). httr2 uses
`@inheritParams` for shared function parameters rather than templates, which is simpler
for functional APIs but less suitable for R6 where methods are not exported individually.

---

## 11. Examples

All three packages agree that examples should:
- Be runnable without side effects (no network, no file writes, no printing large objects)
- Be guarded with `if (pkg_is_available())` when the class requires optional dependencies
  (torch does this: `if (torch_is_installed()) { ... }`)
- Demonstrate construction and at least one meaningful operation
- Avoid being exhaustive — one clear, minimal illustration beats five edge cases

```r
#' @examples
#' p <- ParamDbl$new("x", lower = 0, upper = 1)
#' p$id
#' p$is_bounded
```

---

## 12. Export and Visibility

```r
#' @export                          # Public-facing class
#' @export                          # plus:
#' @keywords internal               # Visible to other packages but hidden from ?-help index
```

Internal classes (used by other packages but not end users) get both tags. Pure internal
implementation details get neither and are not documented with a top-level roxygen block at all —
but mlr3 recommends following the same inline style for private fields and methods even without
tags, for developer readability.

---

## Quick Reference Checklist

```
Top of class file:
  ☐ @title (title case)
  ☐ @description (explicit tag)
  ☐ @format (R6Class or inheritance chain)
  ☐ @family (if part of a group)
  ☐ @export (or @keywords internal)
  ☐ @include (if inheriting across files)

Constructor (initialize):
  ☐ @description "Creates a new instance of this [R6][R6::R6Class] class."
  ☐ @param name (`type(n)`)\cr  description.  (period at end)
  ☐ @return omitted (constructor returns self implicitly)

Public fields:
  ☐ @field name (`type`)\cr  Short description.

Active bindings:
  ☐ @field name (`type`)\cr  Description. Read-only. (if read-only)

Methods:
  ☐ @description
  ☐ @param name (`type`)\cr  description.
  ☐ @return [Type] or self or `NULL`

DESCRIPTION:
  ☐ Roxygen: list(markdown = TRUE, r6 = TRUE)
```
