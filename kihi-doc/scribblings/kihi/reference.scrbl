#lang scribble/manual

@title[#:tag "reference" #:style 'toc]{The Kihi Reference}

@local-table-of-contents[]

@(define-syntax-rule (kihi forms ...)
   (racket forms ...))


@section[#:tag "Core"]{Core}

@subsection[#:tag "primitive"]{Primitives}

The entire semantics of Kihi depend on five primitive operations.  All
of the other program and data definitions in the language can be
represented as some combination of the following programs.

@defthing[apply (∀ (s t) ((s → t) s → t))]{
  Apply a program to the state.

  The contents of the program are spliced onto the top of the execution
  stack, causing the program to output any values it contains and apply
  any sub-programs that appeared inside.
}

@defthing[before (∀ (A s t) ((A s → t) A → (s → t)))]{
  Partially apply a program to a value.

  Consumes a value from directly below a program, and inserts the value
  at the end of the program's execution.  If the program will consume
  input of its own, then this is equivalent to partially applying the
  program to one value.  If not, then the value will be left under the
  results of the program when the program is applied.
}

@defthing[after (∀ (A s t) ((s → t) A → (s → A t)))]{
  Partially return a value from a program.

  Consumes a value from directly below a program, and inserts the value
  at the beginning of the program's execution.  When the program is
  applied, the value will appear before the program's original output.
}

@defthing[copy (∀ (A) (A → A A))]{
  Duplicate a value.
}

@defthing[drop (∀ (A) (A →))]{
  Discard a value.
}


@subsection[#:tag "stack"]{Stack}

Consumption of values is ordered like a stack, and so the values can be
manipulated like a data structure.  The following programs change the
order of values or apply the primitive operations in different locations
than the head of the stack.

@defthing[swap (∀ (A B) (A B → B A))]{
  Swap the order of two values.
}

@defthing[apply-with (∀ (A s t) (A (A s → t) s → t))]{
  Apply a program that is under its own first argument.
}

@defthing[under (∀ (A s t) ((s → t) A s → A t))]{
  Apply a program under the value directly after it.
}

@defthing[under₂ (∀ (A B s t) ((s → t) A B s → A B t))]{
  Apply a program under the two values directly after it.
}


@defthing[over (∀ (A B s t) ((s → B t) A s → B A t))]{
  Apply a program under the value directly after it, but then lift the
  first result of the program over the value it was executed under.
}

@defthing[swap-over (∀ (A B C) (A B C → C A B))]{
  Swap a value over two others to be first in the order.
}

@defthing[swap-under (∀ (A B C) (A B C → B C A))]{
  Swap a value under two others to be third in the order.
}

@defthing[copy-over (∀ (A B) (A B → B A B))]{
  Copy a value over another to be both first and third in the order.
}

@defthing[copy-under (∀ (A B) (A B → A B A))]{
  Copy a value under another to be both first and third in the order.
}


@subsection[#:tag "program"]{Programs}

@defthing[program? (∀ (A) (A → Boolean))]{
  Determine if a value is a program.
}

@defthing[compose (∀ (s t u) ((t → u) (s → t) → (s → u)))]{
  Take two programs, and return a program that applies both.
}

@defthing[flip (∀ (A B s t) ((A B s → t) → (B A s → t)))]{
  Swap the order of the first two inputs of a program.

  Equivalent to composing the program with @kihi[(swap)].
}

@defthing[join (∀ (A s t) ((A A s → t) → (A s → t)))]{
  Duplicate the first input of a program.

  Equivalent to composing the program with @kihi[(copy)].
}

@defthing[Y (∀ (A s t) (((s → t) s → t) → (s → t)))]{
  The Y combinator.  Apply a program to the result of this application.

  This allows a program to refer to itself, as its first input will be
  its own definition, with the same value bound in the same way inside
  of itself.
}


@section[#:tag "syntax"]{Syntax}

@subsection[#:tag "binding"]{Binding}

@defform[(define (f x ...) (t ...))]{
  Define a program and bind it to a name @kihi[f].  Invoking the name
  @kihi[f] will execute the program @kihi[(t ...)].  If extra names
  @kihi[x ...] are defined, the program will also consume the same
  number of values before executing and pointwise bind those names to
  the values.
}

@defform[(bind (x ...) (t ...))]{
  Consume as many values as there are names @kihi[x ...], and then
  apply the following program @kihi[(t ...)] with the names pointwise
  bound to the values.
}

@defform[(let ([(x ...) t ...] ...) (t ...))]{
  Like @kihi[bind], but the bound values are defined directly after the
  names they are bound by rather than the program they are bound in.
  Multiple different sets of bindings can be made at once, and bound
  names are also available in the bindings the follow them.

  The number of names in a binding does not need to match the number of
  values returned by the terms that follow it, so bindings can also
  share values through the stack: the bindings @kihi[([(x y) a b])],
  @kihi[([() a b] [(x y)])], and @kihi[([(x) a b] [(y)])] are all
  equivalent, binding @kihi[x] to @kihi[a] and @kihi[y] to @kihi[b].
}


@section[#:tag "data"]{Data Structures}

@subsection[#:tag "boolean"]{Booleans}

@defthing[boolean? (∀ (A) (A → Boolean))]{
  Determine if a value is a boolean.
}

@defthing[if (∀ (s t) (Boolean (s → t) (s → t) s → t))]{
  Branch on the value of a boolean.
}

@defthing[and (Boolean Boolean → Boolean)]{
  Returns true if both of its inputs are true.
}

@defthing[or (Boolean Boolean → Boolean)]{
  Returns true if either of its inputs are true.
}



@subsection[#:tag "number"]{Numbers}

@defthing[number? (∀ (A) (A → Boolean))]{
  Determine if a value is a number.
}

@defthing[+ (Number Number → Number)]{
  Binary addition.
}

@defthing[- (Number Number → Number)]{
  Subtraction.
}

@defthing[* (Number Number → Number)]{
  Binary multiplication.
}

@defthing[/ (Number Number → Number)]{
  Division.
}


@subsection[#:tag "string"]{Strings}

@defthing[string? (∀ (A) (A → Boolean))]{
  Determine if a value is a string.
}

@defthing[string/length (String → Number)]{
  The number of grapheme clusters in a string.
}

@defthing[string/append (String String → String)]{
  Append two strings together into a single string.
}


@subsection[#:tag "pair"]{Pairs}

Pairs are less valuable than in languages where functions can only
return one value, but it is still useful to be able to generically group
values together.  A pair can be directly deconstructed with
@kihi[unpair], which returns both of the values in the pair.

@defthing[pair (∀ (A B) (A B → Pair A B))]{
  Construct a pair from the top two values.
}

@defthing[unpair (∀ (A B) (Pair A B → A B))]{
  Deconstruct a pair into its two values.
}

@defthing[pair? (∀ (A) (A → Boolean))]{
  Determine if a value is a pair.
}

@defthing[first (∀ (A B) (Pair A B → A))]{
  Retrieve the first value of a pair.
}

@defthing[second (∀ (A B) (Pair A B → B))]{
  Retrieve the second value of a pair.
}

@defthing[pair/fold (∀ (A B s t) ((A B s → t) Pair A B s → t))]{
  Consume a pair and apply a program to its values.
}

@defthing[pair/map (∀ (A B C D s t)
                      ((A → C) (B → D) Pair A B → Pair C D))]{
  Transform each value of a pair with a corresponding program.
}


@subsection[#:tag "option"]{Options}

An option represents the potential for a missing value.  Either there is
@kihi[some] value, or there is @kihi[none].  The value cannot be
accessed directly, since it might not exist, but you can apply a program
to the value on the condition that the value exists with
@kihi[option/fold].

@defthing[none (∀ (A) (→ Option A))]{
  The empty option value.
}

@defthing[some (∀ (A) (A → Option A))]{
  Construct an option containing a value.
}

@defthing[option? (∀ (A) (Option A → Boolean))]{
  Determine if a value is an option.
}

@defthing[none? (∀ (A) (Option A → Boolean))]{
  Determine if a value is an empty option.
}

@defthing[some? (∀ (A) (Option A → Boolean))]{
  Determine if a value is a non-empty option.
}

@defthing[option/fold (∀ (A s) ((A s → s) Option A s → s))]{
  Consume an option and apply a program to the value if the value is
  present.
}

@defthing[option/map (∀ (A s) ((A → B) Option A → Option B))]{
  Transform the value in an option with a program if the value is
  present.
}


@subsection[#:tag "list"]{Lists}

@defthing[nil (∀ (A) (→ List A))]{
  The empty list value.
}

@defthing[cons (∀ (A) (A List A → List A))]{
  Add a value to the head of a list.
}

@defthing[list/fold (∀ (A s) ((A s → s) List A s → s))]{
  Consume a list and apply a program to every value that was in the
  list.
}

@defthing[list/map (∀ (A B) ((A → B) List A → List B))]{
  Transform the values in a list with a program.
}

@defthing[list/append (∀ (A) (List A List A → List A))]{
  Append two lists together to form a single list.
}

@defthing[repeat (∀ (A) (Number A → List A))]{
  Make a list that repeats a value a certain number of times.
}
