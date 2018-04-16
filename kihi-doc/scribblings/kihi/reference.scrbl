#lang scribble/manual

@(require (for-label kihi/main))

@title[#:tag "reference" #:style 'toc]{The Kihi Reference}

@local-table-of-contents[]

@(define-syntax-rule (kihi forms ...)
   (racket forms ...))


@section[#:tag "Core"]{Core}

@subsection[#:tag "primitive"]{Primitives}

The entire semantics of Kihi depend on five primitive operations.  All
of the other procedure and data definitions in the language can be
represented as some combination of the following procedures.

@defthing[apply (∀ (s t) ((s → t) s → t))]{
  Apply a procedure to the state.

  The contents of the procedure are spliced onto the top of the
  execution stack, causing the procedure to output any values it
  contains and apply any sub-procedures that appeared inside.
}

@defthing[left (∀ (A s t) ((s → t) A → (s → A t)))]{
  Partially return a value from a procedure.

  Consumes a value from directly below a procedure, and inserts the
  value at the beginning of the procedure's execution.  When the
  procedure is applied, the value will appear before the procedure's
  original output.
}

@defthing[right (∀ (A s t) ((A s → t) A → (s → t)))]{
  Partially apply a procedure to a value.

  Consumes a value from directly below a procedure, and inserts the
  value at the end of the procedure's execution.  If the procedure will
  consume input of its own, then this is equivalent to partially
  applying the procedure to one value.  If not, then the value will be
  left under the results of the procedure when the procedure is applied.
}

@defthing[copy (∀ (A) (A → A A))]{
  Duplicate a value.
}

@defthing[drop (∀ (A) (A →))]{
  Discard a value.
}

@subsection[#:tag "stack"]{Stack}

Consumption of values is ordered like a stack, and so the values can be
manipulated like a data structure.  The following procedures change the
order of values or apply the primitive operations in different locations
than the head of the stack.

@defthing[swap (∀ (A B) (A B → B A))]{
  Swap the order of the first and second values.
}

@defthing[swap (∀ (A B) (A B C → C B A))]{
  Swap the order of the first and third values.
}

@defthing[apply-with (∀ (A s t) (A (A s → t) s → t))]{
  Apply a procedure that is under its own first argument.
}

@defthing[under (∀ (A s t) ((s → t) A s → A t))]{
  Apply a procedure under the value directly after it.
}

@defthing[under₂ (∀ (A B s t) ((s → t) A B s → A B t))]{
  Apply a procedure under the two values directly after it.
}


@defthing[over (∀ (A B s t) ((s → B t) A s → B A t))]{
  Apply a procedure under the value directly after it, but then lift the
  first result of the procedure over the value it was executed under.
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

@defthing[split (∀ (A B) ((A → B) (A → C) A → B C))]{
  Apply two procedures to a single value.
}

@subsection[#:tag "procedure"]{Procedures}

@defthing[procedure? (∀ (A) (A → Boolean))]{
  Determine if a value is a procedure.
}

@defthing[compose (∀ (s t u) ((t → u) (s → t) → (s → u)))]{
  Take two procedures, and return a procedure that applies both.
}

@defthing[flip (∀ (A B s t) ((A B s → t) → (B A s → t)))]{
  Swap the order of the first two inputs of a procedure.

  Equivalent to composing the procedure with @kihi[(swap)].
}

@defthing[join (∀ (A s t) ((A A s → t) → (A s → t)))]{
  Duplicate the first input of a procedure.

  Equivalent to composing the procedure with @kihi[(copy)].
}

@defthing[rec (∀ (A s t) (((s → t) s → t) s → t))]{
  Allow a procedure to be recursive by apply the given procedure to its
  own fixed point.
}

@defthing[with-arity (∀ (s t) ((s → t) Number s → t))]{
  Apply a Racket procedure, providing as many values from the stack as
  specified by the given number.
}


@section[#:tag "syntax"]{Syntax}

Kihi syntax is currently not extensible, because it is not nested within
a single pair of parentheses and so not delimited as Racket expects.  A
number of syntactic forms are special cased in the expander, and they
are typically followed by one or more delimited syntactic forms that
make them look like they accept literal procedures.  This standard means
that many forms that would be macros in Racket can be regular procedures
in Kihi because a procedure literal and a macro's `body' argument use
exactly the same syntax.

Use of the following syntactic forms can appear anywhere within a Kihi
procedure, but they are divided into two types: statements and
expressions.  Statements are forms whose translation into Racket code is
required to appear directly within a @racket[begin] form, such as
@kihi[define].  As a result of this requirement, statements are yanked
out of their use site to the top of the surrounding context.  For the
special case syntax provided this should never be a problem, because
those forms do not have side effects, and definitions are already
available both forward and backward in the scope that they are defined
in, but it is also possible to escape a more general Racket statement in
which arbitrary side-effecting code may run out of order.

Note that Scribble only permits a single form to appear in the
documentation of a definition, so each of the syntactic forms below are
in parentheses.  When used in Kihi, the forms do not need to be in
parentheses, and delimiting them in this way will return a procedure
whose body is the result of the syntax.

@subsection[#:tag "escaping"]{Escaping}

In order to be able to fully interact with Racket, Kihi provides the
ability to jump out of the language and into Racket.  This is mostly
useful to use syntax that is not special-cased in the expander.

@defform[(racket (f t ...))]{
  Escape to a Racket expression, which is either a function call or a
  syntax form.
}

@defform[(racket/stmt (f t ...))]{
  Escape to a Racket statement.  Pulls the form @racket[(f t ...)] out
  to the surrounding begin, which means that it will execute before the
  surrounding expressions.
}

@subsection[#:tag "modules"]{Modules}

@defform[(require (s ...))]{
  Import definitions as per the Racket require specs (@kihi[s ...]).  A
  special rewriting process occurs within the specs to translate Kihi
  application to Racket application, so that the name of nested specs
  appears before the spec's delimiter, rather than being the first
  element inside.  For example:

  @codeblock[#:keep-lang-line? #f]{
    #lang kihi
    require (only-in (racket/list empty))
  }
}

@defform[(provide (s ...))]{
  Export definitions as per the Racket provide specs (@kihi[s ...]).  A
  special rewriting process occurs within the specs to translate Kihi
  application to Racket application, so that the name of nested specs
  appears before the spec's delimiter, rather than being the first
  element inside.  For example:

  @codeblock[#:keep-lang-line? #f]{
    #lang kihi
    provide (all-from-out (racket/list))
  }
}

@subsection[#:tag "binding"]{Binding}

@defform[(define (f x ...) (t ...))]{
  Define a procedure and bind the name @kihi[f] to it.  Invoking the
  name @kihi[f] will execute the procedure @kihi[(t ...)].  If extra
  names @kihi[x ...] are defined, the procedure will also consume the
  same number of values before executing and pointwise bind those names
  to the values.
}

@defform[(struct (s x ...))]{
  Define a struct and bind the name @kihi[s] to it.  The @kihi[x ...]
  names form the fields of the resulting struct.
}

@defform[(bind (x ...) (t ...))]{
  Consume as many values as there are names @kihi[x ...], and then apply
  the procedure @kihi[(t ...)] with the names pointwise bound to the
  values.
}

@defform[(λ (x ...) (t ...))]{
  A procedure that, when applied, consumes as many values as there are
  names @kihi[x ...], and then applies the procedure @kihi[(t ...)] with
  the names pointwise bound to the values.  Equivalent to using
  @kihi[bind] directly inside of a procedure literal.
}

@defform[(let ([(x ...) t ...] ...) (t ...))]{
  Like @kihi[bind], but the bound values are defined directly after the
  names they are bound by rather than the procedure they are bound in.
  Multiple different sets of bindings can be made at once, and bound
  names are also available in the bindings the follow them.

  The number of names in a binding does not need to match the number of
  values returned by the terms that follow it, so bindings can also
  share values through the stack: the bindings @kihi[([(x y) a b])],
  @kihi[([() a b] [(x y)])], and @kihi[([(x) a b] [(y)])] are all
  equivalent, binding @kihi[x] to @kihi[a] and @kihi[y] to @kihi[b].
}

@defform[(match ([p t ...] ...))]{
  Like Racket's pattern matching form, but consumes a value to match
  against instead of including it in the syntax.  The clauses of the
  match do not need to be pairs, as the terms @kihi[t ...] make up a
  single expression.
}


@section[#:tag "data"]{Data Structures}

@subsection[#:tag "boolean"]{Booleans}

@defthing[boolean? (∀ (A) (A → Boolean))]{
  Determine if a value is a boolean.
}

@defthing[if (∀ (s t) (Boolean (s → t) (s → t) s → t))]{
  Branch on the value of a boolean.
}

@defthing[and (Boolean (→ Boolean) → Boolean)]{
  Returns false if the first argument is false, otherwise returns the
  result of the applying the second argument.
}

@defthing[or (Boolean (→ Boolean) → Boolean)]{
  Returns true if the first argument is true, otherwise returns the
  result of the applying the second argument.
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
  Consume a pair and apply a procedure to its values.
}

@defthing[pair/map (∀ (A B C D s t)
                      ((A → C) (B → D) Pair A B → Pair C D))]{
  Transform each value of a pair with a corresponding procedure.
}

@subsection[#:tag "option"]{Options}

An option represents the potential for a missing value.  Either there is
@kihi[some] value, or there is @kihi[none].  The value cannot be
accessed directly, since it might not exist, but you can apply a
procedure to the value on the condition that the value exists with
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
  Consume an option and apply a procedure to the value if the value is
  present.
}

@defthing[option/map (∀ (A s) ((A → B) Option A → Option B))]{
  Transform the value in an option with a procedure if the value is
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
  Consume a list and apply a procedure to every value that was in the
  list.
}

@defthing[list/map (∀ (A B) ((A → B) List A → List B))]{
  Transform the values in a list with a procedure.
}

@defthing[list/append (∀ (A) (List A List A → List A))]{
  Append two lists together to form a single list.
}

@defthing[repeat (∀ (A) (Number A → List A))]{
  Make a list that repeats a value a certain number of times.
}
