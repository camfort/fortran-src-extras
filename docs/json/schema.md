# fortran-src Fortran code JSON representation
This package contains code to serialize parsed Fortran code to a highly
structured JSON document. This document intends to provide an overview of the
schema. Note that this is not a supported feature, so things may be omitted -
refer to the `Language.Fortran.AST` module in fortran-src and the
`Language.Fortran.Extra.JSON` module in fortran-src-extras for further info.

## Rationale
Due to the large amount of structure in play, it's ideal to use the types
directly by using fortran-src as a dependency in a Haskell project. For cases
where you want to use fortran-src in a non-Haskell project, we provide a rough
JSON serialization of successfully parsed Fortran code, which can be parsed and
used in any context supporting JSON. The hope is that parsing JSON is easier
than parsing Fortran source code directly.

## Major changes from inspiration schema
These notes intend to assist migrating from the schema that inspired this one to
this one. If something is unclear or not mentioned, try checking the relevant
definition in `Language.Fortran.AST`, and fortran-src's changelog.

  * serialize `a` annotation (at `anno` field)
    * previously, a `ToJSON a` instance was enforced, but never used.
  * complete schema: no partial instances, no `error`
  * rework some AST nodes:
    * overhaul `BaseType`
    * comments are now plain strings, rather than objects with a `comment` key
      (that is left up to the parent type)
    * operators use plain English instead of symbols (easier to use in JSON)
    * literals in `Value` use their printed form, and are re-parsed into
      constituent parts during JSON parsing
    * as well as JSON, executable can output pretty formatted YAML

## Outdated schema
### Introduction
We use a BNF-style notation to describe the program structure. Program elements
are generally represented as JSON objects with field names that should hopefully
be self-explanatory. Field values will either be string literals or type
specifications for the value.

For example,

```
value ::= { "tag": "integer", "integer": string }
```

says that a `value` is an object with a `"tag"` field whose value must
be `"integer"` and an `"integer"` field that may contain any `string`.

Type specifications may take three forms

```
type
  ::= id                 # reference to a type
   |  [ type ]           # an array of the underlying type
   |  type ?             # an optional value of the underlying type (may be `null`)
```

Unless otherwise noted, all elements have

  * a  `"span"` field containing a `span`
  * an `"anno"` field containing an annotation - but this will usually be empty

### Schema
The top-level JSON object we produce is a `program`.

```
program
  ::= { "meta_info": {"filename": string}, "program": [program_unit] }
```

```
program_unit
  ::= { "tag": "main", "name": string, "blocks": [block] }
   |  { "tag": "subroutine", "name": string, "arguments": [expression]?, "blocks": [block] }
   |  { "tag": "function", "name": string, "arguments": [expression]?, "type": type_spec, "blocks": [block], "result": expression? }
   |  { "tag": "block_data", "name": string?, "blocks": [block] }
```

```
block
  ::= { "tag": "statement", "label": label?, "statement": statement }
   |  { "tag": "if", "label": label?, "conditions": [expression?], "blocks": [[block]], "end_label": label? }
   |  { "tag": "select", "label": label?, "scrutinee": expression, "ranges": [index?], "blocks": [[block]], "end_label": label? }
   |  { "tag": "do", "label": label?, "target": label?, "do_spec": do_specification, "body": [block], "end_label": label? }
   |  { "tag": "do_while", "label": label?, "target": label?, "condition": expression, "body": [block], "end_label": label? }
```

- NOTE: in `if` and `select`, each condition (resp. range) maps to an element of the `blocks` list

```
statement
  ::= { "tag": "declaration", "type": type_spec, "attributes": [attribute]?, "declarators": [declarator] }
   |  { "tag": "structure", "name": string, "fields": [structure_item] }
   |  { "tag": "save", "variables": [expression]? }
   |  { "tag": "dimension", "declarators": [declarator] }
   |  { "tag": "pointer", "declarators": [declarator] }
   |  { "tag": "data", "data_groups": [data_group] }
   |  { "tag": "automatic", "declarators": [declarator] }
   |  { "tag": "parameter", "declarators": [declarator] }
   |  { "tag": "external", "arguments": [expression] }
   |  { "tag": "intrinsic", "arguments": [expression] }
   |  { "tag": "common", "common_groups": [common_group] }
   |  { "tag": "equivalence", "groups": [[expression]] }
   |  { "tag": "implicit", "implicit_items": [implicit_items]? }
   |  { "tag": "entry", "name": expression, "arguments": [expression]? }
   |  { "tag": "include", "path": expression, "blocks": [block]? }
   |  { "tag": "cycle" }
   |  { "tag": "exit" }
   |  { "tag": "if_logical", "condition": expression, "statement": statement }
   |  { "tag": "if_arithmetic", "expression": expression, "less": expression, "equal": expression, "greater": expression }
   |  { "tag": "function", "name": expression, "arguments": [expression], "body": expression }
   |  { "tag": "assign_expression", "target": expression, "expression": expression }
   |  { "tag": "assign_label", "target": expression, "label": expression }
   |  { "tag": "goto", "target": expression }
   |  { "tag": "goto_assigned", "target": expression, "labels": [expression]? }
   |  { "tag": "goto_computed", "target": expression, "labels": [expression] }
   |  { "tag": "call", "function": expression, "arguments": [argument]? }
   |  { "tag": "return", "target": expression? }
   |  { "tag": "continue" }
   |  { "tag": "end_do" }
   |  { "tag": "stop", "message": expression? }
   |  { "tag": "pause", "message": expression? }
   |  { "tag": "read", "format": ([control_pair] | expression), "arguments": [expression]? }
   |  { "tag": "write", "format": [control_pair], "arguments": [expression]? }
   |  { "tag": "print", "format": expression, "arguments": [expression]? }
   |  { "tag": "type", "format": expression, "arguments": [expression]? }
   |  { "tag": "open", "specification": [control_pair] }
   |  { "tag": "close", "specification": [control_pair] }
   |  { "tag": "inquire", "specification": [control_pair] }
   |  { "tag": "rewind", "specification": ([control_pair] | expression) }
   |  { "tag": "backspace", "specification": ([control_pair] | expression) }
   |  { "tag": "endfile", "specification": ([control_pair] | expression) }
```

- NOTE: the `end_do` statement should be treated like the `continue`
  statement, ie a no-op. The reason it exists is that the do-block
  grouping transformation does not remove `end_do` statements that have
  a label, so

  ```
      do 10 i = 1,10
        x = x + 1
  10  end do
  ```
  
  would be parsed into the following two blocks
  
  ```
  [ 
    { "tag": "do", ... },
    { "tag": "statement, "statement": {"tag": "end_do"}}
  ]
  ```
  
  rather than a single `do` block. We could tweak the transformation to
  swallow the `end_do` statement as it would if the `end do` weren't
  labeled, but according to
  [Sun](https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vn8c/index.html),
  this example is invalid anyway, so perhaps we just want to rewrite the
  `end do` to a `continue` instead.

```
argument ::= { "name": string?, "expression": expression }
```

```
attribute
  ::= { "tag": "parameter" }
   |  { "tag": "dimension", "dimensions": [dimension_declarator] }
   |  { "tag": "external" }
   |  { "tag": "pointer" }
   |  { "tag": "save" }
```

```
control_pair
  ::= { "name": string?, "expression": expression }
```

```
implicit_items
  ::= { "type": type_spec, "items": [implicit_item] }
```

```
implicit_item
  ::= { "tag": "implicit_char", "char": string }
   |  { "tag": "implicit_range", "lower": string, "upper": string }
```

```
common_group
  ::= { "name": expression?, "expressions": [expression] }
```

```
data_group
  ::= { "names": [expression], "initializers": [expression]}
```

```
structure_item
  ::= { "tag": "fields", "type": type_spec, "attributes": [attribute]?, "declarators": [declarator] }
   |  { "tag": "union", "maps": [union_map] }
```

```
union_map
  ::= { "fields": [structure_item] }
```

```
do_specification
  ::= { "initial": statement, "limit": expression, "increment": expression? }
```

```
expression
  ::= { "tag": "value", "value": value }
   |  { "tag": "binary_op", "binary_op": binary_op, "left": expression, "right": expression }
   |  { "tag": "unary_op", "unary_op": unary_op, "expression": expression }
   |  { "tag": "subscript", "expression": expression, "indices": [index] }
   |  { "tag": "deref", "expression": expression, "field": expression }
   |  { "tag": "function_call", "function": expression, "arguments": [argument]? }
   |  { "tag": "implied_do", "do_spec": do_specification, "expressions": [expression] }
   |  { "tag": "initialisation", "expressions": [expression] }
   |  { "tag": "return_spec", "target": expression }
```

```
index
  ::= { "tag": "index_single", "index": expression }
   |  { "tag": "index_range", "lower": expression?, "upper": expression?, "stride": expression? }
```

```
# no span
value
  ::= { "tag": "integer", "value": string }
   |  { "tag": "real", "value": string }
   |  { "tag": "complex", "real": string, "imaginary": string }
   |  { "tag": "hollerith", "value": string }
   |  { "tag": "logical", "value": string }
   |  { "tag": "intrinsic", "value": string }
   |  { "tag": "variable", "value": string }
   |  { "tag": "star" }
```

- NOTE: the `star` value is used is many places where the `*` literal
  appears, e.g. to declare an assumed-length array with `integer arr(*)`.

```
declarator
  ::= { "tag": "decl_variable", "variable": expresison, "length": expression?, "initial": expression? }
   |  { "tag": "decl_array", "array": expression, "dimensions": [dimension_declarator], "length": expression?, "initial": expression? }
```

```
dimension_declarator
  ::= { "lower": expression?, "upper": expression? }
```

```
# no span
unary_op
  ::= ("plus" | "minus" | "not")
```

```
# no span
binary_op
  ::= ("+" | "-" | "*" | "/" | "**" | "//" | ">" | ">=" | "<" | "<=" | "==" | "!=" | "or" | "xor" | "and" | "eqv" | "neqv")
```

```
type_spec
  ::= { "base_type": base_type, "selector": selector }
```

```
base_type
  ::= ("integer" | "real" | "double_precision" | "complex" | "double_complex" | "logical" | "character" | "byte" | string)
```

- NOTE: Any string other than those listed above denotes a user-defined `structure`.

```
selector
  ::= { "length": expression?, "kind": expression? }
```

- NOTE: `length` should only be used for `character`s, `kind` indicates the width of the numeric types.

``` 
span ::= "(<start-line>:<start-col>)-(<end-line>:<end-col>)"
```


