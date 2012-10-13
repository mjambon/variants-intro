(*
  Quick introduction to sum types inspired by elasticsearch search queries

  Try me with:
    $ <install ocaml>
    $ ocamlopt -o variants variants.ml
    $ ./variants
*)

(*
  The goal of this document is to inspire future generations
  to adopt machine readable conventions to describe APIs, when practical.

  This example is a valid OCaml program, and shall serve as an introduction
  to variant types (aka tagged unions, sum types, or algebraic data types).
*)

(*
  Short syntax reference:

  Simple type definition:

    type <lowercase type name> = <type>

  Mutually recursive type definitions use the 'and' keyword:

    type <lowercase type name> = <type>
    and <lowercase type name> = <type>
    and ...

  Record type definition (similar to JSON objects or C structs):

  type <type name> = {
      <field name> : <type>;
      ...
    }
*)

type coord = {
  lat : float;
  lon : float;
}

(*
  Type aliases consist in giving another name to an existing type,
  for clarity. Here are the ones we'll use:
*)

type field_name = string
type field_value = string
type date = string
type radius_km = float


(*
  A few record type definitions used later.

  Note the type of the field 'analyzer'.
  'string option' is a predefined type whose values
  are either

    None

  or

    Some <any string>

  'option' is an algebraic data type. In OCaml it is usually called
  a variant type. The idea is that the values of this type
  are of different kinds, known at compile time. Each kind or variant
  is represented by an uppercase tag ('None' or 'Some') and
  optionally associated with data of a certain type (here 'Some' is associated
  with the type 'string').

  For now, you can just consider that 'None' serves as a null value for
  the type 'string' (or 'float', 'int', etc.).
*)

type match_options = {
  match_phrase : bool;
  analyzer : string option;
  (* and more... *)
}

type dis_max = {
  dis_max_tie_breaker : float option;
  dis_max_boost : float option;
}

(*
  Here comes the core definition of a search query.

  Syntax:
  - the vertical bar '|' is read 'or' and separates the different kinds
    of nodes or leaves of the query tree.
  - 'of' introduces the type of value associated with a given capitalized
    constructor.
  - the star '*' between types is a Cartesian product.

  In the following example, 'Filter of query * filter'
  is one kind of node of a query. Its constructor or tag is 'Filter'
  and it is associated with a pair of values, one of type 'query'
  and one of type 'filter'.
*)

type query =
    Match_all
  | Match of field_name * field_value * match_options
  | Match_some of int * query list
  | And of query list
  | Or of query list * dis_max option
  | And_not of query * query
  | Filter of query * filter

and filter =
    Query of query
  | Int_range of field_name * int option * int option
  | Float_range of field_name * float option * float option
  | Date_range of field_name * date option * date option
  | Geo_distance of field_name * coord * radius_km

(*
  The definitions above are valid OCaml code and the compiler will check
  that the queries that we build conform to these definitions.

  Let's now create a sample query.

  The syntax for naming a value is:

    let <name> = <value>

  Note that we do not need to specify the type of the value.
  The compiler will do this work for us and will report inconsistencies.
*)

let not_phrase = { match_phrase = false; analyzer = None }

let simple_query =
  Match ("name", "john", not_phrase)

let filtered_query =
  Filter (simple_query,
          Geo_distance ("home", { lat = 37.; lon = -122. }, 40.))

let main_query =
  let q2 =
    Match_some (2, [
      Match ("interests", "photography", not_phrase);
      Match ("interests", "bikes", not_phrase);
      Match ("interests", "beer", not_phrase);
      Match ("interests", "trees", not_phrase);
    ])
  in
  And [ filtered_query;  q2 ]

(*
  You should now try to modify or extend the query above,
  and compile this program using the command:

    $ ocamlopt -o variants variants.ml

  ... and run it to see what it does:

    $ ./variants

*)

(*
  We can write functions that operate on queries.
  No need to understand all the details, but it gives us a function
  that prints out a given query, for illustration purposes.

  Syntax: function arguments are whitespace separated just like in the shell.
*)

open Printf

let line indent s =
  printf "%s%s\n" (String.make indent ' ') s


let print_range to_string n name a b =
  let a_str =
    match a with
        None -> "-inf"
      | Some x -> to_string x
  in
  let b_str =
    match b with
        None -> "inf"
      | Some x -> to_string x
  in
  line n (sprintf "field %s: [ %s, %s ]" name a_str b_str)

let rec print_query n q =
  match q with
      Match_all ->
        line n "match_all"
    | Match (name, value, options) ->
        line n (sprintf "match field %s: %s" name value)
    | Match_some (m, l) ->
        line n (sprintf "match at least %i of" m);
        List.iter (print_query (n+2)) l
    | And l ->
        line n "match all of";
        List.iter (print_query (n+2)) l
    | Or (l, dis_max) ->
        line n "match at least one of";
        List.iter (print_query (n+2)) l
    | And_not (q1, q2) ->
        line n "match";
        print_query (n+2) q1;
        line n "but not";
        print_query (n+2) q2
    | Filter (q, x) ->
        line n "match";
        print_query (n+2) q;
        line n "and apply filter";
        print_filter (n+2) x

and print_filter n x =
  match x with
      Query q -> print_query n q
    | Int_range (name, a, b) -> print_range string_of_int (n+2) name a b
    | Float_range (name, a, b) -> print_range string_of_float (n+2) name a b
    | Date_range (name, a, b) -> print_range (fun s -> s) (n+2) name a b
    | Geo_distance (name, {lat; lon}, radius_km) ->
        line n (sprintf "location field %s within %g km from lat=%g, lon=%g"
                  name radius_km lat lon)


let main () =
  print_query 0 main_query

let () = main ()
