module Arbitrary = {
  /* This abstract type is used to represent a disjoint union
   * It's parameter takes a tuple of the possible types that it is, ie:
   * sum((string, int));  // a disjoint union of `string` or `int`
   * sum((bool, string, array(int)));
   */
  type sum('a);

  type arbitrary('a) = {
    .
    "generator": int => 'a,
    "shrink": 'a => array('a),
    "show": 'a => string,
  };

  module Types = {
    type proxy('a) =
      | Proxy;

    type either('a, 'b) =
      | Left('a)
      | Right('b);
    let either: ('a => 'c, 'b => 'c, either('a, 'b)) => 'c =
      (left_fn, right_fn, e) =>
        switch (e) {
        | Left(l) => left_fn(l)
        | Right(r) => right_fn(r)
        };
    let left: 'a => either('a, 'b) = l => Left(l);
    let right: 'b => either('a, 'b) = r => Right(r);
    let bimap: ('a => 'c, 'b => 'd, either('a, 'b)) => either('b, 'd) =
      (left_fn, right_fn, e) =>
        switch (e) {
        | Left(l) => Left(left_fn(l))
        | Right(r) => Right(right_fn(r))
        };
  };

  [@bs.module "jsverify"]
  external sampler : (~arb: arbitrary('a), ~genSize: int=?, unit) => 'a =
    "sampler";

  [@bs.send.pipe: arbitrary('a)]
  external smap :
    ('a => 'b, 'b => 'a, ~newShow: 'b => string=?) => arbitrary('b) =
    "";

  [@bs.module "jsverify"]
  external bless : {.. "generator": int => 'a} => arbitrary('a) = "bless";

  /* * * * * * * * * *
   * For primitives  *
   * * * * * * * * * */
  [@bs.module "jsverify"] external arb_js_bool : arbitrary(bool) = "bool";

  let arb_bool: arbitrary(bool) =
    smap(
      x => x,
      a => a ? true : false,
      ~newShow=string_of_bool,
      arb_js_bool,
    );

  [@bs.module "jsverify"] external arb_nat : arbitrary(int) = "nat";

  [@bs.module "jsverify"]
  external arb_int : (int, int) => arbitrary(int) = "integer";

  [@bs.module "jsverify"]
  external arb_float : (float, float) => arbitrary(float) = "number";

  [@bs.module "jsverify"] external arb_character : arbitrary(string) = "char";
  [@bs.module "jsverify"]
  external arb_ascii_character : arbitrary(string) = "asciichar";

  [@bs.module "jsverify"] external arb_string : arbitrary(string) = "string";
  [@bs.module "jsverify"]
  external arb_not_empty_string : arbitrary(string) = "nestring";

  [@bs.module "jsverify"]
  external arb_ascii_string : arbitrary(string) = "asciistring";
  [@bs.module "jsverify"]
  external arb_not_empty_ascii_string : arbitrary(string) = "asciinestring";

  [@bs.module "jsverify"] external arb_unit : arbitrary(unit) = "unit";

  [@bs.module "jsverify"]
  external arb_array : arbitrary('a) => arbitrary(array('a)) = "array";
  [@bs.module "jsverify"]
  external arb_not_empty_array : arbitrary('a) => arbitrary(array('a)) =
    "nearray";

  [@bs.module "jsverify"]
  external arb_date : arbitrary(Js.Date.t) = "datetime";

  let arb_list: arbitrary('a) => arbitrary(list('a)) =
    a =>
      smap(
        Array.to_list,
        Array.of_list,
        ~newShow=
          l =>
            Js.Json.stringifyAny(Array.of_list(l))
            |> Js.Option.getWithDefault(""),
        arb_array(a),
      );

  /* * * * * * * *
   * For objects *
   * * * * * * * */
  [@bs.module "jsverify"]
  external arb_object : arbitrary(Js.t('a)) = "object";

  [@bs.module "jsverify"] external arb_json : arbitrary(Js.Json.t) = "json";

  [@bs.module "jsverify"]
  external arb_dict : arbitrary('a) => arbitrary(Js.Dict.t('a)) = "dict";

  /* * * * * *
   * Helpers *
   * * * * * */
  [@bs.module "jsverify"]
  external arb_constant : 'a => arbitrary('a) = "constant";

  [@bs.module "jsverify"]
  external non_shrink : arbitrary('a) => arbitrary('a) = "nonshrink";

  /* * * * * * * *
   * Combinators *
   * * * * * * * */
  [@bs.module "jsverify"]
  external arb_small : arbitrary('a) => arbitrary('a) = "small";

  [@bs.module "jsverify"]
  external arb_such_that : (arbitrary('a), 'a => bool) => arbitrary('a) =
    "suchthat";

  [@bs.module "jsverify"]
  external arb_elements : array('a) => arbitrary('a) = "elements";

  [@bs.module "jsverify"]
  external arb_one_of : array(arbitrary('a)) => arbitrary('a) = "oneof";

  [@bs.module "jsverify"]
  external arb_fn : arbitrary('a) => arbitrary('b => 'a) = "asciinestring";

  [@bs.module "jsverify"]
  external arb_tuple :
    ((arbitrary('a), arbitrary('b))) => arbitrary(('a, 'b)) =
    "tuple";

  [@bs.module "jsverify"]
  external arb_tuple' :
    ((arbitrary('a), arbitrary('b), arbitrary('c))) =>
    arbitrary(('a, 'b, 'c)) =
    "tuple";

  [@bs.module "jsverify"]
  external arb_tuple'' :
    ((arbitrary('a), arbitrary('b), arbitrary('c), arbitrary('d))) =>
    arbitrary(('a, 'b, 'c, 'd)) =
    "tuple";

  [@bs.module "jsverify"]
  external unsafe_arb_record :
    (
      [@bs.ignore]
      Types.proxy(Js.t('a)) /* set the record type as key: value */,
      Js.t('b)
    ) => /* set the record type as key: arbitrary(value) */
    arbitrary(Js.t('a)) =
    "record";

  /* Combines several arbitraries (as an untagged union)
   * This is represented as an abstract type `sum('a)`. You'll need to use reflection in
   * order to get the value out (see the `Js.Types` module)
   */
  [@bs.module "jsverify"]
  external arb_sum :
    ((arbitrary('a), arbitrary('b))) => arbitrary(sum(('a, 'b))) =
    "oneof";
  [@bs.module "jsverify"]
  external arb_sum' :
    ((arbitrary('a), arbitrary('b), arbitrary('c))) =>
    arbitrary(sum(('a, 'b, 'c))) =
    "oneof";
  [@bs.module "jsverify"]
  external arb_sum'' :
    ((arbitrary('a), arbitrary('b), arbitrary('c), arbitrary('d))) =>
    arbitrary(sum(('a, 'b, 'c, 'd))) =
    "oneof";

  let arb_null: arbitrary('a) => arbitrary(Js.null('a)) =
    arb => {
      let null: Js.null('a) = Js.null;
      Obj.magic(arb_sum((arb, arb_constant(null))));
    };

  let arb_nullable: arbitrary('a) => arbitrary(Js.nullable('a)) =
    arb => {
      let null: Js.nullable('a) = Js.Nullable.null;
      let undefined: Js.nullable('a) = Js.Nullable.undefined;
      Obj.magic(
        arb_sum'((arb, arb_constant(null), arb_constant(undefined))),
      );
    };

  let arb_option: arbitrary('a) => arbitrary(option('a)) =
    arb =>
      smap(
        Js.Null.toOption,
        Js.Null.fromOption,
        ~newShow=
          a =>
            switch (a) {
            | Some(a') =>
              "Some("
              ++ (Js.Json.stringifyAny(a') |> Js.Option.getWithDefault(""))
              ++ ")"
            | None => "None"
            },
        arb_null(arb),
      );

  let arb_either:
    (arbitrary('a), arbitrary('b)) => arbitrary(Types.either('a, 'b)) =
    (arb_a, arb_b) =>
      smap(
        r => {
          let is_left: sum(({. "left": 'a}, {. "right": 'b})) => bool = [%bs.raw
            {|
            function(r) { return r.left !== undefined ? 1 : 0 }
          |}
          ];
          is_left(r) ?
            Types.Left(Obj.magic(r)##left) :
            Types.Right(Obj.magic(r)##right);
        },
        e =>
          switch (e) {
          | Types.Left(l) => Obj.magic({"left": l})
          | Types.Right(r) => Obj.magic({"right": r})
          },
        ~newShow=
          e =>
            switch (e) {
            | Left(l') =>
              "Left("
              ++ (Js.Json.stringifyAny(l') |> Js.Option.getWithDefault(""))
              ++ ")"
            | Right(r') =>
              "Right("
              ++ (Js.Json.stringifyAny(r') |> Js.Option.getWithDefault(""))
              ++ ")"
            },
        arb_sum((
          unsafe_arb_record(
            Proxy: Types.proxy({. "left": 'a}),
            {"left": arb_a},
          ),
          unsafe_arb_record(
            Proxy: Types.proxy({. "right": 'b}),
            {"right": arb_b},
          ),
        )),
      );
};

module Generator = {
  type generator('a) = int => 'a;

  [@b.send.pipe: generator('a)]
  external bless : (int => 'a) => generator('a) = "bless";

  [@b.send.pipe: generator('a)]
  external map : ('a => 'b) => generator('b) = "map";

  [@b.send.pipe: generator('a)]
  external flatmap : ('a => generator('b)) => generator('b) = "flatmap";

  [@b.send.pipe: generator('a)]
  external constant : 'b => generator('b) = "constant";
};

module Shrink = {
  type shrink('a) = 'a => array('a);

  [@b.send.pipe: shrink('a)]
  external bless : ('a => array('a)) => shrink('a) = "bless";

  [@b.send.pipe: shrink('a)]
  external smap : ('a => 'b, 'b => 'a) => shrink('b) = "smap";
};

module Property = {
  type arbitrary('a) = Arbitrary.arbitrary('a);
  type sum('a) = Arbitrary.sum('a);

  type property('a);
  type async_property('a);

  type abs_result('a);
  type result('a) = {
    .
    "counterexample": 'a,
    "tests": int,
    "shrinks": int,
    "exc": Js.nullable(Js.Exn.t),
    "rngState": string,
  };

  type check_options = {
    .
    "tests": Js.nullable(int),
    "size": Js.nullable(int),
    "quiet": Js.nullable(bool),
    "rngState": Js.nullable(string),
  };

  let options:
    (~tests: int=?, ~size: int=?, ~quiet: bool=?, ~rngState: string=?, unit) =>
    check_options =
    (~tests=?, ~size=?, ~quiet=?, ~rngState=?, _) => {
      "tests": tests |> Js.Nullable.fromOption,
      "size": size |> Js.Nullable.fromOption,
      "quiet": quiet |> Js.Nullable.fromOption,
      "rngState": rngState |> Js.Nullable.fromOption,
    };

  /* Convert the abstract result type coming from the API untagged to a more usable form */
  let to_result: abs_result('a) => option(result('a)) =
    result =>
      switch (Js.Types.classify(result)) {
      | Js.Types.JSObject(value) => Some(Obj.magic(value))
      | _ => None
      };

  [@bs.module "jsverify"]
  external check :
    (~prop: property('a), ~options: check_options=?, unit) => abs_result('a) =
    "check";

  [@bs.module "jsverify"]
  external check' :
    (~prop: async_property('a), ~options: check_options=?, unit) =>
    Js.Promise.t(abs_result('a)) =
    "check";

  [@bs.module "jsverify"]
  external assert' :
    (~prop: property('a), ~options: check_options=?, unit) => unit =
    "assert";

  [@bs.module "jsverify"]
  external assert'' :
    (~prop: async_property('a), ~options: check_options=?, unit) => unit =
    "assert";

  /* * * * * * * * * * * * * * *
   * `forall` with bool  *
   * * * * * * * * * * * * * * */
  [@bs.module "jsverify"]
  external forall1' : (arbitrary('a), 'a => bool) => property('a) = "forall";

  [@bs.module "jsverify"]
  external forall2' :
    (arbitrary('a), arbitrary('b), ('a, 'b) => bool) =>
    property(sum(('a, 'b))) =
    "forall";

  [@bs.module "jsverify"]
  external forall3' :
    (arbitrary('a), arbitrary('b), arbitrary('c), ('a, 'b, 'c) => bool) =>
    property(sum(('a, 'b, 'c))) =
    "forall";

  [@bs.module "jsverify"]
  external forall4' :
    (
      arbitrary('a),
      arbitrary('b),
      arbitrary('c),
      arbitrary('d),
      ('a, 'b, 'c, 'd) => bool
    ) =>
    property(sum(('a, 'b, 'c, 'd))) =
    "forall";

  [@bs.module "jsverify"]
  external forall5' :
    (
      arbitrary('a),
      arbitrary('b),
      arbitrary('c),
      arbitrary('d),
      arbitrary('e),
      ('a, 'b, 'c, 'd, 'e) => bool
    ) =>
    property(sum(('a, 'b, 'c, 'd, 'e))) =
    "forall";

  /* * * * * * * * * * * * * * * * * * *
   * `forall` with bool (async)  *
   * * * * * * * * * * * * * * * * * * */
  [@bs.module "jsverify"]
  external async_forall1 :
    (arbitrary('a), 'a => Js.Promise.t(bool)) => async_property('a) =
    "forall";

  [@bs.module "jsverify"]
  external async_forall2 :
    (arbitrary('a), arbitrary('b), ('a, 'b) => Js.Promise.t(bool)) =>
    async_property(sum(('a, 'b))) =
    "forall";

  [@bs.module "jsverify"]
  external async_forall3 :
    (
      arbitrary('a),
      arbitrary('b),
      arbitrary('c),
      ('a, 'b, 'c) => Js.Promise.t(bool)
    ) =>
    async_property(sum(('a, 'b, 'c))) =
    "forall";

  [@bs.module "jsverify"]
  external async_forall4 :
    (
      arbitrary('a),
      arbitrary('b),
      arbitrary('c),
      arbitrary('d),
      ('a, 'b, 'c, 'd) => Js.Promise.t(bool)
    ) =>
    async_property(sum(('a, 'b, 'c, 'd))) =
    "forall";

  [@bs.module "jsverify"]
  external async_forall5 :
    (
      arbitrary('a),
      arbitrary('b),
      arbitrary('c),
      arbitrary('d),
      arbitrary('e),
      ('a, 'b, 'c, 'd, 'e) => Js.Promise.t(bool)
    ) =>
    async_property(sum(('a, 'b, 'c, 'd, 'e))) =
    "forall";

  /* * * * * * * * * * * * * * * * * * * * *
   * `forall` with bool (for convenience)  *
   * * * * * * * * * * * * * * * * * * * * */
  let forall1 = (a1, fn) => forall1'(a1, a => fn(a));

  let forall2 = (a1, a2, fn) => forall2'(a1, a2, (a, b) => fn(a, b));

  let forall3 = (a1, a2, a3, fn) =>
    forall3'(a1, a2, a3, (a, b, c) => fn(a, b, c));

  let forall4 = (a1, a2, a3, a4, fn) =>
    forall4'(a1, a2, a3, a4, (a, b, c, d) => fn(a, b, c, d));

  let forall5 = (a1, a2, a3, a4, a5, fn) =>
    forall5'(a1, a2, a3, a4, a5, (a, b, c, d, e) => fn(a, b, c, d, e));

  /* * * * * * * * * * * * * * * *
   * `property` with bool  *
   * * * * * * * * * * * * * * * */
  [@bs.module "jsverify"]
  external property1' : (string, arbitrary('a), 'a => bool) => unit =
    "property";

  [@bs.module "jsverify"]
  external async_property1' :
    (string, arbitrary('a), 'a => Js.Promise.t(bool)) => unit =
    "property";

  [@bs.module "jsverify"]
  external property2' :
    (string, arbitrary('a), arbitrary('b), ('a, 'b) => bool) => unit =
    "property";

  [@bs.module "jsverify"]
  external async_property2' :
    (
      string,
      arbitrary('a),
      arbitrary('b),
      ('a, 'b) => Js.Promise.t(bool)
    ) =>
    unit =
    "property";

  [@bs.module "jsverify"]
  external property3' :
    (
      string,
      arbitrary('a),
      arbitrary('b),
      arbitrary('c),
      ('a, 'b, 'c) => bool
    ) =>
    unit =
    "property";

  [@bs.module "jsverify"]
  external async_property3' :
    (
      string,
      arbitrary('a),
      arbitrary('b),
      arbitrary('c),
      ('a, 'b, 'c) => Js.Promise.t(bool)
    ) =>
    unit =
    "property";

  [@bs.module "jsverify"]
  external property4' :
    (
      string,
      arbitrary('a),
      arbitrary('b),
      arbitrary('c),
      arbitrary('d),
      ('a, 'b, 'c, 'd) => bool
    ) =>
    unit =
    "property";

  [@bs.module "jsverify"]
  external async_property4' :
    (
      string,
      arbitrary('a),
      arbitrary('b),
      arbitrary('c),
      arbitrary('d),
      ('a, 'b, 'c, 'd) => Js.Promise.t(bool)
    ) =>
    unit =
    "property";

  [@bs.module "jsverify"]
  external property5' :
    (
      string,
      arbitrary('a),
      arbitrary('b),
      arbitrary('c),
      arbitrary('d),
      arbitrary('e),
      ('a, 'b, 'c, 'd, 'e) => bool
    ) =>
    unit =
    "property";

  [@bs.module "jsverify"]
  external async_property5' :
    (
      string,
      arbitrary('a),
      arbitrary('b),
      arbitrary('c),
      arbitrary('d),
      arbitrary('e),
      ('a, 'b, 'c, 'd, 'e) => Js.Promise.t(bool)
    ) =>
    unit =
    "property";

  /* * * * * * * * * * * * * * * * * * * * * *
   * `property` with bool (for convenience)  *
   * * * * * * * * * * * * * * * * * * * * * */
  let property1 = (s, a1, fn) => property1'(s, a1, a => fn(a));

  let async_property1 = (s, a1, fn) =>
    async_property1'(s, a1, a =>
      fn(a) |> Js.Promise.then_(x => x |> Js.Promise.resolve)
    );

  let property2 = (s, a1, a2, fn) =>
    property2'(s, a1, a2, (a, b) => fn(a, b));

  let async_property2 = (s, a1, a2, fn) =>
    async_property2'(s, a1, a2, (a, b) =>
      fn(a, b) |> Js.Promise.then_(x => x |> Js.Promise.resolve)
    );

  let property3 = (s, a1, a2, a3, fn) =>
    property3'(s, a1, a2, a3, (a, b, c) => fn(a, b, c));

  let async_property3 = (s, a1, a2, a3, fn) =>
    async_property3'(s, a1, a2, a3, (a, b, c) =>
      fn(a, b, c) |> Js.Promise.then_(x => x |> Js.Promise.resolve)
    );

  let property4 = (s, a1, a2, a3, a4, fn) =>
    property4'(s, a1, a2, a3, a4, (a, b, c, d) => fn(a, b, c, d));

  let async_property4 = (s, a1, a2, a3, a4, fn) =>
    async_property4'(s, a1, a2, a3, a4, (a, b, c, d) =>
      fn(a, b, c, d) |> Js.Promise.then_(x => x |> Js.Promise.resolve)
    );

  let property5 = (s, a1, a2, a3, a4, a5, fn) =>
    property5'(s, a1, a2, a3, a4, a5, (a, b, c, d, e) => fn(a, b, c, d, e));

  let async_property5 = (s, a1, a2, a3, a4, a5, fn) =>
    async_property5'(s, a1, a2, a3, a4, a5, (a, b, c, d, e) =>
      fn(a, b, c, d, e) |> Js.Promise.then_(x => x |> Js.Promise.resolve)
    );
};
