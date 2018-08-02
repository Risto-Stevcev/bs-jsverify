open BsMocha.Mocha;
open Verify.Arbitrary;
open Verify.Property;


describe("JsVerify", () => {
  property1("boolean `not` (involution)", arb_bool, b => {
    ! (! b) == b
  });

  property3("nat `+` (associative)", arb_nat, arb_nat, arb_nat, (n1, n2, n3) => {
    (n1 + n2) + n3 == n1 + (n2 + n3)
  });

  property1("Js.null(nat)", arb_null(arb_nat), n => {
    switch (Js.Null.toOption(n)) { | Some(n') => n' >= 0 | None => true }
  });

  property1("Js.nullable(nat)", arb_nullable(arb_nat), n => {
    switch (Js.Nullable.toOption(n)) { | Some(n') => n' >= 0 | None => true }
  });

  property1("option(nat)", arb_option(arb_nat), n => {
    switch n { | Some(n') => n' >= 0 | None => true }
  });

  property1("sum of nats is >= 0", arb_array(arb_nat), a => {
    Array.fold_left((+), 0, a) >= 0
  });

  let arb_record_c = unsafe_arb_record(
    (Proxy : Types.proxy({. "d": bool, "e": Js.null(int) })),
    {"d": arb_js_bool, "e": arb_null(arb_nat)}
  );

  property1(
    "unsafe_arb_record",
    unsafe_arb_record(
      (Proxy : Types.proxy({.
        "a": string, "b": int, "c": {. "d": bool, "e": Js.null(int) }
      })),
      {"a": arb_string, "b": arb_nat, "c": arb_record_c}
    ),
    r => {
      switch (Js.Types.classify(r##a)) { | Js.Types.JSString(_) => true | _ => false } &&
      switch (Js.Types.classify(r##b)) { | Js.Types.JSNumber(n) => n >= 0.0 | _ => false} &&
      switch (Js.Types.classify(r##c)) {
        | Js.Types.JSObject(o) =>
          switch (Js.Types.classify(Obj.magic(o)##d)) {
          | Js.Types.JSTrue | Js.Types.JSFalse => true
          | _ => false
          } &&
          switch (Js.Types.classify(Obj.magic(o)##e)) {
          | Js.Types.JSNull => true
          | Js.Types.JSNumber(n) => n >= 0.0
          | _ => false
          }
        | _ => false
        };
    }
  );

  property1("testing tuple", arb_tuple((arb_nat, arb_nat)), ((a, b)) => {
    a + b >= a && a + b >= b
  });

  property1("testing sum", arb_sum((arb_nat, arb_string)), s => {
    switch (Js.Types.classify(s)) {
    | Js.Types.JSString(_) | Js.Types.JSNumber(_) => true
    | _ => false
    }
  });

  property1("testing either", arb_either(arb_nat, arb_string), e => switch e {
  | Left(l) => switch (Js.Types.classify(l)) {
    | Js.Types.JSNumber(n) => n >= 0.0 | _ => false
    }
  | Right(r) => switch (Js.Types.classify(r)) {
    | Js.Types.JSString(_) => true | _ => false
    }
  });
});
