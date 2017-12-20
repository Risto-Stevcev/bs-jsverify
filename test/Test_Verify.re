open Mocha;
open Verify.Arbitrary;
open Verify.Property;

describe("Mocha Bindings", () => {
  it("2 + 3 is 5", () => expect(2 + 3).to_be(5));
  it("2 + 3 is not 4", () => expect(2 + 3).to_not_be(4));
});

describe("JsVerify", () => {
  property1("boolean `not` (involution)", arb_bool, (b) => {
    let b' = b |> Js.to_bool;
    ! (! b') == b'
  });

  property3("nat `+` (associative)", arb_nat, arb_nat, arb_nat, (n1, n2, n3) => {
    (n1 + n2) + n3 == n1 + (n2 + n3)
  });

  property1("sum of nats is >= 0", arb_array(arb_nat), (a) => {
    Array.fold_left((+), 0, a) >= 0
  });

  property1("testing tuple", arb_tuple((arb_nat, arb_nat)), ((a, b)) => {
    a + b >= a && a + b >= b
  });

  property1("testing sum", arb_sum((arb_nat, arb_string)), (s) => {
    switch (Js.Types.classify(s)) {
    | Js.Types.JSString(_) => true
    | Js.Types.JSNumber(_) => true
    | _ => false
    }
  })
});
