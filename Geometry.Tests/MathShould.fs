namespace Geometry.Tests
open FeldSpar.Framework
open FeldSpar.Framework.Verification
open FeldSpar.Framework.Verification.ApprovalsSupport

type Point = { X:int; Y:int; }
type Origin = Point
type Destination = Point

module ``Math should`` =
    open Geometry.Math

    let first40Primes = [|2 ;3 ;5 ;7 ;11 ;13 ;17 ;19 ;23 ;29 ;31 ;37 ;41 ;43 ;47 ;53 ;59 ;61 ;67 ;71 ;73 ;79 ;83 ;89 ;97 ;101 ;103 ;107 ;109 ;113 ;127 ;131 ;137 ;139 ;149 ;151 ;157 ;163 ;167 ;173|]

    let ``Be able to find the lowest common denominator between 2 numbers`` = 
        Theory({
                Data = 
                    let max = first40Primes.Length
                    let r = System.Random()
                    let testCases = 
                        seq { for i in first40Primes do
                                    let a = first40Primes.[r.Next(max)]
                                    let b = first40Primes.[r.Next(max)]

                                    let gcd = if a = b then a * i else i
                                    yield (i * a, i * b, gcd)
                            }
                    testCases |> Seq.append testCases |> Seq.append testCases
                Base =
                {
                    UnitDescription = (fun (a, b, expected) -> sprintf " %d and %d has lowest common denominator of %d" a b expected)
                    UnitTest =
                        (fun (a, b, expected) _ ->
                            greatestCommonDenominator a b |> expectsToBe expected
                        )
                }
              })

module ``Line should`` =
    open Geometry
    open Line

    let Expected (v:'a) = v

    let ``be able to determine points in line segment`` =
        Theory({
                    Data = [
                                // 3pt line segments
                                ({ X=1; Y=1 }, { X=1; Y=3; },   Expected [ { X=1; Y=1 }; { X=1; Y=2 }; { X=1; Y=3 } ]);
                                ({ X=1; Y=1 }, { X=3; Y=1; },   Expected [ { X=1; Y=1 }; { X=2; Y=1 }; { X=3; Y=1 } ]);
                                ({ X=1; Y=1 }, { X=3; Y=3; },   Expected [ { X=1; Y=1 }; { X=2; Y=2 }; { X=3; Y=3 } ]);
                                ({ X=1; Y=1 }, { X=7; Y=9; },   Expected [ { X=1; Y=1 }; { X=4; Y=5 }; { X=7; Y=9 } ]); // 3 4 5 triangle hypotonuse
                                ({ X=1; Y=1 }, { X=3; Y=5; },   Expected [ { X=1; Y=1 }; { X=2; Y=3 }; { X=3; Y=5 } ]); // m = 1/2
                                // 4pt line segments
                                ({ X=1; Y=1 }, { X=1; Y=4; },   Expected [ { X=1; Y=1 }; { X=1; Y=2 }; { X=1; Y=3 }; { X=1; Y=4 } ]);
                                ({ X=1; Y=1 }, { X=4; Y=1; },   Expected [ { X=1; Y=1 }; { X=2; Y=1 }; { X=3; Y=1 }; { X=4; Y=1 } ]);
                                ({ X=1; Y=1 }, { X=4; Y=4; },   Expected [ { X=1; Y=1 }; { X=2; Y=2 }; { X=3; Y=3 }; { X=4; Y=4 } ]);
                                ({ X=1; Y=1 }, { X=10; Y=13; }, Expected [ { X=1; Y=1 }; { X=4; Y=5 }; { X=7; Y=9 }; { X=10; Y=13; } ]); // 3 4 5 triangle hypotonuse
                                ({ X=1; Y=1 }, { X=4; Y=7; },   Expected [ { X=1; Y=1 }; { X=2; Y=3 }; { X=3; Y=5 }; { X=4; Y=7; } ]); // m = 1/2
                           ] |> Seq.ofList
                    Base =
                    {
                        UnitDescription = (fun (st, en, _) -> sprintf " %s -> %s" (st.ToString()) (en.ToString()) )
                        UnitTest =
                            (fun (st, en, expected) _ ->
                                st |> calculateLineTo en |> List.ofSeq |> expectsToBe expected
                            )
                    }
               })
