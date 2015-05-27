namespace Geometry

type Point = { X:int; Y:int; } with
                override this.ToString () =
                    sprintf "{ X:%d; Y:%d }" this.X this.Y
type Origin = Point
type Destination = Point


module Math = 
    let rec greatestCommonDenominator a b =
        if b = 0 then a
        else
            greatestCommonDenominator b (a % b)

// Thank you begin: @cm_stead https://github.com/cmstead
module Line = 
    open Math

    type Slope =
        {
            Numerator:int;
            Denominator:int;
            GreatestCommonDenominator:int;
        }

    let slopeTo (destination:Destination) (origin:Origin) =
        let dx, dy = destination.X - origin.X, destination.Y - origin.Y
        let gcd = greatestCommonDenominator dx dy

        { Numerator = dx / gcd; Denominator = dy / gcd; GreatestCommonDenominator = gcd }

    let calculateLineTo (destination:Destination) (origin:Origin) =
        let { Numerator = dx; Denominator = dy; GreatestCommonDenominator = steps } = origin |> slopeTo destination

        seq { for i = 0 to steps do
                yield { X = origin.X + i * dx; Y = origin.Y + i * dy}
            }
