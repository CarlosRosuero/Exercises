-- All but A stand out in some way

data Thing = A | B | C | D | E deriving ( Eq , Show )
things = [A, B, C, D, E]

-- Colour, Size, Shape, and Contour thickness

data Colour = Blue | Amber deriving ( Eq , Show )
data Size = Big | Small deriving ( Eq , Show )
data Shape = Disk | Square deriving ( Eq , Show )
data Contour = Thick | Thin deriving ( Eq , Show )

colour :: Thing -> Colour
colour D = Blue
colour _ = Amber
size :: Thing -> Size
size E = Small
size _ = Big
shape :: Thing -> Shape
shape C = Disk
shape _ = Square
contour :: Thing -> Contour
contour B = Thin
contour _ = Thick

type Predicate u = u -> Bool

isBlue :: Predicate Thing
isBlue x = colour x == Blue
isAmber :: Predicate Thing
isAmber x = not ( isBlue x )
isSmall :: Predicate Thing
isSmall x = size x == Small
isBig :: Predicate Thing
isBig x = not ( isSmall x )
isDisk :: Predicate Thing
isDisk x = shape x == Disk
isSquare :: Predicate Thing
isSquare x = not ( isDisk x )
isContourThick :: Predicate Thing
isContourThick x = contour x == Thick
isContourThin :: Predicate Thing
isContourThin x = not ( isContourThick x )

thingsOtherThan :: Thing -> [ Thing ]
thingsOtherThan thing = [ other | other <- things , other /= thing]

properties :: [ Predicate Thing ]
properties = [ isBlue , isAmber , isSmall , isBig , isDisk , isSquare , isContourThick , isContourThin ]

propertiesOf :: Thing -> [ Predicate Thing ]
propertiesOf thing = [ property | property <- properties , property thing == True ]

isPropertyOfAnotherThing :: Predicate Thing -> Thing -> Bool
isPropertyOfAnotherThing property thing = or [ property other | other <- things , other /= thing ]

propertiesOnlyOf :: Thing -> [Predicate Thing]
propertiesOnlyOf thing = [property | property <- properties, property thing, not (isPropertyOfAnotherThing (property) (thing))]

rank :: Thing -> Int
rank thing = length (propertiesOnlyOf thing)

-- A is the only one with rank 0

statement1 = and [isContourThin x | x <- things , isBlue x , isSquare x]
statement2 = or [isSquare x | x <- things , isAmber x]
statement3 = and [isAmber x || isContourThick x | x <- things, isBig x , isSquare x]
statement4 = or [isSmall x | x <- things , isAmber x , isDisk x]

statement5 = not (or [isBlue x | x <- things , isSquare x])
statement6 = and [not (isBlue x) | x <- things , isSquare x]