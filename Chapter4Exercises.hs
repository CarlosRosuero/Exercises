angleVectors :: (Float, Float) -> (Float, Float) -> Float
angleVectors (x,y) (x',y') = acos (x*x'+y*y')/(sqrt(x^2+y^2)*sqrt(x'^2+y'^2))