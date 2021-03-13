module EcCuadratica where

    ec2 :: Float -> Float -> Float -> (Float, Float)
    ec2 a b c = if a <= 0 then error "La variable 'a' no debe ser 0" else (x1,x2)
            where
                discriminante = (b*b) - (4*a*c)
                raiz = sqrt discriminante 
                x1 = (-b + raiz) / (2*a)
                x2 = (-b - raiz) / (2*a)
