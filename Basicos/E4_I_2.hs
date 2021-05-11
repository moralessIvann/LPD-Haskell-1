{-
    Creador: Iván Morales 
    Repositorio: https://github.com/Medina1402/LPD-Haskell 
    Fecha: 13-03-21
    
    Descripcion: *** Ecuación Cuadrática ***
        Escriba un programa en computadora para evaluar las raíces de la ecuación cuadrática
        (ax2+bx+c=0), donde a≠0, b y c son constantes reales.
-}

module E4_I_2 where

    ec2 :: Float -> Float -> Float -> (Float, Float)
    ec2 a b c = if a <= 0 then error "La variable 'a' no debe ser 0" else (x1,x2)
            where
                discriminante = (b*b) - (4*a*c)
                raiz = sqrt discriminante 
                x1 = (-b + raiz) / (2*a)
                x2 = (-b - raiz) / (2*a)