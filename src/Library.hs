module Library where
import PdePreludat

-- 1. Numeros

siguiente :: Number -> Number
siguiente num= num+1

esPositivo :: Number -> Bool
esPositivo num = num > 0

-- escriban el tipo de esta función
inversa :: Number-> Number
inversa n = 1/n

-- 2. Temperatura
celsiusAFahrenheit :: Number -> Number
celsiusAFahrenheit celsius = (celsius * 1.8) + 32

fahrenheitACelsius :: Number -> Number
fahrenheitACelsius fahrenheit = (fahrenheit - 32) / 1.8

-- escriban el tipo de esta función
haceFrioCelsius :: Number->Bool
haceFrioCelsius grados = grados=<8

-- escriban el tipo de esta función
haceFrioFahrenheit:: Number->Bool
haceFrioFahrenheit grados = grados=<46.4

-- 2.5 Bonus OPCIONAL
perimetroCirculo :: Number -> Number
perimetroCirculo radio = 2*pi*radio

perimetroCuadrado :: Number -> Number
perimetroCuadrado lado = lado*4

superficieCuadrado :: Number -> Number
superficieCuadrado lado = lado*lado

superficieCubo :: Number -> Number
superficieCubo lado = 6*lado*lado

superficieCilindro :: Number -> Number -> Number
superficieCilindro radio altura = 2*pi*radio*(radio+altura)
