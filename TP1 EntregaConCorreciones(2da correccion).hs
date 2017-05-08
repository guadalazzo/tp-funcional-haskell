data Cliente = Cliente {nombre::String, resistencia::Int, amigos::[Cliente]} deriving (Show)

--Decidimos utilizar este tipo de dato "data" porque es más declarativo e intuitivo que el tipo de data tuplas, y además 
--cualquier persona que lea el tipo de dato, puede identificar a que tipo de dato corresponde cada elemento de la "data Cliente".
rodri = Cliente {nombre="Rodri", resistencia=55,amigos=[]}
marcos = Cliente {nombre="Marcos", resistencia=40 ,amigos=[rodri]}
cristian = Cliente {nombre="Cristian", resistencia=2 ,amigos=[]}
ana = Cliente {nombre="Ana", resistencia=120 ,amigos=[marcos,rodri]}


cantidadAmigos persona = (length.amigos) persona
esAmigo cliente amigo =  elem (nombre amigo) (nombreAmigos cliente)
nombreAmigos cliente = map nombre (amigos cliente)
puedeAgregar cliente amigo = not(esAmigo cliente amigo) && (nombre cliente /= nombre amigo)
agregarAmigo cliente amigo = cliente{amigos = amigo : amigos cliente}

reconocerAmigo cliente amigo
    |puedeAgregar cliente amigo = agregarAmigo cliente amigo
    |otherwise = cliente

comoEsta (Cliente nombre resistencia amigos) 
    | resistencia > 50 = "fresco"
    | ((>1).length) amigos = "piola"
    | otherwise = "duro"

bajarResistencia cant cliente  = cliente{resistencia = (resistencia cliente) - cant  } 
subirResistencia cant cliente  = cliente{resistencia = (resistencia cliente) + cant  } 


grogxd cliente = cliente{resistencia=0}
bajarResistenciaAmigos cliente = cliente{amigos=(map (bajarResistencia 10 ) (amigos cliente))}

jarraloca cliente =   bajarResistenciaAmigos(bajarResistencia 10 cliente)

klusener gusto = bajarResistencia  (length gusto)

tintico cliente = subirResistencia  (5* cantidadAmigos cliente) cliente  
soda cliente fuerza = cliente{nombre = erepea fuerza++(nombre cliente)}
erepea fuerza = "e"++ (replicate fuerza 'r') ++ "p" 

rescatarse horas 
    | horas > 3 = subirResistencia 200 
    | otherwise = subirResistencia 100  

