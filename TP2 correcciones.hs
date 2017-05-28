import Text.Show.Functions
import Data.List
type Bebida = Cliente->Cliente
type Acciones = Cliente->Cliente 
data Cliente = Cliente {nombre::String, resistencia::Int, amigos::[Cliente],bebidas::[Bebida]} deriving (Show)
data Itinerario = Itinerario{nombreItinerario::String, horas::Float, acciones::[Acciones]} deriving (Show)

--Decidimos utilizar este tipo de dato "data" porque es más declarativo e intuitivo que el tipo de data tuplas, y además 
--cualquier persona que lea el tipo de dato, puede identificar a que tipo de dato corresponde cada elemento de la "data Cliente".

--clientes:
rodri = Cliente {nombre="Rodri", resistencia=55,amigos=[], bebidas=[tintico]}
marcos = Cliente {nombre="Marcos", resistencia=40 ,amigos=[rodri], bebidas=[klusener "guinda"]}
cristian = Cliente {nombre="Cristian", resistencia=2 ,amigos=[], bebidas=[grogxd,jarraloca]}
ana = Cliente {nombre="Ana", resistencia=120 ,amigos=[marcos,rodri], bebidas=[]}
robertoCarlos = Cliente { nombre="Roberto Carlos", resistencia=165, amigos=[], bebidas=[]}
chuckNorris = Cliente {nombre="Chuck", resistencia=1000, amigos=[ana], bebidas=(sodasInfinitas 1) }

--itinerarios:
mezclaExplosiva = Itinerario{nombreItinerario="Mezcla Explosiva",horas=2.5,acciones=[tomar grogxd,tomar grogxd,tomar (klusener "huevo"),tomar (klusener "frutilla")]}
itinerarioBasico = Itinerario{nombreItinerario="Itinerario Básico",horas=5,acciones=[tomar jarraloca,tomar (klusener "chocolate"),rescatarse 2, tomar (klusener "huevo")]}
salidaDeAmigos = Itinerario{nombreItinerario="Salida De Amigos",horas=1,acciones=[tomar (soda 1), tomar tintico, (reconocerAmigo) robertoCarlos,tomar jarraloca]}

cantidadAmigos persona = (length.amigos) persona
esAmigo cliente amigo =  elem (nombre amigo) (nombreAmigos cliente)
nombreAmigos cliente = map nombre (amigos cliente)
puedeAgregar cliente amigo = not(esAmigo cliente amigo) && (nombre cliente /= nombre amigo)
agregarAmigo cliente amigo = cliente{amigos = amigo : amigos cliente}

reconocerAmigo amigo cliente
    |puedeAgregar cliente amigo = agregarAmigo cliente amigo
    |otherwise = cliente


comoEsta cliente 
    |  ((>50).resistencia) cliente = "fresco"
    | ((>1).length.amigos) cliente = "piola"
    | otherwise = "duro"

bajarResistencia cant cliente  = cliente{resistencia = (resistencia cliente) - cant  } 
subirResistencia cant cliente  = cliente{resistencia = (resistencia cliente) + cant  } 

--bebidas
grogxd cliente = cliente{resistencia=0}

bajarResistenciaAmigos cliente = cliente{amigos=(map (bajarResistencia 10 ) (amigos cliente))}

jarraloca cliente =   bajarResistenciaAmigos(bajarResistencia 10 cliente)

klusener gusto = bajarResistencia  (length gusto)

tintico cliente = subirResistencia  (5* cantidadAmigos cliente) cliente

soda  fuerza cliente= cliente{nombre = erepea fuerza++(nombre cliente)}

erepea fuerza = "e"++ (replicate fuerza 'r') ++ "p" 

rescatarse horas
    | horas > 3 = subirResistencia 200 
    | otherwise = subirResistencia 100  

--Punto 1
tomar trago cliente = (trago cliente){bebidas=(bebidas cliente) ++ [trago]} 

tomarTragos listaTragos cliente = foldl (\cliente listaTragos -> tomar listaTragos cliente) cliente listaTragos

cantidadBebidas = (length.bebidas)

ultimaBebida =(last.bebidas)

dameOtro cliente  = ((tomar.ultimaBebida) cliente) cliente 

--Punto 2
puedeTomar cliente trago= (((>0).resistencia.tomar trago) cliente)

cualesPuedeTomar cliente  = filter(puedeTomar cliente)  

cuantasPuedeTomar cliente  = (length.cualesPuedeTomar cliente) 


--Punto 3
itinerario (Itinerario _ _ listaActividades) cliente = foldl (\cliente listaActividades -> listaActividades cliente) cliente listaActividades

--Punto 4
calcularIntensidad itinerario = (((/ horas itinerario).genericLength.acciones) itinerario) 

masIntenso itinerario otroItinerario  | calcularIntensidad itinerario > calcularIntensidad otroItinerario = itinerario
                                      | otherwise = otroItinerario

itinerarioMasIntenso listaItinerarios = foldl1 masIntenso listaItinerarios

elegirMasIntenso listaItinerarios = (itinerario.itinerarioMasIntenso) listaItinerarios 

--Punto 5
sodasInfinitas n =  soda n : sodasInfinitas (n+1)
