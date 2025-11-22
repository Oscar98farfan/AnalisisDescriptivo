import pandas as pd

path_file = "./Data.xlsx"
path_file_2 = "./DataAuxiliar.csv"

data = pd.read_excel(path_file)
dataAux = pd.read_csv(path_file_2, sep=',', encoding='latin1')

#print(data.columns)

#================================ Diccionario de categorias ==========================================
cat_map = {
    "Acotamiento": "Condiciones de la via",
    "Alguien se cayo/avento del vehiculo": "Factor humano",
    "Algún evento nocivo sin detalles": "Otros",
    "Animal montado o vehiculo de traccion animal": "Animales",
    "Animal vivo": "Animales",
    "Arbol": "Elementos externos naturales / urbanos",
    "Arbusto": "Elementos externos naturales / urbanos",
    "Banqueta": "Condiciones de la via",
    "Barda": "Elementos externos naturales / urbanos",
    "Barrera de Cable": "Elementos externos naturales / urbanos",
    "Barrera de Trafico de Concreto": "Elementos externos naturales / urbanos",
    "Buzon de correo": "Elementos externos naturales / urbanos",
    "Calle mal pavimentada": "Condiciones de la via",
    "Ciclista": "Otro usuario de la via",
    "Culvert": "Infraestructura",
    "Desconocido": "Otros",
    "Edificio": "Elementos externos naturales / urbanos",
    "Estructura de puente overhead": "Infraestructura",
    "Fin de valla": "Elementos externos naturales / urbanos",
    "Final Destination ( Vehiculo es golpeado por carga o otro objeto puesto en movimiento por otro vehiculo en movimiento)": "Vehiculos externos",
    "Fuego/Explosion": "Condiciones ambientales / externas",
    "Hidrante": "Elementos externos naturales / urbanos",
    "Immersion total o parcial": "Condiciones ambientales / externas",
    "Jackknife (harmful to this vehicle)": "Factor humano",
    "La carga del vehiculo (cambio, perdida, o otro)": "Carga y objetos transportados",
    "Lastimado en vehiculo, no colision": "Factor humano",
    "Muro": "Elementos externos naturales / urbanos",
    "Nieve": "Condiciones ambientales / externas",
    "No vehiculo o remolque": "Factor humano",
    "Objeto aventado o caido": "Carga y objetos transportados",
    "Objeto desconocido no fijo": "Otros",
    "Objeto fijo desconocido": "Otros",
    "Objeto que se habia caido del vehiculo": "Carga y objetos transportados",
    "Otra barrera de trafico": "Elementos externos naturales / urbanos",
    "Otra no colicion": "Otros",
    "Otro objeto (fijo)": "Elementos externos naturales / urbanos",
    "Otro objeto (no fijo)": "Elementos externos naturales / urbanos",
    "Peaton": "Otro usuario de la via",
    "Piedrotototota": "Elementos externos naturales / urbanos",
    "Poste": "Elementos externos naturales / urbanos",
    "Poste de Señal de trafico": "Elementos externos naturales / urbanos",
    "Poste o otros soportes": "Elementos externos naturales / urbanos",
    "Puente de Ferrocarril": "Infraestructura",
    "Soporte de  señalamiento de transito": "Elementos externos naturales / urbanos",
    "Soporte de Puente": "Infraestructura",
    "Suelo": "Condiciones de la via",
    "Valla Ferrocarril": "Elementos externos naturales / urbanos",
    "Valla para choques": "Elementos externos naturales / urbanos",
    "Vehiculo Aparcado": "Vehiculos externos",
    "Vehiculo en movimiento": "Vehiculos externos",
    "Vehiculo Ferrocarril": "Vehiculos externos",
    "Vehiculo Funcionando": "Vehiculos externos",
    "Veihulo de calle en rieles": "Vehiculos externos",
    "Volcadura": "Factor humano",
    "Zanja": "Condiciones de la via",
}

#========================== Eliminar columnas que no se van a usar =============================================
# Opcion 1: Meter las columnas utiles en lista y usar solo esas.
colums_df = ['Nombre de Estado', 'Peatones', 'Personas no en vehiculos transportadas', 'Vehiculos automotores', 'Vehiculos estacionados', 'Personas;Personas en Vehiculo', 'Mes', 'Dia',  'Año', 'Hora', 'Rural o Urbanoo', 'Causa del Accidente', 'Tipo de Accidente', 'Luz', 'Clima', 'Decesos']

df2 = data[colums_df]

# Opcion 2 Eliminar columnas innecesarios
df = data.drop(columns=['Estado', 'Condado', 'Nombre Condado', 'Ciudad', 'Nombre Ciudad', 'Nombre Mes', 'Nombre Dia',  'Minuto', 'Calle', 'Ruta', 'Nombre Ruta', 'Sistema Funcional', 'Dueño Calle', 'Sistema Nacional Carreteras', 'Jurisdiccion Especial', 'Milla', 'Latitud', 'Longitud', 'Tipo Interseccion', 'Lugar en la calle', 'Zona de Trabajo', 'Autobus Escolar', 'Cruce de Trenes', 'Hora Notificacion', 'Minuto Notificacion', 'Hora Llegada', 'Minuto Llegada'])

#========================== Creacion fecha completa =============================================
# Generacion de fecha
df['Fecha'] = df['Dia'].astype(str) + '-' + df['Mes'].astype(str) + '-' + df['Año'].astype(str)
df['Fecha'] = pd.to_datetime(df['Fecha'], format= '%d-%m-%Y')
# Conversion de la hora
# df['Hora'] = df['Hora'].astype(int).astype(str).str.zfill(2)
# df['Fecha'] = df['Fecha'].astype(str) + ' ' + df['Hora'] + ':00:00'
# df['Fecha'] = pd.to_datetime(df['Fecha'])
# Obtencion de dia
df["DiaSemana"] = df['Fecha'].dt.day_name()

df["DiaSemanaNum"] = df["Fecha"].dt.weekday


# print(df[["Fecha", "Dia", "Mes", "Año", "DiaSemana"]])

#========================== Separar columna con datos pegados =============================================
# Generacion de fecha
# expand=True crea nuevas columnas a partir del resultado de split
df['Personas'] = df['Personas;Personas en Vehiculo'].str.split(';').str[0]
df['Personas en Vehiculo'] = df['Personas;Personas en Vehiculo'].str.split(';').str[1]

#========================== Datos numericos como horas sin dato =============================================
# Los registros que no presentan hora se opta por dejarlos debido a que
# no se pueden igualar a la media o a algun otro dato, debido a que alteran los reusltados
# Como es un analsisi descriptivo se necesita información real para analizar patrones 
# por ello no se elimina ni se transforma. 
# Si fueran datois para un modelo predictivo si se eliminan o transforman.

#========================== Categorizar Causas de accidentes =============================================

df["CausaAccidente"] = df["Causa del Accidente"].map(cat_map)

# print(df["CausaAccidente"].unique())

#========================== Recategorizar Tipo de accidentes =============================================

df["Tipo de Accidente"] = df["Tipo de Accidente"].replace({
    'El primer evento no fue una colision con un vehiculo automotor en transporte': 'Choque secundario',
    'Reportado como desconocido': 'Otro'
})

# print(len(df["Tipo de Accidente"].unique()))

#========================== Combinar Rural o Urbano =============================================

df["Rural o Urbanoo"] = df["Rural o Urbanoo"].replace({
    'Trafficway Not in State Inventory': 'Rural',
    'Unknown': 'Sin dato',
    'Not Reported': 'Sin dato'
})   

# print(df["Rural o Urbanoo"].unique())

#========================== Arreglar columna Luz =============================================

df["Luz"] = df["Luz"].replace({
    'Oscuro - iluminacion desconocida': 'Oscuro sin iluminacion',
    'Otra': 'No reportada',
    'Reportada como desconocida': 'No reportada'
})

df["Luz"] = df["Luz"].replace({
    'Oscuro sin iluminacion': 'Oscuro',
    'Oscuro con iluminacion': 'Oscuro'
})

#========================== Recategorizar Clima =============================================

df["Clima"] = df["Clima"].replace({
    'Reportado como desconocido': 'Otro',
    'Vientos cruzados pero de los mamalones': 'Soplando arena, polvo o tierra',
    'Soplando nieve': 'Soplando arena, polvo o tierra'
})   

df["Clima"] = df["Clima"].replace({
    'Soplando arena, polvo o tierra': 'Soplando arena, polvo, tierra o nieve'
})   

# print(df["Clima"].unique())

#======================== Eliminar columnas innecesarias ===============================

df = df.drop(columns=['Personas;Personas en Vehiculo'])

#======================== Eliminar datos duplicados ===============================

df = df.drop_duplicates()


#===================== Traer la data auxiliar =====================================
# Opcion 2 Eliminar columnas innecesarios
dfAux = dataAux.drop(columns=['Año', 'Estado', 'Tipo de Choque', 'Region', 'Rural o Rubano', 'Tipo Choque'])

#===================== cambiar variables categoricas =====================================

col_categ = ['Camion Grande',	'Motocicleta',	'Exceso de Velocidad',	'Persecución Policial',	'Adolescente (15-20)',	'Anciano (65+)',	'Joven (20-24)',	'Se fugo',	'Distraido',	'Cansancio']

dfAux[col_categ] = dfAux[col_categ].replace({1: 'Si', 2: 'No'})

#===================== Hacer Join con las dos tablas =====================================

dataFinal = pd.merge(df, dfAux, left_on='Numero de Caso', right_on='EstadoCaso', how='outer')

# print(dataFinal.columns)

#===================== Eliminar Columnas y filas innecesarias =====================================
dataFinal = dataFinal.drop(columns=['Numero de Caso', 'EstadoCaso', 'Muertes', 'Ciclista', 'Ciclista Muerto', 'Volcadura', 'Alcoholimetro', 'Salio del Camino', 'Sentido Contrario', 'Peatron', 'Peaton Muerto'])

dataDrop = dataFinal[dataFinal["Hora"] == 99].index
dataFinal = dataFinal.drop(dataDrop)

#========================== Exportar Data Limpia =============================================

dataFinal.to_excel("DataFinal.xlsx", index= False)


#print(df.columns)

# print(df.info())
