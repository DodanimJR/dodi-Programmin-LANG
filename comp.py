f = open(r'./test2.dodi', 'r')
codigoEntrada = f.read()

#############################################
#                                           #
#   Implementación de analizador léxico.    #
#                                           #
#############################################

import ply.lex as lex

# Lista de tokens de palabras reservadas.
reservadas = { 'algoritmo' : 'ALGORITMO',
                'principal' : 'PRINCIPAL',
                'inicio' : 'INICIO',
                'entero' : 'ENTERO',
                'escribir' : 'ESCRIBIR',
                'leer' : 'LEER',
                'fin' : 'FIN',
                'si' : 'SI',
                'sino' : 'SINO',
                'finsi' : 'FINSI',
                'entonces' : 'ENTONCES',
                'para' : 'PARA',
                'hasta' : 'HASTA',
                'incremento' : 'INC',
                'finpara' : 'FINPARA',
                'mientras' : 'MIENTRAS',
                'hacer' : 'HACER',
                'finmientras' : 'FINMIENTRAS',
                'entero' : 'ENTERO',
                'real' : 'REAL', 
                'caracter' : 'CARACTER',
                'cadena' : 'CADENA',
                'no' : 'NO',
                'y' : 'OPLY',
                'o' : 'OPLO'
               }

# Tokens de un solo caracter. 
literals = ['(', ')', ',', ':','<', '>', '=', '+', '-' , '*', '/', '%']

# Tokens con expresiones regulares.
t_CADENALITERAL = r'\"[a-zA-Z 0-9:-_.!*/+\-\#%&@?¿]+\"'
t_COMENTARIO = r'\/\/.*'
t_ignore = ' \t'
t_MAYORIGUAL = r'>='
t_DIFERENTE = r'<>'
t_MENORIGUAL = r'<='
t_IGUAL = r'=='


# Tokens booleanos.
tokens = ['CADENALITERAL', 'ID', 'NUMEROENTERO', 'NUMEROREAL','MAYORIGUAL', 'DIFERENTE', 'MENORIGUAL', 'IGUAL','COMENTARIO', 'VERDADERO', 'FALSO', 'AND', 'OR', 'NOT'] + list(reservadas.values())

# tokens como funciones.
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reservadas.get(t.value, 'ID')
    if t.value.upper() == 'VERDADERO' or t.value.upper() == 'FALSO':
        t.type = t.value.upper()
    return t

def t_NUMEROREAL(t):
    r'[+-]?[0-9]+\.[0-9]+'
    t.value = t.value
    return t

def t_NUMEROENTERO(t):
    r'[+-]?[0-9]+'
    t.value = t.value
    return t

# Tokens booleanos.
t_AND = r'y'
t_OR = r'o'
t_NOT = r'no'

# Especiales requeridos por Ply.
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print("Error no se reconocen los caracteres '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()
lexer.input(codigoEntrada)
for token in lexer:
    print(token)
