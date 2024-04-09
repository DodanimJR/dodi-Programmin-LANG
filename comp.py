import ply.lex as lex
import ply.yacc as yacc

f = open(r'./test2.dodi', 'r')
codigoEntrada = f.read()

#############################################
#                                           #
#   Implementación de analizador léxico.    #
#                                           #
#############################################



# Lista de tokens de palabras reservadas.
reservadas = {
    'algoritmo': 'ALGORITMO',
    'principal': 'PRINCIPAL',
    'inicio': 'INICIO',
    'entero': 'ENTERO',
    'escribir': 'ESCRIBIR',
    'leer': 'LEER',
    'fin': 'FIN',
    'si': 'SI',
    'sino': 'SINO',
    'finsi': 'FINSI',
    'entonces': 'ENTONCES',
    'para': 'PARA',
    'hasta': 'HASTA',
    'incremento': 'INC',
    'finpara': 'FINPARA',
    'mientras': 'MIENTRAS',
    'hacer': 'HACER',
    'finmientras': 'FINMIENTRAS',
    'entero': 'ENTERO',
    'real': 'REAL',
    'caracter': 'CARACTER',
    'cadena': 'CADENA',
    'not': 'OPNOT',
    'and': 'OPAND',
    'or': 'OPOR',
    'xor': 'OPXOR',
    'verdadero': 'VERDADERO',
    'falso': 'FALSO',
    'booleano': 'BOOLEANO'
}

# Tokens de un solo caracter.
literals = ['(', ')', ',', ':', '<', '>', '=', '+', '-', '*', '/', '%']

# Tokens con expresiones regulares.
t_CADENALITERAL = r'\"[a-zA-Z 0-9:-_.!*/+\-\#%&@?¿]+\"'
t_COMENTARIO = r'\/\/.*'
t_ignore = ' \t'
t_MAYORIGUAL = r'>='
t_DIFERENTE = r'<>'
t_MENORIGUAL = r'<='
t_IGUAL = r'=='

# Tokens booleanos.
tokens = ['CADENALITERAL', 'ID', 'NUMEROENTERO', 'NUMEROREAL', 'MAYORIGUAL', 'DIFERENTE', 'MENORIGUAL', 'IGUAL',
          'COMENTARIO'] + list(reservadas.values())

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

# Especiales requeridos por Ply.
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print("Error no se reconocen los caracteres '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()
lexer.input(codigoEntrada)


# for token in lexer:
#     print(token)

## Análisis Sintáctico

precedence = (
    ('left', 'OPOR'),
    ('left', 'OPAND'),
    ('left', 'OPXOR'),
    ('left', 'MAYORIGUAL', 'MENORIGUAL', 'IGUAL', 'DIFERENTE'),
    ('left', '+', '-'),
    ('left', '*', '/'),
    ('right', 'UMENOS'),  # Para el operador unario -
)

# Definición de la gramática.
def p_A(p):
    ''' A : ALGORITMO ID PRINCIPAL INICIO instrucciones FIN '''

def p_instrucciones(p):
    ''' instrucciones : instruccion instrucciones'''

def p_instrucciones_1(p):
    ''' instrucciones :  '''

def p_asig(p):
    ''' asignacion : ID "=" operacion  '''

def p_operacion(p):
    '''operacion :
                 | operacion '+' operacion
                 | operacion '-' operacion
                 | operacion '*' operacion
                 | operacion '/' operacion
                 | operacion MAYORIGUAL operacion
                 | operacion MENORIGUAL operacion
                 | operacion IGUAL operacion
                 | operacion DIFERENTE operacion
                 | operacion '<' operacion
                 | operacion '>' operacion
                 | '(' operacion ')' '''

def p_operacion2(p):
    ' operacion : "-" operacion %prec UMENOS '
    

def p_operacionID(p):
    ' operacion :  ID '

def p_operacion1(p):
    ''' operacion : numero
                  | CADENALITERAL
                  | CARACTER '''
    

def p_instruccion(p):
   ''' instruccion : declaracion
                    | asignacion
                    | mientras
                    | si
                    | para
                    | lectura
                    | escritura '''

def p_numero(p):
    ''' numero : NUMEROENTERO
               | NUMEROREAL '''


def p_declaracion(p):
    ''' declaracion : tipo defineTipo ":" ID listaVar '''

def p_declaracion3(p):
    ' declaracion : tipo defineTipo ":" ID "=" operacion listaVar '

def p_defineTipo(p):
    ' defineTipo : '

def p_lista3(p):
    ''' listaVar : "," ID  listaVar'''

def p_lista1(p):
    ' listaVar : '
    
# Produccion de una lista de variables
def p_lista5(p):
    ''' listaVar : "," ID "=" operacion listaVar '''

def p_tipo(p):
    ''' tipo : ENTERO '''

def p_tipoChar(p):
    ' tipo : CARACTER '
    
def p_tipoFloat(p):
    ' tipo : REAL '
    
def p_tipoCharPtr(p):
    ' tipo : CADENA '

def p_tipoBool(p):
    ' tipo : BOOLEANO '

def p_operacionLogica(p):
    '''operacionLogica : operacion
                    | operacionLogica OPOR operacion
                    | operacionLogica OPAND operacion
                    | operacionLogica OPXOR operacion
                    | OPNOT operacion
                    | '(' operacionLogica ')' '''

def p_comparacion1(p):
    ' comparacion : operando '

def p_comparacion3(p):
    ''' comparacion : operando "<" operando
                    | operando ">" operando
                    | operando MAYORIGUAL operando
                    | operando MENORIGUAL operando
                    | operando IGUAL operando
                    | operando DIFERENTE operando
                    | "(" comparacion ")"'''
    
def p_operando(p):
    ''' operando : operacion
                 | ID
                 | VERDADERO
                 | FALSO'''

def p_lectura(p):
    ''' lectura : LEER "(" ID ")" '''
    
def p_escritura(p):
    ''' escritura : ESCRIBIR "(" listaSalida ")" '''

def p_listaSalida(p):
    ''' listaSalida : listaSalida "," salida
                    | salida'''

def p_salida(p):
    ' salida : ID '

def p_salida2(p):
    ' salida : numero '

def p_salida3(p):
    ' salida :  '


def p_salida4(p):
    ' salida : CADENA '


def p_salida5(p):
    ' salida : CADENALITERAL '
    
def p_esi(p):
    '''si : SI "(" operacionLogica ")" ENTONCES instrucciones sino FINSI
          | SI "(" operacionLogica ")" OPAND "(" operacionLogica ")" ENTONCES instrucciones sino FINSI
          | SI "(" operacionLogica ")" OPOR "(" operacionLogica ")" ENTONCES instrucciones sino FINSI
          | SI "(" operacionLogica ")" OPXOR "(" operacionLogica ")" ENTONCES instrucciones sino FINSI
    '''
        

def p_sino1(p):
    ' sino : '

def p_sino2(p):
    ' sino : SINO instrucciones ' 

def p_mientras(p):
    ' mientras : MIENTRAS "(" operacionLogica ")" HACER instrucciones FINMIENTRAS '

def p_para(p):
    ' para : PARA ID "=" numero HASTA numero inc instrucciones FINPARA '

def p_inc(p):
    ''' inc : INC numero
            |  '''

    
def p_error(p):
    if p:
        print("Error de sintaxis en '%s'" % p.value)
        print("en la línea '%d'" % p.lineno)
        
    else:
        print("Error sintáctico, al final.")

# Construir el parser
yacc.yacc()
yacc.parse(codigoEntrada)
