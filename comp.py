import ply.lex as lex
import ply.yacc as yacc
import ast

f = open(r'./test3.dodi', 'r')
codigoEntrada = f.read()
result = open(r'./result.log', 'w')

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
    'not': 'OPNOT',
    'and': 'OPAND',
    'or': 'OPOR',
    'xor': 'OPXOR',
    'verdadero': 'VERDADERO',
    'falso': 'FALSO',
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
# Lista de salidas de los comandos 'escribir'
salidas_escribir = []

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

variables = {}
# Definición de la gramática.
def p_A(p):
    ''' A : ALGORITMO ID PRINCIPAL INICIO instrucciones FIN '''

def p_instrucciones(p):
    ''' instrucciones : instruccion instrucciones'''
    

def p_instrucciones_1(p):
    ''' instrucciones :  '''
    p[0] = ''

def p_asig(p):
    ''' asignacion : ID "=" operacion  '''
    variables[p[1]] = p[3]


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
    
    p[1] = int(p[1])
    p[3] = int(p[3])   
    if p[2] == '+':
        p[0] = p[1] + p[3]
    elif p[2] == '-':
        p[0] = p[1] - p[3]
    elif p[2] == '*':
        p[0] = p[1] * p[3]
    elif p[2] == '/':
        p[0] = p[1] / p[3]
    elif p[2] == '>=':
        p[0] = p[1] >= p[3]
    elif p[2] == '<=':
        p[0] = p[1] <= p[3]
    elif p[2] == '==':
        p[0] = p[1] == p[3]
    elif p[2] == '<>':
        p[0] = p[1] != p[3]
    elif p[2] == '<':
        p[0] = p[1] < p[3]
    elif p[2] == '>':
        p[0] = p[1] > p[3]

    

    

def p_operacion2(p):
    ' operacion : "-" operacion %prec UMENOS '
    p[0] = p[1] + p[2]
    

def p_operacionID(p):
    ' operacion :  ID '
    p[0] = variables.get(p[1], None)

def p_operacion1(p):
    ''' operacion : NUMEROENTERO
    '''
    p[0] = p[1]
    

def p_instruccion(p):
   ''' instruccion :
                    | operacion
                    | asignacion
                    | mientras
                    | si
                    | para
                    | lectura
                    | escritura '''
   p[0] = p[1]

def p_numero(p):
    ''' numero : NUMEROENTERO
               | NUMEROREAL '''
    p[0] = p[1]


def p_operacionLogica(p):
    '''operacionLogica : operacion
                    | operacionLogica OPOR operacion
                    | operacionLogica OPAND operacion
                    | operacionLogica OPXOR operacion
                    | OPNOT operacion
                    | '(' operacionLogica ')' '''
    p[0] = p[1]

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
    ''' escritura : ESCRIBIR "(" CADENALITERAL ")"
                    | ESCRIBIR "(" operacion ")"
    '''
    p[0] = p[3]
    resultado = p[3]
    salidas_escribir.append(resultado)


    
def p_esi(p):
    '''si : SI "(" operacionLogica ")" ENTONCES instrucciones sino FINSI
          | SI "(" operacionLogica ")" OPAND "(" operacionLogica ")" ENTONCES instrucciones sino FINSI
          | SI "(" operacionLogica ")" OPOR "(" operacionLogica ")" ENTONCES instrucciones sino FINSI
          | SI "(" operacionLogica ")" OPXOR "(" operacionLogica ")" ENTONCES instrucciones sino FINSI
    '''
        

def p_sino1(p):
    ' sino : '
    p[0] = ''

def p_sino2(p):
    ' sino : SINO instrucciones ' 
    p[0] = p[2]

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


# Función para escribir las salidas en el archivo .log
def escribir_log(salidas):
    with open('result.log', 'a') as log_file:  # Abrir el archivo en modo 'a' (append)
        # Escribimos las salidas de los comandos 'escribir'
        if(len(salidas) > 0):
            for salida in salidas:
                log_file.write(str(salida) + '\n')

# Construir el parser
yacc.yacc()
yacc.parse(codigoEntrada)

escribir_log(salidas_escribir)
f.close()
result.close()
