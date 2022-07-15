""" SPI - Simple Pascal Interpreter """
###############################################################################
#                                                                             #
#  LEXER                                                                      #
#                                                                             #
###############################################################################
# Token types
#
# EOF (end-of-file) token is used to indicate that
# there is no more input left for lexical analysis

import sys

final_result = []

INTEGER = 'INTEGER'
REAL = 'REAL'
PLUS = 'PLUS'
MINUS = 'MINUS'
MUL = 'MUL'
DIV = 'DIV'
LPAREN = '('
RPAREN = ')'
EOF = 'EOF'
DOLLER = 'DOLLER'
BOOLEAN = 'BOOLEAN'
QUOTE = "'"
STRING = 'STRING'
BACK_SLASH = '\\'
AND = "AND"
OR = "OR"
E = "E"
NE = "NE"
GTE = "GTE"
GT = "GT"
LT = "LT"
LTE = "LTE"
TRUE = 'TRUE'
FALSE = 'FALSE'
ASSIGN = 'ASSIGN'
SEMI = 'SEMI'
DOT = 'DOT'
ID = 'ID'
BEGIN = 'BEGIN'
END = 'END'
INTEGER_CONST = 'INTEGER_CONST'
REAL_CONST = 'REAL_CONST'
INTEGER_DIV = 'INTEGER_DIV'
FLOAT_DIV = 'FLOAT_DIV'
PROGRAM = 'PROGRAM'
VAR = 'VAR'
COLON = 'COLON'
COMMA = 'COMMA'
IF = 'IF'
ELSEIF = 'ELSEIF'
ELSE = 'ELSE'
THEN = 'THEN'
LOOP = 'LOOP'
PROCEDURE = 'PROCEDURE'
I_PROCEDURE_ST = 'I_PROCEDURE_ST'
I_PROCEDURE_EXP = 'I_PROCEDURE_EXP'
ENDIF = 'ENDIF'
ENDLOOP = 'ENDLOOP'
BREAK = 'BREAK'
CONTINUE = 'CONTINUE'
SQ_LPAREN = '['
SQ_RPAREN = ']'
ARRAY = 'ARRAY'


class Token(object):
    def __init__(self, type, value, lineno=None, colno=None):
        self.type = type
        self.value = value
        self.lineno = lineno
        self.colno = colno

    def __str__(self):
        """String representation of the class instance.
        Examples:
            Token(INTEGER, 3)
            Token(PLUS, '+')
            Token(MUL, '*')
        """
        return 'Token({type}, {value}, position={lineno}:{colno})'.format(
            type=self.type,
            value=repr(self.value),
            lineno=self.lineno,
            colno=self.colno
        )

    def __repr__(self):
        return self.__str__()


class Lexer(object):
    def __init__(self, text):
        # client string input, e.g. "4 + 2 * 3 - 6 / 2"
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        self.current_char = self.text[self.pos]
        self.lineno = 1
        self.colno = 1

    def error(self):
        s = "Invalid Syntax --- '{lexeme}' at position={lineno}:{column}".format(
            lexeme=self.current_char,
            lineno=self.lineno,
            column=self.colno,
        )
        raise Exception(s)

    def advance(self):
        """Advance the `pos` pointer and set the `current_char` variable."""
        if self.current_char == '\n':
            self.lineno += 1
            self.colno = 0

        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None  # Indicates end of input
        else:
            self.current_char = self.text[self.pos]
            self.colno += 1

    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def peek(self, level=1):
        # if self.pos + level <= len(self.text) - 1:
        #     pos = self.pos + level
        #     # print(f"self.text[pos] at {level} level and at {pos} pos", self.text[pos])
        #     return self.text[pos]
        peek_pos = self.pos + level
        if peek_pos > len(self.text) - 1:
            return None
        else:
            return self.text[peek_pos]

    def skip_comment(self):
        while self.current_char != '}':
            self.advance()
        self.advance()  # the closing curly brace

    def number(self):
        """Return a (multidigit) integer or float consumed from the input."""
        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()

        if self.current_char == '.':
            result += self.current_char
            self.advance()

            while (
                    self.current_char is not None and
                    self.current_char.isdigit()
            ):
                result += self.current_char
                self.advance()

            token = Token('REAL_CONST', float(result), self.lineno, self.colno)
        else:
            token = Token('INTEGER_CONST', int(result), self.lineno, self.colno)

        return token

    def string(self):
        result = ''
        while self.current_char is not None and self.current_char != "'":
            if self.current_char == "\\" and self.peek() in ("\\", "'"):
                self.advance()
                if self.current_char == "\\":
                    self.advance()
                    result += '\\'
                if self.current_char == "'":
                    self.advance()
                    result += "'"
            result += self.current_char
            self.advance()
        self.advance()
        return result

    def _id(self):
        result = ""
        while self.current_char is not None and self.current_char.isalnum():
            result += self.current_char
            self.advance()

        token = RESERVED_KEYWORDS.get(result.upper())
        if token is None:
            token = BUILTIN_PROCEDURES_STATEMENT.get(result.upper())

        if token is None:
            token = BUILTIN_PROCEDURES_EXPR.get(result.upper())

        if token is None:
            token = Token(ID, result)

        token.lineno = self.lineno
        token.colno = self.colno
        return token

    def get_next_token(self):
        """Lexical analyzer (also known as scanner or tokenizer)
        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
        """
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue
            if self.current_char == '{':
                self.advance()
                self.skip_comment()
                continue
            if self.current_char.isdigit():
                return self.number()
            if self.current_char == '+':
                self.advance()
                return Token(PLUS, '+', self.lineno, self.colno)
            if self.current_char == '-':
                self.advance()
                return Token(MINUS, '-', self.lineno, self.colno)
            if self.current_char == '*':
                self.advance()
                return Token(MUL, '*', self.lineno, self.colno)
            if self.current_char == '/':
                self.advance()
                return Token(FLOAT_DIV, '/', self.lineno, self.colno)
            if self.current_char == '(':
                self.advance()
                return Token(LPAREN, '(', self.lineno, self.colno)
            if self.current_char == ')':
                self.advance()
                return Token(RPAREN, ')', self.lineno, self.colno)
            if self.current_char == "'":
                # print("starting char is (') : ")
                self.advance()
                return Token(STRING, self.string(), self.lineno, self.colno)
            if self.current_char == "\\":
                self.advance()
                return Token(BACK_SLASH, "\\", self.lineno, self.colno)
            if self.current_char == "$":
                self.advance()
                return Token(DOLLER, "$", self.lineno, self.colno)
            if self.current_char == "=":
                self.advance()
                return Token(E, "=", self.lineno, self.colno)
            if self.current_char == "!" and self.peek() == "=":
                self.advance()
                self.advance()
                return Token(NE, "!=", self.lineno, self.colno)
            if self.current_char == "<" and self.peek() == "=":
                self.advance()
                self.advance()
                return Token(LTE, "<=", self.lineno, self.colno)
            if self.current_char == ">" and self.peek() == "=":
                self.advance()
                self.advance()
                return Token(GTE, ">=", self.lineno, self.colno)
            if self.current_char == ">":
                self.advance()
                return Token(GT, ">", self.lineno, self.colno)
            if self.current_char == "<":
                self.advance()
                return Token(LT, "<", self.lineno, self.colno)
            if self.current_char.isalpha():
                return self._id()
            if self.current_char == ':' and self.peek() == '=':
                self.advance()
                self.advance()
                return Token(ASSIGN, ':=', self.lineno, self.colno)
            if self.current_char == ';':
                self.advance()
                return Token(SEMI, ';', self.lineno, self.colno)
            if self.current_char == '.':
                self.advance()
                return Token(DOT, '.', self.lineno, self.colno)
            if self.current_char == ':':
                self.advance()
                return Token(COLON, ':', self.lineno, self.colno)
            if self.current_char == ',':
                self.advance()
                return Token(COMMA, ',', self.lineno, self.colno)
            if self.current_char == '.':
                self.advance()
                return Token(DOT, '.', self.lineno, self.colno)
            if self.current_char == '[':
                self.advance()
                return Token(SQ_LPAREN, '[', self.lineno, self.colno)
            if self.current_char == ']':
                self.advance()
                return Token(SQ_RPAREN, ']', self.lineno, self.colno)

            self.error()
        return Token(EOF, None, self.lineno, self.colno)


RESERVED_KEYWORDS = {
    'BEGIN': Token('BEGIN', 'BEGIN'),
    'STRING': Token('STRING', 'STRING'),
    'END': Token('END', 'END'),
    'BOOLEAN': Token('BOOLEAN', 'BOOLEAN'),
    'TRUE': Token('BOOLEAN', True),
    'FALSE': Token('BOOLEAN', False),
    'OR': Token('OR', "||"),
    'AND': Token('AND', "&&"),
    'PROGRAM': Token('PROGRAM', 'PROGRAM'),
    'VAR': Token('VAR', 'VAR'),
    'DIV': Token('INTEGER_DIV', 'DIV'),
    'INTEGER': Token('INTEGER', 'INTEGER'),
    'REAL': Token('REAL', 'REAL'),
    'IF': Token('IF', 'IF'),
    'ELSEIF':Token('ELSEIF', 'ELSEIF'),
    'THEN': Token('THEN', 'THEN'),
    'ELSE': Token('ELSE', 'ELSE'),
    'LOOP': Token('LOOP', 'LOOP'),
    'PROCEDURE': Token('PROCEDURE', 'PROCEDURE'),
    'ENDIF': Token('ENDIF', 'ENDIF'),
    'ENDLOOP': Token('ENDLOOP', 'ENDLOOP'),
    'BREAK': Token('BREAK', 'BREAK'),
    'CONTINUE': Token('CONTINUE', 'CONTINUE'),
    'ARRAY': Token('ARRAY', 'ARRAY')
}

BUILTIN_PROCEDURES_STATEMENT = {
    'DISPLAY': Token('I_PROCEDURE_ST', 'display'),

}

BUILTIN_PROCEDURES_EXPR = {
    'LENGTH': Token('I_PROCEDURE_EXP', 'length'),
}


###############################################################################
#                                                                             #
#  PARSER                                                                     #
#                                                                             #
###############################################################################
class AST(object):
    pass


class Compound(AST):
    def __init__(self):
        self.children = []
        self.current_executing_node = None
        self.next_executing_node = None


class Array(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class If_Elif(AST):
    def __init__(self, condition_node, true_body, false_body):
        self.condition_node = condition_node
        self.true_body = true_body
        self.false_body = false_body
        self.alert = None


class Loop(AST):
    def __init__(self, condition_node, true_body, false_body, enclosure=None):
        self.condition_node = condition_node
        self.true_body = true_body
        self.false_body = false_body
        self.parent = None
        self.alert = None


class Continue(AST):
    def __init__(self, loop_node):
        self.loop_node = loop_node


class Break(AST):
    def __init__(self, loop_node):
        self.loop_node = loop_node


class IfBody(AST):
    def __init__(self, statements):
        self.statements = statements
        self.parent = None


class LoopBody(AST):
    def __init__(self, statements):
        self.statements = statements


class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Var(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class NoOp(AST):
    pass


class Program(AST):
    def __init__(self, name, block):
        self.name = name
        self.block = block


class Block(AST):
    def __init__(self, declarations, compound_statement):
        self.declarations = declarations
        self.compound_statement = compound_statement


class VarDecl(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node


class ProcedureDecl(AST):
    def __init__(self, proc_name, params, block_node):
        self.proc_name = proc_name
        self.params = params  # a list of Param nodes
        self.block_node = block_node


class Param(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node


class Type(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class SliceOp(AST):
    def __init__(self, source, slice_config):
        self.source = source
        self.slice_config = slice_config


class UnaryOp(AST):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr


class InBuiltProcedure(AST):
    def __init__(self, name, params, token):
        self.name = name
        self.params = params
        self.token = token


class String(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

    def __str__(self):
        return f'{self.token.type}, {self.token.value}'

    __repr__ = __str__


class Boolean(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class Parser(object):
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def error(self):
        s = "Invalid Syntax at position={lineno}:{column}".format(
            lineno=self.current_token.lineno,
            column=self.current_token.colno
        )
        raise Exception(s)

    def eat(self, token_type):
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()

    def parse(self):
        print('parse is called')
        node = self.program()
        if self.current_token.type != EOF:
            self.error()
        print('parse returned entire AST node ------')
        return node

    def program(self):
        """program : PROGRAM variable SEMI block DOT"""
        print('program is called')
        self.eat(PROGRAM)
        var_node = self.variable()
        prog_name = var_node.value
        self.eat(SEMI)
        block_node = self.block()
        program_node = Program(prog_name, block_node)
        self.eat(DOT)
        print('program node is returned ---')
        return program_node

    def block(self):
        """block : declarations compound_statement"""
        print('block is called')
        declaration_nodes = self.declarations()
        compound_statement_node = self.compound_statement()
        node = Block(declaration_nodes, compound_statement_node)
        print('Block node is returned ---')
        return node

    def declarations(self):
        """declarations : (VAR (variable_declaration SEMI)+)*
                        | (PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI)*
                        | empty
        """
        declarations = []
        print('declaration is called')
        while True:
            if self.current_token.type == VAR:
                print('')
                print('Var declaration is called')
                self.eat(VAR)
                while self.current_token.type == ID:
                    var_decl = self.variable_declaration()
                    declarations.extend(var_decl)
                    self.eat(SEMI)

            elif self.current_token.type == PROCEDURE:
                print('')
                print('Procedure declaration is called')
                self.eat(PROCEDURE)
                proc_name = self.current_token.value
                self.eat(ID)
                params = []

                if self.current_token.type == LPAREN:
                    self.eat(LPAREN)

                    params = self.formal_parameter_list()

                    self.eat(RPAREN)

                self.eat(SEMI)
                block_node = self.block()
                proc_decl = ProcedureDecl(proc_name, params, block_node)
                declarations.append(proc_decl)
                self.eat(SEMI)
            else:
                break
        print('declaration list is returned ---')
        return declarations

    def formal_parameters(self):
        """ formal_parameters : ID (COMMA ID)* COLON type_spec """
        param_nodes = []

        param_tokens = [self.current_token]
        self.eat(ID)
        while self.current_token.type == COMMA:
            self.eat(COMMA)
            param_tokens.append(self.current_token)
            self.eat(ID)

        self.eat(COLON)
        type_node = self.type_spec()

        for param_token in param_tokens:
            param_node = Param(Var(param_token), type_node)
            param_nodes.append(param_node)

        return param_nodes

    def formal_parameter_list(self):
        """ formal_parameter_list : formal_parameters
                                  | formal_parameters SEMI formal_parameter_list
        """
        # procedure Foo();
        if not self.current_token.type == ID:
            return []

        param_nodes = self.formal_parameters()

        while self.current_token.type == SEMI:
            self.eat(SEMI)
            param_nodes.extend(self.formal_parameters())

        return param_nodes

    def variable_declaration(self):
        """variable_declaration : ID (COMMA ID)* COLON type_spec"""
        print('variable declaration is called')
        var_nodes = [Var(self.current_token)]  # first ID
        self.eat(ID)

        while self.current_token.type == COMMA:
            self.eat(COMMA)
            var_nodes.append(Var(self.current_token))
            self.eat(ID)

        self.eat(COLON)

        type_node = self.type_spec()
        var_declarations = [
            VarDecl(var_node, type_node)
            for var_node in var_nodes
        ]
        print('Variable declarations are returned ---')
        return var_declarations

    def type_spec(self):
        """type_spec : INTEGER
                     | REAL
                     | STRING
                     | BOOLEAN
        """
        print('type spec is called')
        token = self.current_token
        print('--- type spec: current token is: ', token)
        if self.current_token.type == INTEGER:
            print('--- integer type check is returned')
            self.eat(INTEGER)
        elif self.current_token.type == REAL:
            print('--- real type check is returned')
            self.eat(REAL)
        elif self.current_token.type == STRING:
            print('--- string type check is returned')
            self.eat(STRING)
        elif self.current_token.type == BOOLEAN:
            print('--- boolean type check is returned')
            self.eat(BOOLEAN)
        elif self.current_token.type == ARRAY:
            print("--- array type check is returned")
            self.eat(ARRAY)
        node = Type(token)
        print('The above token is returned ---')
        return node

    def compound_statement(self, loop_node=None):
        """
        compound_statement: BEGIN statement_list END
        """
        print('')
        print('compound statement is called')
        self.eat(BEGIN)
        nodes = self.statement_list(loop_node)
        self.eat(END)

        root = Compound()
        for node in nodes:
            root.children.append(node)
        print("compound root node is returned --- ")
        return root

    def statement_list(self, loop_node=None):
        """
        statement_list : statement
                       | statement SEMI statement_list
        """
        print('statement list is called')
        node = self.statement(loop_node)
        print('statement_list: type of loop node is', type(loop_node))

        results = [node]

        while self.current_token.type == SEMI:
            self.eat(SEMI)
            results.append(self.statement(loop_node))

        if self.current_token.type == ID:
            self.error()
        print('statement list returned ---')
        return results

    def statement(self, loop_node=None):
        """
        statement : compound_statement
                  | assignment_statement
                  | empty
        """
        print('statement is called')
        print('--- statement: type of loop node is', type(loop_node))
        print('--- statement: token type in statement ', self.current_token)
        if self.current_token.type == BEGIN:
            print('--- compound statement is called')
            node = self.compound_statement(loop_node)
        elif self.current_token.type == IF:
            print('--- if statement is called')
            node = self.if_statement(loop_node)
        elif self.current_token.type == LOOP:
            print('--- loop is called')
            node = self.loop_statement()
        elif self.current_token.type == BREAK:
            print('--- break is called')
            node = self.break_statement(loop_node)
        elif self.current_token.type == CONTINUE:
            print('--- continue is called')
            node = self.continue_statement(loop_node)
        elif self.current_token.type == ID:
            print('--- assignment is called')
            node = self.assignment_statement()
        elif self.current_token.type == I_PROCEDURE_ST:
            print('--- inbuilt procedure', self.current_token.value)
            node = self.inbuilt_procedure_statements()
        else:
            print('--- empty is called')
            node = self.empty()
        return node

    def break_statement(self, loop_node):
        print('break statement is called and eat break')
        self.eat(BREAK)
        # print('break is called')
        # print('--- break_statement: type of loop node is', type(loop_node))
        node = Break(loop_node)
        print('break_statement returned the node ---')
        return node

    def continue_statement(self, loop_node):
        self.eat(CONTINUE)
        print('continue is called')
        print('--- continue_statement: type of loop node is', type(loop_node))
        node = Continue(loop_node)
        print('continue returned the node ---')
        return node

    def inbuilt_procedure_statements(self):
        print('inbuilt_procedure_statement is called')
        if self.current_token.value == 'display':
            token = self.current_token
            self.eat(I_PROCEDURE_ST)
            self.eat(LPAREN)
            param_list = [self.expr()]

            while self.current_token.type == COMMA:
                self.eat(COMMA)
                param_list.append(self.expr())
            self.eat(RPAREN)
            node = InBuiltProcedure(token.value, param_list, token)
            print('display node is returned ---')
            return node

    def inbuilt_procedure_expr(self):
        if self.current_token.value == "length":
            print('----length is called')
            token = self.current_token
            self.eat(I_PROCEDURE_EXP)
            self.eat(LPAREN)

            param_list = [self.expr()]

            self.eat(RPAREN)
            node = InBuiltProcedure(token.value, param_list, token)
            return node

    def if_statement(self, loop_node=None):
        print('if statement is called')
        self.eat(IF)
        condition_node = self.expr()
        self.eat(THEN)

        true_body_statements = self.statement_list(loop_node)

        if self.current_token.type in (ELSEIF, ELSE):
            if self.current_token.type == ELSEIF:
                node = If_Elif(condition_node, IfBody(true_body_statements), self.elseif_statement(loop_node))
            else:
                node = If_Elif(condition_node, IfBody(true_body_statements), self.else_for_if_statement(loop_node))
                node.false_body.parent = node
        else:
            node = If_Elif(condition_node, IfBody(true_body_statements), self.empty())
        node.true_body.parent = node
        self.eat(ENDIF)
        print('if statement returned the node ---')
        return node

    def elseif_statement(self, loop_node=None):
        print('elseif statement is called')
        print('--- elseif statement: type of loop node:', loop_node)
        self.eat(ELSEIF)
        condition_node = self.expr()
        self.eat(THEN)

        true_body_statements = self.statement_list(loop_node)

        if self.current_token.type == ELSEIF:
            node = If_Elif(condition_node, IfBody(true_body_statements), self.elseif_statement(loop_node))
            node.true_body.parent = node
            return node

        if self.current_token.type == ELSE:
            node = If_Elif(condition_node, IfBody(true_body_statements), self.else_for_if_statement(loop_node))
            node.true_body.parent = node
            node.false_body.parent = node
            return node
        print('elif statement returned the node ---')
        node = If_Elif(condition_node, IfBody(true_body_statements), self.empty())
        node.true_body.parent = node
        return node

    def loop_statement(self):
        print('loop statement is called')
        self.eat(LOOP)
        self.eat(IF)
        condition_node = self.expr()
        node = Loop(condition_node, None, None)
        print('---  loop: type of loop node', type(node))
        true_body_statements = self.statement_list(node)

        if self.current_token.type == ELSE:
            node.true_body = LoopBody(true_body_statements)
            node.false_body = self.else_for_loop_statement()
        else:
            node.true_body = LoopBody(true_body_statements)
            node.false_body = self.empty()

        print('loop statement returned the node ---')
        self.eat(ENDLOOP)
        return node

    def else_for_if_statement(self, loop_node=None):
        print('else statement is called')
        print('--- else: type of loop node: ', loop_node)
        self.eat(ELSE)
        true_body_statements = self.statement_list(loop_node)
        print('else statement returned the node ---')
        return IfBody(true_body_statements)

    def else_for_loop_statement(self, loop_node=None):
        print('else statement is called')
        print('--- else: type of loop node: ', loop_node)
        self.eat(ELSE)
        true_body_statements = self.statement_list(loop_node)
        print('else statement returned the node ---')
        return LoopBody(true_body_statements)

    def assignment_statement(self):
        """
        assignment_statement : variable ASSIGN expr
        """
        print('assignment statement is called')
        left = self.variable()
        token = self.current_token
        self.eat(ASSIGN)
        right = self.expr()
        node = Assign(left, token, right)
        print('assignment statement returned the node ---')
        return node

    def variable(self):
        """
        variable : ID
        """
        print('variable is called')
        node = Var(self.current_token)
        self.eat(ID)
        print('variable returned the node ---')
        return node

    def empty(self):
        """An empty production"""
        print('empty is called')
        print('empty returned the node ---')
        return NoOp()

    def expr(self):
        print('expr is called')
        node = self.level_1()

        while self.current_token.type == OR:
            print('--- expr: current token is OR')
            token = self.current_token
            self.eat(OR)
            node = BinOp(left=node, op=token, right=self.level_1())
        return node

    def level_1(self):
        print('level 1 is called')
        node = self.level_2()

        while self.current_token.type == AND:
            print('--- level 1: current token is AND')
            token = self.current_token
            self.eat(AND)
            node = BinOp(left=node, op=token, right=self.level_2())
        print('level 1 returned the node ---')
        return node

    def level_2(self):
        print('level 2 is called')
        node = self.level_3()

        while self.current_token.type in (E, NE, GT, LT, LTE, GTE):
            print('--- level 2: current token is is comparison operator')
            token = self.current_token
            if token.type == E:
                self.eat(E)
            elif token.type == NE:
                self.eat(NE)
            elif token.type == GT:
                self.eat(GT)
            elif token.type == LT:
                self.eat(LT)
            elif token.type == LTE:
                self.eat(LTE)
            elif token.type == GTE:
                self.eat(GTE)
            node = BinOp(left=node, op=token, right=self.level_3())
        print('level 2 returned the node')
        return node

    def level_3(self):
        print('level 3 is called')
        node = self.level_4()

        while self.current_token.type in (PLUS, MINUS):
            print('--- level 3: current token is in PLUS | MINUS')
            token = self.current_token
            if token.type == PLUS:
                self.eat(PLUS)
            elif token.type == MINUS:
                self.eat(MINUS)
            node = BinOp(left=node, op=token, right=self.level_4())
        print('level 3 returned the node ---')
        return node

    def level_4(self):
        print('level 4 is called')
        node = self.level_5()

        while self.current_token.type in (MUL, INTEGER_DIV, FLOAT_DIV):
            print('--- level 4: current token is in MUL | DIV')
            token = self.current_token
            if token.type == MUL:
                self.eat(MUL)
            elif token.type == INTEGER_DIV:
                self.eat(INTEGER_DIV)
            elif token.type == FLOAT_DIV:
                self.eat(FLOAT_DIV)
            node = BinOp(left=node, op=token, right=self.level_5())
        print('level 4 returned the node ---')
        return node

    def level_5(self):
        print('level 5 is called')
        node = self.level_6()

        while self.current_token.type == DOLLER:
            print('--- level 5: current token is DOLLER')
            token = self.current_token
            self.eat(DOLLER)
            node = BinOp(left=node, op=token, right=self.level_6())
        print('level 5 returned the node ---')
        return node

    def get_slice_param(self):
        self.eat(SQ_LPAREN)
        slicing_param = []
        for i in range(3):
            if self.current_token.type == COLON and i < 2:
                self.eat(COLON)
                slicing_param.append(self.empty())
                continue
            elif self.current_token.type == I_PROCEDURE_EXP:
                slicing_param.append(self.expr())
            elif self.current_token.type != SQ_RPAREN:
                slicing_param.append(self.expr())
            else:
                if i == 0:
                    self.error()
                else:
                    break
            if self.current_token.type == COLON and i < 2:
                self.eat(COLON)
            else:
                break
        self.eat(SQ_RPAREN)
        return slicing_param

    def level_6(self):
        print('level 6 is called')
        node = self.level_7()

        while self.current_token.type == SQ_LPAREN:
            node = SliceOp(node, self.get_slice_param())

        return node

    def level_7(self):
        print('level 7 is called')
        token = self.current_token
        print('--- level 7: current token is: ', token)
        if token.type == PLUS:
            self.eat(PLUS)
            node = UnaryOp(token, self.level_7())
            print('level_7 returned the PLUS node ---')
            return node
        elif token.type == MINUS:
            self.eat(MINUS)
            node = UnaryOp(token, self.level_7())
            print('level_7 returned the MINUS node ---')
            return node
        elif token.type == INTEGER_CONST:
            print('level_7 returned the INTEGER CONSTANT node ---')
            self.eat(INTEGER_CONST)
            return Num(token)
        elif token.type == REAL_CONST:
            self.eat(REAL_CONST)
            print('level_7 returned the REAL CONSTANT node ---')
            return Num(token)
        elif token.type == LPAREN:
            print('level_7 encountered LPAREN')
            self.eat(LPAREN)
            node = self.expr()
            print('level_7 encountered RPAREN')
            self.eat(RPAREN)
            return node
        elif token.type == BOOLEAN:
            self.eat(BOOLEAN)
            print('level_7 returned the BOOLEAN node ---')
            return Boolean(token)
        elif token.type == STRING:
            self.eat(STRING)
            print('level_7 returned the STRING node ---')
            return String(token)
        elif token.type == SQ_LPAREN:
            self.eat(SQ_LPAREN)
            value = [self.expr()]
            while self.current_token.type == COMMA:
                self.eat(COMMA)
                value.append(self.expr())
            self.eat(SQ_RPAREN)
            token = Token(ARRAY, value)
            node = Array(token)
            return node
        elif self.current_token.type == I_PROCEDURE_EXP:
            print('--- inbuilt procedure', self.current_token.value)
            node = self.inbuilt_procedure_expr()
            return node
        else:
            print('level_7 encountered ID ---')
            node = self.variable()
            print('level_7 returned the node ---')
            return node


###############################################################################
#                                                                             #
#  INTERPRETER                                                                #
#                                                                             #
###############################################################################
class NodeVisitor(object):
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))


###############################################################################
#                                                                             #
#  SYMBOLS and SYMBOL TABLE                                                   #
#                                                                             #
###############################################################################

class Symbol(object):
    def __init__(self, name, type=None):
        self.name = name
        self.type = type


class VarSymbol(Symbol):
    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return "<{class_name}(name='{name}', type='{type}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            type=self.type,
        )

    __repr__ = __str__


class BuiltinTypeSymbol(Symbol):
    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return self.name

    def __repr__(self):
        return "<{class_name}(name='{name}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
        )


class ProcedureSymbol(Symbol):
    def __init__(self, name, params=None):
        super().__init__(name)
        # a list of formal parameters
        self.params = params if params is not None else []

    def __str__(self):
        return '<{class_name}(name={name}, parameters={params})>'.format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=self.params,
        )

    __repr__ = __str__


class ScopedSymbolTable(object):
    def __init__(self, scope_name, scope_level, enclosing_scope=None):
        self._symbols = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.enclosing_scope = enclosing_scope

    def _init_builtins(self):
        self.insert(BuiltinTypeSymbol('INTEGER'))
        self.insert(BuiltinTypeSymbol('REAL'))
        self.insert(BuiltinTypeSymbol('STRING'))
        self.insert(BuiltinTypeSymbol('BOOLEAN'))
        self.insert(BuiltinTypeSymbol('ARRAY'))

    def __str__(self):
        h1 = 'SCOPE (SCOPED SYMBOL TABLE)'
        lines = ['\n', h1, '=' * len(h1)]
        for header_name, header_value in (
                ('Scope name', self.scope_name),
                ('Scope level', self.scope_level),
                ('Enclosing scope',
                 self.enclosing_scope.scope_name if self.enclosing_scope else None
                 )
        ):
            lines.append('%-15s: %s' % (header_name, header_value))
        h2 = 'Scope (Scoped symbol table) contents'
        lines.extend([h2, '-' * len(h2)])
        lines.extend(
            ('%7s: %r' % (key, value))
            for key, value in self._symbols.items()
        )
        lines.append('\n')
        s = '\n'.join(lines)
        return s

    __repr__ = __str__

    def insert(self, symbol):
        print('Insert: %s' % symbol.name)
        self._symbols[symbol.name] = symbol

    def lookup(self, name, current_scope_only=False):
        print('Lookup: %s. (Scope name: %s)' % (name, self.scope_name))
        # 'symbol' is either an instance of the Symbol class or None
        symbol = self._symbols.get(name)

        if symbol is not None:
            return symbol

        if current_scope_only:
            return None

        # recursively go up the chain and lookup the name
        if self.enclosing_scope is not None:
            return self.enclosing_scope.lookup(name)


class SemanticAnalyzer(NodeVisitor):
    def __init__(self):
        self.current_scope = None
        self.global_scope = global_scope = ScopedSymbolTable(
            scope_name='global',
            scope_level=1,
            enclosing_scope=self.current_scope, # None
        )

    def visit_Block(self, node):
        print('Symbol: visiting block node')
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_Program(self, node):
        print('Symbol: Visiting Program node')
        print('ENTER scope: global')
        # global_scope = ScopedSymbolTable(
        #     scope_name='global',
        #     scope_level=1,
        #     enclosing_scope=self.current_scope, # None
        # )
        self.global_scope._init_builtins()
        self.current_scope = self.global_scope

        # visit subtree
        self.visit(node.block)

        # print('symbol_table : ', self.current_scope._symabols)

        print(self.global_scope)

        self.current_scope = self.current_scope.enclosing_scope
        print('LEAVE scope: global')

    def visit_BinOp(self, node):
        print('Symbol: Visiting BinOp node')
        self.visit(node.left)
        self.visit(node.right)

    def visit_Num(self, node):
        pass

    def visit_Continue(self, node):
        pass

    def visit_Break(self, node):
        pass

    def visit_Boolean(self, node):
        pass

    def visit_String(self, node):
        pass

    def visit_UnaryOp(self, node):
        self.visit(node.expr)

    def visit_Compound(self, node):
        print('Symbol: Visiting Compound Node')
        for child in node.children:
            self.visit(child)

    def visit_NoOp(self, node):
        pass

    def visit_VarDecl(self, node):
        print('Symbol: Visiting VarDecl node')
        type_name = node.type_node.value
        type_symbol = self.current_scope.lookup(type_name)

        # We have all the information we need to create a variable symbol.
        # Create the symbol and insert it into the symbol table.
        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, type_symbol)

        # Signal an error if the table alrady has a symbol
        # with the same name
        if self.current_scope.lookup(var_name, current_scope_only=True):
            raise Exception(
                "Error: Duplicate identifier '%s' found" % var_name
            )

        self.current_scope.insert(var_symbol)

    def visit_ProcedureDecl(self, node):
        proc_name = node.proc_name
        proc_symbol = ProcedureSymbol(proc_name)
        self.current_scope.insert(proc_symbol)

        print('ENTER scope: %s' %  proc_name)
        # Scope for parameters and local variables
        procedure_scope = ScopedSymbolTable(
            scope_name=proc_name,
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope
        )
        self.current_scope = procedure_scope

        # Insert parameters into the procedure scope
        for param in node.params:
            param_type = self.current_scope.lookup(param.type_node.value)
            param_name = param.var_node.value
            var_symbol = VarSymbol(param_name, param_type)
            self.current_scope.insert(var_symbol)
            proc_symbol.params.append(var_symbol)

        self.visit(node.block_node)

        print(procedure_scope)

        self.current_scope = self.current_scope.enclosing_scope
        print('LEAVE scope: %s' %  proc_name)

    def visit_Assign(self, node):
        print('Symbol: Vising Assignment Node')
        # right-hand side
        self.visit(node.right)
        # left-hand side
        self.visit(node.left)

    def visit_Var(self, node):
        print('Symbol: Visiting Var node')
        var_name = node.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            raise Exception(
                "Symbol(identifier) not found '%s'" % var_name
            )

    def visit_If_Elif(self, node):
        self.visit(node.condition_node)
        self.visit(node.true_body)
        self.visit(node.false_body)

    def visit_Loop(self, node):
        self.visit(node.condition_node)
        self.visit(node.true_body)
        self.visit(node.condition_node)
        self.visit(node.false_body)

    def visit_IfBody(self, node):
        for statement in node.statements:
            self.visit(statement)

    def visit_Array(self, node):
        for item in node.value:
            self.visit(item)

    def visit_SliceOp(self, node):
        self.visit(node.source)
        for item in node.slice_config:
            self.visit(item)

    def visit_LoopBody(self, node):
        for statement in node.statements:
            self.visit(statement)

    def visit_InBuiltProcedure(self, node):
        for param in node.params:
            self.visit(param)


class Interpreter(NodeVisitor):
    def __init__(self, tree, semantic_analyzer):
        self.tree = tree
        self.GLOBAL_MEMORY = {}
        self.semantic_analyzer = semantic_analyzer

    def set_current_scope(self):
        self.semantic_analyzer.current_scope = ScopedSymbolTable

    def visit_Program(self, node):
        self.semantic_analyzer.current_scope = self.semantic_analyzer.global_scope
        print('\nvisiting Program node')
        self.visit(node.block)
        self.semantic_analyzer.current_scope = self.semantic_analyzer.current_scope.enclosing_scope

    def visit_Block(self, node):
        print('\nVisiting Block node')
        print('current_scope block node: ', self.semantic_analyzer.current_scope)
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node):
        # Do nothing
        print('\nVisiting Variable Declaration node')
        pass

    def visit_ProcedureDecl(self, node):
        print('\nVisiting Procedure Declaration node')
        pass

    def visit_Type(self, node):
        print('\nVisiting Type node')
        # Do nothing
        pass

    def operator_unsupport_error(self, op1type, op2type, op):
        message = f'Unsupported operands of type {op1type} and {op2type} for operator {op.value} at position={op.lineno}:{op.colno}'
        raise Exception(message)

    def visit_BinOp(self, node):
        print('\nVisiting Binary Operator node')
        if node.op.type == PLUS:
            left_value = self.visit(node.left)
            left_value_type = self.type_provider(left_value)
            right_value = self.visit(node.right)
            right_value_type = self.type_provider(right_value)
            if left_value_type in (INTEGER, REAL) and right_value_type in (INTEGER, REAL):
                result = left_value + right_value
            else:
                self.operator_unsupport_error(left_value_type, right_value_type, node.op)
            # print('Binary Operator PLUS: ', result)
            return result
        elif node.op.type == MINUS:
            left_value = self.visit(node.left)
            left_value_type = self.type_provider(left_value)
            right_value = self.visit(node.right)
            right_value_type = self.type_provider(right_value)
            if left_value_type in (INTEGER, REAL) and right_value_type in (INTEGER, REAL):
                result = left_value - right_value
            else:
                self.operator_unsupport_error(left_value_type, right_value_type, node.op)
            # print('Binary Operator PLUS: ', result)
            return result
        elif node.op.type == MUL:
            left_value = self.visit(node.left)
            left_value_type = self.type_provider(left_value)
            right_value = self.visit(node.right)
            right_value_type = self.type_provider(right_value)
            if left_value_type in (INTEGER, REAL) and right_value_type in (INTEGER, REAL):
                result = left_value * right_value
            else:
                self.operator_unsupport_error(left_value_type, right_value_type, node.op)
            # print('Binary Operator PLUS: ', result)
            return result
        elif node.op.type == INTEGER_DIV:
            left_value = self.visit(node.left)
            left_value_type = self.type_provider(left_value)
            right_value = self.visit(node.right)
            right_value_type = self.type_provider(right_value)
            if left_value_type in (INTEGER, REAL) and right_value_type in (INTEGER, REAL):
                try:
                    result = left_value // right_value
                except:
                    message = f"ZeroDivisionError('integer division by zero') at position={node.op.lineno}:{node.op.colno}"
                    raise Exception(message)
            else:
                self.operator_unsupport_error(left_value_type, right_value_type, node.op)
            # print('Binary Operator PLUS: ', result)
            return result
        elif node.op.type == FLOAT_DIV:
            left_value = self.visit(node.left)
            left_value_type = self.type_provider(left_value)
            right_value = self.visit(node.right)
            right_value_type = self.type_provider(right_value)
            if left_value_type in (INTEGER, REAL) and right_value_type in (INTEGER, REAL):
                try:
                    result = float(left_value) / float(right_value)
                except:
                    message = f'{repr(sys.exc_info()[1])} at position={node.op.lineno}:{node.op.colno}'
                    raise Exception(message)
            else:
                self.operator_unsupport_error(left_value_type, right_value_type, node.op)
            # print('Binary Operator PLUS: ', result)
            return result
        elif node.op.type == DOLLER:
            left_value = self.visit(node.left)
            left_value_type = self.type_provider(left_value)
            right_value = self.visit(node.right)
            right_value_type = self.type_provider(right_value)
            if left_value_type == STRING and right_value_type in STRING:
                result = left_value + right_value
            else:
                self.operator_unsupport_error(left_value_type, right_value_type, node.op)
            # print('Binary Operator PLUS: ', result)
            return result
        elif node.op.type == E:
            left_value = self.visit(node.left)
            right_value = self.visit(node.right)
            result = bool(left_value == right_value)
            return result
        elif node.op.type == NE:
            left_value = self.visit(node.left)
            right_value = self.visit(node.right)
            result = bool(left_value != right_value)
            return result
        elif node.op.type == LT:
            left_value = self.visit(node.left)
            left_value_type = self.type_provider(left_value)
            right_value = self.visit(node.right)
            right_value_type = self.type_provider(right_value)
            if left_value_type in (INTEGER, REAL) and right_value_type in (INTEGER, REAL):
                result = bool(left_value < right_value)
            else:
                self.operator_unsupport_error(left_value_type, right_value_type, node.op)
            # print('Binary Operator PLUS: ', result)
            return result
        elif node.op.type == GT:
            left_value = self.visit(node.left)
            left_value_type = self.type_provider(left_value)
            right_value = self.visit(node.right)
            right_value_type = self.type_provider(right_value)
            if left_value_type in (INTEGER, REAL) and right_value_type in (INTEGER, REAL):
                result = bool(left_value > right_value)
            else:
                self.operator_unsupport_error(left_value_type, right_value_type, node.op)
            # print('Binary Operator PLUS: ', result)
            return result
        elif node.op.type == GTE:
            left_value = self.visit(node.left)
            left_value_type = self.type_provider(left_value)
            right_value = self.visit(node.right)
            right_value_type = self.type_provider(right_value)
            if left_value_type in (INTEGER, REAL) and right_value_type in (INTEGER, REAL):
                result = bool(left_value >= right_value)
            else:
                self.operator_unsupport_error(left_value_type, right_value_type, node.op)
            # print('Binary Operator PLUS: ', result)
            return result
        elif node.op.type == LTE:
            left_value = self.visit(node.left)
            left_value_type = self.type_provider(left_value)
            right_value = self.visit(node.right)
            right_value_type = self.type_provider(right_value)
            if left_value_type in (INTEGER, REAL) and right_value_type in (INTEGER, REAL):
                result = bool(left_value <= right_value)
            else:
                self.operator_unsupport_error(left_value_type, right_value_type, node.op)
            # print('Binary Operator PLUS: ', result)
            return result
        elif node.op.type == AND:
            left_value = self.visit(node.left)
            right_value = self.visit(node.right)
            result = bool(left_value and right_value)
            return result
        elif node.op.type == OR:
            left_value = self.visit(node.left)
            right_value = self.visit(node.right)
            result = bool(left_value or right_value)
            return result

    def visit_Num(self, node):
        print('\nVisiting Number node')
        return node.value

    def visit_String(self, node):
        print('\nVisiting String node')
        return node.value

    def visit_Boolean(self, node):
        print('\nVisiting Boolean node')
        return node.value

    def visit_UnaryOp(self, node):
        print('\nVisiting Unary Operator node')
        op = node.op.type
        if op == PLUS:
            return +self.visit(node.expr)
        elif op == MINUS:
            return -self.visit(node.expr)

    def visit_Array(self, node):
        result = []
        for item in node.value:
            result.append(self.visit(item))
        return result

    def visit_SliceOp(self, node):
        source = self.visit(node.source)
        try:
            length_of_source = len(source)
        except:
            raise Exception(f"{self.type_provider(source)} type can't be sliced or indexed")
        param = []
        for i, item in enumerate(node.slice_config):
            if type(item).__name__ == "NoOp":
                param.append("")
            else:
                param.append(self.visit(item))

        print("-----------parameters: ", param)
        if len(param) == 1:
            return source[param[0]]
        elif len(param) == 2:
            if param[0] == "" and param[1] == "":
                return source[:]
            elif param[0] == "":
                return source[:param[1]]
            elif param[1] == "":
                return source[param[0]:]
            else:
                return source[param[0]: param[1]]
        else:
            if param[0] == "" and param[1] == "" and param[2] == "":
                return source[::]
            elif param[0] == "" and param[1] == "":
                return source[::param[2]]
            elif param[0] == "" and param[2] == "":
                return source[:param[1]:]
            elif param[0] == "":
                return source[:param[1]:param[2]]
            elif param[1] == "" and param[2] == "":
                return source[param[0]::]
            elif param[1] == "":
                return source[param[0]::param[2]]
            elif param[2] == "":
                print(param[0], param[1])
                return source[param[0]:param[1]]
            else:
                return source[param[0]:param[1]:param[2]]

    def visit_Compound(self, node):
        print('\nVisiting Compound node')
        # for child in node.children:
        #     self.visit(child)

        for child_index in range(len(node.children)):
            next_index = child_index + 1
            if next_index <= len(node.children) - 1:
                node.next_executing_node = node.children[next_index]
            else:
                node.next_executing_node = None

            node.current_executing_node = node.children[child_index]
            if isinstance(node.current_executing_node, Loop):
                node.current_executing_node.parent = node
            self.visit(node.children[child_index])
            try:
                if node.current_executing_node.alert == BREAK:
                    # statement.alert = None
                    break
            except:
                pass

    def visit_NoOp(self, node):
        print('\nVisiting NoOp node')
        pass

    def visit_Var(self, node):
        print('\nVisiting Var node')
        var_name = node.value
        val = self.GLOBAL_MEMORY.get(var_name)
        print('Visiting Var: value of variable from global memory', var_name, 'is', val)
        if val is None:
            message = f"Variable '{var_name}' referenced before assignment"
            raise Exception(message)
        else:
            return val

    def type_provider(self, result):
        print(type(result).__name__, '-------------------------')
        if type(result).__name__ == 'int':
            return 'INTEGER'
        elif type(result).__name__ == 'float':
            return 'REAL'
        elif type(result).__name__ == 'bool':
            return 'BOOLEAN'
        elif type(result).__name__ == 'str':
            return 'STRING'
        elif type(result).__name__ == 'list':
            return 'ARRAY'
        else:
            return None

    def type_check(self, var_name, var_type, result):
        print('1st check', var_type.name == 'INTEGER', 'Actual:', type(var_type))
        print('2ns check', type(result).__name__ != 'int')
        if var_type.name == 'INTEGER' and type(result).__name__ != 'int':
            raise Exception(f'Type miss-match Error: On Variable "{var_name}" Expected INTEGER but {self.type_provider(result)} was passed')
        elif var_type.name == REAL and type(result).__name__ not in ('int', 'float'):
            raise Exception(f'Type miss-match Error: On Variable "{var_name}" Expected REAL but {self.type_provider(result)} was passed')
        elif var_type.name == STRING and type(result).__name__ != 'str':
            raise Exception(f'Type miss-match Error: On Variable "{var_name}" Expected STRING but {self.type_provider(result)} was passed')
        elif var_type.name == BOOLEAN and type(result).__name__ != 'bool':
            raise Exception(f'Type miss-match Error: On Variable "{var_name}" Expected BOOLEAN but {self.type_provider(result)} was passed')
        elif var_type.name == ARRAY and type(result).__name__ != 'list':
            raise Exception(f'Type miss-match Error: On Variable "{var_name}" Expected ARRAY but {self.type_provider(result)} was passed')
        else:
            print('Hey I returned True --------------------------')
            return True

    def visit_Assign(self, node):
        print('\nVisiting Assign node')
        var_name = node.left.value
        print("Under assignment: var_name", var_name)
        var_type = self.semantic_analyzer.current_scope.lookup(var_name).type
        print("Under assignment: type", var_type)
        expr_result = self.visit(node.right)
        type_check_result = self.type_check(var_name, var_type, expr_result)
        if type_check_result:
            self.GLOBAL_MEMORY[var_name] = expr_result

    def visit_If_Elif(self, node):
        print('\nVisiting If node')
        condition_result = self.visit(node.condition_node)
        print('Visit IF: condition result: ', condition_result)
        if condition_result:
            self.visit(node.true_body)
        else:
            self.visit(node.false_body)

    def visit_Loop(self, node):
        print('\nVisitig Loop')
        while self.visit(node.condition_node):
            print('--- LOOP: condition is True')
            self.visit(node.true_body)
        else:
            print('--- LOOP: Condtion is false and hence directed to false body')
            self.visit(node.false_body)

    def visit_IfBody(self, node):
        print('\nVisiting IfBody')
        for statement in node.statements:
            if isinstance(statement, Continue):
                print('Visit IfBody: encountered Continue Object hence ')
                self.visit(statement)
                node.parent.alert = CONTINUE
                break
            elif isinstance(statement, Break):
                print('Visit IfBody: encountered Break Object hence ')
                self.visit(statement)
                node.parent.alert = BREAK
                # statement.loop_node.alert = BREAK
                break
            else:
                self.visit(statement)

    def visit_LoopBody(self, node):
        print('\nVisiting LoopBody')
        for statement in node.statements:
            self.visit(statement)
            try:
                if statement.alert == CONTINUE:
                    # statement.alert = None
                    break
                if statement.alert == BREAK:
                    # statement.alert = None
                    break
            except:
                pass

    def visit_Continue(self, node):
        print('\nVisiting Continue node')
        print(type(node.loop_node))
        self.visit(node.loop_node)

    def visit_Break(self, node):
        print('\nVisiting Break node')
        print(type(node.loop_node))
        node.loop_node.alert = BREAK
        print('next node type is : ', type(node.loop_node.parent.next_executing_node), node.loop_node.parent.next_executing_node)
        if node.loop_node.parent.next_executing_node:
            self.visit(node.loop_node.parent.next_executing_node)

    def visit_InBuiltProcedure(self, node):
        print('\nVisiting Inbuilt Procedure node')
        if node.name == 'display':
            for param in node.params:
                final_result.append(self.visit(param))
                final_result.append(" ")
            final_result.append('\n')
        elif node.name == 'length':
            return len(self.visit(node.params[0]))

    def interpret(self):
        tree = self.tree
        if tree is None:
            return ''
        print('\nVisiting Program node')
        return self.visit(tree)


def main(text):
    global final_result
    final_result = []
    text = text.strip()
    try:
        if text.strip() == "":
            sys.exit("blank source code")
        print('-------------------------------')
        print('-------------------------------')
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()
        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.visit(tree)
        # semantic_analyzer.current_scope._symbols
        print('Semantic analysis completed')
        interpreter = Interpreter(tree, semantic_analyzer)
        print('--------------------------------------------------------------------------------')
        print('Interpreter is called')
        result = interpreter.interpret()
        if result is not None:
            final_result.append(result)
        result = [str(item) for item in final_result]
        final_result = []
        result = "".join(result)
        print(result)
        return result
    except:
        print(not(final_result))
        result = ""
        if len(final_result) > 0:
            result = [str(item) for item in final_result]
        final_result = []
        result = "".join(result)
        return f'{result}{repr(sys.exc_info()[0])[8:-2]} : {sys.exc_info()[1]}'


if __name__ == '__main__':
    main()