import clingo.ast
from clingo.ast import ASTType

not_ast_error = TypeError('Literal must be an AST object with a type attribute')

def find_ast_type(ast, ast_type):
    if type(ast) is set:
        ast = [a.ast for a in ast]
    type_search = ASTTypeSearch(ast_type)
    type_search.visit(ast)
    return type_search.ast_objects()

def find_variables(ast):
    return find_ast_type(ast, ASTType.Variable)

def get_predicate_symbol(ast):
    '''Gets the predicate symbol of a literal, atom, or function'''
    function = _find_function(ast)
    return PredicateSymbol(function.name, len(function.arguments))

def get_arguments(ast):
    '''Gets the arguments of a literal, atom, or function'''
    function = _find_function(ast)
    return function.arguments

def _find_function(ast):
    if not hasattr(ast, 'type'):
        raise not_ast_error
    if ast.type == ASTType.Literal:
        if ast.atom.type != ASTType.SymbolicAtom:
            raise TypeError('Literal atom attribute must be of type SymbolicAtom (was %s [%s])' % (ast.atom.type, ast))
    elif ast.type not in [ASTType.SymbolicAtom, ASTType.Function]:
        raise TypeError('%s is not of type "Literal", "SymbolicAtom", or "Function"' % ast.type)
    function_set = find_ast_type(ast, ASTType.Function)
    assert len(function_set) > 0
    function = function_set.pop().unwrap()
    if function.external:
        raise TypeError('Cannot handle external functions (%s)' % function)
    return function

def is_predicate_in_body(rule, predicate, other_conditional=None):
    if not hasattr(rule, 'type'):
        raise not_ast_error
    if rule.type != ASTType.Rule:
        raise TypeError('AST must be of type Rule (found %s)' % rule.type)
    for body_literal in rule.body:
        if body_literal.type == ASTType.ConditionalLiteral:
            body_literal = body_literal.literal
        if body_literal.atom.type == ASTType.Comparison:
            continue # Comparisons can just be ignored, they won't have predicates in them
        if body_literal.atom.type == ASTType.BodyAggregate or body_literal.atom.type == ASTType.Aggregate:
            # element_conditions = [element.condition for element in body_literal.atom.elements]
            # check_literals = []
            # for condition in element_conditions:
            #     check_literals += condition
            continue
        else:
            # TODO Add more special cases
            check_literals = [body_literal]
        for check_literal in check_literals:
            if other_conditional is None or other_conditional(check_literal):
                body_predicate = get_predicate_symbol(check_literal)
                if body_predicate == predicate:
                    return True
    return False

def is_constraint(rule):
    '''Returns whether or not rule is a constraint'''
    if rule.head.type == ASTType.Literal:
        return rule.head.atom.type == ASTType.BooleanConstant
    return False

def ast_equals(ast_A, ast_B):
    return str(ast_A) == str(ast_B)

def is_predicate_in_positive_body(rule, predicate):
    return is_predicate_in_body(rule, predicate, is_positive)

def is_predicate_in_negative_body(rule, predicate):
    return is_predicate_in_body(rule, predicate, lambda literal: not is_positive(literal))

def is_positive(literal):
    '''Returns if the literal has epistemic negation. If the literal atom is a Comparison, will return False.'''
    if not hasattr(literal, 'type'):
        raise not_ast_error
    if literal.type != ASTType.Literal:
        raise TypeError('AST must be of type Literal, was %s (%s)' % (literal.type, literal))
    if literal.atom.type == ASTType.Comparison:
        return False
    return literal.sign == clingo.ast.Sign.NoSign

def get_head_literals(rule):
    if rule.head.type == ASTType.Disjunction:
        head_literals = [cond_lit.literal for cond_lit in rule.head.elements]
    elif rule.head.type == ASTType.Aggregate:
        head_literals = [cond_lit.literal for cond_lit in rule.head.elements]
    elif rule.head.type == ASTType.Literal and hasattr(rule.head.atom, 'term') and rule.head.atom.term.type == ASTType.Pool:
        head_literals = rule.head.atom.term.arguments
    else:
        head_literals = [rule.head]
    return head_literals

def copy(ast, transformer=None): # TODO Remove the need for this transformer
    """Deep copies an AST node. Will maintain head-body links if passed a Transformer."""
    copier = ASTCopier(transformer)
    copied = copier.deep_copy(ast)
    return copied

def is_rule(x):
    if isinstance(x, clingo.ast.AST):
        return x.type == ASTType.Rule
    return False

class ASTVisitor(object):

    def visit(self, x):
        if isinstance(x, clingo.ast.AST):
            attr = "visit_" + str(x.type)
            if hasattr(self, attr):
                return getattr(self, attr)(x)
            else:
                after = self.visit_children(x)
                return after
        elif isinstance(x, list):
            return [self.visit(y) for y in x]
        elif x is None:
            return x
        else:
            raise TypeError("unexpected type (%s)" % type(x))

    def visit_children(self, x):
        for key in x.child_keys:
            child_x = self.visit(getattr(x, key))
            x[key] = child_x
        return x

class ASTWrapper(object):

    def __init__(self, ast):
        self.ast = ast
    
    def __hash__(self):
        return -hash(str(self.ast))

    def __str__(self):
        return '<%s>' % str(self.ast)
    
    def __repr__(self):
        return '<%s>' % repr(self.ast)
    
    def __eq__(self, other):
        return ast_equals(self, other)
    
    def __ne__(self, other):
        return not self.__eq__(other)
    
    def unwrap(self):
        return self.ast

class RuleNormalizer(ASTVisitor):
    """Renames all variables in a standardized way to help identify equivalent ASTs
    The first variable encountered (and all of its occurrences) will be renamed to V1, second variable to V2, etc.
    Additionally sorts the rule body
    """
    
    def normalize(self, ast):
        self.var_dict = {}
        return self.visit(ast)

    def visit_Variable(self, var):
        if var.name not in self.var_dict:
            normalized_name = 'V%d' % len(self.var_dict.keys())
            self.var_dict[var.name] = normalized_name
        else:
            normalized_name = self.var_dict[var.name]
        return clingo.ast.Variable(var.location, normalized_name)

    def visit_Rule(self, rule):
        if hasattr(rule, 'body'):
            rule.body.sort()
        return self.visit_children(rule)

class ASTTypeSearch(ASTVisitor):

    def __init__(self, ast_type):
        self.counter = {}
        setattr(self, 'visit_%s' % str(ast_type), self.generic_visit)
    
    def generic_visit(self, x):
        wrapped_x = ASTWrapper(x)
        count = self.counter.get(wrapped_x, 0)
        self.counter[wrapped_x] = count + 1
        return x
    
    def ast_objects(self):
        return set(self.counter.keys())

    def count(self, x):
        return self.counter.get(ASTWrapper(x), 0)

class ASTCopier(ASTVisitor):

    def __init__(self, transformer=None):
        self.base_transformer = transformer

    def deep_copy(self, ast):
        return self.visit(ast)

    def visit(self, x):
        if isinstance(x, clingo.ast.AST):
            x = clingo.ast.AST(x.type, **dict(x))
            if self.base_transformer and hasattr(x, 'projected_rule_id'): # TODO Remove this projector-specific logic from here?
                self.base_transformer.add_body_function_link(x.projected_rule_id, x)
            return super(ASTCopier, self).visit(x)
        else:
            return super(ASTCopier, self).visit(x)

class PredicateSymbol(object):
    '''A class representation of a predicate symbol. Name and arity are immutable.'''

    def __init__(self, name, arity):
        self.__name = name
        self.__arity = arity
    
    def name(self):
        '''Returns the name of the predicate symbol'''
        return self.__name

    def arity(self):
        '''Returns the arity of the predicate symbol'''
        return self.__arity
    
    def __hash__(self):
        return hash(str(self))
    
    def __str__(self):
        return '%s/%d' % (self.__name, self.__arity)
    
    def __repr__(self):
        return str(self)
    
    def __eq__(self, value):
        if not isinstance(value, PredicateSymbol):
            return False
        return str(self) == str(value)
    