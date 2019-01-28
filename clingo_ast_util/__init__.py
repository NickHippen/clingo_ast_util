import clingo.ast

not_ast_error = TypeError('Literal must be an AST object with a type attribute')

def find_ast_type(ast, ast_type):
    if type(ast) is set:
        ast = [a.ast for a in ast]
    type_search = ASTTypeSearch(ast_type)
    type_search.visit(ast)
    return type_search.ast_objects()

def find_variables(ast):
    return find_ast_type(ast, clingo.ast.ASTType.Variable)

def get_predicate_symbol(ast):
    '''Gets the predicate symbol of a literal, atom, or function'''
    if not hasattr(ast, 'type'):
        raise not_ast_error
    if ast.type == clingo.ast.ASTType.Literal:
        if ast.atom.type != clingo.ast.ASTType.SymbolicAtom:
            raise TypeError('Literal atom attribute must be of type SymbolicAtom (was %s)' % ast.atom.type)
    elif ast.type not in [clingo.ast.ASTType.SymbolicAtom, clingo.ast.ASTType.Function]:
        raise TypeError('%s is not of type "Literal", "SymbolicAtom", or "Function"' % ast.type)
    function_set = find_ast_type(ast, clingo.ast.ASTType.Function)
    assert len(function_set) > 0
    function = function_set.pop().unwrap()
    if function.external:
        raise TypeError('Cannot handle external functions')
    return '%s/%d' % (function.name, len(function.arguments))

def is_predicate_in_positive_body(rule, predicate):
    if not hasattr(rule, 'type'):
        raise not_ast_error
    if rule.type != clingo.ast.ASTType.Rule:
        raise TypeError('AST must be of type Rule (found %s)' % rule.type)
    for body_literal in rule.body:
        try:
            if is_positive(body_literal):
                body_predicate = get_predicate_symbol(body_literal)
            else:
                continue
        except TypeError:
            raise TypeError('Unsupported literal in body of rule')
        if body_predicate == predicate:
            return True
    return False

def is_positive(literal):
    '''Returns if the literal has epistemic negation. If the literal atom is a Comparison, will return False.'''
    if not hasattr(literal, 'type'):
        raise not_ast_error
    if literal.type != clingo.ast.ASTType.Literal:
        raise TypeError('AST must be of type Literal')
    if literal.atom.type == clingo.ast.ASTType.Comparison:
        return False
    return literal.sign == clingo.ast.Sign.NoSign

def get_head_literals(rule):
    if rule.head.type == clingo.ast.ASTType.Disjunction:
        head_literals = [cond_lit.literal for cond_lit in rule.head.elements]
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
        return x.type == clingo.ast.ASTType.Rule
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
        return str(self.ast) == str(other.ast)
    
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
