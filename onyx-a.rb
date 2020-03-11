PREC_CLOSED = {
    :closed => [
        [[/\G\(/, /\G\)/,], -> (i) { -> (s) { i.(s) } }]
    ],
}

PREC_CALL = {
    :left => [
        [[/\G/,], ->(l, r) { ->(s) { l.(s).(r.(s)) } }]
    ],
}

PREC_POWER = {
    :right => [
        [[/\G\*\*/,], -> (l, r) { ->(s) { l.(s) ** r.(s) } }]
    ],
}

PREC_TIMES = {
    :left => [
        [[/\G\*/,], -> (l, r) { -> (s) { l.(s) * r.(s) } }],
        [[/\G\//,], -> (l, r) { -> (s) { l.(s) / r.(s) } }],
        [[/\G%/,], -> (l, r) { -> (s) { l.(s) % r.(s) } }],
    ],
}

PREC_PLUS = {
    :left => [
        [[/\G\+/,], -> (l, r) { -> (s) { l.(s) + r.(s) } }],
        [[/\G-/,], -> (l, r) { -> (s) { l.(s) - r.(s) } }],
    ],
}

PREC_AND = {
    :left => [
        [[/\G&&/,], -> (l, r) { -> (s) { l.(s) && r.(s) } }],
    ],
}

PREC_OR = {
    :left => [
        [[/\G\|\|/,], -> (l, r) { -> (s) { l.(s) || r.(s) } }],
    ],
}

PREC_SEPARATOR = {
    :right => [
        [[/\G,/,], -> (l, r) { -> (s) { [l.(s), r.(s)] } }],
        [[/\G;/,], -> (l, r) { -> (s) { l.(s); r.(s) } }],
    ],
}

PREC_FLOW = {
    :prefix => [
        [[/\G\bif\b/, /\G\bthen\b/, /\G\belse\b/,], -> (c, t, f) { -> (s) { c.(s) ? t.(s) : f.(s) } }]
    ]
}

PREC_TOP = PREC_FLOW

PRECS = [PREC_FLOW, PREC_SEPARATOR, PREC_OR, PREC_AND, PREC_PLUS, PREC_TIMES, PREC_POWER, PREC_CALL, PREC_CLOSED]
PRECS.each_with_index {|prec, i|
    prec[:tighter] = PRECS[(i + 1)..] + [:literal]
}

def dbgo(obj)
    return obj.to_s unless obj.is_a? Hash
    obj = obj.clone
    obj.delete :tighter
    return obj.to_s.gsub(/<Proc.+?>/, 'fn')
end

def d(obj)
    if obj.is_a? String then
        print obj + '\n'
    else
        p dbgo(obj)
    end
    obj
end

module ExpressionParser
    def p_expr(node)
        jump
        start = @index
        old_node = @node
        @node = node
        if @cache[[start,node,:expr]] then
            result, @index = @cache[[start,node,:expr]]
            @node = old_node
            return result
        end
        result = node == :literal ? p_literal : p_hat
        @cache[[start,node,:expr]] = result, @index
        @node = old_node
        result
    end
    
    def p_literal; literal end
    
    def op(key, hole: :none)
        return p_literal if @node == :literal
        ops = @node[key]
        return unless ops
        start = @index
        ops.lazy.each {|op, create_lambda|
            cached, @index = @cache[[start, ops, op]]
            return cached if cached
            @index = start
            ope = eat(op[0])
            next unless ope
            exprs = []
            next unless op[1..].all? {|regex|
                exprs.push(expr = parse)
                next if !eat(regex)
                expr
            }
            result = case hole
            when :right
                -> (inner) { create_lambda.(*exprs, inner) }
            when :left
                -> (inner) { create_lambda.(inner, *exprs) }
            when :both
                -> (left, right) { create_lambda.(left, *exprs, right) }
            else
                create_lambda.(*exprs)
            end
            @cache[[start, ops, op]] = result, @index
            return result
        }
        @index = start
        nil
    end
    
    def p_hat
        closed_expr = op(:closed)
        return closed_expr if closed_expr
        start = @index
        if tighter_expr = p_up then
            if non_expr = op(:non) and tighter_expr_2 = p_up then
                return non_expr.(tighter_expr, tighter_expr_2)
            end 
        end
        @index = start
        right_exprs = []
        while right_expr = p_right do right_exprs.push right_expr end
        if right_exprs.length != 0 and tighter_expr = p_up then
            return right_exprs.reverse_each.reduce(tighter_expr) {|prev, expr| expr.(prev) }
        end
        @index = start
        if tighter_expr = p_up then
            left_exprs = []
            while left_expr = p_left do left_exprs.push left_expr end
            if left_exprs.length != 0 then
                return left_exprs.reduce(tighter_expr) {|prev, expr| expr.(prev) }
            end
        end
        @index = start
        nil
    end
    
    def p_right
        prefix_expr = op(:prefix, hole: :right)
        return prefix_expr if prefix_expr
        start = @index
        if tighter_expr = p_up and right_expr = op(:right, hole: :both) then
            return -> (inner) { right_expr.(tighter_expr, inner) }
        end
        @index = start
        nil
    end
    
    def p_left
        postfix_expr = op(:postfix, hole: :left)
        return postfix_expr if postfix_expr
        start = @index
        if left_expr = op(:left, hole: :both) and tighter_expr = p_up then
            return -> (inner) { left_expr.(inner, tighter_expr) }
        end
        @index = start
        nil
    end
    
    def p_up
        return p_literal if @node == :literal
        @node[:tighter].lazy.map {|tighter| p_expr(tighter) }.find &:itself
    end
end

class Parser
    def initialize(str)
        @string = str
        @index = 0
        @jumps = {}
        @cache = {}
    end
    
    def jump
        if @jumps[@index] then
            @index = @jumps[@index]
        else
            index = @index
            while match = /\G[ \t\n]+/.match(@string, @index) || /\G#.+/.match(@string, @index) do
                @index += match[0].length
            end
            @jumps[index] = @index
        end
    end
    
    def eat(token)
        jump
        match = token.match @string, @index
        if match then @index += match[0].length end
        match[0] if match
    end
    
    include ExpressionParser
    
    def literal
        result = decimal || integer || string
        return result if result
        result = boolean
        return result if result != nil
        ident
    end
    
    def decimal; (result = result.to_f; -> (s) { result }) if result = eat(/\G\d+\.\d+/) end
    def integer; (result = result.to_i; -> (s) { result }) if result = eat(/\G\d+/) end
    def string; (result = result.undump; -> (s) { result }) if result = eat(/\G'(?:[^']|\\[\\'nt])'/) end
    def boolean; (result = result == 'true'; -> (s) { result }) if result = eat(/\G\btrue\b|\G\bfalse\b/) end
    def ident; (result = result.to_sym; -> (s) { s[result] }) if result = eat(/\G\w+|\G\S*_(\S+_)*\S*/) end
    
    def parse(prec=PREC_TOP)
        expr = p_expr(prec)
        return expr if expr
        return unless prec != :literal and tighter = prec[:tighter]
        tighter.lazy.map {|tighter_node|
            parse tighter_node
        }.find &:itself
    end
end

class Scope
    def initialize(parent=nil, lookup=nil)
        @parent = parent || {}
        @lookup = lookup || {}
    end
    
    def [](key)
        result = @lookup[key]
        return result != nil ? @lookup[key] : @parent[key]
    end
    
    def []=(key, value)
        if @lookup[key] != nil || @parent[key] == nil then
            @lookup[key] = value
        else
            @parent[key] = value
        end
    end
end

DEFAULT_SCOPE = Scope.new
DEFAULT_SCOPE[:print] = ->(o) { puts o }
DEFAULT_SCOPE[:add] = ->(a) { ->(b) { a + b }  }

# if true then 10 * (2 ** 3) else 123); 2; 3
do_trace = false
trace = TracePoint.new(:b_call) { |tp| puts "lambda at #{tp.method_id} (#{tp.path}:#{tp.lineno})" } if do_trace
parse = Parser.new(ARGF.read).parse
scope = Scope.new(DEFAULT_SCOPE)
trace.enable if do_trace
p parse.(scope)
trace.disable if do_trace
