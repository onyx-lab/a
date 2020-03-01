require_relative './scope.rb'

class Instance
    attr_accessor :value
    
    def initialize(type)
        @value = nil
    end
end

class Type
    def initialize(members)
        @members = {}
        members.each {|name, value|
            if name[0] == ' ' then
                case name
                when :' new' then
                    define_singleton_method :new, -> (*args) {
                        obj = Instance.new(self)
                        value.(obj, *args)
                        return args
                    }
                    next
                when :' parents' then
                    @parents = value.map {|name| TYPES[name] }
                    next
                end
            end
            @members[name] = value
        }
    end
    
    def self.from_obj(obj)
        Type.new obj
    end
end

class Member
    def initialize(value)
        @value = value
        @type = value.type
    end
    
    def self.from_obj(obj)
        Member.new obj
    end
end

class TypeclassInstance
    def initialize(members)
        @lookup = {}
        instances.each {|type_names, value|
            type_names = [type_names] unless type_names === Array
            @lookup[type_names.map {|name| TYPES[name] }] = value
        }
    end
    
    def resolve(obj)
        instances.each {|type, value|
            return value if is_subtype?(obj, type)
        }
    end
    
    def self.from_obj(obj)
        new_obj = {}
        obj.each {|key, value|
            new_obj[key] = Method.new value
        }
        TypeclassInstance.new obj
    end
end

class Typeclass
    def initialize(instances)
        instances.each {|type_name, value|
            #
        }
    end
    
    def self.from_obj(obj)
        Typeclass.new obj
    end
end

TYPES = {
    :object => {
        #
    },
    :boolean => {
        :' parents' => [:object],
        :' new' => ->(o, value) { o.value = value }
    },
    :integer => {
        :' parents' => [:object],
        :' new' => ->(o, value) { o.value = value }
    },
    :floating_point => {
        :' parents' => [:object],
        :' new' => ->(o, value) { o.value = value }
    },
    :string => {
        :' parents' => [:object],
        :' new' => ->(o, value) { o.value = value }
    }
}

TYPES.each {|k, v|
    TYPES[k] = Type.from_obj v
}

UNESCAPES = {'\'' => '\'', '\n' => 'n', '\t' => 't', '\\' => '\\'}

TYPECLASSES = {
    # Haskell-like
    # :Show => {
    #     :' declarations' => {
    #         :show => { :parameters => [:a], type => [:a, TYPES[:string]] }
    #     },
    #     :integer => {
    #         :show => -> (o) { o.to_s }
    #     },
    #     :floating_point => {
    #         :show => -> (o) { o.to_s }
    #     },
    #     :string => {
    #         :show => -> (o) { '\'' + o.gsub(/['\n\t\\]/) {|m| UNESCAPES[m] } + '\'' }
    #     }
    # },
    :Function => {
        :' declarations' => {
            #
        }
    }
}

TYPECLASSES.each {|k, v|
    TYPECLASSES[k] = Type.from_obj v if k[0] != ' '
}

def _f(a, b)
    TYPECLASSES[:Function].new(TYPES[a], TYPES[b])
end

INTERNAL = {
    :variables => {
        :add_int => {
            :value => -> (a, b) { a + b },
            :type => f(:integer, :integer)
        },
        :add_floating_point => {
            :value => -> (a, b) { a + b },
            :type => f(:floating_point, :floating_point)
        },
        :subtract_int => {
            :value => -> (a, b) { a - b },
            :type => f(:integer, :integer)
        },
        :subtract_floating_point => {
            :value => -> (a, b) { a - b },
            :type => f(:floating_point, :floating_point)
        },
        :a => {}
    }
    :types => TYPES,
    :typeclasses => TYPECLASSES
}

def type_is_subtype?(child, parent)
    !child[:parents] && child[:parents].any? {|p| type_is_subtype?(p, parent) }
end

def type_is_supertype?(parent, child)
    type_is_subtype?(child, parent)
end

def is_subtype?(obj, type)
    type_is_subtype?(obj.type, type)
end

def is_supertype?(obj, type)
    type_is_supertype?(obj.type, type)
end