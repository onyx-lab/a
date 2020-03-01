class Scope
    def initialize(parent=nil, lookup=nil)
        @parent = parent
        @lookup = lookup || {}
    end
    
    def [](key)
        result = parent[key]
        result != nil ? parent[key] : lookup[key]
    end
    
    def []=(key, value)
        if parent[key] != nil then
            parent[key] = value
        else
            lookup[key] = value
        end
        nil
    end
end

class Metascope
    def initialize(parent=nil, lookup=nil)
        lookup ||= {}
        lookup.each {|key, value|
            lookup[key] = Scope.new(parent[key], lookup[key])
        }
        parent.each {|key, value|
            next unless lookup[key]
            lookup[key] = Scope.new(parent[key], {})
        }
        @parent = parent
        @lookup = lookup
    end
    
    def [](key)
        lookup[key]
    end
end
