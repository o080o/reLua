
-- object Tree A tree! what more description do you want??
-- 	var Tree.val = String
-- 	var Tree.children = {Tree} | {}

local re = {}


local DefaultTable = {}
DefaultTable.__index = DefaultTable
setmetatable(DefaultTable, DefaultTable)
function DefaultTable.__call(table, val)
	local self = {}
	local mt = {}
	if type(val) == "function" then
		mt.__index = function(table, key)
			local v = val(key)
			table[key] = v
			return v	
		end
	elseif type(val) == "table" then
		mt.__index = function(table, key)
			local t = {}
			for k,v in pairs(val) do
				t[k]=v
			end
			table[key] = t
			return t
		end
	else
		mt.__index = function(table, key)
			 table[key] = val
			return val
		end
	end
	return setmetatable(self, mt)
end

local ListTable = {}
ListTable.__index = ListTable
setmetatable(ListTable, ListTable)
function ListTable.__call()
	return DefaultTable({})
end


local Tree = {}
Tree.__index = Tree
-- function newTree(* val, {Tree} children) return Tree A Tree object with val as its value and *children* for children
function Tree.new(val, children)
	local t
	if children then
		t = {val=val, children=children}
	else
		t = {val=val, children={}}
	end
	setmetatable(t, Tree)
	return t
end	
--function Tree.empty(Tree self) return Bool True if self is an empty Tree, false otherwise.
--A Tree is empty if has no children and has no val field.
function Tree.empty(self)
	if not self.val then
		return true
	end
	return false
end
-- function Tree.concat(Tree left, Tree right) return Tree The concatenation of the left and right Trees
function Tree.concat(left, right)
	if left:empty() then return right end
	if right:empty() then return left end
	return Tree.new("CONCAT", {left, right})
end
-- function Tree.print(Tree self) Print the tree to stdout
function Tree.print(self, indent)
	local indent = indent or ""
	local val = tostring(self.val) or "EMPTY"
	print(indent .. "->" .. val)
	for _,t in pairs(self.children) do
		t:print( indent .. "|" )
	end
end

-- object Stack a filo stack. 
local Stack = {}
Stack.__index = Stack
--function Stack.new() return Stack A new empty stack
function Stack.new()
	return setmetatable({}, Stack)
end
-- function Stack.push(self, val) Adds val ontop of the stack
function Stack:push(val)
	table.insert(self, val)
end
-- function Stack.pop(self) return * The first item on the stack
function Stack:pop()
	return table.remove(self)
end



-- functions in grammar table take an AST,a stack, and a string (the unparsed input), and return an AST, and optionally an integer number of characterss to skip for parsing
-- the stack is used for open/close parens, alternations, etc, by pushing the current AST onto the stack, and concatenating to it later by poping it off the stack.
grammar = setmetatable({}, {__index=function(t,k) return function(root, stack) return grammar.literal(root, stack, k) end end})
grammar.ESC = "/" --define the escape character

grammar.literal=function(root, stack, c)
	root.children[1] = root.children[1]:concat(Tree.new(c))
	return root
end

function CharacterClass(negate)
	negate = negate or false
	local self
	self = DefaultTable(negate)
	self.isCharacterClass = true
	mt = getmetatable(self)
	function mt.__tostring(self)
		local str = {}
		table.insert(str, "[")
		if negate then table.insert(str, "^") end
		for k,v in pairs(self) do
			if k ~= "isCharacterClass" then
				if negate and not v or not negate and v then table.insert(str, k) end
			end
		end
		table.insert(str, "]")
		return table.concat(str)
	end
	return self
end

testclass = CharacterClass(true)
print (testclass["b"])
assert(testclass["b"])

local dotclass = CharacterClass(true)
local charclasses=0
grammar["["]=function(root,stack,str)
	local class = CharacterClass()
	local i=0
	for c in str:gmatch(".") do
		i = i+1
		if i == 1 and c == "^" then
			negate = true
			class = CharacterClass(true)
		elseif c == grammar.ESC and not escaped then 
			escaped = true
		elseif c == "]" and not escaped then
			break
		else
			if not negate then
				class[c] = true
			else
				class[c] = false
			end
		end
	end
	print("New character class:", charclasses+1, class)
	-- we could test for duplicate classes here as well..
	charclasses=charclasses+1
	root.children[1] = root.children[1]:concat(Tree.new(class))
	return root,i
end
grammar["."]=function(root,stack)
	root.children[1] = root.children[1]:concat(Tree.new(dotclass))
	return root
end
grammar["?"]=function(root,stack)
	if root.children[1].val == "CONCAT" then
		root.children[1].children[2] = Tree.new("QUESTION", {root.children[1].children[2]})
	else
		root.children[1] = Tree.new("QUESTION", {root.children[1]})
	end
	return root
end
grammar["+"]=function(root,stack)
	if root.children[1].val == "CONCAT" then
		root.children[1].children[2] = Tree.new("PLUS", {root.children[1].children[2]})
	else
		root.children[1] = Tree.new("PLUS", {root.children[1]})
	end
	return root
end
grammar["-"]=function(root,stack)
	if root.children[1].val == "CONCAT" then
		root.children[1].children[2] = Tree.new("MINUS", {root.children[1].children[2]})
	else
		root.children[1] = Tree.new("MINUS", {root.children[1]})
	end
	return root
end
grammar["*"]=function(root,stack)
	if root.children[1].val == "CONCAT" then
		root.children[1].children[2] = Tree.new("STAR", {root.children[1].children[2]})
	else
		root.children[1] = Tree.new("STAR", {root.children[1]})
	end
	return root
end
grammar["|"]=function(root,stack)
	newroot = Tree.new("EMPTY", {Tree.new()})
	root.children[1] = Tree.new("|", {root.children[1], newroot})
	return newroot
end
grammar["("]=function(root,stack)
	newroot = Tree.new("CAPTURE", {Tree.new()})
	root.children[1] = root.children[1]:concat(newroot)
	stack:push(root)
	return newroot
end
grammar[")"]=function(root,stack)
	newroot = stack:pop()
	return newroot
end

--data NFA 
--data NFA.states = [State]
--data NFA.edges = [{Input:[State]}]
--data State = Table Holds some arbitrary state information
--data Input = Char
local stateIdx = 0
local State = {}
State.__index = State
-- function State.new() return State A new state
function State.new()
	stateIdx = stateIdx + 1
	return setmetatable({id=tostring(stateIdx)}, State)
end
function State.__tostring(self)
	return "State: " .. self.id
end




local NDFA = {}
NDFA.__index = NDFA
-- function NDFA.new() return NDFA A new machine with only a single state and no final states.
function NDFA.new()
	local m = {}
	local initState = State.new()
	m.start = initState

	m.states = {}
	m.states[initState]=true

	m.edges = DefaultTable( function() return ListTable() end )
	m = setmetatable(m, NDFA)
	return m
end

--function NDFA.final(self) return [State] The final states of the machine
function NDFA:final()
	local states = {}
	for s,_ in pairs(self.states) do
		if s.isFinal then
			table.insert(states, s)
		end
	end
	return states
end

--function NDFA:addEdge(State source, Input input, State dest) create an edge leading from source to dest given the input.
function NDFA:addEdge(source, input, dest)
	print(source, input, dest)
	if type(input) == "table" and input.isCharacterClass then
		self.edges[source]["CLASS"][input] = dest
	else
		table.insert(self.edges[source][input], dest )
	end
end

--function NDFA:insert(NDFA m) return NDFA A machine with the states and edges of m added to self with no link between them. 
function NDFA:insert(m)
	-- add all states in m to self and copy edges
	for s,_ in pairs(m.states) do
		self.states[s] = true
		self.edges[s] = m.edges[s]
	end
	return self
end

--function NDFA:concat(NDFA m1, NDFA m2) return NDFA A new NDFA resulting from concatenating m2 to m1
function NDFA.concat(m1, m2)
	-- link all end states of m1 to the start of m2
	local m2Start = m2.start
	print("concating")
	for i,s in pairs(m1:final()) do
		s.isFinal = false
		m1:addEdge( s, "EPSILON", m2Start )
	end
	m1:insert(m2)
	return m1
end

-- function NDFA:addState(State source, Input input, Bool final) Add a new State to this machine, adding edges from the given state ID to it.
function NDFA:addState(source, input, final)
	local state = State.new() 
	if final then 
		state.isFinal = true
	end
	--table.insert(self.states, state)
	self.states[state] = true
	if source and input then
		self:addEdge(source, input, state)
	end
	return state
end

function NDFA:execute(input)
	print("exec:", input)
	self:init(input)
	for c in input:gmatch(".") do
		print("in:", c)
		if not self:step(c) then
			break
		end
		--execute "hit" functions in states
		--for s,path in pairs(self.curState) do
			--print("state:",i, s[1], s[2])
			--if s.hit then s.hit(self,s,c, path) end
		--end
	end
	local match = false
	for _,s in pairs( self:final() ) do
		if self.curState[s] then
			match=true
			break
		end
	end
	
	if match then
		print(self.regex,"Matched:", input)
		self:printMatches()
	else
		print(self.regex,"Failed:", input)
	end
	return match
end

function NDFA:printMatches()
	local finals = self:final()
	local path
	for _,s in pairs(finals) do
		local path1 = self.curState[s]
		path = compare(path1, path)
	end
	for n = 1,path.nGroups*3,3 do
		if path[n+1] and path[n+2] then
			print("match: ", (n-1)/3+1, "["..path[n+1]..":"..path[n+2].."]", self.input:sub(path[n+1], path[n+2]))
		end
	end
end

local Path = {}
Path.__index = Path

function Path.new(...)
	return setmetatable({nGroups=0}, Path)
end
function Path.__tostring(path)
	local str = {}
	table.insert( str, "Path:")
	for n = 1,path.nGroups*3,3 do
		table.insert(str, "  group:")
		table.insert(str, (n-1)/3+1)
		if path[n+1] then
			table.insert(str, " [")
			table.insert(str, path[n+1])
			table.insert(str, ":")
			if path[n+2] then table.insert(str, path[n+2])
			else table.insert(str, "...") end
			table.insert(str, "]")
		else
			table.insert(str, " None")
		end
	end
	return table.concat(str)
end
function NDFA:init(input)
	print("Init")
	self.steps = 0
	self.input = input
	self.curState = {}
	for s,fs in pairs( eClosure(self, self.start) ) do
		local path = Path.new()
		for _,f in ipairs(fs) do
			f(self, s, "", path)
		end
		self.curState[s] = path
	end
	self.subMatches = {} -- completed matches
	self.partialMatches = {} -- partial, incomplete, matches

	for s,path in pairs(self.curState) do
		print("istate:", s, path)
		--if s.hit then s.hit(self,s,"",path) end
	end
end

function compare( path1, path2)
	if not path2 then return path1 end
	-- compare path1 to path2 and return the optimal one.
	local nGroups = math.max( path1.nGroups, path2.nGroups)
	print("compare:")
	print(" ", path1)
	print(" ", path2)

	for n = 1,nGroups*3,3 do
		assert( path1[n] == path2[n], "Mismatched maximality for group" )
		local maxify = path1[n +0] 
		print("group:", n, maxify)

		local len1, len2 = 0,0
		print(path1[n+2], path1[n+1], path2[n+2], path2[n+1])
		if path1[n+2] then len1 = path1[n+2] - path1[n+1] end
		if path2[n+2] then len2 = path2[n+2] - path2[n+1] end

		if len1>len2 then
			if maxify then print("","",path1) else print("","",path2) end
			if maxify then return path1 else return path2 end
		elseif len2>len1 then
			if maxify then print("","",path2) else print("","",path1) end
			if maxify then return path2 else return path1 end
		end

		-- so far they are equivilent if we have not returned by now.
	end
	-- all groupings are equivilent
	return path1
end

copyops = 0
function copy(path)
	copyops = copyops + 1 -- keep track of table creation operations for debugging/performance reasons.
	local newpath = Path.new()
	for k,v in pairs(path) do
		newpath[k] = v
	end
	return newpath
end

function NDFA:step(input)
	local nextState = {}
	local isAlive = false
	self.steps = self.steps + 1
	function addState(s, path)
		isAlive = true
		newstates = eClosure(self, s)
		for state,fs in pairs(newstates) do
			local newpath = copy( path )
			for _,f in ipairs(fs) do
				f(self, state, input, newpath)
			end
			nextState[state] = compare( newpath, nextState[state] )
		end
	end

	for state,path in pairs(self.curState) do
		for table,s in pairs(self.edges[state]["CLASS"])do
			if table[input] then
				addState(s, path)
			else
				print("failed:", table, input)
			end
		end
		for _,s in pairs(self.edges[state][input]) do
			addState(s, path)
		end
	end
	self.curState = nextState
	print("current state:")
	for state,path in pairs(self.curState) do
		print("", state, path)
	end
	return isAlive
end

function NDFA.__tostring(self)
	return "NDFA: " .. self
end

--function eClosure(NDFA, Int) return {State:[Function]} List of all states accessible via epsilon transitions from the given state, and a list of functions to execute for that state. This function is cached, and the returned table should NOT be modified.
function eClosure(m, start, mutable)
	if not m.cache then m.cache = DefaultTable( function() return ListTable() end ) end
	local cache = m.cache.eClosure[start]
	if cache.clean then 
		return cache.val --return the actual cache, assuming caller will not attempt to modify it
	else

		print("eClosure:", start)
		val = eClosure2(m, start, {}, {}, 1)
		-- cache it
		cache.val = val; cache.clean = true
		return val
	end
end
function eClosure2(m, start, prevStates, depthTable, depth)
	print("", start, start.hit)

	local cache = m.cache.eClosure[start]
	-- no more intermediate caching
		local states = {}

		-- add the initial state and compile the function list
		states[start] = {}
		depthTable[start] = depth

		-- find all states with Epsilon transitions from here
		for _,s in pairs( m.edges[start]["EPSILON"]) do
			if not depthTable[s] or depthTable[s] > depth then
				eClosure2(m,s, states, depthTable, depth+1)
			end
		end
		-- copy start.hit function into the cache
		if start.hit then 
			for k,v in pairs(states) do
				print("", "", "adding:", start, start.hit, "into", k)
				table.insert( v, 1, start.hit)
			end
		end
		-- copy this value into the prevStates table.
		for k,v in pairs(states) do
			prevStates[k]=v
		end
	return prevStates 
end

-- function compile(String) return Regex
-- compiles the regex string into an NFA.
function re.compile(regex)
	local ast = parse(regex)
	local names = {"One", "Two", "Three", "Four?"}
	stateIdx = 0
	local nfa = buildNDFA(ast, {captureNames=names, groupN=0})
	nfa.regex = regex
	return nfa
end

-- helper functions for building NDFA's with submatching enabled.
function startGroup(i, maxify)
	return function(self, state, c, path)
		local ii = (i-1)*3 +1
		-- use as an array to avoid unnessesary table creation when we clone paths.
		path[ii] = maxify
		path[ii +1] = self.steps + 1
		path[ii +2] = nil
		if path.nGroups<i then path.nGroups=i end
		print("start group", i, state, path)
	end
end
function stopGroup(i)
	return function (self, state, c, path)
		local ii = (i-1)*3 +1
		if path[ii +1] then
			if path.nGroups<i then path.nGroups=i end
			path[ii +2] = self.steps
		end
		print("stop group", i, state, path)
	end
end
-- function buildNFA(Tree, NDFA) return NDFA builds an NFA from the given regex AST
function buildNDFA(ast, state) 
	local mt = {}
	mt.__index = function(table, key)
		if not key then print("nil literal?") else
			print("lit:",key)
		end
		return function(machines)
			local m = NDFA.new()
			m:addState(m.start,key,true)
			return m
		end
	end
	local fragments = setmetatable({}, mt)
	function fragments.CONCAT(machines)
		local m0 = nil
		for _,m1 in ipairs(machines) do
			if m0 then
				print("c1")
				m0:concat(m1) 
			else
				m0 = m1
			end
		end
		return m0
	end
	function fragments.DOT(machines)
		local m = NDFA.new()
		m:addState(m.start,"DOT",true)
		return m
	end
	function fragments.QUESTION(machines, buildState)
		local m = NDFA.new()
		local m1 = machines[1]
		local m2 = NDFA.new()
		m.start.isFinal = true
		m2.start.isFinal = true
		m:concat(m1):concat(m2)

		buildState.groupN = buildState.groupN + 1
		local i= buildState.groupN
		m.start.hit = startGroup(i, true)
		m2.start.hit = stopGroup(i)
		m:addEdge(m.start, "EPSILON", m2.start)
		return m
	end
	function fragments.PLUS(machines, buildState)
		local m = NDFA.new()
		local m1 = machines[1]
		local m2 = NDFA.new()
		m.start.isFinal = true
		m2.start.isFinal = true
		local finalStates = m1:final()
		m:concat(m1):concat(m2)

		buildState.groupN = buildState.groupN + 1
		local i= buildState.groupN

		local start = m1.start
		for _,stop in pairs(finalStates) do
			m:addEdge(stop,"EPSILON", start)
		end
		m.start.hit = startGroup(i, true)
		m2.start.hit = stopGroup(i)

		return m
	end
	function fragments.MINUS(machines, buildState)
		local m = fragments.STAR(machines, buildState)
		local i= buildState.groupN
		m.start.hit = startGroup(i, false)
		return m
	end
	function fragments.STAR(machines, buildState)
		local m = NDFA.new()
		local m1 = machines[1]
		local m2 = NDFA.new()
		m.start.isFinal = true
		m2.start.isFinal = true
		local finalStates = m1:final()
		m:concat(m1):concat(m2)
		

		buildState.groupN = buildState.groupN + 1
		local i= buildState.groupN

		local start = m1.start
		for _,stop in pairs(finalStates) do
			m:addEdge(stop,"EPSILON", start)
			m:addEdge(start,"EPSILON", stop)
		end
		m.start.hit = startGroup(i, true)
		m2.start.hit = stopGroup(i)

		return m
	end
	fragments["|"] = function(machines)
		local m = NDFA.new()
		local m1 = machines[1]
		local m2 = machines[2]
		m:insert(m1)
		m:insert(m2)
		m:addEdge(m.start, "EPSILON", m1.start)
		m:addEdge(m.start, "EPSILON", m2.start)
		return m
	end
	function fragments.ROOT(...)
		return fragments.EMPTY(...)
	end
	function fragments.EMPTY(machines)
		if machines[1] then
			return machines[1]
		else
			return NDFA.new()
		end
	end
	function fragments.CAPTURE(machines, buildState)
		local m1 = NDFA.new()
		--print("names:", buildState.captureNames)
		local name = table.remove( buildState.captureNames, 1)
		buildState.groupN = buildState.groupN + 1
		local i= buildState.groupN
		print("capture:", name)
		m1.start.isFinal = true
		m1.start.captureName = name

		m1.start.hit = startGroup(i, true)

		local m2 = machines[1]
		local m3 = NDFA.new()
		m3.start.isFinal =  true
		m3.start.captureName = name

		m3.start.hit = stopGroup(i)

		m1:concat(m2):concat(m3)
		return m1
	end

	local children = {}
	for i,c in pairs( ast.children ) do
		local m = buildNDFA(c,state)
		children[i] = m
	end
	if not children then print("???") end
	local val = ast.val or "Empty"
	local m = fragments[val](children, state)
	return m
end

--function parse(String) return Tree An AST of the Regex language to ease the creation of the final NFA
function parse(regex)
	local root = Tree.new("ROOT", {Tree.new()})
	local realroot = root
	print("parsing  " .. regex)
	local stack = Stack.new()
	-- parse the regex grammar
	local escaped = false
	local characterclass = true
	local i=0
	while i < #regex do
		i=i+1
		local c= regex:sub(i,i)
		local rest= regex:sub(i+1) or ""
		print("p:", c, rest)
		if escaped then
			if grammar[ grammar.ESC .. c ] then
				root = grammar[grammar.ESC..c](root, stack)
			else
				root = grammar.literal(root, stack, c)
			end
		elseif c == grammar.ESC then
			escaped = true
		else
			root,n = grammar[c](root, stack, rest)
			if n then i=i+n end
		end
	end
	assert(not stack:pop(), "Regex parse stack is not empty. Possible missing close paren?")
	realroot:print()
	return realroot
end
return re
