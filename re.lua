
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
	local val = self.val or "EMPTY"
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



grammar = {}
grammar.ESC = "/"
grammar.literal=function(root,stack,c)
	root.children[1] = root.children[1]:concat(Tree.new(c))
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
local i = 0
local State = {}
State.__index = State
-- function State.new() return State A new state
function State.new()
	i = i + 1
	return setmetatable({id=tostring(i)}, State)
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
	if not self.edges[source] then self.edges[source] = {} end
	if not self.edges[source][input] then self.edges[source][input] = {} end
	table.insert(self.edges[source][input], dest )
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
		self.steps = self.steps + 1
		--execute "hit" functions in states
		for s,path in pairs(self.curState) do
			print(path)
			--print("state:",i, s[1], s[2])
			if s.hit then s.hit(self,s,c, path) end
		end
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
		local matches = self.subMatches
		--printMatches("root",matches)
	else
		print(self.regex,"Failed:", input)
	end
	return match
end

function NDFA:init(input)
	print("Init")
	self.curState = {}
	for s,_ in pairs( eClosure(self, self.start) ) do
		self.curState[s] = {}
	end
	self.subMatches = {} -- completed matches
	self.partialMatches = {} -- partial, incomplete, matches

	self.steps = 0
	self.input = input
	for s,path in pairs(self.curState) do
		print("istate:", s, path)
		if s.hit then s.hit(self,s,"",path) end
	end
end

function compare( path1, path2)
	if not path2 then return path1 end
	return path1
end

copyops = 0
function copy(path)
	copyops = copyops + 1 -- keep track of table creation operations for debugging/performance reasons.
	local newpath = {}
	for k,v in pairs(path) do
		newpath[k] = v
	end
	return path
end

function NDFA:step(input)
	local nextState = {}
	local isAlive = false
	for state,path in pairs(self.curState) do
		for _,s in pairs(self.edges[state][input]) do
			isAlive = true
			newstates = eClosure(self, s)
			for k,v in pairs(newstates) do
				nextState[k] = copy( compare( path, nextState[k] ) )
			end
		end
	end
	self.curState = nextState
	return isAlive
end

function NDFA.__tostring(self)
	return "NDFA: " .. self
end

--function eClosure(NDFA, Int) return {Int:State} List of all states accessible via epsilon transitions from the given state
function eClosure(m, start, prevStates)
	if not m.cache then m.cache = ListTable() end
	if not m.cache.eClosure.copyops then m.cache.eClosure.copyops = ListTable() end
	val = eClosure2(m, start, prevStates)

	-- perform all cache copy operations not what we are in the lowest level recursion
	for src, destinations in pairs(m.cache.eClosure.copyops) do
		for _,dst in pairs(destinations) do
			local cache = m.cache.eClosure[dst]
			cache.clean = true
			for k,v in pairs( m.cache.eClosure[src] ) do
				cache[k] = v
			end
		end
		m.cache.eClosure.copyops[src] = nil
	end
	return val
end
function eClosure2(m, start, prevStates)
	-- caches results, as well as results of intermediate steps, even with cyclic graphs
	prevStates = prevStates or {}

	local states = m.cache.eClosure[start] or {}
	if not states.clean then
		setmetatable(states, {__index=prevStates}) -- set __index metamethod to avoid recursion by inclusion testing, but doin't add it to the list of found states so far
		-- add the initial state
		states[start] = true; states.clean = true

		-- find all states with Epsilon transitions from here
		for _,s in pairs( m.edges[start]["EPSILON"]) do
			if not states[s] then -- test to avoid infinite recursion
				eClosure2(m,s, states)
			else
				-- possible infinite recursion. defer caching
				states.clean = false
				table.insert( m.cache.eClosure.copyops[s], start)
			end
		end
		m.cache.eClosure[start] = setmetatable( states, {})-- remove metatable so that the cache is not influenced by prevStates
	end

	for s,_ in pairs(states) do
		prevStates[s] = true
	end
	prevStates.clean = nil -- dont copy the cache's "clean" field...
	return prevStates, cacheThingyTable
end

-- function compile(String) return Regex
-- compiles the regex string into an NFA.
function re.compile(regex)
	local ast = parse(regex)
	local names = {"One", "Two", "Three", "Four?"}
	local nfa = buildNDFA(ast, {captureNames=names, groupN=0})
	nfa.regex = regex
	return nfa
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
--[[
	function fragments.PLUS(machines)
		local m = machines[1]
		local finalStates = m:final()
		for _,s in pairs(finalStates) do
			m:addEdge(s, "EPSILON", m.start)
		end
		return m
	end
--]]
	function fragments.STAR(machines, buildState)
		local m = NDFA.new()
		m.start.isFinal = true
		m:concat(machines[1])
		local final = m:addState(m.start, "EPSILON", true)
		print("startgrouper", m.start)
		print("finalgrouper", final)
		m:addEdge(final,"EPSILON", m.start)
		local finalStates = machines[1]:final()
		for _,s in pairs(finalStates) do
			m:addEdge(s, "EPSILON", final)
		end
		buildState.groupN = buildState.groupN + 1
		local i= buildState.groupN
		function m.start.hit(self, state, c, path)
			print("start group")
			-- use as an array to avoid unnessesary table creation when we clone paths.
			path[i*3] = true
			path[i*3 +1] = self.steps
			path[i*3 +2] = nil
		end
		function final.hit(self, state, c, path)
			print("what", path)
			if path[i*3 +1] then
				path[i*3 +2] = self.steps
			end
		end
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
		print("capture:", name)
		m1.start.isFinal = true
		m1.start.captureName = name
		m1.start.hit = function(self,state,c)
			local match = {name=state.captureName, start=self.steps}
			print("start name:", match.name)
			self.partialMatches[state.captureName] = match
		end
		local m2 = machines[1]
		local m3 = NDFA.new()
		m3.start.isFinal =  true
		m3.start.captureName = name
		m3.start.hit = function(self,state,c)
			local name = state.captureName
			print("name", name)
			local match = self.partialMatches[name]
			match.finish = self.steps
			self.partialMatches[name] = nil

			print("name2:",name)
			print("match", match.start, match.finish)

			if not self.subMatches[name] then self.subMatches[name] = {} end
			table.insert(self.subMatches[name], match)
		end

		print("c2")
		m1:concat(m2):concat(m3)
		return m1
	end

	local children = {}
	for i,c in pairs( ast.children ) do
		local m = buildNDFA(c,state)
		children[i] = m
	end
	if not children then print("???") end
	local m = fragments[(ast.val or "EMPTY")](children, state)
	return m
end

--function parse(String) return Tree An AST of the Regex language to easy the creation of the final NFA
function parse(regex)
	local root = Tree.new("ROOT", {Tree.new()})
	local realroot = root
	print("parsing  " .. regex)
	local stack = Stack.new()
	-- parse the regex grammar
	local escaped = false
	for c in regex:gmatch(".") do
		if escaped then
			if grammar[ grammar.ESC .. c ] then
				root = grammar[grammar.ESC..c](root, stack)
			else
				root = grammar.literal(root, stack, c)
			end
		elseif c == grammar.ESC then
			escaped = true
		elseif grammar[c] then
			root = grammar[c](root, stack)
		else
			root = grammar.literal(root, stack, c)
		end
	end
	assert(not stack:pop(), "Regex parse stack is not empty. Possible missing close paren?")
	realroot:print()
	return realroot
end
return re
