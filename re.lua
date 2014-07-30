
-- object Tree A tree! what more description do you want??
-- 	var Tree.val = String
-- 	var Tree.children = {Tree} | {}

local re = {}


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
local State = {}
State.__index = State
-- function State.new() return State A new state
function State.new()
	return setmetatable({}, State)
end




local NDFA = {}
NDFA.__index = NDFA
-- function NDFA.new() return NDFA A new machine with only a single state and no final states.
function NDFA.new()
	local m = {}
	m.states = {State.new()}
	m.edges = {}
	m = setmetatable(m, NDFA)
	return m
end

--function NDFA.final(self) return {State} The final states of the machine
function NDFA:final()
	local states = {}
	for i,s in pairs(self.states) do
		if s.isFinal then
			states[i] = s
		end
	end
	return states
end

--function NDFA:addEdge(Int source, Input input, Int dest) create an edge leading from source to dest given the input.
function NDFA:addEdge(source, input, dest)
	if not self.edges[source] then self.edges[source] = {} end
	if not self.edges[source][input] then self.edges[source][input] = {} end
	table.insert(self.edges[source][input], dest )
end

--function NDFA:insert(NDFA m) return NDFA A machine with the states and edges of m added to self with no link between them.
function NDFA:insert(m)
	local nStates = #self.states
	-- add all states in m to self
	for i,s in pairs(m.states) do
		local i2 = i + nStates
		self.states[i2] = s
		if m.edges[i] then
			for input, states in pairs(m.edges[i]) do
				for k,dest in pairs(states) do
					self:addEdge(i2, input, dest+nStates)
				end
			end
		end
	end
	-- check if for onDeath method
	--[[
	if m.onDeath then
		-- add some stuff to detect when this partial machine dies
		self.partials[n] = {}
		self.partials[n].states = {...}
		self.partials[n].onDeath = m.onDeath
		m.states[1].hit = function(self,state,c)
			m.states[1].hit(self,state,c)
			self.partials[n].isAlive = true
		end

	end
	--]]
	return self
end

--function NDFA:concat(NDFA m1, NDFA m2) return NDFA A new NDFA resulting from concatenating m2 to m1
function NDFA.concat(m1, m2)
	local m1States = #m1.states
	-- link all end states of m1 to the start of m2
	for i,s in pairs(m1:final()) do
		s.isFinal = false
		m1:addEdge( i, "EPSILON", m1States+1 )
	end
	m1:insert(m2)


	return m1
end

-- function NDFA:addState(Int source, Input input, Bool final) Add a new State to this machine, adding edges from the given state ID to it.
function NDFA:addState(source, input, final)
	local state = State.new() 
	if final then 
		state.isFinal = true
	end
	table.insert(self.states, state)
	if source and input then
		self:addEdge(source, input, #self.states)
	end
	return #self.states
end

printMatches = function(name,subMatches)
	for name, matches in pairs(subMatches) do
		for _,match in ipairs(matches) do
			print(match)
			print(name, match.start, match.finish)
		end
	end
end

function NDFA:execute(input)
	print("exec:", input)
	self:init(input)
	local match = true
	for c in input:gmatch(".") do
		print(c)
		if not self:step(c) then
			match = false
			break
		end
		self.steps = self.steps + 1
		for i,s in ipairs(self.curState) do
			--print("state:",i, s[1], s[2])
			if s[2].hit then s[2].hit(self,s[2],c) end
		end
	end
	if match then
		print(self.regex,"Matched:", input)
		local matches = self.subMatches
		printMatches("root",matches)
	else
		print(self.regex,"Failed:", input)
	end
	return match
end

function NDFA:init(input)
	print("Init")
	self.curState = eClosure(self, 1)
	self.subMatches = {} -- completed matches
	self.partialMatches = {} -- partial, incomplete, matches
	self.steps = 0
	self.input = input
	for i,s in ipairs(self.curState) do
		print("istate:",i, s[1], s[2])
		if s[2].hit then s[2].hit(self,s[2],"") end
	end
end

function NDFA:step(input)
	local nextState = {}
	local isDead = false
	for _,s2 in pairs(self.curState) do
		local i = s2[1]
		local s = s2[2]
		if self.edges[i] and self.edges[i][input] then
			isDead = true
			for _,n in pairs(self.edges[i][input]) do
				eClosure(self, n, nextState)
			end
		end
	end
	self.curState = nextState
	return isDead
end

-- function NDFA:Print() Print a representation of an NDFA
function NDFA:print()
	for i,s in pairs(self.states) do
		print(i,s)
	end

	for source,e in pairs(self.edges) do
		print(source,":")
		for input,v in pairs(e) do
			for i,dest in pairs(v) do
				print(source, input, "->",dest)
			end
		end
	end
end

--function eClosure(NDFA, Int) return {Int:State} List of all states accessible via epsilon transitions from the given state
function eClosure(m, i, prevStates)

	-- check for cached value
	if not m.cache then m.cache = {} end
	if not m.cache.eClosure then m.cache.eClosure = {} end
	local cached = m.cache.eClosure[i]
	local states

	if cached then
		states = cached
	else
		states = {}
		states.lookup = {}
		if prevStates and prevStates.lookup then
			for k,_ in pairs(prevStates.lookup) do
				states.lookup[k] = true
			end
		end
		
		-- add the initial state
		if not states.lookup[i] then
			states.lookup[i] = true
			table.insert(states, {i, m.states[i]})
		end

		-- find all states with Epsilon transitions from here
		local nextStates
		if m.edges[i] then
			nextStates = m.edges[i]["EPSILON"]
		end
		if nextStates then
			for _,s in pairs(nextStates) do
				if not states.lookup[s] then
					table.insert(states, {s,m.states[s]})
					states.lookup[s] = true
					local moreStates = eClosure(m,s, states)
				end
			end
		end
		-- cache it!
		m.cache.eClosure[i] = states
	end
	if not prevStates then prevStates = {} end
	if not prevStates.lookup then prevStates.lookup = {} end
	for _,v in ipairs(states) do
		table.insert(prevStates, v)
	end
	for k,_ in pairs(states.lookup) do
		prevStates.lookup[k] = true
	end
	return prevStates
end

-- function compile(String) return Regex
-- compiles the regex string into an NFA.
function re.compile(regex)
	local ast = parse(regex)
	local names = {"One", "Two", "Three", "Four?"}
	local nfa = buildNDFA(ast, {captureNames=names})
	nfa.regex = regex
	nfa:print()
	return nfa
end

-- function buildNFA(Tree, NDFA) return NDFA builds an NFA from the given regex AST
function buildNDFA(ast, state) 
	local mt = {}
	mt.__index = function(table, key)
		if not key then print("nil literal?") else
			print(key)
		end
		return function(machines)
			local m = NDFA.new()
			m:addState(1,key,true)
			return m
		end
	end
	local fragments = setmetatable({}, mt)
	function fragments.CONCAT(machines)
		local m0 = nil
		for _,m1 in pairs(machines) do
			if m0 then
				m0:concat(m1) 
			else
				m0 = m1
			end
		end
		return m0
	end
	function fragments.PLUS(machines)
		local m = machines[1]
		local finalStates = m:final()
		for i,_ in pairs(finalStates) do
			m:addEdge(i, "EPSILON", 1)
		end
		return m
	end
	function fragments.STAR(machines)
		local m = machines[1]
		local si=m:addState(nil,nil, true)
		m:addEdge(1,"EPSILON", si)
		local finalStates = m:final()
		for i,_ in pairs(finalStates) do
			m:addEdge(i, "EPSILON", 1)
		end
		return m
	end
	fragments["|"] = function(machines)
		local m = NDFA.new()
		local m1 = machines[1]
		local m2 = machines[2]
		m:addEdge(1, "EPSILON", #m.states + 1)
		m:insert(m1)
		m:addEdge(1, "EPSILON", #m.states + 1)
		m:insert(m2)
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
		m1.states[1].isFinal = true
		m1.states[1].captureName = name
		m1.states[1].hit = function(self,state,c)
			local match = {name=state.captureName, start=self.steps}
			print("start name:", match.name)
			self.partialMatches[state.captureName] = match
		end
		local m2 = machines[1]
		local m3 = NDFA.new()
		m3.states[1].isFinal =  true
		m3.states[1].captureName = name
		m3.states[1].hit = function(self,state,c)
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

		m1:concat(m2):concat(m3)
		m1.onDeath = function(self,c)
			print("Capture ", buildState.captureNames, "death")
		end
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
