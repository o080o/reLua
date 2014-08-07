data = require("reData")
local Stack = data.Stack
local Tree = data.Tree
local CharacterClass = data.CharacterClass

local dotclass = CharacterClass("[^]")

function addNode(root, n)
	root.children[1] = root.children[1]:concat(n)
	return root
end
function lastNode(root)
	if root.children[1].val == "CONCAT" then
		return table.remove(root.children[1].children)
	else
		local v = table.remove(root.children)
		table.insert(root.children, Tree.new())
		return v
	end
end


function newNode(val)
	return function(root, stack)
		return addNode(root, Tree.new(val))
	end
end
function newRepetition(val)
	return function(root, stack)
		local n = lastNode(root)
		return addNode(root, Tree.new(val, {n}))
	end
end

local P = {}

-- functions in grammar table take an AST,a stack, and a string (the unparsed input), and return an AST, and optionally an integer number of characterss to skip for parsing
-- the stack is used for open/close parens, alternations, etc, by pushing the current AST onto the stack, and concatenating to it later by poping it off the stack.
grammar = setmetatable({}, {__index=function(t,k) return function(root, stack) return grammar.literal(root, stack, k) end end})
ESC = "/" --define the escape character

grammar.literal=function(root, stack, c)
	root.children[1] = root.children[1]:concat(Tree.new(c))
	return root
end
grammar.multi = {}
grammar.multi["(?R)"]=newNode("RECURSE")
grammar[ESC.."R"]=grammar.multi["(?R)"]

grammar["["]=function(root,stack,str)
	local i=0
	for c in str:gmatch(".") do
		i = i+1
		if c == ESC and not escaped then 
			escaped = true
		elseif c == "]" and not escaped then
			break
		end
	end
	local class = CharacterClass( "["..str:sub(1,i))
	print("New character class:", class)
	addNode(root, Tree.new(class))
	return root,i
end
grammar["."]=newNode(dotclass)
grammar["?"]=newRepetition("QUESTION")
grammar["+"]=newRepetition("PLUS")
grammar["-"]=newRepetition("MINUS")
grammar["*"]= newRepetition("STAR")
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


--function parse(String) return Tree An AST of the Regex language to ease the creation of the final NFA
function P.parse(regex)
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
			if grammar[ ESC .. c ] then
				root = grammar[ESC..c](root, stack)
			else
				root = grammar.literal(root, stack, c)
			end
		elseif c == ESC then
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

return P
