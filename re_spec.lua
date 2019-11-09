local re = require("re")
local say = require("say")

-- Set up a regex matching assert:
local function regex_matches(state, arguments)
	local regex = arguments[1]
	local str = arguments[2]
	return re.compile(regex):execute(str) ~= nil
end

say:set_namespace("en")
say:set("assertion.regex_matches.positive", "Expected regex %s to match: %s")
say:set("assertion.regex_matches.negative",
	"Expected regex %s to not match: %s")
assert:register("assertion", "regex_matches", regex_matches,
	"assertion.regex_matches.positive", "assertion.regex_matches.negative")


describe("re execute", function()
	it("should match simple regexes", function()
		assert.regex_matches("abc", "abc")
	end)

	it("shouldn't match a simple non-match", function()
		assert.not_regex_matches("abc", "def")
	end)

	describe("dot chars", function()
		it("should match any char", function()
			assert.regex_matches("...", "abc")
		end)
	end)

	it("should allow escaping metacharacters", function()
		-- ESC set by re to `/` by default
		assert.regex_matches(ESC .. "+abc", "+abc")
		assert.regex_matches("abc" .. ESC .. "+abc", "abc+abc")
		assert.regex_matches(ESC .. ".", ".")
		assert.not_regex_matches(ESC .. ".", "!")

		-- Multiple escapes
		assert.regex_matches(ESC .. "." .. ESC .. "..", "..a")
		assert.not_regex_matches(ESC .. "." .. ESC .. "..", ".ab")
	end)

	describe("character classes", function()
		it("should match basic classes", function()
			assert.regex_matches("ab[cdef]+gh", "abcgh")
			assert.regex_matches("ab[cdef]+gh", "abcdegh")
			assert.not_regex_matches("ab[cdef]+gh", "abzgh")
		end)

		it("should allow ']' to be escaped", function()
			assert.regex_matches("a[b" .. ESC .. "]]+gh." .. ESC .. ".", "ab]gh!.")
		end)

		it("should allow / to be escaped", function()
		assert.regex_matches("a[" .. ESC .. ESC .. "]+gh." .. ESC .. ".",
			"a" .. ESC .. ESC .. "gh!.")
		end)

		it("shouldn't affect escaping outside the class", function()
			assert.regex_matches("[a" .. ESC .. "]]+b" .. ESC .. ".", "a]ab.")
			assert.not_regex_matches("[a" .. ESC .. "]]+b" .. ESC .. ".", "a]ab!")
		end)

		it("should not include / in the class", function()
			assert.not_regex_matches("a[b" .. ESC .. "]]+c", "a" .. ESC .. "c")
		end)
	end)
end)
