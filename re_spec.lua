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
	end)
end)
