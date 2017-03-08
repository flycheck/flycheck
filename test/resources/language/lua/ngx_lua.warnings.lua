local name = ngx.var.arg_name or "Anonymous"

local function test(arg)
    local var2
    return var2
end

ngx.say("Hello, ", name, "!")
