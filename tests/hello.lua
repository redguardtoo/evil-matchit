if configTable:FindFirstChild(configName) then
    configs[configName] = configTable:FindFirstChild(configName).Value
else
    configs[configName] = defaultValue
end

local thread = coroutine.create(function()
    while true do
        wait()
        if state then
            display.Text = state.Name
        end
    end
end)
