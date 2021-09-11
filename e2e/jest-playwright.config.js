const serverConfigForTask = (filename, port) => {
    return {
        command: `PORT=${port} stack runghc --cwd=../ e2e/task/${filename} > /dev/null`,
        port: port,
        launchTimeout: 10000,
        waitOnScheme: {
            delay: 1000
        },
        debug: true
    };
};

module.exports = {
    // Other browsers can be tested as well by adding 'webkit' and/or 'firefox'
    // here. Note that we explicitly only depend on playwright-chromium to save
    // some image space & build time.
    browsers: ['chromium'],
    serverOptions: [
        serverConfigForTask('UpdateString.hs', 3000),
        serverConfigForTask('UpdateBoolean.hs', 3001),
        serverConfigForTask('UpdateInt.hs', 3002),
        serverConfigForTask('Pair.hs', 3003),
        serverConfigForTask('Step.hs', 3004),
        serverConfigForTask('View.hs', 3005),
        serverConfigForTask('Select.hs', 3006),
        serverConfigForTask('Lift.hs', 3007),
        serverConfigForTask('Fail.hs', 3008),
        serverConfigForTask('EnterString.hs', 3009),
        serverConfigForTask('EnterBoolean.hs', 3010),
        serverConfigForTask('EnterInt.hs', 3011),
        serverConfigForTask('TemperatureCalculator.hs', 3012),
        serverConfigForTask('ContinueOnEnter.hs', 3013),
        serverConfigForTask('Validation.hs', 3014),
        serverConfigForTask('Delay.hs', 3015),
        serverConfigForTask('ChatSession.hs', 3016),
        serverConfigForTask('Change.hs', 3017),
        serverConfigForTask('Choose.hs', 3018)
    ]
}
