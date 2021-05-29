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
        serverConfigForTask('StringUpdate.hs', 3000),
        serverConfigForTask('BooleanUpdate.hs', 3001),
        serverConfigForTask('IntUpdate.hs', 3002),
        serverConfigForTask('Pair.hs', 3003),
        serverConfigForTask('Step.hs', 3004),
        serverConfigForTask('View.hs', 3005),
        serverConfigForTask('Select.hs', 3006)
    ]
}
