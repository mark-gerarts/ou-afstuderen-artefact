module.exports = {
    // Other browsers can be tested as well by adding 'webkit' and/or 'firefox'
    // here. Note that we explicitly only depend on playwright-chromium to save
    // some image space & build time.
    browsers: ['chromium'],
    serverOptions: {
        // @todo: In order to support multiple servers for multiple test
        // applications, we will have to add a configurable port to the stack
        // setup.
        command: 'stack runghc --cwd=../ app/Test/StringUpdate.hs',
        port: 3000,
        launchTimeout: 10000,
        waitOnScheme: {
            delay: 1000
        },
        debug: true
    }
}
