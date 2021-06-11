// TemperatureCalculator.hs
describe('view', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3012'),
            page.waitForResponse('**/initial-task')
        ]);
    });

    afterEach(async () => {
        await page.click('#btn-reset');
    });

    const buttonSelector = (label) => `xpath=//button[text()="${label}"]`;

    const clickButton = async (label) => {
        await Promise.all([
            page.click(buttonSelector(label)),
            page.waitForResponse('**/interact')
        ]);
    }

    test('it should calculate from celcius to fahrenheit', async () => {
        await clickButton("Celcius to Fahrenheit");

        await Promise.all([
            page.fill('input[type="number"]', '24'),
            page.waitForResponse('**/interact')
        ]);

        await clickButton("Continue");

        expect(page).toHaveText("75.2");
    });

    test('it should calculate from fahrenheit to celcius', async () => {
        await clickButton("Fahrenheit to Celcius");

        await Promise.all([
            page.fill('input[type="number"]', '32'),
            page.waitForResponse('**/interact')
        ]);

        await clickButton("Continue");

        expect(page).toHaveText("0.0");
    });
});
