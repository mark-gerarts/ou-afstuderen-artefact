// Step.hs
describe('step', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3004'),
            page.waitForResponse('**/initial-task')
        ]);
    });

    afterEach(async () => {
        await page.click('#btn-reset');
    });

    const continueButtonSelector = 'xpath=//button[text()="Continue"]';

    test('it should display the page', async () => {
        await expect(page).toHaveSelector("#halogen-app");
    });

    test('it should display a continue button', async () => {
        await expect(page).toHaveSelector(continueButtonSelector);
    });

    test('it should increment the given value when clicking continue', async () => {
        // Explicitly set the value
        const newValue = Math.floor(Math.random() * 100);

        await Promise.all([
            page.waitForResponse('**/interact'),
            page.fill('input[type="number"]', newValue.toString())
        ]);
        await Promise.all([
            page.waitForResponse('**/interact'),
            page.click(continueButtonSelector)
        ]);

        await expect(page).toEqualText('.panel-block p', (newValue + 1).toString())
    });
});
