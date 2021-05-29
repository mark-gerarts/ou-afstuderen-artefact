// Fail.hs
describe('view', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3008'),
            page.waitForResponse('**/initial-task')
        ]);
    });

    afterEach(async () => {
        await page.click('#btn-reset');
    });

    test('it should display the page', async () => {
        await expect(page).toHaveSelector("#halogen-app");
    });

    test('it should display the "Fail" message', async () => {
        await expect(page).toHaveText('Fail');
    });
});
