// Lift.hs
describe('view', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3007'),
            page.waitForResponse('**/initial-task')
        ]);
    });

    afterEach(async () => {
        await page.click('#btn-reset');
    });

    test('it should display the page', async () => {
        await expect(page).toHaveSelector("#halogen-app");
    });

    test('it should display the "Done" message', async () => {
        await expect(page).toHaveText('Done');
    });
});
