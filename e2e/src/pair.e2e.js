// Pair.hs
describe('pair', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3003'),
            page.waitForResponse('**/initial-task')
        ]);
    });

    afterEach(async () => {
        await page.click('#btn-reset');
    });

    test('it should display the page', async () => {
        await expect(page).toHaveSelector("#halogen-app");
    });

    test('it should display two tasks', async () => {
        await expect(page).toHaveSelectorCount('input', 2);
    });
});
