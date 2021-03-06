// ContinueOnEnter.hs
describe('continue on enter', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3013'),
            page.waitForResponse('**/initial-task')
        ]);
    });

    afterEach(async () => {
        await page.click('#btn-reset');
    });

    test('it should continue when pressing enter', async () => {
        await Promise.all([
            page.waitForResponse('**/interact'),
            page.fill('input[type="number"]', '11')
        ]);

        await Promise.all([
            page.waitForResponse('**/interact'),
            page.keyboard.press("Enter"),
        ]);

        await expect(page).toEqualText('.panel-block p', '12')
    });
});
