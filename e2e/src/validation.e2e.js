// Validation.hs
describe('int validation', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3014'),
            page.waitForResponse('**/initial-task')
        ]);
    });

    afterEach(async () => {
        await page.click('#btn-reset');
    });

    test('it should indicate an error when invalid', async () => {
        await page.type('input[type="number"]', 'not a number');
        await expect(page).toHaveSelector('input[type="number"].is-danger');
    });

    test('it should not indicate an error when valid', async () => {
        await page.type('input[type="number"]', '123');
        await expect(page).not.toHaveSelector('input[type="number"].is-danger', { timeout: 1000 });
    });
});
