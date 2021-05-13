// View.hs
describe('view', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3005'),
            page.waitForResponse('**/initial-task')
        ]);
    });

    afterEach(async () => {
        await page.click('#btn-reset');
    });

    test('it should display the page', async () => {
        await expect(page).toHaveSelector("#halogen-app");
    });

    test('it should display the task value', async () => {
        await expect(page).toEqualText('.panel-block p', 'Some value');
    });

    test('it should not be editable', async () => {
        await expect(page).not.toHaveSelector('input');
    });
});
