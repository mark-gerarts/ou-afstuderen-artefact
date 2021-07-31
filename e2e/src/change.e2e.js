// Change.hs
describe('change', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3017'),
            page.waitForResponse('**/initial-task')
        ]);
    });

    afterEach(async () => {
        await page.click('#btn-reset');
    });

    test('it should display the page', async () => {
        await expect(page).toHaveSelector("#halogen-app");
    });

    test('it should display two change tasks', async () => {
        await expect(page).toHaveSelectorCount('xpath=//p[contains(text(), "Change Task")]', 2);
    });

    test('it should display 24 when filling in a number greater than 5', async () => {
        await Promise.all([
            page.waitForRequest('**/interact'),
            page.waitForResponse('**/interact'),
            page.fill('input', '12')
        ]);

        expect(page).toHaveText('24');
    });
});
