describe('app', () => {
    beforeEach(async () => {
        await page.goto('http://localhost:3000')
    });

    afterEach(async () => {
        await page.click('#btn-reset');
    });

    test('it should display the page', async () => {
        await expect(page).toHaveSelector("#halogen-app");
    });
})
