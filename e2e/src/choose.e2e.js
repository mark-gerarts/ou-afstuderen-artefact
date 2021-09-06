// Choose.hs
describe('pair', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3018'),
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

    test('it should display the page', async () => {
        await expect(page).toHaveSelector("#halogen-app");
    });

    test('it should display two tasks', async () => {
        await expect(page).toHaveSelectorCount('article.panel', 2);
    });

    test('it should display a divider', async () => {
        await expect(page).toHaveSelector('.divider');
    });

    test('it should continue with only one task when entering a value', async () => {
        await clickButton('Belgium');
        await expect(page).toHaveSelectorCount('article.panel', 1);
    });
});
