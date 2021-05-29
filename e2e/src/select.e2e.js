// Select.hs
describe('view', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3006'),
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

    test('it should display all select options', async () => {
        await expect(page).toHaveSelector(buttonSelector('A'));
        await expect(page).toHaveSelector(buttonSelector('B'));
        await expect(page).toHaveSelector(buttonSelector('C'));
    });

    test('it should display the select options in their appropriate tasks', async () => {
        // The left task should contain A and B, the right one C.

        const taskAndButtonSelector = (taskNumber, label) => `xpath=//div[@class="columns"]/div[@class="column" and position()=${taskNumber}]//button[text()="${label}"]`;

        // Test if A and B are in the left task
        await expect(page).toHaveSelector(taskAndButtonSelector(1, 'A'));
        await expect(page).toHaveSelector(taskAndButtonSelector(1, 'B'));

        // Test if C is in the right task
        await expect(page).toHaveSelector(taskAndButtonSelector(2, 'C'));

        // Test if the buttons don't appear anywhere else
        await expect(page).toHaveSelectorCount(buttonSelector('A'), 1);
        await expect(page).toHaveSelectorCount(buttonSelector('B'), 1);
        await expect(page).toHaveSelectorCount(buttonSelector('C'), 1);
    });

    test('it should display only the continue button outside of the tasks', async () => {
        // The continue button appears only after pressing C.
        await page.click(buttonSelector('C'));

        // Test the continue button is rendered only once and in the bottom
        // right.
        await expect(page).toHaveSelectorCount(buttonSelector('Continue'), 1);
        await expect(page).toHaveSelector('xpath=//div[@class="buttons is-right"]/button[text()="Continue"]');
    });

    test('it should pass the correct input when clicking an option', async () => {
        const reset = async () => await page.click('#btn-reset');

        await clickButton('A');
        await expect(page).toHaveText("Clicked A");
        await reset();

        await clickButton('B');
        await expect(page).toHaveText("Clicked B");
        await reset();

        await clickButton('C');
        await expect(page).toHaveText("Clicked C");
        await reset();
    });

    test('it should handle clicking the continue button', async () => {
        await clickButton('C');
        await clickButton('Continue');
        await expect(page).toHaveText("Continued");
    });
});
