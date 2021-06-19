// EnterBoolean.hs
describe('boolean enter', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3010'),
            page.waitForResponse('**/initial-task')
        ]);
    });

    afterEach(async () => {
        await page.click('#btn-reset');
    });

    test('it should display the page', async () => {
        await expect(page).toHaveSelector("#halogen-app");
    });

    test('it should display a radio input', async () => {
        await expect(page).toHaveSelector('input[type ="radio"]');
        await expect(page).toHaveSelectorCount('input[type ="radio"]', 2);
    });

    test('it should check nothing by default', async () => {
        await expect(page).not.toHaveSelector('input[type="radio"]:checked', { timeout: 500 });
    });

    test('it should send a true value to the server when clicking "true"', async () => {
        const [request, response] = await Promise.all([
            page.waitForRequest('**/interact'),
            page.waitForResponse('**/interact'),
            page.click('.radio-true input')
        ]);

        expect(request.postDataJSON().value).toBe(true);

        // Check if the server has interpreted the true value.
        const responseJson = JSON.parse(await response.text());
        expect(responseJson.task.editor.value).toBe(true);
    });

    test('it should send a false value to the server when clicking "false"', async () => {
        const [request, response] = await Promise.all([
            page.waitForRequest('**/interact'),
            page.waitForResponse('**/interact'),
            page.click('.radio-false input')
        ]);

        expect(request.postDataJSON().value).toBe(false);

        // Check if the server has interpreted the false value.
        const responseJson = JSON.parse(await response.text());
        expect(responseJson.task.editor.value).toBe(false);
    });
});
