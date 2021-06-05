// UpdateBoolean.hs
describe('boolean update', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3001'),
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

    test('it should check True by default because the server value is True', async () => {
        await expect(page).toHaveSelector('input[type="radio"]:nth-child(1):checked');
    });

    test('it should send the checkbox value to the server', async () => {
        const [request, response] = await Promise.all([
            page.waitForRequest('**/interact'),
            page.waitForResponse('**/interact'),
            page.click('input[type="radio"]:first-child')
        ]);

        // Check if the value sent to the server is false, since we unchecked
        // the checkbox.
        expect(request.postDataJSON().value).toBe(false);

        // Check if the server has interpreted the false value.
        const responseJson = JSON.parse(await response.text());
        expect(responseJson.task.editor.value).toBe(false);
    });
});
