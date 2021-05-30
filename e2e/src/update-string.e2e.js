// UpdateString.hs
describe('string update', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3000'),
            page.waitForResponse('**/initial-task')
        ]);
    });

    afterEach(async () => {
        await page.click('#btn-reset');
    });

    test('it should display the page', async () => {
        await expect(page).toHaveSelector("#halogen-app");
    });

    test('it should display a string input', async () => {
        await expect(page).toHaveSelector('input[type="text"]');
    });

    test('it should contain a default value', async () => {
        await expect(page).toEqualValue('input[type="text"]', 'Hello');
    });

    test('it should send the input value to the server', async () => {
        const [request, response] = await Promise.all([
            page.waitForRequest('**/interact'),
            page.waitForResponse('**/interact'),
            page.fill('input[type="text"]', 'This is the new value'),
            page.click('.btn-update-submit')
        ]);

        expect(request.postDataJSON().value).toBe('This is the new value');

        const responseJson = JSON.parse(await response.text());
        expect(responseJson.task.editor.value).toBe('This is the new value');
    });
});
