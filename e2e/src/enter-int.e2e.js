// EnterInt.hs
describe('int enter', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3011'),
            page.waitForResponse('**/initial-task')
        ]);
    });

    afterEach(async () => {
        await page.click('#btn-reset');
    });

    test('it should display the page', async () => {
        await expect(page).toHaveSelector("#halogen-app");
    });

    test('it should display a number input', async () => {
        await expect(page).toHaveSelector('input[type="number"]');
    });

    test('it should not contain a default value', async () => {
        await expect(page).toEqualValue('input[type="number"]', '');
    });

    test('it should send the input value to the server', async () => {
        const [request, response] = await Promise.all([
            page.waitForRequest('**/interact'),
            page.waitForResponse('**/interact'),
            page.fill('input[type="number"]', '24')
        ]);

        expect(request.postDataJSON().value).toBe(24);

        const responseJson = JSON.parse(await response.text());
        expect(responseJson.task.editor.value).toBe(24);
    });
});
