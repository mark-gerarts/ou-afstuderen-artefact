// ChatSession.hs
describe('watch', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3015'),
            page.waitForResponse('**/initial-task')
        ]);
    });

    afterEach(async () => {
        await page.click('#btn-reset');
    });

    test('it should display the page', async () => {
        await expect(page).toHaveSelector("#halogen-app");
    });

    test('it should display a chat history', async () => {
        await expect(page).toHaveText('Watch Task');
    });

    test('it should append the chat when filling the inputs', async () => {
        const chat = '.panel-block p'
        const input = 'xpath=(//input)[1]';
        const sendButton = 'xpath=//button[text()="Send"]';

        await Promise.all([
            page.waitForRequest('**/interact'),
            page.waitForResponse('**/interact'),
            page.fill(input, 'Hello!')
        ]);

        await Promise.all([
            page.waitForRequest('**/interact'),
            page.waitForResponse('**/interact'),
            page.click(sendButton)
        ]);

        expect(page).toHaveText(chat, "Tim: 'Hello!'");

        await Promise.all([
            page.waitForRequest('**/interact'),
            page.waitForResponse('**/interact'),
            // We can recycle the selector because the first input is
            // temporarily replaced by a Done at this stage.
            page.fill(input, 'Hi there!')
        ]);

        [request, resp] = await Promise.all([
            page.waitForRequest('**/interact'),
            page.waitForResponse('**/interact'),
            page.click(sendButton)
        ]);

        expect(page).toHaveText(chat, "Tim: 'Hello!'\nNico: 'Hi there!'");
    });
});
