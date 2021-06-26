// Delay.hs
describe('Delay', () => {
    beforeEach(async () => {
        await Promise.all([
            page.goto('http://localhost:3015'),
            page.waitForResponse('**/initial-task')
        ]);
    });

    afterEach(async () => {
        await page.click('#btn-reset');
    });

    test('it should be able to fill in a number with more than one digit', async () => {
        // If this test fails it means the delay was not respected, and the
        // interact request was sent on the first input change.
        await Promise.all([
            page.waitForResponse('**/interact'),
            page.type('input[type="number"]', '1234')
        ]);

        await expect(page).toEqualText('.panel-block p', 'x >= 10')
    });
});
