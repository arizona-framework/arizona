import { test, expect } from '@playwright/test';
import { waitForCondition, collectWebSocketMessages } from '../utils.js';

test.describe('Arizona Hello World App', () => {
  test('should load hello world page and display initial button', async ({ page }) => {
    await page.goto('/hello-world');

    // Check that the page loads with the button
    const button = page.locator('button:has-text("Say Hello!")');
    await expect(button).toBeVisible();

    // Verify the button has the correct onclick attribute
    await expect(button).toHaveAttribute('onclick', "arizona.pushEvent('hello_world')");

    // Verify only one view container exists (no duplication)
    const viewContainers = page.locator('#view');
    await expect(viewContainers).toHaveCount(1);
  });

  test('should establish WebSocket connection and receive initial render', async ({ page }) => {
    const initialMessages = collectWebSocketMessages(page, 'initial_render');

    await page.goto('/hello-world');

    // Wait for initial render message to arrive
    await waitForCondition(() => {
      return initialMessages.length > 0;
    });

    const initialMessage = initialMessages[0];
    expect(initialMessage).toBeTruthy();
    expect(initialMessage.type).toBe('initial_render');
    expect(initialMessage.stateful_id).toBe('view');
    expect(initialMessage.structure).toBeDefined();
    expect(typeof initialMessage.structure).toBe('object');
  });

  test('should update view via WebSocket after button click and verify DOM update', async ({
    page,
  }) => {
    const diffMessages = collectWebSocketMessages(page, 'diff');

    await page.goto('/hello-world');

    // Wait for initial load
    const button = page.locator('button:has-text("Say Hello!")');
    await expect(button).toBeVisible();

    // Click the button
    await button.click();

    // Wait for WebSocket diff message
    await waitForCondition(() => {
      return diffMessages.length > 0;
    });

    const diffMessage = diffMessages[0];
    expect(diffMessage.type).toBe('diff');
    expect(diffMessage.stateful_id).toBe('view');
    expect(diffMessage.changes).toBeDefined();
    expect(Array.isArray(diffMessage.changes)).toBe(true);

    // Verify the diff contains element changes
    expect(diffMessage.changes.length).toBeGreaterThan(0);

    const [elementIndex, newValue] = diffMessage.changes[0];
    expect(typeof elementIndex).toBe('number');

    // The new value should be a string "Hello, World!"
    expect(typeof newValue).toBe('string');

    // Verify UI updated - button should be replaced with text
    await expect(button).not.toBeVisible();

    // Check that the new content is visible
    const viewDiv = page.locator('#view');
    await expect(viewDiv).toContainText('Hello, World!');
  });

  test('should receive correct diff structure with string content', async ({ page }) => {
    const diffMessages = collectWebSocketMessages(page, 'diff');

    await page.goto('/hello-world');

    // Click button to trigger update
    const button = page.locator('button:has-text("Say Hello!")');
    await button.click();

    // Wait for diff message
    await waitForCondition(() => {
      return diffMessages.length > 0;
    });

    const diffMessage = diffMessages[0];

    // The changes should contain the element index and the new string value
    const [elementIndex, newValue] = diffMessage.changes[0];

    // Verify structure matches expected format: [[index, "Hello, World!"]]
    expect(typeof elementIndex).toBe('number');
    expect(typeof newValue).toBe('string');

    // Verify content
    expect(newValue).toContain('Hello');
    expect(newValue).toContain('World');
  });

  test('should handle single click and verify no errors', async ({ page }) => {
    await page.goto('/hello-world');

    const button = page.locator('button:has-text("Say Hello!")');
    await expect(button).toBeVisible();

    // Click once (button will be replaced with text after first click)
    await button.click();

    // Wait a bit for all processing
    await page.waitForTimeout(500);

    // Should show the greeting (no errors)
    const viewDiv = page.locator('#view');
    await expect(viewDiv).toContainText('Hello, World!');
  });
});
