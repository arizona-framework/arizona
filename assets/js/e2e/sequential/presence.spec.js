import { test, expect } from '@playwright/test';
import { waitForCondition, waitForText } from '../test-utils.js';

test.describe.configure({ mode: 'serial' });

test.describe('Arizona Presence System', () => {
  test('should load presence page and display initial state', async ({ page }) => {
    await page.goto('/presence');

    // Check that the page loads with initial presence view
    await expect(page.getByTestId('presence-view')).toBeVisible();
    await expect(page.getByText('User Presence Demo')).toBeVisible();
    await expect(page.getByText("This example shows Arizona's hybrid approach")).toBeVisible();

    // Check user info section exists (may show loading or generated user info)
    await expect(page.locator('.user-id')).toBeVisible();
    await expect(page.locator('.user-name')).toBeVisible();

    // Check buttons are present
    await expect(page.getByTestId('join-btn')).toBeVisible();
    await expect(page.getByTestId('leave-btn')).toBeVisible();
    await expect(page.getByTestId('leave-btn')).toBeDisabled();

    // Check online users section exists
    await expect(page.getByText(/Online Users \(\d+\)/)).toBeVisible();
    await expect(page.locator('#users-list')).toBeAttached();
  });

  test('should handle complete presence workflow with render_map', async ({ page }) => {
    test.setTimeout(30000); // Increase timeout for slower CI environments

    await page.goto('/presence');

    // Wait for WebSocket connection before proceeding
    await page.waitForLoadState('networkidle');

    // Wait for client-side user info generation and verify format
    // Use longer timeout and more frequent checks for flaky CI environments
    await waitForCondition(
      async () => {
        const userIdText = (await page.locator('.user-id').textContent()).trim();
        const userNameText = (await page.locator('.user-name').textContent()).trim();
        return (
          userIdText !== 'Loading...' &&
          userNameText !== 'Loading...' &&
          userIdText.startsWith('user_') &&
          userNameText.startsWith('USER')
        );
      },
      300,
      50
    );

    const userId = (await page.locator('.user-id').textContent()).trim();
    const userName = (await page.locator('.user-name').textContent()).trim();
    expect(userId).toMatch(/^user_\d+_\d+$/);
    expect(userName).toMatch(/^USER\d+_\d+$/);

    const initialCountText = await page.getByText(/Online Users \(\d+\)/).textContent();
    const initialCount = parseInt(initialCountText.match(/\((\d+)\)/)[1]);

    // Join presence and verify all updates
    // Set up promise to wait for network request before clicking
    const joinRequestPromise = page.waitForResponse(
      (response) => response.url().includes('/api/presence/join') && response.status() === 200,
      { timeout: 10000 }
    );

    await page.getByTestId('join-btn').click();

    // Wait for the join API request to complete
    await joinRequestPromise;

    // Wait for status to become visible, then check text
    // This prevents race condition where status auto-hides after 3s
    await page.locator('#status').waitFor({ state: 'visible', timeout: 5000 });
    await waitForText(page.locator('#status'), 'Successfully joined presence!', 5000);

    // Check class immediately while status is still visible
    await expect(page.locator('#status')).toHaveClass(/success/, { timeout: 500 });
    await expect(page.getByTestId('join-btn')).toBeDisabled({ timeout: 2000 });
    await expect(page.getByTestId('leave-btn')).not.toBeDisabled({ timeout: 2000 });

    // Verify WebSocket count update
    await expect(page.getByText(new RegExp(`Online Users \\(${initialCount + 1}\\)`))).toBeVisible({
      timeout: 2000,
    });

    // Verify user appears in list and render_map structure
    await waitForCondition(
      async () => {
        const userItems = page.locator('.user-item');
        const count = await userItems.count();
        if (count === 0) return false;
        for (let i = 0; i < count; i++) {
          const itemText = await userItems.nth(i).textContent();
          if (itemText.includes(userId) && itemText.includes(userName)) {
            return true;
          }
        }
        return false;
      },
      200,
      100
    );

    // Test render_map structure
    const firstUserItem = page.locator('.user-item').first();
    const itemText = await firstUserItem.textContent();
    const trimmedText = itemText.replace(/\s+/g, ' ').trim();
    expect(trimmedText).toMatch(/^USER\d+_\d+ \(ID: user_\d+_\d+\)$/);
    await expect(firstUserItem).toHaveClass('user-item');

    const strongElement = firstUserItem.locator('strong');
    await expect(strongElement).toBeVisible();
    const strongText = await strongElement.textContent();
    expect(strongText).toMatch(/^USER\d+_\d+$/);

    // Leave presence and verify cleanup
    // Set up promise to wait for network request before clicking
    const leaveRequestPromise = page.waitForResponse(
      (response) => response.url().includes('/api/presence/leave') && response.status() === 200,
      { timeout: 10000 }
    );

    await page.getByTestId('leave-btn').click();

    // Wait for the leave API request to complete
    await leaveRequestPromise;

    // Wait for status to become visible, then check text
    await page.locator('#status').waitFor({ state: 'visible', timeout: 5000 });
    await waitForText(page.locator('#status'), 'Successfully left presence!', 5000);

    await expect(page.getByTestId('join-btn')).not.toBeDisabled({ timeout: 2000 });
    await expect(page.getByTestId('leave-btn')).toBeDisabled({ timeout: 2000 });

    // Verify count decreases
    await waitForCondition(
      async () => {
        const countText = await page.getByText(/Online Users \(\d+\)/).textContent();
        const newCount = parseInt(countText.match(/\((\d+)\)/)[1]);
        return newCount === initialCount;
      },
      50,
      200
    );
  });

  test('should handle network errors gracefully', async ({ page }) => {
    await page.goto('/presence');

    // Wait for user info
    await waitForCondition(async () => {
      const userIdText = await page.locator('.user-id').textContent();
      return userIdText !== 'Loading...';
    });

    // Block network requests to simulate failure
    await page.route('/api/presence/join', (route) => {
      route.abort('failed');
    });

    // Try to join - should show error message
    await page.getByTestId('join-btn').click();

    // Wait for error status message
    await waitForCondition(async () => {
      const statusEl = page.locator('#status');
      const isVisible = await statusEl.isVisible();
      if (!isVisible) return false;
      const text = await statusEl.textContent();
      return text.includes('Network error');
    });

    // Check error styling
    await expect(page.locator('#status')).toHaveClass(/error/);

    // Button states should remain unchanged (join enabled, leave disabled)
    await expect(page.getByTestId('join-btn')).not.toBeDisabled();
    await expect(page.getByTestId('leave-btn')).toBeDisabled();
  });
});
