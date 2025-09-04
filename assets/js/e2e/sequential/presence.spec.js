import { test, expect } from '@playwright/test';
import { waitForCondition, waitForText, waitForElementCount } from '../test-utils.js';

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

  test('should generate user info client-side and update display', async ({ page }) => {
    await page.goto('/presence');

    // Wait for client-side user info generation
    await waitForCondition(async () => {
      const userIdText = (await page.locator('.user-id').textContent()).trim();
      const userNameText = (await page.locator('.user-name').textContent()).trim();
      return (
        userIdText !== 'Loading...' &&
        userNameText !== 'Loading...' &&
        userIdText.startsWith('user_') &&
        userNameText.startsWith('USER')
      );
    });

    // Verify user info format
    const userId = (await page.locator('.user-id').textContent()).trim();
    const userName = (await page.locator('.user-name').textContent()).trim();

    expect(userId).toMatch(/^user_\d+_\d+$/);
    expect(userName).toMatch(/^USER\d+_\d+$/);
  });

  test('should join presence via REST API and show success message', async ({ page }) => {
    await page.goto('/presence');

    // Wait for user info to be generated
    await waitForCondition(async () => {
      const userIdText = await page.locator('.user-id').textContent();
      return userIdText !== 'Loading...';
    });

    // Click join button
    await page.getByTestId('join-btn').click();

    // Wait for success status message
    await waitForCondition(async () => {
      const statusEl = page.locator('#status');
      const isVisible = await statusEl.isVisible();
      if (!isVisible) return false;
      const text = await statusEl.textContent();
      return text === 'Successfully joined presence!';
    });

    // Check status message styling
    await expect(page.locator('#status')).toHaveClass(/success/);

    // Check button states changed
    await expect(page.getByTestId('join-btn')).toBeDisabled();
    await expect(page.getByTestId('leave-btn')).not.toBeDisabled();
  });

  test('should receive real-time presence updates via WebSocket', async ({ page }) => {
    await page.goto('/presence');

    // Wait for initial load
    await waitForCondition(async () => {
      const userIdText = await page.locator('.user-id').textContent();
      return userIdText !== 'Loading...';
    });

    // Get initial online users count
    const initialCountText = await page.getByText(/Online Users \(\d+\)/).textContent();
    const initialCount = parseInt(initialCountText.match(/\((\d+)\)/)[1]);

    // Join presence
    await page.getByTestId('join-btn').click();

    // Wait for success message
    await waitForText(page.locator('#status'), 'Successfully joined presence!');

    // Wait for WebSocket presence event to update the user list
    await waitForCondition(
      async () => {
        const countText = await page.getByText(/Online Users \(\d+\)/).textContent();
        const newCount = parseInt(countText.match(/\((\d+)\)/)[1]);
        return newCount > initialCount;
      },
      50,
      100
    );

    // Verify user appears in the online users list
    const userId = (await page.locator('.user-id').textContent()).trim();
    const userName = (await page.locator('.user-name').textContent()).trim();

    await waitForCondition(async () => {
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
    });
  });

  test('should leave presence via REST API and update real-time', async ({ page }) => {
    await page.goto('/presence');

    // Wait for user info and join presence first
    await waitForCondition(async () => {
      const userIdText = await page.locator('.user-id').textContent();
      return userIdText !== 'Loading...';
    });

    await page.getByTestId('join-btn').click();
    await waitForText(page.locator('#status'), 'Successfully joined presence!');

    // Wait a moment for the status to disappear
    await page.waitForTimeout(3500);

    // Get current online users count
    const beforeLeaveCountText = await page.getByText(/Online Users \(\d+\)/).textContent();
    const beforeLeaveCount = parseInt(beforeLeaveCountText.match(/\((\d+)\)/)[1]);

    // Leave presence
    await page.getByTestId('leave-btn').click();

    // Wait for success message
    await waitForText(page.locator('#status'), 'Successfully left presence!');

    // Check button states changed back
    await expect(page.getByTestId('join-btn')).not.toBeDisabled();
    await expect(page.getByTestId('leave-btn')).toBeDisabled();

    // Wait for WebSocket presence event to update the user list
    await waitForCondition(
      async () => {
        const countText = await page.getByText(/Online Users \(\d+\)/).textContent();
        const newCount = parseInt(countText.match(/\((\d+)\)/)[1]);
        return newCount < beforeLeaveCount;
      },
      50,
      100
    );

    // Verify user no longer appears in the online users list
    const userId = await page.locator('.user-id').textContent();

    await waitForCondition(async () => {
      const userItems = page.locator('.user-item');
      const count = await userItems.count();

      for (let i = 0; i < count; i++) {
        const itemText = await userItems.nth(i).textContent();
        if (itemText.includes(userId)) {
          return false; // User still found, wait more
        }
      }
      return true; // User not found, left successfully
    });
  });

  test('should handle multiple users joining and leaving', async ({ browser }) => {
    // Create two browser contexts to simulate multiple users
    const context1 = await browser.newContext();
    const context2 = await browser.newContext();

    const page1 = await context1.newPage();
    const page2 = await context2.newPage();

    try {
      // Load presence page in both tabs
      await page1.goto('/presence');
      await page2.goto('/presence');

      // Wait for both pages to load
      await waitForCondition(async () => {
        const user1Id = await page1.locator('.user-id').textContent();
        const user2Id = await page2.locator('.user-id').textContent();
        return user1Id !== 'Loading...' && user2Id !== 'Loading...';
      });

      // Get initial counts
      const initialCount1Text = await page1.getByText(/Online Users \(\d+\)/).textContent();
      const initialCount = parseInt(initialCount1Text.match(/\((\d+)\)/)[1]);

      // Have first user join
      await page1.getByTestId('join-btn').click();
      await waitForText(page1.locator('#status'), 'Successfully joined presence!');

      // Both pages should see the updated count
      await waitForCondition(async () => {
        const count1Text = await page1.getByText(/Online Users \(\d+\)/).textContent();
        const count2Text = await page2.getByText(/Online Users \(\d+\)/).textContent();
        const count1 = parseInt(count1Text.match(/\((\d+)\)/)[1]);
        const count2 = parseInt(count2Text.match(/\((\d+)\)/)[1]);
        return count1 === initialCount + 1 && count2 === initialCount + 1;
      });

      // Have second user join
      await page2.getByTestId('join-btn').click();
      await waitForText(page2.locator('#status'), 'Successfully joined presence!');

      // Both pages should see the further updated count
      await waitForCondition(async () => {
        const count1Text = await page1.getByText(/Online Users \(\d+\)/).textContent();
        const count2Text = await page2.getByText(/Online Users \(\d+\)/).textContent();
        const count1 = parseInt(count1Text.match(/\((\d+)\)/)[1]);
        const count2 = parseInt(count2Text.match(/\((\d+)\)/)[1]);
        return count1 === initialCount + 2 && count2 === initialCount + 2;
      });

      // Have first user leave
      await page1.getByTestId('leave-btn').click();
      await waitForText(page1.locator('#status'), 'Successfully left presence!');

      // Both pages should see the reduced count
      await waitForCondition(async () => {
        const count1Text = await page1.getByText(/Online Users \(\d+\)/).textContent();
        const count2Text = await page2.getByText(/Online Users \(\d+\)/).textContent();
        const count1 = parseInt(count1Text.match(/\((\d+)\)/)[1]);
        const count2 = parseInt(count2Text.match(/\((\d+)\)/)[1]);
        return count1 === initialCount + 1 && count2 === initialCount + 1;
      });
    } finally {
      await context1.close();
      await context2.close();
    }
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

  test('should render users using render_map functionality', async ({ page }) => {
    await page.goto('/presence');

    // Wait for initial load
    await waitForCondition(async () => {
      const userIdText = await page.locator('.user-id').textContent();
      return userIdText !== 'Loading...';
    });

    // Join presence
    await page.getByTestId('join-btn').click();
    await waitForText(page.locator('#status'), 'Successfully joined presence!');

    // Wait for user to appear in list
    await waitForCondition(async () => {
      const userItems = page.locator('.user-item');
      return (await userItems.count()) > 0;
    });

    // Verify user item structure (rendered by render_map)
    const userItems = page.locator('.user-item');
    const firstUserItem = userItems.first();

    // Check that user item contains both user name and ID (handle multiline text)
    const itemText = await firstUserItem.textContent();
    const trimmedText = itemText.replace(/\s+/g, ' ').trim();
    expect(trimmedText).toMatch(/^USER\d+_\d+ \(ID: user_\d+_\d+\)$/);

    // Check that it has the correct CSS class from render_map template
    await expect(firstUserItem).toHaveClass('user-item');

    // Verify structure contains strong tag for user name
    const strongElement = firstUserItem.locator('strong');
    await expect(strongElement).toBeVisible();
    const strongText = await strongElement.textContent();
    expect(strongText).toMatch(/^USER\d+_\d+$/);
  });
});
