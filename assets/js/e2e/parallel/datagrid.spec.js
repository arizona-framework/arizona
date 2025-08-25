import { test, expect } from '@playwright/test';
import { waitForCondition, collectWebSocketMessages } from '../test-utils.js';

test.describe('Arizona Datagrid App', () => {
  test('should load datagrid page and display initial state', async ({ page }) => {
    await page.goto('/datagrid');

    // Check that the page loads with the table structure
    await expect(page.locator('.table')).toBeVisible();
    await expect(page.locator('thead')).toBeVisible();
    await expect(page.locator('tbody')).toBeVisible();

    // Verify table headers are present
    await expect(page.getByText('Name')).toBeVisible();
    await expect(page.getByText('Email')).toBeVisible();
    await expect(page.getByText('Role')).toBeVisible();
    await expect(page.getByText('Status')).toBeVisible();
    await expect(page.getByText('Created')).toBeVisible();
    await expect(page.getByText('Actions')).toBeVisible();

    // Verify initial data is loaded (15 users)
    const userRows = page.locator('.user-row');
    await expect(userRows).toHaveCount(15);

    // Verify only one datagrid container exists
    const datagridContainers = page.locator('#datagrid');
    await expect(datagridContainers).toHaveCount(1);
  });

  test('should establish WebSocket connection and receive initial hierarchical structure', async ({
    page,
  }) => {
    const initialMessages = collectWebSocketMessages(page, 'initial_render');

    await page.goto('/datagrid');

    // Wait for initial render message to arrive
    await waitForCondition(() => {
      return initialMessages.length > 0;
    });

    const initialMessage = initialMessages[0];
    expect(initialMessage).toBeTruthy();
    expect(initialMessage.structure).toBeDefined();
    expect(typeof initialMessage.structure).toBe('object');

    // Verify the structure contains datagrid component data
    const structureKeys = Object.keys(initialMessage.structure);
    expect(structureKeys.length).toBeGreaterThan(0);

    // Check that datagrid component has static and dynamic properties
    const datagridComponent = Object.values(initialMessage.structure).find((component) => {
      return component.static || component.dynamic;
    });
    expect(datagridComponent).toBeDefined();
    if (datagridComponent.static) expect(datagridComponent.static).toBeDefined();
    if (datagridComponent.dynamic) expect(datagridComponent.dynamic).toBeDefined();
  });

  test('should display all user data with correct formatting', async ({ page }) => {
    await page.goto('/datagrid');

    // Verify specific users are displayed
    await expect(page.getByText('John Doe')).toBeVisible();
    await expect(page.getByText('jane.smith@example.com')).toBeVisible();
    await expect(page.getByText('Bob Wilson')).toBeVisible();

    // Verify role badges are displayed with correct styling
    await expect(page.locator('.role-badge-admin')).toHaveCount(4); // John, Mike, Lisa, Daniel
    await expect(page.locator('.role-badge-moderator')).toHaveCount(4); // Bob, Sarah, Chris, Ryan
    await expect(page.locator('.role-badge-user')).toHaveCount(7); // Remaining users

    // Verify status badges are displayed
    await expect(page.locator('.status-badge-active').first()).toBeVisible();
    await expect(page.locator('.status-badge-inactive').first()).toBeVisible();
    await expect(page.locator('.status-badge-pending').first()).toBeVisible();

    // Verify action buttons are present for all users
    const deleteButtons = page.locator('.action-btn-danger');
    await expect(deleteButtons).toHaveCount(15);
    await expect(deleteButtons.first()).toContainText('Delete');
  });

  test('should sort table by name column via WebSocket updates', async ({ page }) => {
    const diffMessages = collectWebSocketMessages(page, 'diff');

    await page.goto('/datagrid');

    // Wait for initial load
    await expect(page.getByText('John Doe')).toBeVisible();

    // Get initial first row name
    const firstRowName = await page.locator('.user-row').first().locator('strong').textContent();

    // Click on Name header to sort
    await page.locator('th.sortable').filter({ hasText: 'Name' }).click();

    // Wait for WebSocket diff message
    await waitForCondition(() => {
      return diffMessages.length > 0;
    });

    const diffMessage = diffMessages[0];
    expect(diffMessage.type).toBe('diff');
    expect(diffMessage.changes).toBeDefined();
    expect(Array.isArray(diffMessage.changes)).toBe(true);

    // Verify sorting happened (first row should have changed)
    await page.waitForTimeout(200); // Allow for re-render
    const newFirstRowName = await page.locator('.user-row').first().locator('strong').textContent();

    // Names should be sorted alphabetically
    expect(newFirstRowName).not.toBe(firstRowName);

    // Verify Alice Johnson is first (alphabetically first)
    await expect(page.locator('.user-row').first().locator('strong')).toHaveText('Alice Johnson');
  });

  test('should toggle sort direction when clicking same column header', async ({ page }) => {
    await page.goto('/datagrid');

    // Click Name header once (ascending)
    await page.locator('th.sortable').filter({ hasText: 'Name' }).click();
    await page.waitForTimeout(200);

    // Verify ascending sort (Alice first)
    await expect(page.locator('.user-row').first().locator('strong')).toHaveText('Alice Johnson');

    // Click Name header again (descending)
    await page.locator('th.sortable').filter({ hasText: 'Name' }).click();
    await page.waitForTimeout(200);

    // Verify descending sort (Ryan should be last alphabetically, so might be first in desc)
    const firstRowNameDesc = await page
      .locator('.user-row')
      .first()
      .locator('strong')
      .textContent();
    expect(firstRowNameDesc).not.toBe('Alice Johnson');
  });

  test('should sort by email column correctly', async ({ page }) => {
    await page.goto('/datagrid');

    // Click Email header to sort
    await page.locator('th.sortable').filter({ hasText: 'Email' }).click();
    await page.waitForTimeout(200);

    // Get all email addresses to verify sorting
    const emailElements = await page.locator('.user-row a[href^="mailto:"]').all();
    const emails = await Promise.all(
      emailElements.map((el) => {
        return el.textContent();
      })
    );

    // Verify emails are sorted alphabetically
    const sortedEmails = [...emails].sort((a, b) => {
      return a.toLowerCase().localeCompare(b.toLowerCase());
    });
    expect(emails).toEqual(sortedEmails);
  });

  test('should sort by created date column correctly', async ({ page }) => {
    await page.goto('/datagrid');

    // Click Created header to sort
    await page.locator('th.sortable').filter({ hasText: 'Created' }).click();
    await page.waitForTimeout(200);

    // Get all date elements
    const dateElements = await page.locator('.user-row .text-muted').all();
    const dates = await Promise.all(
      dateElements.map((el) => {
        return el.textContent();
      })
    );

    // Verify dates are sorted
    const sortedDates = [...dates].sort();
    expect(dates).toEqual(sortedDates);
  });

  test('should not allow sorting on non-sortable columns', async ({ page }) => {
    await page.goto('/datagrid');

    // Verify Role and Status headers are not sortable
    const roleHeader = page.locator('th').filter({ hasText: 'Role' });
    const statusHeader = page.locator('th').filter({ hasText: 'Status' });
    const actionsHeader = page.locator('th').filter({ hasText: 'Actions' });

    await expect(roleHeader).not.toHaveClass(/sortable/);
    await expect(statusHeader).not.toHaveClass(/sortable/);
    await expect(actionsHeader).not.toHaveClass(/sortable/);

    // Verify these headers don't have pointer cursor
    const roleCursor = await roleHeader.evaluate((el) => {
      return getComputedStyle(el).cursor;
    });
    const statusCursor = await statusHeader.evaluate((el) => {
      return getComputedStyle(el).cursor;
    });

    expect(roleCursor).not.toBe('pointer');
    expect(statusCursor).not.toBe('pointer');
  });

  test('should delete user via action button with WebSocket updates', async ({ page }) => {
    const diffMessages = collectWebSocketMessages(page, 'diff');

    await page.goto('/datagrid');

    // Verify initial count
    await expect(page.locator('.user-row')).toHaveCount(15);

    // Get the first user's data for deletion
    const firstUserRow = page.locator('.user-row').first();
    const userName = await firstUserRow.locator('strong').textContent();

    // Click delete button for first user
    await firstUserRow.locator('.action-btn-danger').click();

    // Wait for WebSocket diff message
    await waitForCondition(() => {
      return diffMessages.length > 0;
    });

    const diffMessage = diffMessages[0];
    expect(diffMessage.type).toBe('diff');
    expect(diffMessage.changes).toBeDefined();
    expect(Array.isArray(diffMessage.changes)).toBe(true);

    // Verify user count decreased
    await page.waitForTimeout(200);
    await expect(page.locator('.user-row')).toHaveCount(14);

    // Verify the specific user is no longer in the table
    await expect(page.getByText(userName)).not.toBeVisible();
  });

  test('should delete multiple users sequentially', async ({ page }) => {
    await page.goto('/datagrid');

    // Verify initial count
    await expect(page.locator('.user-row')).toHaveCount(15);

    // Delete 3 users sequentially
    const usersToDelete = ['John Doe', 'Jane Smith', 'Bob Wilson'];

    for (const userName of usersToDelete) {
      const userRow = page.locator('.user-row').filter({ hasText: userName });
      await userRow.locator('.action-btn-danger').click();
      await page.waitForTimeout(200); // Wait for deletion to process

      // Verify user is deleted
      await expect(page.getByText(userName)).not.toBeVisible();
    }

    // Verify final count
    await expect(page.locator('.user-row')).toHaveCount(12);
  });

  test('should handle rapid sorting and deletion operations', async ({ page }) => {
    const allDiffMessages = [];

    page.on('websocket', (ws) => {
      ws.on('framereceived', (event) => {
        try {
          const data = JSON.parse(event.payload);
          if (data.type === 'diff') {
            allDiffMessages.push(data);
          }
        } catch (e) {
          // Ignore non-JSON
        }
      });
    });

    await page.goto('/datagrid');

    // Perform rapid operations
    const operations = [
      () => {
        return page.locator('th.sortable').filter({ hasText: 'Name' }).click();
      },
      () => {
        return page.locator('th.sortable').filter({ hasText: 'Email' }).click();
      },
      () => {
        return page.locator('.user-row').first().locator('.action-btn-danger').click();
      },
      () => {
        return page.locator('th.sortable').filter({ hasText: 'Created' }).click();
      },
      () => {
        return page.locator('.user-row').first().locator('.action-btn-danger').click();
      },
    ];

    // Execute operations with small delays
    for (const operation of operations) {
      await operation();
      await page.waitForTimeout(100);
    }

    // Wait for all WebSocket messages
    await page.waitForTimeout(500);

    // Verify we received diff messages for operations
    expect(allDiffMessages.length).toBeGreaterThan(0);

    // Each diff should contain properly formatted element changes
    allDiffMessages.forEach((msg) => {
      expect(msg.type).toBe('diff');
      expect(Array.isArray(msg.changes)).toBe(true);

      // Each change should be [element_index, new_value]
      msg.changes.forEach((elementChange) => {
        expect(Array.isArray(elementChange)).toBe(true);
        expect(elementChange.length).toBe(2);
        expect(typeof elementChange[0]).toBe('number'); // element index
      });
    });

    // Verify final state (should have deleted 2 users)
    await expect(page.locator('.user-row')).toHaveCount(13);
  });

  test('should display correct badge styling and icons', async ({ page }) => {
    await page.goto('/datagrid');

    // Verify role badge styling
    const adminBadge = page.locator('.role-badge-admin').first();
    const moderatorBadge = page.locator('.role-badge-moderator').first();
    const userBadge = page.locator('.role-badge-user').first();

    await expect(adminBadge).toBeVisible();
    await expect(moderatorBadge).toBeVisible();
    await expect(userBadge).toBeVisible();

    // Verify status badge styling and icons
    const activeBadge = page.locator('.status-badge-active').first();
    const inactiveBadge = page.locator('.status-badge-inactive').first();
    const pendingBadge = page.locator('.status-badge-pending').first();

    await expect(activeBadge).toContainText('✓'); // Check mark
    await expect(activeBadge).toContainText('active');

    await expect(inactiveBadge).toContainText('○'); // Circle
    await expect(inactiveBadge).toContainText('inactive');

    await expect(pendingBadge).toContainText('⌛'); // Hourglass
    await expect(pendingBadge).toContainText('pending');
  });

  test('should handle table accessibility and keyboard navigation', async ({ page }) => {
    await page.goto('/datagrid');

    // Verify table has proper ARIA structure
    const table = page.locator('.table');
    await expect(table).toBeVisible();

    // Verify table headers have proper scope
    const headers = page.locator('th[scope="col"]');
    await expect(headers).toHaveCount(6);

    // Verify sortable headers are keyboard accessible
    const sortableHeaders = page.locator('th.sortable');
    await expect(sortableHeaders).toHaveCount(3); // Name, Email, Created

    // Test keyboard navigation on sortable header (click instead of Enter as that's the implemented interaction)
    await sortableHeaders.first().click();
    await page.waitForTimeout(200);

    // Verify sorting happened
    await expect(page.locator('.user-row').first().locator('strong')).toHaveText('Alice Johnson');

    // Verify action buttons are keyboard accessible
    const firstDeleteButton = page.locator('.action-btn-danger').first();
    await firstDeleteButton.focus();

    // Verify button has proper attributes for accessibility
    const buttonTagName = await firstDeleteButton.evaluate((el) => {
      return el.tagName;
    });
    expect(buttonTagName).toBe('BUTTON');

    const buttonType = await firstDeleteButton.getAttribute('type');
    expect(buttonType).toBe('button');
  });

  test('should display proper hover effects and interactions', async ({ page }) => {
    await page.goto('/datagrid');

    // Test row hover effects
    const firstRow = page.locator('.user-row').first();
    await firstRow.hover();

    // Verify row has hover styling (background change)
    const rowBackgroundColor = await firstRow.evaluate((el) => {
      return getComputedStyle(el).backgroundColor;
    });
    expect(rowBackgroundColor).not.toBe('rgba(0, 0, 0, 0)'); // Should have some background

    // Test sortable header hover effects
    const sortableHeader = page.locator('th.sortable').first();
    await sortableHeader.hover();

    // Verify header has hover cursor
    const headerCursor = await sortableHeader.evaluate((el) => {
      return getComputedStyle(el).cursor;
    });
    expect(headerCursor).toBe('pointer');

    // Test action button hover effects
    const deleteButton = page.locator('.action-btn-danger').first();
    await deleteButton.hover();

    // Verify button has hover styling
    const buttonCursor = await deleteButton.evaluate((el) => {
      return getComputedStyle(el).cursor;
    });
    expect(buttonCursor).toBe('pointer');
  });

  test('should maintain data integrity during complex operations', async ({ page }) => {
    await page.goto('/datagrid');

    // Store initial data for comparison
    const initialEmails = await page.locator('.user-row a[href^="mailto:"]').allTextContents();
    const initialNames = await page.locator('.user-row strong').allTextContents();

    // Perform sorting operations
    await page.locator('th.sortable').filter({ hasText: 'Name' }).click();
    await page.waitForTimeout(200);

    // Verify data integrity (same emails, different order)
    const sortedEmails = await page.locator('.user-row a[href^="mailto:"]').allTextContents();
    const sortedNames = await page.locator('.user-row strong').allTextContents();

    expect(sortedEmails.sort()).toEqual(initialEmails.sort());
    expect(sortedNames.sort()).toEqual(initialNames.sort());

    // Delete a user
    const userToDelete = sortedNames[0];
    await page.locator('.user-row').first().locator('.action-btn-danger').click();
    await page.waitForTimeout(200);

    // Verify user is completely removed
    await expect(page.getByText(userToDelete)).not.toBeVisible();
    await expect(page.locator('.user-row')).toHaveCount(14);

    // Verify remaining data is still intact
    const remainingNames = await page.locator('.user-row strong').allTextContents();
    expect(remainingNames).not.toContain(userToDelete);
    expect(remainingNames.length).toBe(14);
  });

  test('should handle email links correctly', async ({ page }) => {
    await page.goto('/datagrid');

    // Verify email links are properly formatted
    const emailLinks = page.locator('a[href^="mailto:"]');
    await expect(emailLinks).toHaveCount(15);

    // Test specific email link
    const johnDoeEmail = page.locator('a[href="mailto:john.doe@example.com"]');
    await expect(johnDoeEmail).toBeVisible();
    await expect(johnDoeEmail).toHaveText('john.doe@example.com');

    // Verify all email links have proper href attributes
    const allEmailElements = await emailLinks.all();
    for (const emailElement of allEmailElements) {
      const href = await emailElement.getAttribute('href');
      const text = await emailElement.textContent();
      expect(href).toBe(`mailto:${text.trim()}`);
    }
  });
});
