import { test, expect } from '@playwright/test';
import { waitForCondition, collectWebSocketMessages } from '../utils.js';

test.describe('Arizona Nested Components', () => {
  test('should load nested components page with complex hierarchy', async ({ page }) => {
    await page.goto('/nested');

    // Verify view-level component
    await expect(page.getByTestId('view-counter')).toHaveText('View Counter: 0');
    await expect(page.getByTestId('view-increment')).toBeVisible();
    await expect(page.getByTestId('view-broadcast')).toBeVisible();

    // Verify stateless component shows parent counter
    await expect(page.getByTestId('stateless-parent-counter')).toHaveText('Parent Counter: 0');

    // Verify parent stateful component
    await expect(page.getByTestId('parent-stateful-counter')).toHaveText('Counter: 0');
    await expect(page.getByTestId('parent-increment')).toBeVisible();
    await expect(page.getByTestId('parent-propagate')).toBeVisible();

    // Verify nested stateless shows parent counter
    await expect(page.getByTestId('nested-stateless-parent-counter')).toHaveText(
      'Parent Counter: 0'
    );

    // Verify child stateful components
    await expect(page.getByTestId('child-stateful-counter')).toHaveText('Counter: 0');
    await expect(page.getByTestId('child-increment')).toBeVisible();

    // Verify grandchild components exist
    await expect(page.getByTestId('grandchild-stateful-counter-grandchild-stateful-1')).toHaveText(
      'Counter: 0'
    );
    await expect(page.getByTestId('grandchild-stateful-counter-grandchild-stateful-2')).toHaveText(
      'Counter: 0'
    );

    // Verify leaf component
    await expect(page.getByTestId('leaf-stateful-counter')).toHaveText('Counter: 0');
    await expect(page.getByTestId('leaf-increment')).toBeVisible();
  });

  test('should update view counter and propagate to stateless child', async ({ page }) => {
    await page.goto('/nested');

    // Initial state
    await expect(page.getByTestId('view-counter')).toHaveText('View Counter: 0');
    await expect(page.getByTestId('stateless-parent-counter')).toHaveText('Parent Counter: 0');

    // Increment view counter
    await page.getByTestId('view-increment').click();

    // Verify view counter updated
    await expect(page.getByTestId('view-counter')).toHaveText('View Counter: 1');

    // Verify stateless component reflects new parent counter
    await expect(page.getByTestId('stateless-parent-counter')).toHaveText('Parent Counter: 1');
  });

  test('should update parent stateful and propagate to nested stateless', async ({ page }) => {
    await page.goto('/nested');

    // Initial state
    await expect(page.getByTestId('parent-stateful-counter')).toHaveText('Counter: 0');
    await expect(page.getByTestId('nested-stateless-parent-counter')).toHaveText(
      'Parent Counter: 0'
    );
    await expect(page.getByTestId('deep-stateless-parent-counter')).toHaveText('Parent Counter: 0');

    // Increment parent
    await page.getByTestId('parent-increment').click();

    // Verify parent updated
    await expect(page.getByTestId('parent-stateful-counter')).toHaveText('Counter: 1');

    // Verify nested stateless components reflect new parent counter
    await expect(page.getByTestId('nested-stateless-parent-counter')).toHaveText(
      'Parent Counter: 1'
    );
    await expect(page.getByTestId('deep-stateless-parent-counter')).toHaveText('Parent Counter: 1');
  });

  test('should propagate changes from parent to all descendant stateful components', async ({
    page,
  }) => {
    await page.goto('/nested');

    // Initial state - all counters at 0
    await expect(page.getByTestId('parent-stateful-counter')).toHaveText('Counter: 0');
    await expect(page.getByTestId('child-stateful-counter')).toHaveText('Counter: 0');
    await expect(page.getByTestId('grandchild-stateful-counter-grandchild-stateful-1')).toHaveText(
      'Counter: 0'
    );
    await expect(page.getByTestId('leaf-stateful-counter')).toHaveText('Counter: 0');

    // Trigger propagation from parent
    await page.getByTestId('parent-propagate').click();

    // Verify parent counter incremented by 10
    await expect(page.getByTestId('parent-stateful-counter')).toHaveText('Counter: 10');

    // Verify only ACTUAL descendants received the propagated value (10)
    // Note: child-stateful-1 is a SIBLING of parent-stateful, not a descendant
    await expect(page.getByTestId('child-stateful-counter')).toHaveText('Counter: 0');
    await expect(page.getByTestId('grandchild-stateful-counter-grandchild-stateful-1')).toHaveText(
      'Counter: 10'
    );
    await expect(page.getByTestId('leaf-stateful-counter')).toHaveText('Counter: 10');

    // Verify nested stateless components also updated
    await expect(page.getByTestId('nested-stateless-parent-counter')).toHaveText(
      'Parent Counter: 10'
    );
    await expect(page.getByTestId('deep-stateless-parent-counter')).toHaveText(
      'Parent Counter: 10'
    );
  });

  test('should handle independent updates of child components', async ({ page }) => {
    await page.goto('/nested');

    // Increment child independently
    await page.getByTestId('child-increment').click();
    await expect(page.getByTestId('child-stateful-counter')).toHaveText('Counter: 1');

    // Verify parent not affected
    await expect(page.getByTestId('parent-stateful-counter')).toHaveText('Counter: 0');

    // Verify other grandchildren not affected
    await expect(page.getByTestId('grandchild-stateful-counter-grandchild-stateful-1')).toHaveText(
      'Counter: 0'
    );
    await expect(page.getByTestId('grandchild-stateful-counter-grandchild-stateful-2')).toHaveText(
      'Counter: 0'
    );

    // Increment grandchild independently
    await page.getByTestId('grandchild-increment-grandchild-stateful-1').click();
    await expect(page.getByTestId('grandchild-stateful-counter-grandchild-stateful-1')).toHaveText(
      'Counter: 1'
    );

    // Verify siblings not affected
    await expect(page.getByTestId('child-stateful-counter')).toHaveText('Counter: 1'); // Still 1
    await expect(page.getByTestId('grandchild-stateful-counter-grandchild-stateful-2')).toHaveText(
      'Counter: 0'
    ); // Still 0
  });

  test('should broadcast from view to stateful components', async ({ page }) => {
    await page.goto('/nested');

    // Trigger broadcast from view
    await page.getByTestId('view-broadcast').click();

    // Wait a bit for event propagation
    await page.waitForTimeout(100);

    // Verify parent received the broadcast and incremented by 10
    await expect(page.getByTestId('parent-stateful-counter')).toHaveText('Counter: 10');

    // Verify descendants also received propagated value
    await expect(page.getByTestId('child-stateful-counter')).toHaveText('Counter: 10');
    await expect(page.getByTestId('grandchild-stateful-counter-grandchild-stateful-1')).toHaveText(
      'Counter: 10'
    );
    await expect(page.getByTestId('leaf-stateful-counter')).toHaveText('Counter: 10');
  });

  test('should receive multiple diff messages during propagation', async ({ page }) => {
    const diffMessages = collectWebSocketMessages(page, 'diff');

    await page.goto('/nested');

    // Wait for initial load
    await expect(page.getByTestId('parent-stateful-counter')).toHaveText('Counter: 0');

    const initialDiffCount = diffMessages.length;

    // Trigger propagation which should update multiple components
    await page.getByTestId('parent-propagate').click();

    // Wait for updates
    await expect(page.getByTestId('parent-stateful-counter')).toHaveText('Counter: 10');

    // Wait for all diff messages to arrive
    await page.waitForTimeout(500);

    // Verify we received multiple diff messages (one for each component update)
    expect(diffMessages.length).toBeGreaterThan(initialDiffCount);
    expect(diffMessages.length - initialDiffCount).toBeGreaterThan(1); // At least 2 diffs

    // Verify all diff messages have proper structure
    diffMessages.slice(initialDiffCount).forEach((msg) => {
      expect(msg.type).toBe('diff');
      expect(msg.stateful_id).toBeDefined();
      expect(Array.isArray(msg.changes)).toBe(true);
    });
  });

  test('should handle complex mixed propagation scenario', async ({ page }) => {
    await page.goto('/nested');

    // Step 1: Increment view counter
    await page.getByTestId('view-increment').click();
    await page.getByTestId('view-increment').click();
    await expect(page.getByTestId('view-counter')).toHaveText('View Counter: 2');
    await expect(page.getByTestId('stateless-parent-counter')).toHaveText('Parent Counter: 2');

    // Step 2: Increment parent stateful independently
    await page.getByTestId('parent-increment').click();
    await page.getByTestId('parent-increment').click();
    await page.getByTestId('parent-increment').click();
    await expect(page.getByTestId('parent-stateful-counter')).toHaveText('Counter: 3');

    // Step 3: Increment child independently
    await page.getByTestId('child-increment').click();
    await expect(page.getByTestId('child-stateful-counter')).toHaveText('Counter: 1');

    // Step 4: Propagate from parent (should update parent + descendants only)
    await page.getByTestId('parent-propagate').click();
    await expect(page.getByTestId('parent-stateful-counter')).toHaveText('Counter: 13'); // 3 + 10

    // Only parent's ACTUAL descendants should be 13
    // child-stateful-1 is a SIBLING of parent-stateful, not a descendant
    await expect(page.getByTestId('child-stateful-counter')).toHaveText('Counter: 1');
    await expect(page.getByTestId('grandchild-stateful-counter-grandchild-stateful-1')).toHaveText(
      'Counter: 13'
    );
    await expect(page.getByTestId('leaf-stateful-counter')).toHaveText('Counter: 13');

    // Verify view counter unchanged
    await expect(page.getByTestId('view-counter')).toHaveText('View Counter: 2');
  });

  test('should maintain independent state between sibling components', async ({ page }) => {
    await page.goto('/nested');

    // Increment first grandchild
    await page.getByTestId('grandchild-increment-grandchild-stateful-1').click();
    await page.getByTestId('grandchild-increment-grandchild-stateful-1').click();
    await expect(page.getByTestId('grandchild-stateful-counter-grandchild-stateful-1')).toHaveText(
      'Counter: 2'
    );

    // Increment second grandchild differently
    await page.getByTestId('grandchild-increment-grandchild-stateful-2').click();
    await expect(page.getByTestId('grandchild-stateful-counter-grandchild-stateful-2')).toHaveText(
      'Counter: 1'
    );

    // Increment leaf
    await page.getByTestId('leaf-increment').click();
    await page.getByTestId('leaf-increment').click();
    await page.getByTestId('leaf-increment').click();
    await expect(page.getByTestId('leaf-stateful-counter')).toHaveText('Counter: 3');

    // Verify all maintain independent state
    await expect(page.getByTestId('grandchild-stateful-counter-grandchild-stateful-1')).toHaveText(
      'Counter: 2'
    );
    await expect(page.getByTestId('grandchild-stateful-counter-grandchild-stateful-2')).toHaveText(
      'Counter: 1'
    );
    await expect(page.getByTestId('leaf-stateful-counter')).toHaveText('Counter: 3');

    // Verify parents still at 0
    await expect(page.getByTestId('parent-stateful-counter')).toHaveText('Counter: 0');
    await expect(page.getByTestId('child-stateful-counter')).toHaveText('Counter: 0');
  });

  test('should render child-stateful correctly after view re-render (stateful inside stateless)', async ({
    page,
  }) => {
    // Capture console logs
    page.on('console', msg => {
      if (msg.text().includes('[DEBUG]')) {
        console.log('BROWSER:', msg.text());
      }
    });

    await page.goto('/nested');

    // Verify child-stateful-1 (nested inside stateless wrapper) renders correctly initially
    await expect(page.getByTestId('child-stateful-counter')).toBeVisible();
    await expect(page.getByTestId('child-stateful-counter')).toHaveText('Counter: 0');
    await expect(page.getByTestId('child-increment')).toBeVisible();
    await expect(page.getByTestId('child-notify-parent')).toBeVisible();

    // Verify child's nested components also visible
    await expect(page.getByTestId('child-nested-stateless-counter')).toBeVisible();
    await expect(page.getByTestId('child-nested-stateless-counter')).toHaveText('Child Counter: 0');
    await expect(page.getByTestId('grandchild-stateful-counter-grandchild-stateful-2')).toBeVisible();

    // Increment view counter (triggers view re-render with full HTML patch)
    await page.getByTestId('view-increment').click();
    await expect(page.getByTestId('view-counter')).toHaveText('View Counter: 1');

    // CRITICAL: Verify child-stateful-1 STILL renders as proper HTML component
    // (not as placeholder text like "30610")
    await expect(page.getByTestId('child-stateful-counter')).toBeVisible();
    await expect(page.getByTestId('child-stateful-counter')).toHaveText('Counter: 0');
    await expect(page.getByTestId('child-increment')).toBeVisible();
    await expect(page.getByTestId('child-notify-parent')).toBeVisible();

    // Verify child component is still interactive after view re-render
    await page.getByTestId('child-increment').click();
    await expect(page.getByTestId('child-stateful-counter')).toHaveText('Counter: 1');

    // Verify child's nested stateless updated with new counter
    await expect(page.getByTestId('child-nested-stateless-counter')).toHaveText('Child Counter: 1');

    // Increment view again to trigger another re-render
    await page.getByTestId('view-increment').click();
    await expect(page.getByTestId('view-counter')).toHaveText('View Counter: 2');

    // Verify child-stateful-1 STILL works after second view re-render
    await expect(page.getByTestId('child-stateful-counter')).toBeVisible();
    await expect(page.getByTestId('child-stateful-counter')).toHaveText('Counter: 1');
    await page.getByTestId('child-increment').click();
    await expect(page.getByTestId('child-stateful-counter')).toHaveText('Counter: 2');
  });
});
