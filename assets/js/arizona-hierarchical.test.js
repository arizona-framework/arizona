/**
 * Comprehensive test suite for arizona_hierarchical.js
 *
 * Tests the client-side hierarchical structure manager that:
 * - Receives hierarchical structures from arizona_hierarchical (Erlang)
 * - Applies diffs from arizona_differ (Erlang)
 * - Generates HTML from structures
 * - Handles nested stateful/stateless components and lists
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { ArizonaHierarchical } from './arizona-hierarchical.js';

describe('ArizonaHierarchical', () => {
  let client;

  beforeEach(() => {
    client = new ArizonaHierarchical();
    // Clear console warnings for clean test output
    vi.spyOn(console, 'warn').mockImplementation(() => {});
  });

  describe('Initialization', () => {
    it('should initialize with empty structure', () => {
      expect(client.isInitialized()).toBe(false);
      expect(client.getStructure()).toEqual({});
      expect(client.getComponentIds()).toEqual([]);
    });

    it('should initialize with basic hierarchical structure', () => {
      const structure = {
        root: {
          static: ['<div class="app">', '</div>'],
          dynamic: ['Welcome to Arizona'],
        },
      };

      client.initialize(structure);

      expect(client.isInitialized()).toBe(true);
      expect(client.getComponentIds()).toEqual(['root']);
      expect(client.getStructure().root).toEqual(structure.root);
    });

    it('should deep clone structure to prevent mutations', () => {
      const structure = {
        root: {
          static: ['<h1>', '</h1>'],
          dynamic: ['Original'],
        },
      };

      client.initialize(structure);
      structure.root.dynamic[0] = 'Modified';

      expect(client.getStructure().root.dynamic[0]).toBe('Original');
    });

    it('should initialize with multiple components', () => {
      const structure = {
        root: {
          static: ['<div>', '</div>'],
          dynamic: [{ type: 'stateful', id: 'counter' }],
        },
        counter: {
          static: ['<span>Count: ', '</span>'],
          dynamic: ['0'],
        },
      };

      client.initialize(structure);

      expect(client.getComponentIds().sort()).toEqual(['counter', 'root']);
      expect(client.getStructure().root).toEqual(structure.root);
      expect(client.getStructure().counter).toEqual(structure.counter);
    });
  });

  describe('Diff Application', () => {
    describe('Basic element changes', () => {
      beforeEach(() => {
        client.initialize({
          root: {
            static: ['<div>', '</div>'],
            dynamic: ['Initial Value'],
          },
        });
      });

      it('should apply simple element change', () => {
        const changes = [[1, 'Updated Value']];

        client.applyDiff('root', changes);

        expect(client.getStructure().root.dynamic[0]).toBe('Updated Value');
      });

      it('should apply multiple element changes', () => {
        // Initialize with multiple dynamic elements
        client.initialize({
          root: {
            static: ['<div>', ' - ', '</div>'],
            dynamic: ['First', 'Second'],
          },
        });

        const changes = [
          [1, 'Updated First'],
          [2, 'Updated Second'],
        ];

        client.applyDiff('root', changes);

        expect(client.getStructure().root.dynamic[0]).toBe('Updated First');
        expect(client.getStructure().root.dynamic[1]).toBe('Updated Second');
      });

      it('should handle numeric values', () => {
        const changes = [[1, 42]];

        client.applyDiff('root', changes);

        expect(client.getStructure().root.dynamic[0]).toBe(42);
      });

      it('should warn for unknown components then throw', () => {
        const consoleSpy = vi.spyOn(console, 'warn');

        expect(() => {
          client.applyDiff('unknown', [[1, 'test']]);
        }).toThrow();

        expect(consoleSpy).toHaveBeenCalledWith(
          '[Arizona] StatefulId unknown not found in structure'
        );
      });
    });

    describe('Hierarchical structure changes (fingerprint mismatch)', () => {
      beforeEach(() => {
        client.initialize({
          root: {
            static: ['<div>', '</div>'],
            dynamic: ['Original'],
          },
        });
      });

      it('should handle stateful structure replacement', () => {
        const newStatefulStruct = {
          type: 'stateful',
          id: 'new-component',
          static: ['<section>', '</section>'],
          dynamic: ['New Content'],
        };

        client.applyDiff('root', newStatefulStruct);

        expect(client.getStructure()['new-component']).toEqual(newStatefulStruct);
      });

      it('should handle stateless structure as element value', () => {
        const statelessStruct = {
          type: 'stateless',
          static: ['<span>', '</span>'],
          dynamic: ['Stateless Content'],
        };

        const changes = [[1, statelessStruct]];

        client.applyDiff('root', changes);

        expect(client.getStructure().root.dynamic[0]).toEqual(statelessStruct);
      });

      it('should handle list structure as element value', () => {
        const listStruct = {
          type: 'list',
          static: ['<li>', '</li>'],
          dynamic: [['Item 1'], ['Item 2'], ['Item 3']],
        };

        const changes = [[1, listStruct]];

        client.applyDiff('root', changes);

        expect(client.getStructure().root.dynamic[0]).toEqual(listStruct);
      });
    });

    describe('List component updates', () => {
      beforeEach(() => {
        client.initialize({
          root: {
            static: ['<ul>', '</ul>'],
            dynamic: [
              {
                type: 'list',
                static: ['<li>', '</li>'],
                dynamic: [['Old Item 1'], ['Old Item 2']],
              },
            ],
          },
        });
      });

      it('should update list dynamic content', () => {
        const newListData = [['New Item 1'], ['New Item 2'], ['New Item 3']];
        const changes = [[1, newListData]];

        client.applyDiff('root', changes);

        const listComponent = client.getStructure().root.dynamic[0];
        expect(listComponent.dynamic).toEqual(newListData);
      });
    });

    describe('Stateless component updates', () => {
      beforeEach(() => {
        client.initialize({
          root: {
            static: ['<div>', '</div>'],
            dynamic: [
              {
                type: 'stateless',
                static: ['<p>', '</p>'],
                dynamic: ['Original Text'],
              },
            ],
          },
        });
      });

      it('should update stateless component with nested diff array', () => {
        // Simulates traditional stateless diff format
        const nestedDiff = [[1, 'Updated Text']];
        const changes = [[1, nestedDiff]];

        client.applyDiff('root', changes);

        const statelessComponent = client.getStructure().root.dynamic[0];
        expect(statelessComponent.dynamic[0]).toBe('Updated Text');
      });
    });
  });

  describe('HTML Generation', () => {
    describe('Basic component rendering', () => {
      it('should generate HTML from simple structure', () => {
        client.initialize({
          root: {
            static: ['<div class="app">', '</div>'],
            dynamic: ['Hello Arizona'],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe('<div class="app">Hello Arizona</div>');
      });

      it('should generate HTML with multiple static/dynamic elements', () => {
        client.initialize({
          root: {
            static: ['<h1>', '</h1><p>', '</p>'],
            dynamic: ['Title', 'Content'],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe('<h1>Title</h1><p>Content</p>');
      });

      it('should handle more static than dynamic elements', () => {
        client.initialize({
          root: {
            static: ['<div>', '<span>Static</span>', '</div>'],
            dynamic: ['Dynamic'],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe('<div>Dynamic<span>Static</span></div>');
      });

      it('should handle more dynamic than static elements', () => {
        client.initialize({
          root: {
            static: ['<ul>'],
            dynamic: ['<li>Item 1</li>', '<li>Item 2</li>', '</ul>'],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe('<ul><li>Item 1</li><li>Item 2</li></ul>');
      });

      it('should warn for missing components then throw', () => {
        const consoleSpy = vi.spyOn(console, 'warn');

        expect(() => {
          client.generateStatefulHTML('nonexistent');
        }).toThrow();

        expect(consoleSpy).toHaveBeenCalledWith(
          '[Arizona] StatefulId nonexistent not found in structure'
        );
      });
    });

    describe('Nested stateful components', () => {
      it('should render nested stateful components', () => {
        client.initialize({
          root: {
            static: ['<div>', '</div>'],
            dynamic: [{ type: 'stateful', id: 'counter' }],
          },
          counter: {
            static: ['<span>Count: ', '</span>'],
            dynamic: ['5'],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe('<div><span>Count: 5</span></div>');
      });

      it('should handle multiple nested components', () => {
        client.initialize({
          root: {
            static: ['<div>', ' ', '</div>'],
            dynamic: [
              { type: 'stateful', id: 'header' },
              { type: 'stateful', id: 'footer' },
            ],
          },
          header: {
            static: ['<h1>', '</h1>'],
            dynamic: ['Header'],
          },
          footer: {
            static: ['<p>', '</p>'],
            dynamic: ['Footer'],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe('<div><h1>Header</h1> <p>Footer</p></div>');
      });

      it('should handle deeply nested components', () => {
        client.initialize({
          root: {
            static: ['<div>', '</div>'],
            dynamic: [{ type: 'stateful', id: 'level1' }],
          },
          level1: {
            static: ['<section>', '</section>'],
            dynamic: [{ type: 'stateful', id: 'level2' }],
          },
          level2: {
            static: ['<p>', '</p>'],
            dynamic: ['Deep Content'],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe('<div><section><p>Deep Content</p></section></div>');
      });
    });

    describe('Stateless components', () => {
      it('should render inline stateless components', () => {
        client.initialize({
          root: {
            static: ['<div>', '</div>'],
            dynamic: [
              {
                type: 'stateless',
                static: ['<h1>', '</h1>'],
                dynamic: ['Stateless Title'],
              },
            ],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe('<div><h1>Stateless Title</h1></div>');
      });

      it('should handle multiple stateless components', () => {
        client.initialize({
          root: {
            static: ['<div>', ' ', '</div>'],
            dynamic: [
              {
                type: 'stateless',
                static: ['<h1>', '</h1>'],
                dynamic: ['Title'],
              },
              {
                type: 'stateless',
                static: ['<p>', '</p>'],
                dynamic: ['Paragraph'],
              },
            ],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe('<div><h1>Title</h1> <p>Paragraph</p></div>');
      });
    });

    describe('List components', () => {
      it('should render list components', () => {
        client.initialize({
          root: {
            static: ['<ul>', '</ul>'],
            dynamic: [
              {
                type: 'list',
                static: ['<li>', '</li>'],
                dynamic: [['Item 1'], ['Item 2'], ['Item 3']],
              },
            ],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe('<ul><li>Item 1</li><li>Item 2</li><li>Item 3</li></ul>');
      });

      it('should handle complex list items', () => {
        client.initialize({
          root: {
            static: ['<div class="items">', '</div>'],
            dynamic: [
              {
                type: 'list',
                static: ['<div class="item">', ' - ', '</div>'],
                dynamic: [
                  ['First', 'Description'],
                  ['Second', 'Other'],
                ],
              },
            ],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe(
          '<div class="items"><div class="item">First - Description</div><div class="item">Second - Other</div></div>'
        );
      });

      it('should handle empty lists', () => {
        client.initialize({
          root: {
            static: ['<ul>', '</ul>'],
            dynamic: [
              {
                type: 'list',
                static: ['<li>', '</li>'],
                dynamic: [],
              },
            ],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe('<ul></ul>');
      });
    });

    describe('IoData flattening', () => {
      it('should flatten simple nested arrays', () => {
        client.initialize({
          root: {
            static: ['<div>', '</div>'],
            dynamic: [['Hello', ' ', 'World']],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe('<div>Hello World</div>');
      });

      it('should flatten deeply nested arrays', () => {
        client.initialize({
          root: {
            static: ['<div>', '</div>'],
            dynamic: [['Start', [' nested ', ['deeply', ' nested']], ' End']],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe('<div>Start nested deeply nested End</div>');
      });

      it('should handle mixed types in nested arrays', () => {
        client.initialize({
          root: {
            static: ['<div>', '</div>'],
            dynamic: [['String', 42, ' and ', true, ' value']],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe('<div>String42 and true value</div>');
      });

      it('should handle nested arrays with components', () => {
        client.initialize({
          root: {
            static: ['<div>', '</div>'],
            dynamic: [['Before', { type: 'stateful', id: 'inner' }, 'After']],
          },
          inner: {
            static: ['<span>', '</span>'],
            dynamic: ['Middle'],
          },
        });

        const html = client.generateStatefulHTML('root');

        expect(html).toBe('<div>Before<span>Middle</span>After</div>');
      });
    });
  });

  describe('Component Management', () => {
    beforeEach(() => {
      client.initialize({
        root: {
          static: ['<div>', '</div>'],
          dynamic: ['Root'],
        },
        sidebar: {
          static: ['<aside>', '</aside>'],
          dynamic: ['Sidebar'],
        },
      });
    });

    it('should get component by ID', () => {
      const component = client.getStructure().root;

      expect(component).toEqual({
        static: ['<div>', '</div>'],
        dynamic: ['Root'],
      });
    });

    it('should return null for nonexistent component', () => {
      const component = client.getStructure().nonexistent;

      expect(component).toBeUndefined();
    });

    it('should check component existence via structure', () => {
      const structure = client.getStructure();
      expect('root' in structure).toBe(true);
      expect('sidebar' in structure).toBe(true);
      expect('nonexistent' in structure).toBe(false);
    });

    it('should get all component IDs', () => {
      const ids = client.getComponentIds().sort();

      expect(ids).toEqual(['root', 'sidebar']);
    });

    it('should clear all components', () => {
      client.clear();

      expect(client.isInitialized()).toBe(false);
      expect(client.getComponentIds()).toEqual([]);
      expect(client.getStructure()).toEqual({});
    });
  });

  describe('Patch Creation', () => {
    beforeEach(() => {
      client.initialize({
        root: {
          static: ['<div class="app">', '</div>'],
          dynamic: ['Hello Arizona'],
        },
      });
    });

    it('should create HTML patch', () => {
      const patch = client.createPatch('root');

      expect(patch.type).toBe('html_patch');
      expect(patch.statefulId).toBe('root');
      expect(patch.html).toBe('<div class="app">Hello Arizona</div>');
    });

    it('should create initial render patch', () => {
      const patch = client.createInitialPatch('root');

      expect(patch.type).toBe('initial_render');
      expect(patch.statefulId).toBe('root');
      expect(patch.html).toBe('<div class="app">Hello Arizona</div>');
    });
  });

  describe('Integration Scenarios', () => {
    it('should handle complete LiveView workflow', () => {
      // 1. Initialize with initial hierarchical structure (from WebSocket)
      client.initialize({
        root: {
          static: ['<div class="counter">', '</div>'],
          dynamic: [{ type: 'stateful', id: 'counter-1' }],
        },
        'counter-1': {
          static: ['<span>Count: ', '</span><button>+</button>'],
          dynamic: ['0'],
        },
      });

      // 2. Generate initial HTML
      let html = client.generateStatefulHTML('root');
      expect(html).toBe('<div class="counter"><span>Count: 0</span><button>+</button></div>');

      // 3. Apply diff from arizona_differ (simulating button click)
      client.applyDiff('counter-1', [[1, '1']]);

      // 4. Generate updated HTML
      html = client.generateStatefulHTML('root');
      expect(html).toBe('<div class="counter"><span>Count: 1</span><button>+</button></div>');

      // 5. Create patch for DOM update
      const patch = client.createPatch('root');
      expect(patch.html).toBe('<div class="counter"><span>Count: 1</span><button>+</button></div>');
    });

    it('should handle fingerprint mismatch with structure replacement', () => {
      // 1. Initial structure
      client.initialize({
        root: {
          static: ['<div>', '</div>'],
          dynamic: ['Simple content'],
        },
      });

      // 2. Fingerprint mismatch: replace with complex structure
      const newStructure = {
        type: 'stateful',
        id: 'complex-view',
        static: ['<section class="complex">', '</section>'],
        dynamic: [{ type: 'stateful', id: 'nested' }],
      };

      client.applyDiff('root', newStructure);

      // 3. Add nested component
      client.initialize({
        'complex-view': newStructure,
        nested: {
          static: ['<p>', '</p>'],
          dynamic: ['Nested content'],
        },
      });

      // 4. Generate HTML from new structure
      const html = client.generateStatefulHTML('complex-view');
      expect(html).toBe('<section class="complex"><p>Nested content</p></section>');
    });

    it('should handle mixed component types in realistic scenario', () => {
      // Simulate a complex application structure
      client.initialize({
        root: {
          static: ['<div class="app">', '</div>'],
          dynamic: [{ type: 'stateful', id: 'layout' }],
        },
        layout: {
          static: ['<header>', '</header><main>', '</main>'],
          dynamic: [
            { type: 'stateful', id: 'nav' },
            {
              type: 'stateless',
              static: ['<section>', '</section>'],
              dynamic: [
                {
                  type: 'list',
                  static: ['<article>', '</article>'],
                  dynamic: [['Article 1'], ['Article 2'], ['Article 3']],
                },
              ],
            },
          ],
        },
        nav: {
          static: ['<nav><ul>', '</ul></nav>'],
          dynamic: [
            {
              type: 'list',
              static: ['<li>', '</li>'],
              dynamic: [['Home'], ['About'], ['Contact']],
            },
          ],
        },
      });

      const html = client.generateStatefulHTML('root');

      expect(html).toBe(
        '<div class="app">' +
          '<header><nav><ul><li>Home</li><li>About</li><li>Contact</li></ul></nav></header>' +
          '<main><section><article>Article 1</article><article>Article 2</article><article>Article 3</article></section></main>' +
          '</div>'
      );
    });
  });

  describe('Edge Cases', () => {
    it('should handle empty diff arrays', () => {
      client.initialize({
        root: {
          static: ['<div>', '</div>'],
          dynamic: ['Original'],
        },
      });

      expect(() => {
        return client.applyDiff('root', []);
      }).not.toThrow();
      expect(client.getStructure().root.dynamic[0]).toBe('Original');
    });

    it('should handle empty string values', () => {
      client.initialize({
        root: {
          static: ['<div>', '', '</div>'],
          dynamic: ['content'],
        },
      });

      const html = client.generateStatefulHTML('root');
      expect(html).toBe('<div>content</div>');
    });

    it('should handle components with empty dynamic arrays', () => {
      client.initialize({
        root: {
          static: ['<div>Static only</div>'],
          dynamic: [],
        },
      });

      const html = client.generateStatefulHTML('root');
      expect(html).toBe('<div>Static only</div>');
    });
  });
});
