/* global require, describe, test, expect */

const { patch } = require("../priv/static/assets/js/arizona-patch.js");

describe("Patch Test", () => {
  test("should patch diff", () => {
    const rendered = [
      "template",
      ['<div id="', '"> ', "</div>"],
      [
        "app",
        [
          "template",
          [
            '<div id="counter">\n    <span>',
            '</span>\n    <button\n        type="button"\n        onclick=',
            ">\n        Increment\n    </button>\n</div>",
          ],
          ["0", '\'arizona.send("app", "incr", 1)\''],
        ],
      ],
    ];

    let diff = {
      1: {
        0: "1",
      },
    };
    expect(patch(rendered, diff)).toBe(`<div id="app"> <div id="counter">
    <span>1</span>
    <button
        type="button"
        onclick='arizona.send("app", "incr", 1)'>
        Increment
    </button>
</div></div>`);

    diff = {
      1: {
        0: "2",
      },
    };
    expect(patch(rendered, diff)).toBe(`<div id="app"> <div id="counter">
    <span>2</span>
    <button
        type="button"
        onclick='arizona.send("app", "incr", 1)'>
        Increment
    </button>
</div></div>`);
  });
});
