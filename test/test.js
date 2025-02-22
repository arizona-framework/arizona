/* global describe, test, expect */

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
    const diff = {
      1: {
        0: "1",
        1: '\'arizona.send("app", "incr", 1)\'',
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
  });
});

function patch(rendered, diff) {
  if (Array.isArray(rendered)) {
    if (rendered[0] === "template" && rendered.length === 3) {
      const staticList = rendered[1];
      const dynamicList = rendered[2];
      return patchTemplate(staticList, dynamicList, diff);
    } else if (rendered[0] === "list" && rendered.length === 3) {
      const staticList = rendered[1];
      const dynamicList = rendered[2];
      return patchList(staticList, dynamicList);
    } else {
      return rendered;
    }
  } else {
    return rendered;
  }
}

function patchTemplate(staticList, dynamicList, diff) {
  dynamicList = patchDynamic(dynamicList, diff);
  return zip(staticList, dynamicList);
}

function patchList(staticList, dynamicList) {
  return dynamicList.forEach((d) => zip(staticList, d));
}

function patchDynamic(dynamicList, diff) {
  for (const [index, value] of Object.entries(diff)) {
    if (typeof value === "object" && !Array.isArray(value)) {
      dynamicList[index] = patch(dynamicList[index], value);
    } else {
      dynamicList[index] = value;
    }
  }
  return dynamicList;
}

function zip(staticList, dynamicList) {
  let str = "";
  for (let i = 0; i < Math.max(staticList.length, dynamicList.length); i++) {
    const dynamic = dynamicList[i] ?? "";
    str += `${staticList[i] ?? ""}${Array.isArray(dynamic) ? zip(dynamic[0], dynamic[1]) : dynamic
      }`;
  }
  return str;
}
