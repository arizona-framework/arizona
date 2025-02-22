/* global module */
"use strict";

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
    str += `${staticList[i] ?? ""}${
      Array.isArray(dynamic) ? zip(dynamic[0], dynamic[1]) : dynamic
    }`;
  }
  return str;
}

try {
  module.exports.patch = patch;
} catch {
  /* required for testing */
}
