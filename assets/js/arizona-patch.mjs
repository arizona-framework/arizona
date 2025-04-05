// --------------------------------------------------------------------
// API function definitions
// --------------------------------------------------------------------

export function patch(rendered, diff) {
  if (rendered[0] === 'template' && rendered.length === 3) {
    const staticList = rendered[1];
    const dynamicList = [...rendered[2]];
    return patchTemplate(staticList, dynamicList, diff);
  } else if (rendered[0] === 'list' && rendered.length === 3) {
    const staticList = rendered[1];
    const dynamicList = [...rendered[2]];
    return patchList(staticList, dynamicList);
  } else {
    return rendered;
  }
}

// --------------------------------------------------------------------
// Private functions
// --------------------------------------------------------------------

function patchTemplate(staticList, dynamicList, diff) {
  dynamicList = patchDynamic(dynamicList, diff);
  return zip(staticList, dynamicList, diff);
}

function patchList(staticList, dynamicList) {
  return dynamicList.forEach((d) => zip(staticList, d));
}

function patchDynamic(dynamicList, diff) {
  if (!diff) return dynamicList;
  for (const [index, value] of Object.entries(diff)) {
    if (typeof value === 'object' && !Array.isArray(value)) {
      dynamicList[index] = patch(dynamicList[index], value);
    } else {
      dynamicList[index] = value;
    }
  }
  return dynamicList;
}

function zip(staticList, dynamicList, diff) {
  let str = '';
  for (let i = 0; i < Math.max(staticList.length, dynamicList.length); i++) {
    str += `${staticList[i] ?? ''}${patch(dynamicList[i] ?? '', diff ? diff[i] : null)}`;
  }
  return str;
}
