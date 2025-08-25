/**
 * Sanitizes user input before logging.
 * Strips all control characters, excessive whitespace, and line breaks.
 * @param {*} value - The value to sanitize
 * @returns {string} - The sanitized string safe for logging
 */
export function sanitizeForLog(value) {
  if (typeof value !== 'string') value = String(value);
  // Remove all Unicode line and paragraph separators, tabs, and ASCII control chars (excluding \x20 space)
  return value
    .replace(/[\r\n\u2028\u2029\t\v\f\b\0\x1B]/g, '')
    .replace(/[\x00-\x1f\x7f]/g, '') // further strip ASCII control chars
    .trim();
}
