"use strict";

exports._writeText = function _writeText({ navigator, text }) {
  navigator.clipboard.writeText(text);
}
