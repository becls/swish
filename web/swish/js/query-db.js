// Copyright 2018 Beckman Coulter, Inc.
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

function onLoad() {
  var offsetInput = document.getElementById('offsetInput');
  if (offsetInput) {
    offsetInput.value = '';
    updateButton();
    offsetInput.onkeypress = filterKeys;
    offsetInput.onkeyup = updateButton;
  }
  var rowForm = document.getElementById('rowForm');
  if (rowForm) {
    rowForm.onsubmit = rowToOffset;
  }
}

//Allow 'Enter' key for form submission and digits 0-9, but discard other keys
function filterKeys(event) {
  if (event.which == 13) {
    return;
  } else if (event.which < 48 || event.which > 57) {
    event.preventDefault();
  }
}

function updateButton() {
  if (Number(document.getElementById('offsetInput').value)) {
    document.getElementById('offsetButton').removeAttribute('disabled');
  } else {
    document.getElementById('offsetButton').setAttribute('disabled', 'true');
  }
}

function rowToOffset() {
  //Prevent going back to offset 0 when there is no input row
  var offsetInput = document.getElementById('offsetInput');
  var row = Number(offsetInput.value)
  if (!row || row > Number.MAX_SAFE_INTEGER) {
    event.preventDefault();
    return;
  }
  offsetInput.value = Math.max(0, row - 1);
  return true;
}

document.onreadystatechange = onLoad;
