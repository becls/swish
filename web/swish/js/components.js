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

function AddEvent(elem, type, eventHandle)
{
  if (elem == null || elem == undefined) return;

  if (elem.addEventListener) {
    elem.addEventListener(type, eventHandle, false);
  } else if ( elem.attachEvent ) {
    elem.attachEvent("on" + type, eventHandle);
  }
};

var FunctionStack = new Array();
var IsOnLoadingSet = false;
function OnLoad(func)
{
  FunctionStack.push(func);

  if (IsOnLoadingSet)
    return;

  IsOnLoadingSet = true;
  AddEvent(window, "load", function(){ DoOnLoad()});
}

function DoOnLoad()
{
  for (x in FunctionStack)
  {
    func = FunctionStack[x];
    func();
  }
}

function SetWidth()
{
  if (document.body && document.body.offsetWidth) {
    winW = document.body.offsetWidth;
    winH = document.body.offsetHeight;
  }
  if (document.compatMode=='CSS1Compat' &&
      document.documentElement &&
      document.documentElement.offsetWidth ) {
    winW = document.documentElement.offsetWidth;
    winH = document.documentElement.offsetHeight;
  }
  if (window.innerWidth && window.innerHeight) {
    winW = window.innerWidth;
    winH = window.innerHeight;
  }

  document.body.style.width=winW-28+"px";
  document.body.style.height=winH +"px";
}

OnLoad(SetWidth);
AddEvent(window, "resize", function() { SetWidth(); } );
