(function(a,b){"object"==typeof exports&&"undefined"!=typeof module?b(require("lodash")):"function"==typeof define&&define.amd?define(["lodash"],b):b(a._)})(this,function(a){"use strict";function b(a){var b=function(){for(var c=[],d=arguments.length;d--;)c[d]=arguments[d];return a.call.apply(a,[this,b].concat(c))};return b}a=a&&a.hasOwnProperty("default")?a["default"]:a,a.mixin({combinations:function c(d,e){d=a.values(d);var c=[];return b(function(b,f,g){f.length<e?a.find(d,function(c,d){b(a.concat(f,[c]),d+1)},g):c.push(f)})([],0),c}})});
