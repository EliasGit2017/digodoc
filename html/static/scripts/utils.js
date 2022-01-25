/*
    Bunch of usefull JS functions to do stuff that can be done with JQuery / ReactJS one liners
    Ocaml JQuery could be sooo usefull 
*/

/*********************************************************************************************************/

/*Fade in transition : follow inline comments to make a fade out transition*/
var el = document.getElementById("div1");

function fadeIn(el, time) {
  el.style.opacity = 0; /* 1 */

  var last = +new Date();
  var tick = function() {
    el.style.opacity = +el.style.opacity + (new Date() - last) / time; /* el.style.opacity -  (new ... */
    last = +new Date();

    if (+el.style.opacity < 1) { /* > 0 */
      (window.requestAnimationFrame && requestAnimationFrame(tick)) || setTimeout(tick, 16);
    }
  };

  tick();
}

fadeIn(el, 3000); /*3000 -> time to display*/

/*********************************************************************************************************/

<script>

function fadeIn(time) {
  var el = document.getElementById("this");
  el.style.opacity = 0;
  el.style.display = "block"; /*not so smooth*/

  var last = +new Date();
  var tick = function () {
    el.style.opacity = +el.style.opacity + (new Date() - last) / time;
    last = +new Date();

    if (+el.style.opacity < 1) {
      (window.requestAnimationFrame && requestAnimationFrame(tick)) || setTimeout(tick, 16);
    }
  };

  tick();
}

function fadeOut(time) {
  var el = document.getElementById("this");
  el.style.opacity = 1;

  var last = +new Date();
  var tick = function () {
    el.style.opacity = +el.style.opacity - (new Date() - last) / time;
    last = +new Date();

    if (+el.style.opacity > 0) {
      (window.requestAnimationFrame && requestAnimationFrame(tick)) || setTimeout(tick, 16);
    }
    else {
      el.style.display = "none"; /*kinda okay*/
    }
  };

  tick();
}

// let box = document.getElementById('this'),
//     btn = document.getElementById('fpattern_fulltext');

// btn.addEventListener('click', function () {

//   if (box.classList.contains('myhidden')) {
//     box.classList.remove('myhidden');
//     setTimeout(function () {
//       box.classList.remove('visuallyhidden');
//     }, 20);
//   } else {
//     box.classList.add('visuallyhidden');
//     box.addEventListener('transitionend', function (e) {
//       box.classList.add('myhidden');
//     }, {
//       capture: false,
//       once: true,
//       passive: false
//     });
//   }

// }, false);
</script>