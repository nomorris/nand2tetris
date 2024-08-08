// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/09/Square/Square.jack

/** Implements a graphical square.
    The square has top-left x and y coordinates, and a size. */
class RandomGrid {

   field int seed; // Random seed given at startup

   function Array RandomGrid(int seed) {
      let x = Array.new(81);
      let i = 0;
      let r = seed;
      let arr
      while (i < 81) {
         let r = next(r);
         x[i] = mod(r, 9);
         i = i + 1;
      }
      while (i > 40) {
         let r = next(r);
         x[mod(r, 81)] = 0;
         i = i - 1;
      }
      return x;
   }
   
   function int next(int r) {
      let s = mod(241 * r + 239, 256) 
   }
   
   function int mod(int a, int b) {
      let r = a - (b * (a / b));
      return r;
   }
   
   function Array Instantiate() {
       let q = 0;
       let l = Array.new(81);
       while (q < 81) {
          l[q] = mod()
       }
   }
}
