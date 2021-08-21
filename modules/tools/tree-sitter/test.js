// this is a test file i am useing to test functionallity with treesitter

// blocks
x = 10

// calls
call();

// class
class Car {
  constructor(name, year) {
    this.name = name;
    this.year = year;
  }
  age(x) {
    return x - this.year;
  }
}

// comments
/*
 * multiline my dude
 * a poem
 * roses are red
 * violets are blue
 * there is a man behind you
 * and he had an axe with him to
 */


// conditional
if (x == 1) {
  doStuff();
} else {
  x = new Car.age(2020)
}


// function
function name(arg) {
  doStuff(arg);
}

const arrowFunc = arg => {
  dosStuff(arg);
}

// loop
for (let i = 0; i <= 10; i++) {
  doStuff(i);
}
// param
