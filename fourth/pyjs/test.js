const {PyVal} = require(".");

const np = PyVal.imp("numpy");
const plt = PyVal.imp("matplotlib.pyplot");

const mat = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
console.log(np.invert(mat));

const xs = np.arange(0, 20, 0.01);
const ys = np.sin(xs);

plt.plot(xs, ys);
plt.show();
