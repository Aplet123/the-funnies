const {PyVal} = require(".");

const np = PyVal.imp("numpy");
const plt = PyVal.imp("matplotlib.pyplot");

const xs = np.arange(0, 20, 0.01);
const ys = np.sin(xs);

plt.plot(xs, ys);
plt.show();

setTimeout(() => {}, 5000);
