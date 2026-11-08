import sys
import time
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

def main(ext="png"):
    x = np.linspace(0, 20, 5000)
    y1 = np.sin(x)*np.exp(-0.1*x)
    y2 = np.cos(2*x)*np.exp(-0.05*x)
    y3 = 0.5*np.sin(0.5*x)
    plt.plot(x, y1, "b-", label="damped sin", lw=2)
    plt.plot(x, y2, "r--", label="damped cos")
    plt.plot(x, y3, "g-", label="slow sin")
    plt.fill_between(x, y1, y2, alpha=0.2)
    plt.title("Complex benchmark")
    plt.xlabel("time")
    plt.ylabel("amplitude")
    plt.grid(True)
    plt.legend()
    plt.savefig("complex_py." + ext)

if __name__ == "__main__":
    t0 = time.perf_counter()
    main(*sys.argv[1:2])
    print(f"RUN_SECONDS {time.perf_counter() - t0:.6f}")
