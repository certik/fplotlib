import sys
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

def main(ext="png"):
    x = np.linspace(0, 4*np.pi, 200)
    plt.plot(x, np.sin(x)*np.exp(-0.1*x), "b-")
    plt.savefig("simple_py." + ext)

if __name__ == "__main__":
    main(*sys.argv[1:2])
