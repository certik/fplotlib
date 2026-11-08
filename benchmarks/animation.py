import sys
import time
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.animation import FuncAnimation, PillowWriter


def main(ext="gif"):
    n_frames = 40
    x = np.linspace(0, 10, 240)
    envelope = np.exp(-0.15*x)
    fig, ax = plt.subplots()

    # Every frame is cleared and redrawn, like clf() + plot() in the Fortran program.
    def draw(frame):
        ax.clear()
        y = np.sin(2*x - 2*np.pi*frame/n_frames)*envelope
        ax.plot(x, y, "b-", lw=2)
        ax.fill_between(x, y, color="b", alpha=0.2)
        ax.set_xlim(0, 10)
        ax.set_ylim(-1.1, 1.1)
        ax.set_title("Travelling wave")
        ax.set_xlabel("x")
        ax.set_ylabel("displacement")
        ax.grid(True)

    anim = FuncAnimation(fig, draw, frames=n_frames)
    anim.save("animation_py." + ext, writer=PillowWriter(fps=20))


if __name__ == "__main__":
    t0 = time.perf_counter()
    main(*sys.argv[1:2])
    print(f"RUN_SECONDS {time.perf_counter() - t0:.6f}")
