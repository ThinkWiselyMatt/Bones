import matplotlib.pyplot as plt
import numpy as np

def plot_sine_wave():
    x = np.linspace(0, 10, 100)
    y = np.sin(x)
    plt.plot(x, y)
    plt.title("Sine Wave")
    plt.xlabel("x")
    plt.ylabel("sin(x)")
    plt.grid(True)
    plt.savefig("sine_wave.png")

if __name__ == "__main__":
    plot_sine_wave()
