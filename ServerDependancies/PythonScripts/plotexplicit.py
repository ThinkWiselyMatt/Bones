import matplotlib.pyplot as plt


def plot_sine_wave():
    fig = plt.figure()
    ax = fig.subplots()
    ax.plot([1, 2, 3, 4], [0, 0.5, 1, 0.2])
    plt.savefig("explicit.png")

if __name__ == "__main__":
    plot_sine_wave()
