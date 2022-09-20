import matplotlib.pyplot as plt


def reward(n):
    return 18000000 - logOfXInBaseB(n, 2) * 1000000

def logOfXInBaseB(n, b):
    if n < b:
        return 0
    else:
        return 1 + logOfXInBaseB(n // b, b)


def amount(n):
    r = reward(n)
    if r < 1000000:
        return 0
    else:
        return r


if __name__ == "__main__":
    maximum = 524268
    total = 0
    n = 5
    for i in range(262143+1):
        # print(i, amount(i))
        total += amount(i)
        if i % pow(10,n-2) == 0:
            plt.scatter(i, amount(i)/pow(10,6), c='k')
        if i in [pow(10, i) for i in range(n+1)]:
            print(i, total/pow(10,6), total/pow(10,6)/maximum)
    #print(total/pow(10,6), total,maximum)
    plt.show()
