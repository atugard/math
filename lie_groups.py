import numpy as np 
import math 
import matplotlib.pyplot as plt 
import matplotlib.gridspec as gridspec
from matplotlib.animation import FuncAnimation

x,y = np.meshgrid(np.linspace(-10,10,10), np.linspace(-10,10,10))
id_m = np.array([[1, 0], 
                 [0, 1]])

def elliptical_matrix(a,b):
    return lambda t: math.exp(a*t) * np.array([[math.cos(b*t), -math.sin(b*t)], 
                                               [math.sin(b*t), math.cos(b*t)]])
def hyperbolic_matrix(a,b):
    return lambda t: math.exp(a*t) * np.array([[math.cosh(b*t), math.sinh(b*t)], 
                                               [math.sinh(b*t), math.cosh(b*t)]])
def parabolic_matrix(a,b):
    return lambda t: math.exp(a*t) * np.array([[1  , 0], 
                                               [b*t, 1]])
def scalar_matrix(a,b):
    return lambda t: math.exp(a*t) * np.array(id_m)

def dotEval(m,w):
    return lambda t: np.dot(m(t),w)

def morphGrid(t,x,y, matrix):
    new_x = []
    new_y = []
    for (a,b) in zip(x,y):
        for (c,d) in zip (a,b):
            v = np.dot(matrix(t), np.array([c,d]))
            new_x.append(v[0])
            new_y.append(v[1])
    new_x = np.array(new_x).reshape(x.shape)
    new_y = np.array(new_y).reshape(y.shape)
    return [new_x,new_y]

####################################################################

u,v = morphGrid(0,x,y, parabolic_matrix(1,2))
fig, ax = plt.subplots()
strm = ax.streamplot(x, y, u, v, color=u, linewidth=2, cmap='autumn')
#fig.colorbar(strm.lines)

def animate(i):
    ax.collections = [] # clear lines streamplot
    ax.patches = [] # clear arrowheads streamplot
    u,v = morphGrid(i*.1,x,y, parabolic_matrix(3,1))
    strm = ax.streamplot(x, y, u, v, color=u, linewidth=2, cmap='autumn')
    return strm

ani = FuncAnimation(fig, animate, frames = 200, interval=500, blit=False, repeat=True)
plt.show()

####################################################################
