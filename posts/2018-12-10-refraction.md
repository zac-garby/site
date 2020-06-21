---
title: Simulating Refraction
banner: /images/refraction-short.png
---

When a ray of light comes into contact with a new medium (a block of glass, for example,) it will change its direction and speed. This is because the wave has a width and as it reaches the boundary of two mediums one side of the ray will start to move faster or slower than it was before (okay, it doesn't *actually* happen because of this, but it's a good explanation). This effect is called refraction.

If the angle to the normal (called the angle of incidence, or $\theta_1$) is large enough, the ray will actually not go through the boundary at all but will instead bounce off as if it were a mirror. This effect is known as total internal reflection, or TIR.

Refraction follows the equation $n_1 sin(\theta_1) = n_2 sin(\theta_2)$, where $n_1$ and $n_2$ are the refractive indices of each medium and $\theta_1$ and $\theta_2$ are the angles of incidence and refraction.

A refractive index is a measure of how slow light moves in a medium, compared to the speed in a vacuum. This is typically a small number, around 2, but never smaller than 1 because light can't go faster than it does in a vacuum.

At first, it may seem like an easy task to simulate refraction on a computer. After all, you just need to work out some angles right? You won't be saying that after you've read this! (Wait, is that a good thing?)

# Finding the intersection

Before we can even find the direction of a refracted ray, it makes sense to find where, and even if, the ray actually intersects the boundary. To do this, the ray and the boundary should be represented as line equations. Since we're working with vectors, it makes sense to use the vector form of lines.

$$ \vec{i_{s}} = \begin{bmatrix} i_x \\ i_y \end{bmatrix}, \vec{l_{s}} = \begin{bmatrix} l_x \\ l_y \end{bmatrix} $$
$$ \vec{i_{d}} = \begin{bmatrix} i_\alpha \\ i_\beta \end{bmatrix}, \vec{l_{d}} = \begin{bmatrix} l_\alpha \\ l_\beta \end{bmatrix} $$
$$ i: \vec{r} = \vec{i_{s}} + \lambda \vec{i_{d}}, \lambda \gt 0 $$
$$ l: \vec{r} = \vec{l_{s}} + \mu \vec{l_{d}}, \mu_1 \leq \mu \leq \mu_2 $$

Here, $i$ is a line representing the incident ray and $l$ is a line representing the boundary it will intersect with. $\vec{r}$ is a variable representing a generic point on either line. $\lambda > 0$ means that the line $i$ only exists in one direction, or in other words it's a *ray*. $\mu_1 \leq \mu \leq \mu_2$ means that the line $l$ only exists between two points, meaning that it is a line segment.

To find where these two lines intersect, we should first ignore the conditions making them a ray and a line segment and instead consider them both infinite lines in each direction. Then, the two $\vec{r}$ values can be equated and solved for values of $\lambda$ and $\mu$.

$$ \vec{i_{s}} + \lambda \vec{i_{d}} = \vec{l_{s}} + \mu \vec{l_{d}} \Rightarrow \begin{cases} i_x + \lambda i_\alpha = l_x + \mu i_\alpha \\ i_y + \lambda i_\beta = l_y + \mu i_\beta \end{cases} \Rightarrow \begin{cases} \lambda = -\frac{l_\alpha(l_y - r_y) + l_\beta(r_x - l_x)}{r_\alpha l_\beta - l_\alpha r_\beta} \\ \mu = -\frac{r_\alpha(r_y - l_y) + r_\beta(l_x - r_x)}{r_\alpha l_\beta - l_\alpha r_\beta} \end{cases} $$

Yikes. And, no, I didn't solve that myself. That's what Wolfram Alpha is for :) So now we have the values of $\lambda$ and $\mu$ where the two lines intersect, we can check if they lie in the domains of the lines. This will be the case if and only if $\lambda > 0, \mu_1 \leq \mu \leq \mu_2$.

# Finding the refracted vector

One we know if and where the ray and the line intersect, we can find a vector line equation of the refracted ray. This ray can be written as:

$$ \vec{\rho_{s}} = \begin{bmatrix} \rho_x \\ \rho_y \end{bmatrix}, \vec{\rho_{d}} = \begin{bmatrix} \rho_\alpha \\ \rho_\beta \end{bmatrix} $$
$$ \rho: \vec{r} = \vec{\rho_s} + \nu \vec{\rho_d}, \nu \gt 0 $$

Where $\rho$ is the line representing the refracted ray (I would have used $r$ but that variable is already used...) This is a very similar format to the two other lines, the only difference being the name of the scalar argument is $\nu$ instead of $\lambda$ or $\mu$.

Now how can we find out the parameters $\vec{\rho_s}$ and $\vec{\rho_d}$?

I decided to go with a rather elegant approach using matrices. The high level plan is to change the basis of each of the vectors being used into a new basis $B$ such that:
        
$$ \vec{l_d}\ =\ \begin{bmatrix} 0 \\ 1 \end{bmatrix}_B $$
        
And the vector perpendicular to $\vec{l_d}$, which I'll call $\vec{l_p}$:
        
$$ \vec{l_p}\ =\ \begin{bmatrix} 1 \\ 0 \end{bmatrix}_B $$

This transformation will keep any angles the same luckily, since the two basis vectors map to another set of two perpendicular vectors.
        
Another important observation to make is that the position vectors for each line are irrelevant in the calculation of the refraction vector because refraction will work exactly the same at any point on the line.

To change the basis of the vectors, a change of basis matrix must be constructed. The first column will be the vector which should map to the unit vector parallel to the y-axis and the second column will be the vector which should map to the unit vector parallel to the x-axis. In this case, the change of basis matrix will look like this:

$$ B = \begin{bmatrix} -l_\beta && l_\alpha \\ l_\alpha && l_\beta \end{bmatrix} $$

Using this matrix, a vector can be transformed to and from basis $B$ by the following operations, where $[\vec{v}]_B$ denotes the vector $\vec{v}$ transformed to basis $B$ and $B^{-1}$ is the inverse of matrix $B$. Note that $B$ must have an inverse since $\vec{l_d}$ and $\vec{l_p}$ are linearly independent.

$$ [\vec{v}]_B = B^{-1}\vec{v} $$
$$ \vec{v} = B [\vec{v}]_B $$

The first thing we need to do with this change of basis vector is convert $\vec{i_d}$ to basis $B$:

$$ [\vec{i_d}]_B = B^{-1} \vec{i_d} $$

Remember we want to use the equation $n_1 sin\theta_1 = n_2 sin\theta_2$. Since we are trying to find the vector of the refracted ray, this will be more useful in the form $\theta_2 = asin(\frac{n_1}{n_2} sin\theta_1)$ (where $asin$ is the inverse of the $sin$ function) because if we have the angle we can easily find the refracted vector using some basic trigonometry.

$\theta_1$ is the angle between the direction of the incident ray $[\vec{i}]_B$ and the x-axis, meaning the inverse tan function can be used to calculate $\theta_1$.

$$ \theta_1 = atan\frac{i_{B\beta}}{i_{B\alpha}} $$

The result of doing this is that it will always be between $0$ and $\frac{\pi}{2}$.

Now an expression can be formed for $\theta_2$ using $\theta_2 = asin(\frac{n_1}{n_2} sin\theta_1)$ and the fact that $sin(atan(x)) = \frac{x}{\sqrt{1 + x^2}}$. First, two variables $R$ and $X$ are defined which will be used in the future and are just to save time writing out the same thing again and again.

$$ R = \frac{i_{B\beta}}{i_{B\alpha}} $$
$$ X = \frac{n_1}{n_2} \frac{R}{1 + R^2} $$

At this point, $\theta_2$ could be calculated with $asin(X)$, but one thing has to be done beforehand. The issue is that $asin(X)$ has a domain of $|X| \leq 1$, however nothing is stopping the value of $X$ from being out of that domain. The reason for this is quite cool actually &mdash; if $|X| > 1$, then total internal reflection should take place! Remember I mentioned this earlier. TIR is when the light will actually reflect off of the boundary instead of refracting through it.

For this edge case, the value of the refraction vector in basis $B$ is actually much easier to calculate than it would be otherwise. It is just the incident ray in basis $B$ reflected across the y-axis (it is reflected in the y-axis as opposed to the x-axis because the change of basis matrix maps the boundary line to the north-pointing vector). So, if TIR happens then $[\vec{\rho_d}]_B$ can be calculated with the following equation.

$$ |X| > 1 \implies  [\vec{\rho_d}]_B = \begin{bmatrix} -1 && 0 \\ 0 && 1 \end{bmatrix} \vec{i_d}_B = \begin{bmatrix} -i_{B\alpha} \\ i_{B\beta} \end{bmatrix}_B $$

If TIR doesn't happen and instead the ray refracts through the boundary, more trigonometry must be used. The $x$ and $y$ components of $[\vec{\rho_d}]_B$ are calculated using the sine and cosine of $\theta_2$, since they are the opposite and adjacent sides of a right-triangle of angle $\theta_2$ in the direction of the refracted ray. But first, $\theta_2$ must be calculated.

$$ \theta_2 = asin(X) $$
$$ |X| \leq 1 \implies [\vec{\rho_d}]_B \begin{bmatrix} sign(i_{B\alpha}) * cos(\theta_2) \\ sign(i_{B\beta}) * sin(\theta_2) \end{bmatrix}_B $$

The reason that the $sign(..)$ function calls are there is because after refraction the signs of the $x$ and $y$ components of the ray are unchanged.

Using the identity $cos(asin(X)) = \sqrt{1 - X^2}$ and the fact that $asin$ and $sin$ are inverse functions, this can be rewritten as:

$$ |X| \leq 1 \implies [\vec{\rho_d}]_B = \begin{bmatrix} sign(i_{B\alpha}) * \sqrt{1 - X^2} \\ sign(i_{B\beta}) * X \end{bmatrix}_B $$

Combining the conditional cases into a piecewise definition, this can also be written (more elegantly in my opinion) like:

$$ [\vec{\rho_d}]_B = \begin{cases} \begin{bmatrix} -i_{B\alpha} \\ i_{B\beta} \end{bmatrix}_B, |X| > 1 \\ \begin{bmatrix} sign(i_{B\alpha}) * \sqrt{1 - X^2} \\ sign(i_{B\beta}) * X \end{bmatrix}_B, |X| \leq 1 \end{cases} $$

It should become obvious now why changing the basis was a very useful thing to do. There is no mention of $\vec{l_d}$ anywhere in the expression for $[\vec{\rho_d}]_B$ at all, making the maths much cleaner and easy to work with. And now we're done with basis $B$, we can easily transform back to the standard basis:

$$ \vec{\rho_d} = B [\vec{\rho_d}]_B $$

Expanding this equation out gives:

$$ \vec{\rho_d} = \begin{bmatrix} -l_\beta && l_\alpha \\ l_\alpha && l_\beta \end{bmatrix} \begin{cases} \begin{bmatrix} -i_{B\alpha} \\ i_{B\beta} \end{bmatrix}_B, |\frac{n_1}{n_2} \frac{\frac{i_{B\beta}}{i_{B\alpha}}}{1 + (\frac{i_{B\beta}}{i_{B\alpha}})^2}| > 1 \\ \begin{bmatrix} sign(i_{B\alpha}) * \sqrt{1 - \frac{n_1}{n_2} \frac{\frac{i_{B\beta}}{i_{B\alpha}}}{1 + (\frac{i_{B\beta}}{i_{B\alpha}})^2}^2} \\ sign(i_{B\beta}) * \frac{n_1}{n_2} \frac{\frac{i_{B\beta}}{i_{B\alpha}}}{1 + (\frac{i_{B\beta}}{i_{B\alpha}})^2} \end{bmatrix}_B, |\frac{n_1}{n_2} \frac{\frac{i_{B\beta}}{i_{B\alpha}}}{1 + (\frac{i_{B\beta}}{i_{B\alpha}})^2}| \leq 1 \end{cases} $$

Pretty cool, right? ... no? Well anyway, that's the equation we can now use to calculate the direction of a refracted ray given the various properties of the situation. I'll use a proper matrix library in the real program, though, and calculate some of it ahead of time to save processing time, but it's essentially just that.

# Outcome

I've implemented this in Go, [here](https://github.com/zac-garby/refraction). Here's a screenshot, showing normal refracted rays as well as one which undergoes total internal reflection.

![](/images/refraction.png)
